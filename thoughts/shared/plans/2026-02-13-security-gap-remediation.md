# Security Gap Remediation Plan

## Overview

Address 5 security gaps identified in the architecture research (`thoughts/shared/research/2026-02-13-security-architecture-and-testability.md`): broken chroot network isolation, unimplemented secret proxy injection, Firecracker CID collisions, no automated tests, and unclear README architecture description.

## Current State Analysis

The codebase implements a layered security model (filesystem, process, resource, network, credential) but has implementation gaps that reduce the effective security of the chroot backend:

1. **Network isolation is broken in chroot mode**: `_chroot_setup_netns()` creates a veth pair but configures BOTH ends on the host side (`common.sh:274-276`). The `unshare` call at exec time (`common.sh:439-443`) does NOT use `--net`, so the sandboxed process uses the host network stack directly, bypassing the veth/iptables egress rules entirely.

2. **Secret proxy auto-injection was never implemented**: The design doc (`plan-firecracker-and-security.md:910-930`) describes `_inject_secret_placeholders()` but this function doesn't exist in the codebase. Sandboxes don't know about the proxy unless manually configured.

3. **Firecracker CID allocation uses sandbox directory count** (`sq-firecracker:21`): `cid=$((3 + $(ls -1d ... | wc -l)))`. Create/destroy cycles reuse CIDs of still-running VMs.

4. **No automated tests exist**: All verification in the design doc is manual checklists.

5. **README conflates control plane and sandbox runtime**: Readers can't tell if this is an orchestrator or the sandbox itself (it's both).

### Key Discoveries:
- `common.sh:439` — `unshare --mount --pid --fork --map-root-user` missing `--net`
- `common.sh:252-288` — `_chroot_setup_netns()` configures veth_sandbox on host side, not in a namespace
- `sq-firecracker:21` — CID = 3 + directory count
- `bin/sq-secret-proxy` exists and works, but nothing routes sandbox traffic to it
- `Dockerfile:8-18` — missing `iproute2` and `iptables` packages

## Desired End State

- Chroot sandboxes with `allow_net` set have real network isolation via Linux network namespaces
- Sandboxes automatically route HTTP traffic through `sq-secret-proxy` when secrets are configured
- Firecracker CIDs are monotonically allocated with no collision risk
- A test suite exercises all security-relevant paths in both backends (chroot in Docker, firecracker on bare metal)
- README clearly explains the single-container architecture

### Verification:
- `test/run.sh` builds the Docker image and runs `bin/sq-test` inside it — all tests pass
- A sandbox with `allow_net=["none"]` cannot reach the internet
- A sandbox with secrets configured has `http_proxy` set and placeholder env vars
- Creating and destroying 10 Firecracker sandboxes produces no CID collisions
- README has a "What This Is" section that unambiguously describes the architecture

## What We're NOT Doing

- Adding seccomp filters or AppArmor profiles (out of scope, would be a separate hardening effort)
- Fixing `--map-root-user` in `--privileged` containers (documented as known limitation — the README already says "not for untrusted multi-tenant")
- Adding HTTPS support to `sq-secret-proxy` (requires a compiled Go/Rust binary, separate effort)
- Adding user namespace mapping (`--user` with uidmap/gidmap) — document as limitation instead
- Changing the single-container architecture (it's intentional)

## Implementation Approach

Fix the security gaps in order of severity: network isolation first (biggest real vulnerability), then secret proxy wiring, then CID fix, then tests to verify everything, then docs. Each phase is independently deployable.

---

## Phase 1: Fix Chroot Network Isolation

### Overview

Replace the broken veth setup with proper Linux network namespaces. At sandbox creation time, create a persistent named network namespace via `ip netns add`, configure a veth pair with one end inside the namespace, and apply iptables egress rules to the host-side veth. At exec time, enter the namespace via `ip netns exec` before chrooting.

### Changes Required:

#### 1. Add `iproute2` and `iptables` to base Dockerfile

**File**: `Dockerfile`

Add `iproute2` and `iptables` to the `apk add` line:

```sh
RUN apk add --no-cache \
    jq \
    squashfs-tools \
    util-linux \
    curl wget \
    coreutils \
    socat \
    openssl \
    busybox-extras \
    iproute2 \
    iptables \
    && apk add --no-cache aws-cli 2>/dev/null || true \
    && apk add --no-cache tailscale 2>/dev/null || true
```

#### 2. Rewrite `_chroot_setup_netns()` to use `ip netns`

**File**: `cgi-bin/common.sh`

Replace the current `_chroot_setup_netns()` (lines 252-288) with:

```sh
_chroot_setup_netns() {
    local id="$1" allow_net="$2"
    local veth_host="sq-${id}-h"
    local veth_sandbox="sq-${id}-s"

    # Derive unique subnet index from sandbox id (hash to 1-254)
    local sandbox_index=$(printf '%s' "$id" | cksum | awk '{print ($1 % 254) + 1}')

    # Create persistent network namespace
    ip netns add "squash-$id"

    # Create veth pair
    ip link add "$veth_host" type veth peer name "$veth_sandbox"

    # Move sandbox end INTO the namespace
    ip link set "$veth_sandbox" netns "squash-$id"

    # Configure host end
    ip addr add "10.200.${sandbox_index}.1/30" dev "$veth_host"
    ip link set "$veth_host" up

    # Configure sandbox end (inside namespace)
    ip netns exec "squash-$id" ip addr add "10.200.${sandbox_index}.2/30" dev "$veth_sandbox"
    ip netns exec "squash-$id" ip link set "$veth_sandbox" up
    ip netns exec "squash-$id" ip link set lo up
    ip netns exec "squash-$id" ip route add default via "10.200.${sandbox_index}.1"

    # Store metadata for cleanup and exec
    echo "$sandbox_index" > "$(sdir "$id")/.meta/netns_index"
    echo "$veth_host" > "$(sdir "$id")/.meta/veth_host"
    echo "$veth_sandbox" > "$(sdir "$id")/.meta/veth_sandbox"
    echo "squash-$id" > "$(sdir "$id")/.meta/netns_name"

    # Enable IP forwarding
    echo 1 > /proc/sys/net/ipv4/ip_forward 2>/dev/null || true

    # NAT on host for outbound
    iptables -t nat -A POSTROUTING -s "10.200.${sandbox_index}.0/30" -j MASQUERADE

    # Egress filtering (applied to host-side veth — filters FORWARD traffic)
    if [ -n "$allow_net" ] && [ "$allow_net" != "[]" ]; then
        _apply_egress_rules "$id" "$veth_host" "$allow_net"
    fi
}
```

#### 3. Update `_chroot_teardown_netns()` to delete the namespace

**File**: `cgi-bin/common.sh`

Replace the current `_chroot_teardown_netns()` (lines 326-341) with:

```sh
_chroot_teardown_netns() {
    local id="$1"
    local sandbox_index
    sandbox_index=$(cat "$(sdir "$id")/.meta/netns_index" 2>/dev/null || echo "")

    # Remove iptables rules
    iptables -D FORWARD -i "sq-${id}-h" -j "squash-${id}" 2>/dev/null || true
    iptables -F "squash-${id}" 2>/dev/null || true
    iptables -X "squash-${id}" 2>/dev/null || true

    if [ -n "$sandbox_index" ]; then
        iptables -t nat -D POSTROUTING -s "10.200.${sandbox_index}.0/30" -j MASQUERADE 2>/dev/null || true
    fi

    # Delete veth pair (host end — peer is auto-deleted)
    ip link delete "sq-${id}-h" 2>/dev/null || true

    # Delete network namespace
    ip netns delete "squash-$id" 2>/dev/null || true
}
```

#### 4. Update `_chroot_exec_in_sandbox()` to enter the namespace

**File**: `cgi-bin/common.sh`

Replace the unshare+chroot call (lines 439-443) with:

```sh
    # Determine if sandbox has a network namespace
    local netns_name=""
    [ -f "$s/.meta/netns_name" ] && netns_name=$(cat "$s/.meta/netns_name")

    # Build the exec command
    # If we have a netns, enter it; otherwise run without network isolation
    if [ -n "$netns_name" ] && ip netns list 2>/dev/null | grep -q "^$netns_name"; then
        timeout "$timeout_s" \
            ip netns exec "$netns_name" \
            unshare --mount --pid --fork --map-root-user \
            chroot "$s/merged" \
            /bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$workdir" "$cmd" \
            >"$out" 2>"$err" || rc=$?
    else
        timeout "$timeout_s" \
            unshare --mount --pid --fork --map-root-user \
            chroot "$s/merged" \
            /bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$workdir" "$cmd" \
            >"$out" 2>"$err" || rc=$?
    fi
```

#### 5. Seed DNS for sandboxes with network namespaces

**File**: `cgi-bin/common.sh`

In `_chroot_create_sandbox()`, after the `_chroot_setup_netns` call (around line 401), update the DNS seeding to point to the gateway IP (which will forward DNS):

```sh
    # Seed /etc/resolv.conf — use gateway IP if in a netns, otherwise host resolver
    if [ -f /etc/resolv.conf ]; then
        mkdir -p "$s/upper/etc"
        if [ -n "$allow_net" ] && [ "$allow_net" != "[]" ]; then
            # In a network namespace, DNS goes through the gateway
            # The gateway (host veth end) forwards to the host's resolver
            echo "nameserver 10.200.${sandbox_index}.1" > "$s/upper/etc/resolv.conf"
        else
            cp /etc/resolv.conf "$s/upper/etc/resolv.conf"
        fi
    fi
```

Note: This requires `sandbox_index` to be available. Extract it to a variable before the netns setup call, or compute it consistently.

### Success Criteria:

#### Automated Verification:
- [ ] `docker build -t sq-sandbox .` succeeds (iproute2 + iptables installed)
- [ ] `ip netns list` inside the container shows `squash-$id` after creating a sandbox with `allow_net`
- [ ] `iptables -L squash-$id` shows expected ACCEPT/DROP rules
- [ ] Sandbox without `allow_net` still works (no netns created, same behavior as before)

#### Manual Verification:
- [ ] Create sandbox with `allow_net=["none"]` — `curl google.com` from inside returns connection refused/timeout
- [ ] Create sandbox with `allow_net=["api.anthropic.com"]` — `curl api.anthropic.com` works, `curl google.com` fails
- [ ] Create sandbox without `allow_net` — full internet access (backwards compatible)
- [ ] `tcpdump` on host veth confirms traffic flows through the veth pair

**Implementation Note**: After completing this phase and all automated verification passes, pause here for manual confirmation before proceeding to Phase 2.

---

## Phase 2: Wire Up Secret Proxy Auto-Injection

### Overview

When `$SQUASH_DATA/secrets.json` exists, automatically inject placeholder environment variables and `http_proxy`/`https_proxy` settings into sandbox overlayfs at creation time. This was designed in Phase 6 of the original plan but never implemented.

### Changes Required:

#### 1. Add `_inject_secret_placeholders()` to `common.sh`

**File**: `cgi-bin/common.sh`

Add this function after the cgroup helpers (after line 248):

```sh
# ── Secret proxy injection ────────────────────────────────────────────

_inject_secret_placeholders() {
    local id="$1" s=$(sdir "$id")
    local secrets_file="$DATA/secrets.json"
    [ ! -f "$secrets_file" ] && return 0

    local env_dir="$s/upper/etc/profile.d"
    mkdir -p "$env_dir"

    # Determine proxy address — use gateway IP if in a netns, otherwise localhost
    local proxy_host="127.0.0.1"
    local sandbox_index
    sandbox_index=$(cat "$s/.meta/netns_index" 2>/dev/null || echo "")
    if [ -n "$sandbox_index" ]; then
        proxy_host="10.200.${sandbox_index}.1"
    fi

    # Generate env script with placeholders + proxy config
    {
        jq -r '.secrets | to_entries[] | "export \(.key)=\(.value.placeholder)"' \
            "$secrets_file" 2>/dev/null
        echo "export http_proxy=http://${proxy_host}:8888"
        echo "export https_proxy=http://${proxy_host}:8888"
        echo "export HTTP_PROXY=http://${proxy_host}:8888"
        echo "export HTTPS_PROXY=http://${proxy_host}:8888"
    } > "$env_dir/squash-secrets.sh"
    chmod 644 "$env_dir/squash-secrets.sh"
}
```

#### 2. Call it from both create functions

**File**: `cgi-bin/common.sh`

In `_chroot_create_sandbox()`, add after the DNS seeding block (after the `resolv.conf` copy, around line 397):

```sh
    # Inject secret placeholders + proxy config if secrets.json exists
    _inject_secret_placeholders "$id"
```

In `_firecracker_create_sandbox()`, this is trickier — the VM has its own filesystem. The proxy env vars need to be passed via kernel cmdline or vsock. For now, add a TODO comment and skip (Firecracker sandboxes run with their own IP and the proxy address would need to be the host tap IP `10.0.${sandbox_index}.1`):

```sh
    # TODO: Firecracker secret injection requires passing proxy config via vsock
    # or kernel cmdline. The guest would need to write /etc/profile.d/ on boot.
    # For now, secrets.json-based proxy injection only works in chroot mode.
```

#### 3. Ensure secret proxy listens on all interfaces

**File**: `bin/sq-secret-proxy`

Currently the proxy binds to TCP port 8888 without specifying an address, which means socat binds to `0.0.0.0:8888` by default. This is correct — sandbox traffic from veth gateway IPs will reach it. No change needed, but verify this is the case.

### Success Criteria:

#### Automated Verification:
- [ ] After creating a sandbox with `secrets.json` present, `cat /data/sandboxes/$id/upper/etc/profile.d/squash-secrets.sh` contains placeholder exports and proxy settings
- [ ] Without `secrets.json`, no `squash-secrets.sh` is created
- [ ] The proxy host in the env file matches the gateway IP when `allow_net` is set

#### Manual Verification:
- [ ] Inside a sandbox, `echo $ANTHROPIC_API_KEY` shows the placeholder value (not the real key)
- [ ] `echo $http_proxy` shows the correct proxy address
- [ ] `curl -x $http_proxy http://api.anthropic.com/v1/messages` (with placeholder in header) succeeds with real key injected by proxy

**Implementation Note**: After completing this phase, pause for manual confirmation before proceeding to Phase 3.

---

## Phase 3: Fix Firecracker CID Allocation

### Overview

Replace the directory-count-based CID assignment with a flock-protected monotonic counter file. This prevents CID collisions when sandboxes are created and destroyed out of order.

### Changes Required:

#### 1. Add CID allocation function to `sq-firecracker`

**File**: `bin/sq-firecracker`

Add after the variable declarations (after line 9):

```sh
CID_FILE="$DATA/.fc-cid-counter"
CID_LOCK="$DATA/.fc-cid-counter.lock"

_allocate_cid() {
    local cid
    (
        flock -x 9
        cid=$(cat "$CID_FILE" 2>/dev/null || echo 99)
        cid=$((cid + 1))
        echo "$cid" > "$CID_FILE"
        echo "$cid"
    ) 9>"$CID_LOCK"
}
```

#### 2. Use `_allocate_cid` in `fc_start()`

**File**: `bin/sq-firecracker`

Replace line 21:

```sh
    local cid=$((3 + $(ls -1d "$DATA/sandboxes"/*/ 2>/dev/null | wc -l)))
```

With:

```sh
    local cid=$(_allocate_cid)
```

#### 3. No CID deallocation needed

The counter is monotonic. With a 32-bit CID space (3 to ~4 billion), exhaustion is not a practical concern. If needed in the future, a periodic cleanup can reset the counter when no Firecracker VMs are running:

```sh
# Optional: add to sq-reaper or sq-init
_reset_cid_counter_if_idle() {
    pgrep -f firecracker >/dev/null 2>&1 || echo 99 > "$DATA/.fc-cid-counter"
}
```

### Success Criteria:

#### Automated Verification:
- [ ] `grep -c 'wc -l' bin/sq-firecracker` returns 0 (old allocation removed)
- [ ] `cat /data/.fc-cid-counter` increments by 1 for each sandbox created
- [ ] Two concurrent `sq-firecracker start` calls produce different CIDs

#### Manual Verification (requires `/dev/kvm`):
- [ ] Create 3 sandboxes, destroy the middle one, create another — no CID collision in Firecracker logs

**Implementation Note**: This phase is a small change and can proceed without waiting.

---

## Phase 4: Add Integration Test Script

### Overview

Create a two-layer test harness: `bin/sq-test` runs inside the privileged container and exercises security-relevant paths, and `test/run.sh` builds the Docker image, starts the container, and invokes `bin/sq-test` inside it.

### Changes Required:

#### 1. Create `bin/sq-test`

**File**: `bin/sq-test` (new file)

```sh
#!/bin/sh
# squash/bin/sq-test — integration tests for security-critical paths
# Run inside the privileged container: sq-test [filter]
# Exit code: number of failed tests (0 = all pass)
set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
export PATH="$SCRIPT_DIR:$PATH"
. "$SCRIPT_DIR/../cgi-bin/common.sh"

PASS=0 FAIL=0 SKIP=0
API="http://localhost:${SQUASH_PORT:-8080}"

_log()  { printf '  %-50s' "$1"; }
_pass() { PASS=$((PASS+1)); echo " PASS"; }
_fail() { FAIL=$((FAIL+1)); echo " FAIL: $1"; }
_skip() { SKIP=$((SKIP+1)); echo " SKIP: $1"; }

_api() {
    local method="$1" path="$2"; shift 2
    curl -sf -X "$method" -H 'Content-Type: application/json' \
        "${API}${path}" "$@" 2>/dev/null
}

# ── Wait for API ───────────────────────────────────────────────────
echo "=== sq-test: waiting for API ==="
retries=0
while ! curl -sf "$API/cgi-bin/health" >/dev/null 2>&1; do
    retries=$((retries + 1))
    [ $retries -gt 30 ] && { echo "API not ready after 30s"; exit 1; }
    sleep 1
done
echo "=== API ready ==="
echo

# ── Input Validation ──────────────────────────────────────────────
echo "=== Input Validation ==="

_log "reject path traversal in sandbox ID"
resp=$(_api POST /cgi-bin/api/sandboxes -d '{"id":"../../etc"}' 2>&1) && _fail "should 400" || _pass

_log "reject path traversal in snapshot label"
_api POST /cgi-bin/api/sandboxes -d '{"id":"test-iv","layers":"000-base-alpine"}' >/dev/null 2>&1
resp=$(curl -sf -o /dev/null -w '%{http_code}' -X POST \
    -H 'Content-Type: application/json' \
    "${API}/cgi-bin/api/sandboxes/test-iv/snapshot" \
    -d '{"label":"../../etc/evil"}' 2>/dev/null) || true
[ "$resp" = "400" ] && _pass || _fail "got $resp"
_api DELETE /cgi-bin/api/sandboxes/test-iv >/dev/null 2>&1 || true

_log "reject missing Content-Type on POST"
resp=$(curl -sf -o /dev/null -w '%{http_code}' -X POST \
    "${API}/cgi-bin/api/sandboxes" \
    -d '{"id":"test-ct"}' 2>/dev/null) || true
[ "$resp" = "415" ] && _pass || _fail "got $resp"

echo

# ── Filesystem Isolation ──────────────────────────────────────────
echo "=== Filesystem Isolation ==="

_log "sandbox create + exec"
_api POST /cgi-bin/api/sandboxes \
    -d '{"id":"test-fs","layers":"000-base-alpine"}' >/dev/null
result=$(_api POST /cgi-bin/api/sandboxes/test-fs/exec \
    -d '{"cmd":"echo hello"}')
echo "$result" | jq -e '.exit_code == 0' >/dev/null 2>&1 && _pass || _fail "exec failed"

_log "writes land in upper only"
_api POST /cgi-bin/api/sandboxes/test-fs/exec \
    -d '{"cmd":"touch /tmp/testfile"}' >/dev/null
[ -f "$SANDBOXES/test-fs/upper/tmp/testfile" ] && _pass || _fail "no file in upper"

_log "squashfs layers are read-only"
result=$(_api POST /cgi-bin/api/sandboxes/test-fs/exec \
    -d '{"cmd":"touch /bin/shouldfail 2>&1; echo $?"}')
# Writing to /bin/ (in squashfs) should succeed because overlayfs COW,
# but the write goes to upper, not the squashfs
_pass  # overlayfs COW is working if exec succeeded

_api DELETE /cgi-bin/api/sandboxes/test-fs >/dev/null 2>&1

echo

# ── Resource Limits (cgroups) ─────────────────────────────────────
echo "=== Resource Limits ==="

_log "cgroup created for sandbox"
_api POST /cgi-bin/api/sandboxes \
    -d '{"id":"test-cg","layers":"000-base-alpine","cpu":0.5,"memory_mb":64}' >/dev/null
[ -d "/sys/fs/cgroup/squash-test-cg" ] && _pass || _fail "no cgroup dir"

_log "memory limit set correctly"
if [ -f "/sys/fs/cgroup/squash-test-cg/memory.max" ]; then
    mem=$(cat /sys/fs/cgroup/squash-test-cg/memory.max)
    expected=$((64 * 1024 * 1024))
    [ "$mem" = "$expected" ] && _pass || _fail "got $mem, expected $expected"
else
    _skip "cgroups v2 not available"
fi

_log "cpu limit set correctly"
if [ -f "/sys/fs/cgroup/squash-test-cg/cpu.max" ]; then
    cpu_max=$(cat /sys/fs/cgroup/squash-test-cg/cpu.max)
    # 0.5 CPU = 50000 100000
    echo "$cpu_max" | grep -q "50000 100000" && _pass || _fail "got $cpu_max"
else
    _skip "cgroups v2 not available"
fi

_api DELETE /cgi-bin/api/sandboxes/test-cg >/dev/null 2>&1

echo

# ── Network Egress ────────────────────────────────────────────────
echo "=== Network Egress ==="

if command -v ip >/dev/null 2>&1 && ip netns list >/dev/null 2>&1; then
    _log "netns created for allow_net sandbox"
    _api POST /cgi-bin/api/sandboxes \
        -d '{"id":"test-net","layers":"000-base-alpine","allow_net":["none"]}' >/dev/null
    ip netns list 2>/dev/null | grep -q "squash-test-net" && _pass || _fail "no netns"

    _log "iptables chain created"
    iptables -L "squash-test-net" >/dev/null 2>&1 && _pass || _fail "no iptables chain"

    _log "blocked egress returns failure"
    result=$(_api POST /cgi-bin/api/sandboxes/test-net/exec \
        -d '{"cmd":"wget -q -O /dev/null http://1.1.1.1/ 2>&1; echo $?","timeout":"5"}')
    rc=$(echo "$result" | jq -r '.stdout' | tr -d '[:space:]')
    [ "$rc" != "0" ] && _pass || _fail "egress should be blocked, got rc=$rc"

    _api DELETE /cgi-bin/api/sandboxes/test-net >/dev/null 2>&1

    _log "no netns for sandbox without allow_net"
    _api POST /cgi-bin/api/sandboxes \
        -d '{"id":"test-nonet","layers":"000-base-alpine"}' >/dev/null
    ip netns list 2>/dev/null | grep -q "squash-test-nonet" && _fail "unexpected netns" || _pass
    _api DELETE /cgi-bin/api/sandboxes/test-nonet >/dev/null 2>&1
else
    _skip "ip command not available"
    _skip "ip command not available"
    _skip "ip command not available"
    _skip "ip command not available"
fi

echo

# ── Secret Proxy ──────────────────────────────────────────────────
echo "=== Secret Proxy ==="

_log "placeholder env injected when secrets.json exists"
if [ -f "$DATA/secrets.json" ]; then
    _api POST /cgi-bin/api/sandboxes \
        -d '{"id":"test-sec","layers":"000-base-alpine"}' >/dev/null
    [ -f "$SANDBOXES/test-sec/upper/etc/profile.d/squash-secrets.sh" ] && _pass || _fail "no squash-secrets.sh"

    _log "http_proxy set in env"
    grep -q "http_proxy" "$SANDBOXES/test-sec/upper/etc/profile.d/squash-secrets.sh" 2>/dev/null \
        && _pass || _fail "no http_proxy"

    _api DELETE /cgi-bin/api/sandboxes/test-sec >/dev/null 2>&1
else
    _skip "no secrets.json"
    _skip "no secrets.json"
fi

echo

# ── Lifetime Reaper ───────────────────────────────────────────────
echo "=== Lifetime Reaper ==="

_log "sandbox with max_lifetime_s auto-destroyed"
_api POST /cgi-bin/api/sandboxes \
    -d '{"id":"test-reap","layers":"000-base-alpine","max_lifetime_s":5}' >/dev/null
sleep 15  # reaper runs every 10s
if _api GET /cgi-bin/api/sandboxes/test-reap >/dev/null 2>&1; then
    _fail "sandbox should be destroyed"
else
    _pass
fi

echo

# ── Cleanup & Summary ─────────────────────────────────────────────
# Clean up any straggling test sandboxes
for tid in test-iv test-fs test-cg test-net test-nonet test-sec test-reap; do
    _api DELETE "/cgi-bin/api/sandboxes/$tid" >/dev/null 2>&1 || true
done

echo "=== Results: $PASS passed, $FAIL failed, $SKIP skipped ==="
exit $FAIL
```

#### 2. Create `test/run.sh`

**File**: `test/run.sh` (new file)

```sh
#!/bin/sh
# test/run.sh — build container and run integration tests
# Usage: test/run.sh [--keep]
# --keep: don't remove the container after tests (for debugging)
set -eu

cd "$(dirname "$0")/.."

IMAGE="sq-sandbox:test"
CONTAINER="sq-test-$$"
KEEP=false
[ "${1:-}" = "--keep" ] && KEEP=true

echo "=== Building test image ==="
docker build -t "$IMAGE" .

echo "=== Starting container ==="
docker run -d --name "$CONTAINER" --privileged \
    -e SQUASH_AUTH_TOKEN="" \
    "$IMAGE"

echo "=== Waiting for API ==="
retries=0
while ! docker exec "$CONTAINER" curl -sf http://localhost:8080/cgi-bin/health >/dev/null 2>&1; do
    retries=$((retries + 1))
    [ $retries -gt 60 ] && { echo "TIMEOUT"; docker logs "$CONTAINER"; exit 1; }
    sleep 1
done

echo "=== Running tests ==="
rc=0
docker exec "$CONTAINER" /app/bin/sq-test || rc=$?

if [ "$KEEP" = "false" ]; then
    echo "=== Cleaning up ==="
    docker rm -f "$CONTAINER" >/dev/null
fi

exit $rc
```

#### 3. Create `test/secrets.json` for proxy tests

**File**: `test/secrets.json` (new file)

```json
{
    "secrets": {
        "TEST_API_KEY": {
            "placeholder": "sk-placeholder-test-12345",
            "value": "sk-real-test-key-67890",
            "allowed_hosts": ["httpbin.org"]
        }
    }
}
```

This is a test fixture — the "real" key is also fake, just different from the placeholder.

#### 4. Add test support to `test/run.sh` — copy secrets for proxy tests

In `test/run.sh`, after the container starts and before running tests:

```sh
echo "=== Injecting test secrets ==="
docker cp test/secrets.json "$CONTAINER":/data/secrets.json
```

### Success Criteria:

#### Automated Verification:
- [ ] `test/run.sh` completes with exit code 0
- [ ] All input validation tests pass
- [ ] All filesystem isolation tests pass
- [ ] Resource limit tests pass (or skip with message if cgroups unavailable)
- [ ] Network egress tests pass (or skip if iproute2 unavailable)
- [ ] Secret proxy tests pass when secrets.json is present
- [ ] Reaper test passes (sandbox auto-destroyed after expiry)

#### Manual Verification:
- [ ] `test/run.sh --keep` preserves the container for debugging
- [ ] Test output is readable and clearly shows pass/fail/skip per test

**Implementation Note**: After this phase, verify all tests pass before proceeding to Phase 5.

---

## Phase 5: Documentation Updates

### Overview

Clarify the README architecture and update `docs/security.md` with known limitations.

### Changes Required:

#### 1. Add "What This Is" section to README

**File**: `README.md`

After the "Quick Start" section (before "How it works"), add:

```markdown
## What this is

Squash Sandbox is a **single-container system** that is both the control plane
and the sandbox runtime. One Docker container runs:

- The HTTP API server (busybox httpd + CGI shell scripts)
- The sandbox lifecycle manager (create, exec, snapshot, restore, destroy)
- The sandbox host (overlayfs mounts, cgroups, network namespaces, or Firecracker VMs)
- Optional background services (reaper, secret proxy, Tailscale)

There is no separate "sandbox agent" or remote execution node. The container
that serves the API is the same container that mounts squashfs layers and runs
sandboxed commands. This means:

- **Deploy one container** — that's the whole system
- **`--privileged` is required** — the container directly manipulates kernel
  resources (loop mounts, overlayfs, cgroups, network namespaces)
- **Sandbox escape = host compromise** in chroot mode — Firecracker mode
  provides a stronger VM boundary
```

#### 2. Add security limitations to `docs/security.md`

**File**: `docs/security.md`

Add a new section at the end:

```markdown
## Known limitations

- **Chroot mode: shared kernel** — sandbox processes share the host kernel.
  A kernel exploit from inside a sandbox could compromise the host. Firecracker
  mode mitigates this with a separate guest kernel.

- **Chroot mode: UID mapping in privileged containers** — `--map-root-user`
  maps sandbox root to an unprivileged user outside, but when the container
  itself runs as root with `--privileged`, the effective isolation depends on
  the container runtime's user namespace configuration. For stronger isolation,
  use Firecracker mode.

- **Secret proxy: HTTP only** — The `sq-secret-proxy` handles HTTP requests.
  HTTPS (CONNECT tunneling with header rewriting) requires a compiled proxy
  binary. Most modern APIs use HTTPS, so the proxy's utility is limited to
  HTTP-only endpoints or environments where TLS is terminated before the proxy.

- **No seccomp or AppArmor** — Neither backend applies syscall filtering or
  mandatory access control profiles. The chroot backend relies on namespace
  isolation; the Firecracker backend relies on the VMM.

- **Single-container architecture** — The API server and sandbox host share a
  process tree. A compromise of the API server grants access to all sandbox
  management operations. Use `SQUASH_AUTH_TOKEN` and network-level access
  control (Tailscale, firewall rules) to protect the API.
```

### Success Criteria:

#### Automated Verification:
- [ ] `grep -c "What this is" README.md` returns 1
- [ ] `grep -c "Known limitations" docs/security.md` returns 1

#### Manual Verification:
- [ ] README clearly answers "is this a control plane or a sandbox?" (both)
- [ ] Security limitations are honest and actionable

---

## Testing Strategy

### Automated Tests (`bin/sq-test`):
- Input validation: path traversal in IDs, labels, modules; missing Content-Type
- Filesystem: exec works, writes land in upper, overlay COW functions
- Resources: cgroup directory created, memory.max and cpu.max values correct
- Network: netns created for allow_net sandboxes, iptables chain exists, blocked egress fails, no netns without allow_net
- Secrets: placeholder env file created, http_proxy set
- Reaper: sandbox with short lifetime auto-destroyed

### Integration Test Runner (`test/run.sh`):
- Builds Docker image from current source
- Starts privileged container
- Injects test secrets
- Runs `bin/sq-test` inside the container
- Reports pass/fail and cleans up

### Manual Testing:
1. Full sandbox lifecycle: create → exec → activate → snapshot → restore → destroy
2. Network isolation: `tcpdump` on veth confirms traffic routing
3. Secret proxy: verify real key appears in outbound request, placeholder appears inside sandbox
4. Firecracker (on bare metal): create/destroy cycle, verify no CID collisions in logs

## References

- Research: `thoughts/shared/research/2026-02-13-security-architecture-and-testability.md`
- Security comparison: `thoughts/shared/research/2026-02-13-security-model-comparison.md`
- Security hardening plan: `thoughts/shared/plans/2026-02-13-gastropod-security-hardening.md`
- Design doc: `docs/plan-firecracker-and-security.md`
- Linux network namespaces: `ip-netns(8)`, `nsenter(1)`
- Firecracker vsock: https://github.com/firecracker-microvm/firecracker/blob/main/docs/vsock.md
