# Squash v3: Firecracker Backend + Security Features

## Overview

Add a Firecracker microVM backend to Squash alongside the existing chroot mode,
plus three security features inspired by Deno Sandbox: resource limits, network
egress control, and secret materialization. The API surface stays identical —
the backend is a deployment-time choice via `SQUASH_BACKEND=chroot|firecracker`.

## Current State

- Single isolation mode: `unshare --mount --pid --fork --map-root-user` + `chroot`
- No resource limits beyond per-exec timeout
- Full network access (host resolv.conf copied in)
- No credential isolation
- Works on any host with privileged container support (Fly, ECS, Hetzner VPS)

## Desired End State

- Two backends behind the same API, selected by `SQUASH_BACKEND` env var
- Per-sandbox CPU, memory, and lifetime limits
- Per-sandbox network egress whitelisting (`allow_net`)
- Secret proxy that injects credentials only for approved outbound requests
- Firecracker mode works on bare metal with `/dev/kvm` access

### Verification:
- All existing API calls work identically in both modes
- `sq-ctl create` / `exec` / `snapshot` / `restore` / `destroy` pass in both backends
- Resource limits enforced (OOM kills, CPU throttle, auto-reap)
- Network egress blocked for non-whitelisted hosts
- Secrets not readable from inside sandbox

## What We're NOT Doing

- No multi-tenant hardening (still internal/agent use)
- No GPU passthrough
- No live migration between backends
- No Windows guest support
- No custom kernel builds — we use a prebuilt Firecracker-compatible kernel

## Architecture

```
                     ┌─────────────────────────────┐
                     │  REST API (busybox httpd)    │
                     │  cgi-bin/api/sandboxes       │
                     └──────────┬──────────────────┘
                                │
                     ┌──────────▼──────────────────┐
                     │  common.sh (dispatch layer)  │
                     │  SQUASH_BACKEND=?            │
                     └──┬───────────────────────┬──┘
                        │                       │
           ┌────────────▼─────────┐  ┌─────────▼──────────────┐
           │  chroot backend      │  │  firecracker backend    │
           │  overlayfs+unshare   │  │  microVM+vsock          │
           │  cgroups v2          │  │  native resource limits │
           │  netns+nftables      │  │  tap+nftables           │
           └──────────────────────┘  └─────────────────────────┘

                     ┌─────────────────────────────┐
                     │  sq-secret-proxy (optional)  │
                     │  HTTP proxy on host          │
                     │  injects credentials for     │
                     │  approved outbound requests  │
                     └─────────────────────────────┘
```

---

## Phase 1: Backend Abstraction

### Overview
Refactor `common.sh` so every sandbox operation dispatches to backend-specific
functions. Chroot mode keeps working identically — this is pure refactoring.

### Changes Required:

#### 1. `cgi-bin/common.sh`

Add backend dispatch at the top:

```sh
BACKEND="${SQUASH_BACKEND:-chroot}"
```

Rename current functions to `_chroot_*` prefixed versions, then add dispatch
wrappers:

```sh
# Current create_sandbox becomes _chroot_create_sandbox (same code)
# Current exec_in_sandbox becomes _chroot_exec_in_sandbox (same code)
# etc.

create_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_create_sandbox "$@" ;;
        firecracker) _firecracker_create_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

exec_in_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_exec_in_sandbox "$@" ;;
        firecracker) _firecracker_exec_in_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

# Same pattern for: destroy_sandbox, activate_module,
# snapshot_sandbox, restore_sandbox, sandbox_info, mounted
```

The `_firecracker_*` functions are stubs that return errors until Phase 5.

#### 2. `Dockerfile`

Add `SQUASH_BACKEND` env var:

```dockerfile
ENV SQUASH_BACKEND=chroot
```

#### 3. `cgi-bin/health`

Report active backend:

```sh
json_ok "$(jq -n \
    --arg backend "$SQUASH_BACKEND" \
    --arg ts "$ts_status" --arg ts_ip "$ts_ip" \
    --argjson sb "$sb_count" --argjson mod "$mod_count" --argjson base "$has_base" \
    '{status:"ok",backend:$backend,tailscale:{status:$ts,ip:$ts_ip},
      sandboxes:$sb,modules:$mod,base_ready:$base}')"
```

### Success Criteria:

#### Automated:
- [ ] All existing API calls return identical results
- [ ] Health endpoint reports `"backend":"chroot"`
- [ ] `SQUASH_BACKEND=firecracker` returns clear "not implemented" errors

#### Manual:
- [ ] Create, exec, snapshot, restore, destroy cycle works unchanged

---

## Phase 2: Resource Limits

### Overview
Add per-sandbox CPU, memory, and lifetime limits. In chroot mode, use cgroups v2.
In Firecracker mode (Phase 5), these map to native VM config.

### API Changes:

Sandbox creation accepts new optional fields:

```json
{
    "id": "dev",
    "owner": "alice",
    "layers": "000-base-alpine,100-python312",
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800
}
```

Defaults: `cpu=2.0`, `memory_mb=1024`, `max_lifetime_s=0` (no limit).

### Changes Required:

#### 1. `cgi-bin/common.sh` — Chroot Backend

New function to set up cgroups:

```sh
_cgroup_setup() {
    local id="$1" cpu="$2" mem_mb="$3"
    local cg="/sys/fs/cgroup/squash-$id"
    mkdir -p "$cg"

    # CPU limit (cpu.max: quota period_us)
    # cpu=1.0 means 100000us per 100000us period
    local quota=$(echo "$cpu * 100000" | bc | cut -d. -f1)
    echo "$quota 100000" > "$cg/cpu.max"

    # Memory limit
    local mem_bytes=$((mem_mb * 1024 * 1024))
    echo "$mem_bytes" > "$cg/memory.max"

    echo "$cg"
}

_cgroup_teardown() {
    local id="$1"
    local cg="/sys/fs/cgroup/squash-$id"
    rmdir "$cg" 2>/dev/null || true
}
```

Modify `_chroot_exec_in_sandbox` to run inside the cgroup:

```sh
# Before the unshare call, add the shell to the cgroup
echo $$ > "$cg/cgroup.procs"
```

#### 2. Metadata storage

Store limits in sandbox `.meta/`:

```sh
echo "$cpu"           > "$s/.meta/cpu"
echo "$mem_mb"        > "$s/.meta/memory_mb"
echo "$max_lifetime"  > "$s/.meta/max_lifetime_s"
```

#### 3. Auto-reaper for `max_lifetime_s`

New helper `bin/sq-reaper`:

```sh
#!/bin/sh
# Runs in background, checks sandbox ages, destroys expired ones
while true; do
    for sdir in "$SANDBOXES"/*/; do
        [ ! -f "$sdir/.meta/max_lifetime_s" ] && continue
        max=$(cat "$sdir/.meta/max_lifetime_s")
        [ "$max" = "0" ] && continue
        created=$(date -d "$(cat "$sdir/.meta/created")" +%s 2>/dev/null)
        now=$(date +%s)
        age=$((now - created))
        if [ "$age" -gt "$max" ]; then
            id=$(basename "$sdir")
            echo "[sq-reaper] destroying expired sandbox: $id (age=${age}s, max=${max}s)"
            destroy_sandbox "$id"
        fi
    done
    sleep 10
done
```

Start from `entrypoint.sh`:

```sh
sq-reaper &
```

#### 4. `cgi-bin/api/sandboxes` — Parse new fields

```sh
cpu=$(echo "$body" | jq -r '.cpu // "2.0"')
mem=$(echo "$body" | jq -r '.memory_mb // "1024"')
maxlife=$(echo "$body" | jq -r '.max_lifetime_s // "0"')
```

Pass to `create_sandbox` and store in metadata.

#### 5. `sandbox_info` — Report limits

Add cpu, memory_mb, max_lifetime_s to the info JSON output.

### Success Criteria:

#### Automated:
- [ ] Sandbox created with `memory_mb=64` gets OOM-killed when allocating 128MB
- [ ] Sandbox with `max_lifetime_s=30` auto-destroyed after 30 seconds
- [ ] Default sandboxes (no limits specified) work as before

#### Manual:
- [ ] CPU-limited sandbox shows throttling under load

---

## Phase 3: Network Egress Control

### Overview
Add per-sandbox network egress whitelisting. Sandboxes specify allowed outbound
hosts at creation time. All other egress is blocked.

### API Changes:

```json
{
    "id": "agent-sandbox",
    "owner": "agent",
    "layers": "000-base-alpine",
    "allow_net": ["api.anthropic.com", "*.github.com", "pypi.org"]
}
```

Default: `allow_net=[]` means **allow all** (backwards compatible).
Set `allow_net=["none"]` to block all egress.

### Changes Required:

#### 1. Network namespace for chroot mode

Currently `unshare` uses `--mount --pid`. Add `--net` to give each sandbox its
own network namespace, then set up a veth pair to connect it to the host:

```sh
_chroot_setup_netns() {
    local id="$1" allow_net="$2"
    local veth_host="sq-${id}-h"
    local veth_sandbox="sq-${id}-s"
    local subnet="10.200.${sandbox_index}.0/30"  # /30 = 2 usable IPs

    # Create veth pair
    ip link add "$veth_host" type veth peer name "$veth_sandbox"
    ip addr add "10.200.${sandbox_index}.1/30" dev "$veth_host"
    ip link set "$veth_host" up

    # Move sandbox end into the network namespace
    ip link set "$veth_sandbox" netns "$sandbox_pid"

    # Inside sandbox: configure IP + default route
    nsenter -t "$sandbox_pid" -n ip addr add "10.200.${sandbox_index}.2/30" dev "$veth_sandbox"
    nsenter -t "$sandbox_pid" -n ip link set "$veth_sandbox" up
    nsenter -t "$sandbox_pid" -n ip link set lo up
    nsenter -t "$sandbox_pid" -n ip route add default via "10.200.${sandbox_index}.1"

    # NAT on host for outbound
    iptables -t nat -A POSTROUTING -s "10.200.${sandbox_index}.0/30" -j MASQUERADE

    # Egress filtering
    if [ -n "$allow_net" ] && [ "$allow_net" != "[]" ]; then
        _apply_egress_rules "$id" "$veth_host" "$allow_net"
    fi
}
```

#### 2. Egress rules via nftables/iptables

```sh
_apply_egress_rules() {
    local id="$1" iface="$2" allow_net="$3"
    local chain="squash-${id}"

    iptables -N "$chain"
    iptables -A FORWARD -i "$iface" -j "$chain"

    # Allow DNS (needed to resolve allowed hosts)
    iptables -A "$chain" -p udp --dport 53 -j ACCEPT
    iptables -A "$chain" -p tcp --dport 53 -j ACCEPT

    # Allow each host pattern
    echo "$allow_net" | jq -r '.[]' | while read -r host; do
        case "$host" in
            none) ;; # block everything
            *\**)
                # Wildcard: resolve at connection time via ipset or DNS-based matching
                # For MVP: log + warn that wildcards need sq-secret-proxy (Phase 6)
                echo "[net] WARN: wildcard $host requires proxy mode" >&2
                ;;
            *)
                # Resolve and allow
                for ip in $(getent hosts "$host" 2>/dev/null | awk '{print $1}'); do
                    iptables -A "$chain" -d "$ip" -j ACCEPT
                done
                ;;
        esac
    done

    # Default: drop
    iptables -A "$chain" -j DROP
}
```

#### 3. Metadata

Store `allow_net` in `.meta/allow_net` as JSON array.

#### 4. Cleanup on destroy

```sh
_chroot_teardown_netns() {
    local id="$1"
    iptables -D FORWARD -i "sq-${id}-h" -j "squash-${id}" 2>/dev/null || true
    iptables -F "squash-${id}" 2>/dev/null || true
    iptables -X "squash-${id}" 2>/dev/null || true
    ip link delete "sq-${id}-h" 2>/dev/null || true
}
```

#### 5. Firecracker mode (wired in Phase 5)

Firecracker sandboxes get a tap device. Egress rules apply to the tap interface
instead of veth. Same iptables logic, different interface name.

### Success Criteria:

#### Automated:
- [ ] Sandbox with `allow_net=["none"]` cannot `curl` any host
- [ ] Sandbox with `allow_net=["api.anthropic.com"]` can reach that host, nothing else
- [ ] Sandbox with no `allow_net` has full access (backwards compat)
- [ ] DNS resolution works for allowed hosts

#### Manual:
- [ ] Confirm no traffic leaks to blocked hosts via tcpdump

---

## Phase 4: Guest VM Image

### Overview
Build the minimal rootfs and init system for Firecracker guest VMs. The guest
boots, mounts squashfs drives as overlayfs layers, and listens on vsock for
commands.

### New Files:

#### 1. `vm/init` — Guest init script

This is PID 1 inside the microVM:

```sh
#!/bin/sh
# Squash Firecracker guest init
# Mounts squashfs drives, sets up overlayfs, starts vsock command listener.

set -eu

# Mount essential filesystems
mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev
mkdir -p /dev/pts /dev/shm
mount -t devpts devpts /dev/pts
mount -t tmpfs tmpfs /dev/shm

# Parse kernel cmdline for layer config
# Format: squash.layers=3 squash.allow_net=api.example.com,pypi.org
LAYER_COUNT=$(cat /proc/cmdline | tr ' ' '\n' | sed -n 's/squash.layers=//p')
ALLOW_NET=$(cat /proc/cmdline | tr ' ' '\n' | sed -n 's/squash.allow_net=//p')

# Mount squashfs layers from virtio block devices
# /dev/vda = guest rootfs (this image)
# /dev/vdb = first squashfs layer (base)
# /dev/vdc = second layer
# etc.
LOWER=""
i=0
for dev in /dev/vd[b-z]; do
    [ -b "$dev" ] || continue
    [ "$i" -ge "${LAYER_COUNT:-99}" ] && break
    mp="/layers/$i"
    mkdir -p "$mp"
    mount -o ro -t squashfs "$dev" "$mp" 2>/dev/null || continue
    [ -n "$LOWER" ] && LOWER="$mp:$LOWER" || LOWER="$mp"
    i=$((i + 1))
done

# Set up overlayfs
mkdir -p /sandbox/{upper,work,merged}
mount -t tmpfs tmpfs /sandbox/upper
mkdir -p /sandbox/upper/data /sandbox/work
mount -t overlay overlay \
    -o "lowerdir=$LOWER,upperdir=/sandbox/upper/data,workdir=/sandbox/work" \
    /sandbox/merged

# Seed DNS
echo "nameserver 10.0.0.1" > /sandbox/merged/etc/resolv.conf 2>/dev/null || true

# Network setup
ip link set lo up
ip addr add 10.0.0.2/30 dev eth0 2>/dev/null || true
ip link set eth0 up 2>/dev/null || true
ip route add default via 10.0.0.1 2>/dev/null || true

echo "[squash-guest] ready — layers=$i"

# Start vsock command listener on port 5000
exec /usr/local/bin/sq-vsock-handler
```

#### 2. `vm/sq-vsock-handler` — Vsock command listener

A small shell script using `socat` for vsock:

```sh
#!/bin/sh
# Listens on vsock port 5000, accepts JSON commands, executes in sandbox.
# Protocol: one JSON object per connection.
#   Request:  {"cmd":"uname -a","workdir":"/","timeout":300}
#   Response: {"exit_code":0,"stdout":"...","stderr":"..."}

MERGED="/sandbox/merged"

handle_request() {
    local req=$(cat)
    local cmd=$(echo "$req" | jq -er '.cmd // empty' 2>/dev/null)
    local wd=$(echo "$req" | jq -er '.workdir // "/"' 2>/dev/null)
    local tout=$(echo "$req" | jq -er '.timeout // 300' 2>/dev/null)

    if [ -z "$cmd" ]; then
        jq -n '{exit_code:-1,stdout:"",stderr:"missing cmd"}'
        return
    fi

    local out=$(mktemp) err=$(mktemp)
    local rc=0
    timeout "$tout" chroot "$MERGED" /bin/sh -c "cd $wd 2>/dev/null || true; $cmd" \
        >"$out" 2>"$err" || rc=$?

    jq -n \
        --argjson rc "$rc" \
        --arg stdout "$(head -c 65536 "$out")" \
        --arg stderr "$(head -c 65536 "$err")" \
        '{exit_code:$rc,stdout:$stdout,stderr:$stderr}'

    rm -f "$out" "$err"
}

export -f handle_request
export MERGED

# socat VSOCK-LISTEN forks a handler per connection
exec socat VSOCK-LISTEN:5000,reuseaddr,fork EXEC:"/bin/sh -c handle_request"
```

#### 3. `bin/sq-mkvm` — Build guest image + fetch kernel

```sh
#!/bin/sh
# Build the Firecracker guest rootfs and download a compatible kernel.
set -eu

DATA="${SQUASH_DATA:-/data}"
VM_DIR="$DATA/vm"
WORK="/tmp/sq-mkvm.$$"

log() { echo "[sq-mkvm] $*"; }
cleanup() { rm -rf "$WORK"; }
trap cleanup EXIT
mkdir -p "$WORK" "$VM_DIR"

# --- Build guest rootfs as ext4 image ---
build_rootfs() {
    local rootfs="$WORK/rootfs"
    local img="$VM_DIR/guest-rootfs.ext4"
    mkdir -p "$rootfs"

    log "building guest rootfs"

    # Start from Alpine minirootfs
    local url="https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/x86_64/alpine-minirootfs-3.21.0-x86_64.tar.gz"
    wget -qO "$WORK/alpine.tar.gz" "$url"
    tar xzf "$WORK/alpine.tar.gz" -C "$rootfs"

    # Install guest dependencies
    chroot "$rootfs" /sbin/apk --no-cache add \
        socat jq coreutils squashfs-tools

    # Install our init and handler
    cp vm/init "$rootfs/init"
    chmod +x "$rootfs/init"
    mkdir -p "$rootfs/usr/local/bin"
    cp vm/sq-vsock-handler "$rootfs/usr/local/bin/"
    chmod +x "$rootfs/usr/local/bin/sq-vsock-handler"

    # Create ext4 image (256MB, will be sparse)
    dd if=/dev/zero of="$img" bs=1M count=256
    mkfs.ext4 -F -d "$rootfs" "$img"

    log "guest rootfs: $img ($(du -sh "$img" | cut -f1))"
}

# --- Download Firecracker kernel ---
fetch_kernel() {
    local kernel="$VM_DIR/vmlinux"
    [ -f "$kernel" ] && { log "kernel already present"; return; }

    log "downloading Firecracker-compatible kernel"
    local fc_ver="v1.10.1"
    local url="https://github.com/firecracker-microvm/firecracker/releases/download/${fc_ver}/vmlinux-6.1-x86_64.bin"
    wget -qO "$kernel" "$url" || {
        log "WARN: kernel download failed, trying alternate"
        # Fallback: build from source or use another prebuilt
        url="https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.10/x86_64/vmlinux-6.1"
        wget -qO "$kernel" "$url"
    }

    log "kernel: $kernel"
}

# --- Download Firecracker binary ---
fetch_firecracker() {
    local fc="$VM_DIR/firecracker"
    [ -f "$fc" ] && { log "firecracker binary already present"; return; }

    log "downloading Firecracker"
    local fc_ver="v1.10.1"
    local url="https://github.com/firecracker-microvm/firecracker/releases/download/${fc_ver}/firecracker-${fc_ver}-x86_64.tgz"
    wget -qO "$WORK/fc.tgz" "$url"
    tar xzf "$WORK/fc.tgz" -C "$WORK"
    cp "$WORK"/release-*/firecracker-*-x86_64 "$fc"
    chmod +x "$fc"

    log "firecracker: $fc"
}

case "${1:-all}" in
    rootfs)      build_rootfs ;;
    kernel)      fetch_kernel ;;
    firecracker) fetch_firecracker ;;
    all)         build_rootfs; fetch_kernel; fetch_firecracker ;;
    *)           echo "usage: $0 [rootfs|kernel|firecracker|all]" ;;
esac
```

### Success Criteria:

#### Automated:
- [ ] `sq-mkvm all` produces `guest-rootfs.ext4`, `vmlinux`, `firecracker` in `$DATA/vm/`
- [ ] Guest rootfs contains `/init` and `/usr/local/bin/sq-vsock-handler`
- [ ] Guest rootfs boots to init in a manual Firecracker test

#### Manual:
- [ ] Boot the guest manually with Firecracker, verify init mounts layers and vsock listener starts

---

## Phase 5: Firecracker Backend

### Overview
Wire up the Firecracker backend functions. Each sandbox gets its own Firecracker
process. Communication via vsock.

### New File: `bin/sq-firecracker`

Helper script for VM lifecycle:

```sh
#!/bin/sh
# sq-firecracker — manage Firecracker microVMs for Squash sandboxes
set -eu

DATA="${SQUASH_DATA:-/data}"
VM_DIR="$DATA/vm"
FC="$VM_DIR/firecracker"
KERNEL="$VM_DIR/vmlinux"
ROOTFS="$VM_DIR/guest-rootfs.ext4"

# Start a Firecracker VM for a sandbox
# Usage: sq-firecracker start <sandbox-id> <cpu> <mem_mb> <squashfs1> [squashfs2 ...]
fc_start() {
    local id="$1" cpu="$2" mem="$3"; shift 3
    local sock="$DATA/sandboxes/$id/.meta/fc.sock"
    local pid_file="$DATA/sandboxes/$id/.meta/fc.pid"
    local log_file="$DATA/sandboxes/$id/.meta/fc.log"
    local cid_file="$DATA/sandboxes/$id/.meta/fc.cid"

    # Assign a unique CID for vsock (3+)
    local cid=$((3 + $(ls -1d "$DATA/sandboxes"/*/ 2>/dev/null | wc -l)))
    echo "$cid" > "$cid_file"

    # Start Firecracker
    rm -f "$sock"
    "$FC" --api-sock "$sock" > "$log_file" 2>&1 &
    echo $! > "$pid_file"
    sleep 0.5

    # Configure VM via API
    # Kernel + boot args
    local layer_count=$#
    local boot_args="console=ttyS0 reboot=k panic=1 pci=off squash.layers=$layer_count"
    curl -s --unix-socket "$sock" -X PUT "http://localhost/boot-source" \
        -H 'Content-Type: application/json' \
        -d "{\"kernel_image_path\":\"$KERNEL\",\"boot_args\":\"$boot_args\"}"

    # Guest rootfs (vda)
    curl -s --unix-socket "$sock" -X PUT "http://localhost/drives/rootfs" \
        -H 'Content-Type: application/json' \
        -d "{\"drive_id\":\"rootfs\",\"path_on_host\":\"$ROOTFS\",\"is_root_device\":true,\"is_read_only\":true}"

    # Squashfs layers as additional drives (vdb, vdc, ...)
    local i=0
    for sqfs in "$@"; do
        curl -s --unix-socket "$sock" -X PUT "http://localhost/drives/layer$i" \
            -H 'Content-Type: application/json' \
            -d "{\"drive_id\":\"layer$i\",\"path_on_host\":\"$sqfs\",\"is_root_device\":false,\"is_read_only\":true}"
        i=$((i + 1))
    done

    # Machine config
    curl -s --unix-socket "$sock" -X PUT "http://localhost/machine-config" \
        -H 'Content-Type: application/json' \
        -d "{\"vcpu_count\":${cpu%.*},\"mem_size_mib\":$mem}"

    # Vsock
    curl -s --unix-socket "$sock" -X PUT "http://localhost/vsock" \
        -H 'Content-Type: application/json' \
        -d "{\"guest_cid\":$cid,\"uds_path\":\"$DATA/sandboxes/$id/.meta/vsock.sock\"}"

    # Network (tap device)
    # Created by _firecracker_setup_network before this call
    curl -s --unix-socket "$sock" -X PUT "http://localhost/network-interfaces/eth0" \
        -H 'Content-Type: application/json' \
        -d "{\"iface_id\":\"eth0\",\"host_dev_name\":\"sq-${id}-tap\"}"

    # Boot
    curl -s --unix-socket "$sock" -X PUT "http://localhost/actions" \
        -H 'Content-Type: application/json' \
        -d '{"action_type":"InstanceStart"}'

    # Wait for vsock to be ready
    local retries=0
    while [ $retries -lt 20 ]; do
        echo '{"cmd":"echo ready"}' | socat - VSOCK-CONNECT:$cid:5000 2>/dev/null && break
        retries=$((retries + 1))
        sleep 0.1
    done
}

# Send command to VM via vsock
fc_exec() {
    local id="$1" cmd="$2" workdir="${3:-/}" timeout="${4:-300}"
    local cid=$(cat "$DATA/sandboxes/$id/.meta/fc.cid")

    jq -n --arg cmd "$cmd" --arg wd "$workdir" --argjson t "$timeout" \
        '{cmd:$cmd,workdir:$wd,timeout:$t}' \
    | socat -T"$timeout" - VSOCK-CONNECT:$cid:5000
}

# Stop VM
fc_stop() {
    local id="$1"
    local pid_file="$DATA/sandboxes/$id/.meta/fc.pid"
    [ -f "$pid_file" ] && kill "$(cat "$pid_file")" 2>/dev/null || true
    rm -f "$DATA/sandboxes/$id/.meta/fc.sock"
}

# Hot-add a drive (for activate_module)
fc_add_drive() {
    local id="$1" drive_id="$2" sqfs_path="$3"
    local sock="$DATA/sandboxes/$id/.meta/fc.sock"

    curl -s --unix-socket "$sock" -X PUT "http://localhost/drives/$drive_id" \
        -H 'Content-Type: application/json' \
        -d "{\"drive_id\":\"$drive_id\",\"path_on_host\":\"$sqfs_path\",\"is_root_device\":false,\"is_read_only\":true}"

    # Tell guest to remount overlayfs with new drive
    local cid=$(cat "$DATA/sandboxes/$id/.meta/fc.cid")
    echo '{"cmd":"__squash_remount"}' | socat - VSOCK-CONNECT:$cid:5000
}

case "$1" in
    start)    shift; fc_start "$@" ;;
    exec)     shift; fc_exec "$@" ;;
    stop)     shift; fc_stop "$@" ;;
    add-drive) shift; fc_add_drive "$@" ;;
esac
```

### Backend functions in `common.sh`:

```sh
_firecracker_create_sandbox() {
    local id="$1" owner="$2" layer_csv="$3" task="${4:-}"
    local s=$(sdir "$id")

    exists "$id" && { echo "already exists" >&2; return 1; }

    # Validate layers
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        mod_exists "$mod" || { echo "module not found: $mod" >&2; return 2; }
    done

    mkdir -p "$s/.meta/log"
    echo "$owner"      > "$s/.meta/owner"
    echo "$task"       > "$s/.meta/task"
    echo "$layer_csv"  > "$s/.meta/layers"
    date -Iseconds     > "$s/.meta/created"
    date -Iseconds     > "$s/.meta/last_active"

    # Set up tap device for networking
    _firecracker_setup_network "$id"

    # Collect squashfs paths (in layer order, base first)
    local sqfs_args=""
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        sqfs_args="$sqfs_args $(mod_path "$mod")"
    done

    local cpu=$(cat "$s/.meta/cpu" 2>/dev/null || echo 2)
    local mem=$(cat "$s/.meta/memory_mb" 2>/dev/null || echo 1024)

    sq-firecracker start "$id" "$cpu" "$mem" $sqfs_args
}

_firecracker_exec_in_sandbox() {
    local id="$1" cmd="$2" workdir="${3:-/}" timeout_s="${4:-300}"
    local s=$(sdir "$id")

    date -Iseconds > "$s/.meta/last_active"

    local seq=$(( $(ls "$s/.meta/log/" 2>/dev/null | wc -l) + 1 ))
    local logf="$s/.meta/log/$(printf '%04d' $seq).json"
    local t0=$(date -Iseconds)

    # Send command to VM
    local result=$(sq-firecracker exec "$id" "$cmd" "$workdir" "$timeout_s")

    local t1=$(date -Iseconds)
    local rc=$(echo "$result" | jq -r '.exit_code')
    local out=$(echo "$result" | jq -r '.stdout')
    local err=$(echo "$result" | jq -r '.stderr')

    jq -n \
        --argjson seq "$seq" --arg cmd "$cmd" --arg workdir "$workdir" \
        --argjson rc "$rc" --arg t0 "$t0" --arg t1 "$t1" \
        --arg stdout "$out" --arg stderr "$err" \
        '{seq:$seq,cmd:$cmd,workdir:$workdir,exit_code:$rc,started:$t0,finished:$t1,stdout:$stdout,stderr:$stderr}' \
        > "$logf"

    cat "$logf"
    return $rc
}

_firecracker_destroy_sandbox() {
    local id="$1" s=$(sdir "$id")
    [ ! -d "$s" ] && return 0
    sq-firecracker stop "$id"
    _firecracker_teardown_network "$id"
    rm -rf "$s"
}

_firecracker_mounted() {
    # "Mounted" means VM is running
    local pid_file="$(sdir "$1")/.meta/fc.pid"
    [ -f "$pid_file" ] && kill -0 "$(cat "$pid_file")" 2>/dev/null
}
```

### Success Criteria:

#### Automated:
- [ ] `SQUASH_BACKEND=firecracker sq-ctl create test anon 000-base-alpine` boots a VM
- [ ] `sq-ctl exec test "uname -a"` returns Linux kernel info via vsock
- [ ] `sq-ctl exec test "cat /etc/os-release"` shows Alpine (from base layer)
- [ ] `sq-ctl activate test 100-python312` adds Python and `python3 --version` works
- [ ] `sq-ctl destroy test` kills the VM and cleans up
- [ ] Health endpoint reports `"backend":"firecracker"`

#### Manual:
- [ ] Full create/exec/snapshot/restore/destroy cycle works end-to-end
- [ ] VM boots in < 1 second
- [ ] Resource limits (CPU, memory) enforced by VM config

---

## Phase 6: Secret Materialization Proxy

### Overview
An HTTP proxy that injects real credentials into outbound requests from sandboxes.
Sandboxes see placeholder values for secrets. Real credentials only materialize
at the proxy for requests to approved hosts.

### Architecture

```
Sandbox (env: API_KEY=sk-placeholder-abc123)
    │
    ▼ http_proxy=http://10.0.0.1:8888
    │
┌───▼────────────────────────────────────┐
│  sq-secret-proxy (runs on host)        │
│                                        │
│  1. Intercept outbound request         │
│  2. Check destination against allow_net│
│  3. If allowed, replace placeholder    │
│     headers/body with real credentials │
│  4. Forward request to destination     │
│  5. Return response to sandbox         │
└────────────────────────────────────────┘
```

### New File: `bin/sq-secret-proxy`

Implemented as a small HTTP CONNECT proxy using `socat` + shell, or for HTTPS
support, a minimal Python/Go binary. Given the Squash philosophy, start with
HTTP-only support via shell, with a note that HTTPS requires a compiled binary.

**Secret config file** at `$DATA/secrets.json`:

```json
{
    "secrets": {
        "ANTHROPIC_API_KEY": {
            "placeholder": "sk-placeholder-anthropic",
            "value": "sk-ant-real-key-here",
            "allowed_hosts": ["api.anthropic.com"]
        },
        "GITHUB_TOKEN": {
            "placeholder": "ghp-placeholder-github",
            "value": "ghp_real_token_here",
            "allowed_hosts": ["api.github.com", "*.github.com"]
        }
    }
}
```

### Changes Required:

#### 1. Sandbox creation — inject placeholders

When secrets are configured, the sandbox gets placeholder env vars
instead of real values. In the create flow:

```sh
_inject_secret_placeholders() {
    local id="$1" s=$(sdir "$id")
    local secrets_file="$DATA/secrets.json"
    [ ! -f "$secrets_file" ] && return

    local env_dir="$s/upper/etc/profile.d"
    mkdir -p "$env_dir"

    # Generate env script with placeholders
    jq -r '.secrets | to_entries[] | "export \(.key)=\(.value.placeholder)"' \
        "$secrets_file" > "$env_dir/squash-secrets.sh"

    # Also set http_proxy to route through our proxy
    echo "export http_proxy=http://10.0.0.1:8888" >> "$env_dir/squash-secrets.sh"
    echo "export https_proxy=http://10.0.0.1:8888" >> "$env_dir/squash-secrets.sh"
}
```

#### 2. Proxy implementation

For MVP, a simple `socat`-based HTTP proxy that rewrites headers:

```sh
#!/bin/sh
# sq-secret-proxy — credential-injecting HTTP proxy
# Listens on port 8888, forwards requests, injects real secrets.

SECRETS_FILE="${SQUASH_DATA:-/data}/secrets.json"
PORT=8888

handle_request() {
    # Read HTTP request
    local request_line
    read -r request_line
    local method=$(echo "$request_line" | awk '{print $1}')
    local url=$(echo "$request_line" | awk '{print $2}')
    local host=$(echo "$url" | sed -n 's|https\?://\([^/]*\).*|\1|p')

    # Read headers, check for placeholder values, replace with real ones
    local headers=""
    while IFS= read -r line; do
        line=$(echo "$line" | tr -d '\r')
        [ -z "$line" ] && break

        # Check each secret for placeholder in header values
        local replaced=false
        for key in $(jq -r '.secrets | keys[]' "$SECRETS_FILE"); do
            local placeholder=$(jq -r ".secrets[\"$key\"].placeholder" "$SECRETS_FILE")
            if echo "$line" | grep -q "$placeholder"; then
                # Verify host is allowed for this secret
                local allowed=$(jq -r ".secrets[\"$key\"].allowed_hosts[]" "$SECRETS_FILE")
                if echo "$allowed" | grep -qF "$host"; then
                    local real=$(jq -r ".secrets[\"$key\"].value" "$SECRETS_FILE")
                    line=$(echo "$line" | sed "s/$placeholder/$real/g")
                fi
            fi
        done
        headers="$headers$line\r\n"
    done

    # Forward to destination with replaced headers
    # (Full implementation would use curl or socat to forward)
}
```

**Note:** A production-grade proxy for HTTPS (which most APIs use) will require
a compiled binary — a ~200-line Go or Rust program that does CONNECT tunneling
with header rewriting. This is the one place where shell hits its limits.
The plan accounts for this as a Go binary: `vm/sq-secret-proxy.go`, compiled
to a static binary and included in the host image.

#### 3. Start proxy from entrypoint

```sh
[ -f "$SQUASH_DATA/secrets.json" ] && sq-secret-proxy &
```

### Success Criteria:

#### Automated:
- [ ] Sandbox env vars show placeholder values, not real keys
- [ ] `curl -H "Authorization: Bearer $ANTHROPIC_API_KEY" https://api.anthropic.com/...` succeeds
  (proxy replaces placeholder with real key)
- [ ] Same curl to unapproved host sends the placeholder (not the real key)
- [ ] No way to read real key value from inside sandbox

#### Manual:
- [ ] Verify with tcpdump that real credentials never appear in sandbox-side traffic
- [ ] Test with actual API calls through the proxy

---

## Phase 7: Bare Metal Deploy

### Overview
Deployment config for Hetzner dedicated servers (or any bare metal with KVM).

### Changes Required:

#### 1. `deploy/hetzner-baremetal/docker-compose.yml`

```yaml
services:
  squash:
    build: ../..
    privileged: true
    restart: unless-stopped
    ports: ["8080:8080"]
    volumes:
      - squash-data:/data
      - /dev/kvm:/dev/kvm  # KVM passthrough
    environment:
      SQUASH_BACKEND: firecracker
      SQUASH_AUTH_TOKEN: ${SQUASH_AUTH_TOKEN}
      TAILSCALE_AUTHKEY: ${TAILSCALE_AUTHKEY:-}
    devices:
      - /dev/kvm:/dev/kvm

volumes:
  squash-data:
```

#### 2. `Dockerfile.firecracker` (extended image)

```dockerfile
FROM alpine:3.21

RUN apk add --no-cache \
    jq squashfs-tools util-linux curl wget coreutils \
    socat iptables iproute2 bc \
    && apk add --no-cache tailscale 2>/dev/null || true

COPY bin/         /app/bin/
COPY cgi-bin/     /app/cgi-bin/
COPY vm/          /app/vm/
COPY entrypoint.sh /app/
COPY static/      /app/static/

RUN chmod +x /app/bin/* /app/cgi-bin/* /app/cgi-bin/api/* /app/entrypoint.sh

VOLUME /data
EXPOSE 8080

ENV SQUASH_DATA=/data \
    SQUASH_PORT=8080 \
    SQUASH_BACKEND=firecracker \
    SQUASH_AUTH_TOKEN="" \
    TAILSCALE_AUTHKEY="" \
    TAILSCALE_HOSTNAME="squash"

WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
```

#### 3. `bin/sq-init` updates

When `SQUASH_BACKEND=firecracker`, first-boot also provisions the VM image:

```sh
if [ "$SQUASH_BACKEND" = "firecracker" ]; then
    if [ ! -f "$DATA/vm/firecracker" ]; then
        log "firecracker backend: provisioning VM components"
        "$SCRIPT_DIR/sq-mkvm" all
    else
        log "firecracker backend: VM components present"
    fi
fi
```

### Success Criteria:

#### Automated:
- [ ] `docker-compose up` with `SQUASH_BACKEND=firecracker` starts successfully
- [ ] `sq-init` provisions Firecracker binary, kernel, and guest rootfs on first boot
- [ ] Health endpoint shows `"backend":"firecracker"`

#### Manual:
- [ ] Full sandbox lifecycle works on Hetzner dedicated server
- [ ] Firecracker VMs boot and respond via vsock
- [ ] Network egress control works via tap device

---

## New Files Summary

```
bin/
  sq-firecracker      Firecracker VM lifecycle management
  sq-mkvm             Build guest rootfs + fetch kernel + firecracker binary
  sq-reaper           Auto-destroy expired sandboxes
  sq-secret-proxy     Credential-injecting HTTP proxy (Go binary for HTTPS)
vm/
  init                Guest PID 1 — mounts layers, starts vsock listener
  sq-vsock-handler    Vsock command handler inside guest
deploy/
  hetzner-baremetal/  docker-compose for bare metal with /dev/kvm
Dockerfile.firecracker  Extended image with Firecracker deps
```

## Modified Files Summary

```
cgi-bin/common.sh     Backend dispatch, cgroup setup, netns setup
cgi-bin/api/sandboxes  Parse new fields (cpu, memory_mb, max_lifetime_s, allow_net)
cgi-bin/health         Report backend type
bin/sq-init            Provision Firecracker on first boot
Dockerfile             Add SQUASH_BACKEND env var
entrypoint.sh          Start reaper + secret proxy
```

## Implementation Order

Phases 1-3 can be done and tested with just the chroot backend. Phases 4-5 add
Firecracker. Phase 6 adds security. Phase 7 is deploy config. Each phase is
independently testable and deployable.

## References

- [Firecracker docs](https://github.com/firecracker-microvm/firecracker/blob/main/docs/)
- [Deno Sandbox blog post](https://deno.com/blog/introducing-deno-sandbox)
- [coder/httpjail](https://github.com/coder/httpjail) — egress proxy reference
