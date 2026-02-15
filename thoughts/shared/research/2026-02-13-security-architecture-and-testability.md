---
date: 2026-02-13T14:30:00-08:00
researcher: reuben
git_commit: f8b3bc4dcd1579817f45e76ec9e7fbdab59d489a
branch: main
repository: sq-sandbox
topic: "Security architecture evaluation: control plane vs sandbox, and testability without real VMs"
tags: [research, codebase, security, architecture, testability, isolation]
status: complete
last_updated: 2026-02-13
last_updated_by: reuben
---

# Research: Security Architecture & Testability

**Date**: 2026-02-13
**Git Commit**: f8b3bc4
**Branch**: main
**Repository**: sq-sandbox

## Research Question

1. Is this a control plane, the sandbox itself, or both?
2. How do you evaluate and test the security story without spinning up real VMs everywhere?

## Summary

**Squash Sandbox is both the control plane AND the sandbox runtime.** It is a single deployable unit (one Docker container) that contains the API server, the sandbox management logic, and the sandbox execution environment all in one process tree. There is no separate "sandbox agent" — the same Alpine container that serves the HTTP API also loop-mounts squashfs layers, creates overlayfs stacks, runs `unshare + chroot` to execute sandboxed commands, manages cgroups, configures iptables rules, and optionally launches Firecracker VMs.

**For testability**, the architecture has a split: the chroot backend can be fully exercised on any Linux host with a privileged Docker container (no KVM needed), while the Firecracker backend requires bare metal with `/dev/kvm`. The security mechanisms decompose into independently testable layers — input validation, iptables rules, cgroup enforcement, filesystem isolation, and secret proxy behavior — many of which can be verified without a full VM. However, end-to-end integration testing of the complete security stack does require a real Linux environment with privilege escalation.

---

## Detailed Findings

### 1. Control Plane vs. Sandbox: It's Both in One Container

The system runs as a single container with a process tree rooted at `entrypoint.sh`:

```
entrypoint.sh
  ├── sq-init              (one-shot: builds base modules, remounts surviving sandboxes)
  ├── sq-reaper &          (background: polls sandbox ages, destroys expired ones)
  ├── sq-secret-proxy &    (background: socat listener on :8888 for credential injection)
  └── exec start-api       (foreground: busybox httpd on :8080)
       └── httpd
            ├── cgi-bin/health
            ├── cgi-bin/api/modules
            └── cgi-bin/api/sandboxes    ← REST handler, sources common.sh
                 └── common.sh           ← ALL sandbox operations live here
                      ├── create:  mkdir + loop-mount squashfs + mount overlay + cgroup + veth+iptables
                      ├── exec:    unshare --mount --pid --fork --map-root-user + chroot
                      ├── destroy: umount + rm -rf + iptables cleanup + cgroup teardown
                      └── (or firecracker: start VM process, talk via vsock)
```

**Source**: `entrypoint.sh:1-12`, `bin/start-api:1-10`, `cgi-bin/api/sandboxes:1-118`, `cgi-bin/common.sh:1-948`

There is no separation between the control plane (API, lifecycle management) and the data plane (actual sandbox execution). The CGI handler that receives the HTTP request is the same process that calls `mount`, `unshare`, `chroot`, and `iptables`. This is a deliberate architectural choice — the entire system is ~1000 lines of POSIX shell.

**Implications:**
- The Docker container running the API IS the sandbox host — it needs `--privileged` because it directly manipulates kernel resources (overlayfs, cgroups, network namespaces, loop devices)
- A sandbox escape means control-plane compromise — there's no additional barrier between a broken-out sandbox and the API server process
- In Firecracker mode, the barrier is stronger: the guest VM communicates only via vsock, and the host-side Firecracker process is a separate binary. But the API server still shares the host with the Firecracker VMM

### 2. The Two Isolation Backends

#### 2a. Chroot Backend (`SQUASH_BACKEND=chroot`)

**Isolation primitives** used at exec time (`common.sh:439-443`):

```sh
unshare --mount --pid --fork --map-root-user \
    chroot "$s/merged" \
    /bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$workdir" "$cmd"
```

- `--mount`: Separate mount namespace
- `--pid`: Separate PID namespace
- `--fork`: Required for PID namespace
- `--map-root-user`: Maps UID 0 inside → unprivileged outside
- **`--net` is NOT used** in the unshare call; network isolation is via veth pair + iptables, set up at sandbox creation time, not at exec time

**What is NOT isolated at exec time:**
- No separate network namespace in the `unshare` call (the veth/iptables setup handles this at create-time, but the exec itself doesn't enter a netns)
- No separate IPC namespace
- No separate UTS namespace
- No seccomp filter
- No AppArmor/SELinux profile
- No user namespace beyond `--map-root-user` (which maps sandbox root to host root when running as privileged container)

#### 2b. Firecracker Backend (`SQUASH_BACKEND=firecracker`)

**Isolation primitives** (`bin/sq-firecracker`, `vm/init`, `vm/sq-vsock-handler`):
- Separate kernel (guest boots its own Linux kernel)
- Separate virtual hardware (virtio block, virtio net, vsock)
- Memory and CPU limits are VM configuration, not cgroup overlays
- Communication only via vsock port 5000 — no shared filesystem, no shared PID space
- Guest init (`vm/init`) mounts squashfs layers as overlayfs inside the VM, same pattern as chroot mode

### 3. Security Mechanisms and Their Testability

#### 3a. Input Validation (testable without VM)

**Location**: `common.sh:15-25`

```sh
valid_id()     — alphanumeric + dash + underscore
valid_label()  — alphanumeric + dash + underscore + dot
valid_module() — alphanumeric + dash + underscore + dot
```

Applied at: sandbox create (API handler), snapshot label, restore label, module activate, `mod_exists()`.

**Testability**: These are pure shell functions. Can be tested with curl against a running container or even unit-tested in isolation. No VM or privileged access needed. Example test:
```sh
# Should return 400
curl -X POST .../snapshot -d '{"label":"../../etc/evil"}' -H 'Content-Type: application/json'
```

#### 3b. Filesystem Isolation (testable with privileged Docker, no KVM)

**Location**: `common.sh:196-218` (overlay mount), `common.sh:345-413` (chroot create)

The overlayfs stack is: read-only squashfs layers (loop-mounted) + writable tmpfs upper. All writes land in upper/. Destroying the sandbox wipes upper/ but leaves the shared module images intact.

**Testability**: The chroot backend exercises all of this. A `docker run --privileged` container on any Linux host (x86 or ARM) is sufficient. You can:
- Create a sandbox, write files, verify they appear in upper/ only
- Verify the squashfs layers are mounted read-only
- Verify `chroot` confines the process to merged/

#### 3c. Resource Limits — cgroups v2 (testable with privileged Docker, no KVM)

**Location**: `common.sh:227-248`

```sh
_cgroup_setup() — creates /sys/fs/cgroup/squash-$id, writes cpu.max and memory.max
```

The cgroup is created at sandbox creation time. The exec handler writes `$$ > $cg/cgroup.procs` before running the command.

**Testability**: Works in privileged Docker on any Linux host with cgroups v2. Test by creating a sandbox with `memory_mb=64`, then running a command that allocates >64MB — should get OOM-killed.

**Caveat**: The `echo $$ > cgroup.procs` at `common.sh:436-437` adds the CGI handler's PID to the cgroup before forking `unshare`. This means the cgroup applies to the handler process itself, not just the sandboxed command. In practice, the handler exits quickly after the exec, but this is an imprecise cgroup application.

#### 3d. Network Egress (testable with privileged Docker + iptables)

**Location**: `common.sh:252-341`

Per-sandbox iptables chains with:
- DNS always allowed (UDP/TCP 53)
- Established/related allowed
- Each allowed host resolved via `getent hosts`, IPs added as ACCEPT
- Default: DROP

**Testability**: Works in privileged Docker with iptables support. Can test by:
- Creating sandbox with `allow_net=["api.anthropic.com"]`
- Verifying `iptables -L squash-$id` shows the expected rules
- From inside sandbox, `curl api.anthropic.com` should succeed, `curl google.com` should fail

**Limitation that affects testing**: The veth pair is created at sandbox creation time, but the `unshare` command does NOT use `--net`, so the sandbox process shares the host network namespace unless additional steps are taken. The iptables FORWARD rules rely on traffic going through the veth interface, which requires the sandbox's traffic to actually route through it. The current code creates the veth pair and configures both ends on the host side (`common.sh:274-276` — both `veth_host` and `veth_sandbox` get IPs assigned on the host), but the sandbox process (via `unshare + chroot`) doesn't enter a separate network namespace. This means the egress filtering may not actually intercept sandbox traffic in chroot mode — the sandbox would use the host's network stack directly, bypassing the veth/iptables setup.

In Firecracker mode, this is handled correctly: the VM has its own network stack, and the tap device is the only path out, so iptables on the tap device works as intended.

#### 3e. Secret Proxy (testable without VM)

**Location**: `bin/sq-secret-proxy:1-132`

socat TCP-LISTEN:8888 → per-connection handler that:
1. Reads HTTP request line + headers
2. Extracts destination host from URL
3. For each header containing a placeholder string: checks if destination host is in that secret's `allowed_hosts`
4. If allowed, replaces placeholder with real value
5. Forwards via `curl -K` config file

**Testability**: The proxy is a standalone process. Can test by:
- Starting it with a test `secrets.json`
- Sending HTTP requests through it (e.g., `curl -x http://localhost:8888 http://api.anthropic.com/...`)
- Verifying placeholder replacement for allowed hosts
- Verifying placeholder NOT replaced for disallowed hosts

**Current limitations**:
- HTTP-only — HTTPS (CONNECT tunneling) is explicitly documented as needing a compiled Go/Rust binary (`sq-secret-proxy:7-8`)
- The proxy runs on the host, not in the sandbox. Sandbox processes need `http_proxy=http://10.0.0.1:8888` set to route through it. This is noted in the design doc but the create flow doesn't automatically inject the proxy env vars — `_inject_secret_placeholders()` exists only in the design doc (`plan-firecracker-and-security.md:915-929`), not in the actual code

#### 3f. API Authentication (testable without VM)

**Location**: `common.sh:119-125`

```sh
check_auth() {
    local token="${SQUASH_AUTH_TOKEN:-}"
    [ -z "$token" ] && return 0                            # no auth configured → allow all
    [ "Bearer $token" = "${HTTP_AUTHORIZATION:-}" ] && return 0
    json_err 401 "unauthorized"
    exit 0
}
```

- Not constant-time comparison (acknowledged in `thoughts/shared/plans/2026-02-13-gastropod-security-hardening.md:29`)
- Auth is optional — empty `SQUASH_AUTH_TOKEN` means no auth

#### 3g. Lifetime Enforcement (testable without VM)

**Location**: `bin/sq-reaper:1-31`

Background loop every 10 seconds. Reads `max_lifetime_s` from metadata, compares with `created` timestamp, calls `destroy_sandbox` if expired.

**Testability**: Works in any environment. Create a sandbox with `max_lifetime_s=15`, wait 20 seconds, verify it's destroyed.

### 4. What You Can't Test Without Real Infrastructure

| Requirement | Minimum Environment |
|---|---|
| Chroot sandbox lifecycle (create/exec/destroy) | Privileged Docker on any Linux |
| cgroup enforcement (OOM kill, CPU throttle) | Privileged Docker with cgroups v2 |
| iptables egress rules (create/verify) | Privileged Docker with iptables |
| Actual egress filtering in chroot mode | Needs investigation — veth setup may not work correctly without entering netns |
| Firecracker VM boot + vsock communication | Bare metal with `/dev/kvm` (or nested virt) |
| Firecracker egress filtering (tap device) | Bare metal with `/dev/kvm` |
| Secret proxy (HTTP) | Any environment with socat |
| Secret proxy (HTTPS) | Not implemented |
| End-to-end: sandbox can't read real secrets | Privileged Docker + secret proxy running |
| End-to-end: sandbox can't reach blocked hosts | Privileged Docker + working egress filtering |

### 5. Architectural Confusion in the README

The README is somewhat confusing because it describes the system from two perspectives simultaneously without clearly separating them:

1. **As a tool you deploy** (control plane perspective): "docker run... create a sandbox... run a command" — implies it's an API service that manages sandboxes
2. **As the sandbox environment** (runtime perspective): "overlayfs merge... chroot target... Firecracker microVM" — implies it IS the sandbox

The reality is that it's both, bundled in one container. There is no concept of a remote sandbox agent, a separate sandbox node, or a control-plane-to-data-plane split. The `--privileged` Docker container is simultaneously:
- The HTTP API server (busybox httpd)
- The sandbox orchestrator (common.sh dispatch)
- The sandbox host (overlayfs, cgroups, iptables, Firecracker VMM)
- The secret proxy (sq-secret-proxy)
- The reaper (sq-reaper)

This single-container-does-everything approach is intentional — it's the "PorteuX pattern, distro-agnostic" ethos applied to sandbox management. But it means the security boundary between "management" and "execution" is entirely within-process.

## Code References

- `entrypoint.sh:1-12` — Process tree root: init, reaper, proxy, API
- `bin/start-api:1-10` — busybox httpd launch
- `cgi-bin/common.sh:15-25` — Input validation functions
- `cgi-bin/common.sh:119-125` — Bearer token auth
- `cgi-bin/common.sh:227-248` — cgroup setup/teardown
- `cgi-bin/common.sh:252-341` — Network namespace + iptables egress
- `cgi-bin/common.sh:345-413` — Chroot backend: create sandbox
- `cgi-bin/common.sh:417-463` — Chroot backend: exec in sandbox
- `cgi-bin/common.sh:610-653` — Firecracker backend: network (tap)
- `cgi-bin/common.sh:657-701` — Firecracker backend: create sandbox
- `cgi-bin/common.sh:705-733` — Firecracker backend: exec (via sq-firecracker)
- `bin/sq-firecracker:1-139` — Firecracker VM lifecycle (start, exec, stop, add-drive)
- `bin/sq-secret-proxy:1-132` — HTTP-only secret materialization proxy
- `bin/sq-reaper:1-31` — Background sandbox reaper
- `vm/init:1-80` — Firecracker guest PID 1
- `vm/sq-vsock-handler:1-92` — Guest-side vsock command handler
- `docs/security.md:1-89` — User-facing security documentation
- `docs/plan-firecracker-and-security.md:1-1136` — Full design doc with phase breakdown

## Historical Context (from thoughts/)

- `thoughts/shared/research/2026-02-13-security-model-comparison.md` — Compares Squash to Deno Sandbox and NanoClaw; documents that Squash chroot mode explicitly does NOT use `--net` in the unshare call
- `thoughts/shared/plans/2026-02-13-gastropod-security-hardening.md` — Security hardening plan: input validation, command injection fix, eval elimination, JSON hygiene. Many of these changes appear to already be implemented in the current code

## Open Questions

1. **Does chroot-mode egress filtering actually work?** The veth pair is created and iptables rules are installed, but the sandbox process via `unshare` doesn't enter a network namespace (`--net` is absent). The sandbox may use the host network stack directly, making the iptables FORWARD rules on the veth interface ineffective. This needs integration testing.

2. **Is `--map-root-user` effective in a `--privileged` container?** The `unshare` flag maps UID 0 inside the namespace to an unprivileged UID outside. But when the container itself runs as root with `--privileged`, the host UID mapping may still be UID 0. The actual isolation provided depends on the user namespace configuration of the outer container.

3. **Secret proxy env injection is not implemented.** The design doc describes `_inject_secret_placeholders()` which would set `http_proxy` inside sandboxes, but this function doesn't exist in the current codebase. Without it, sandboxes don't automatically route through the secret proxy.

4. **CID collision risk in Firecracker mode.** `sq-firecracker:21` assigns vsock CIDs based on the count of sandbox directories, not by tracking used CIDs. If sandboxes are created and destroyed out of order, CIDs could collide.

5. **No tests exist.** There are no automated tests, integration tests, or CI pipelines in the repository. All verification described in the design doc is manual.
