# Unprivileged Sandbox — Drop Docker, Go Fully Userspace

## Overview

Replace all privileged kernel operations (loop mounts, overlayfs, chroot, veth/iptables)
with unprivileged userspace alternatives (squashfuse, fuse-overlayfs, bubblewrap, pasta).
Remove the Docker container wrapper entirely. Deploy via Nix profile, static tarball, or
NixOS module. The secret proxy remains the credential security boundary; network egress
filtering (iptables) is dropped.

## Current State Analysis

Every sandbox operation requires `--privileged` Docker:
- `mount -o loop -t squashfs` → CAP_SYS_ADMIN
- `mount -t overlay` → CAP_SYS_ADMIN
- `mount -t tmpfs` → CAP_SYS_ADMIN
- `ip netns add` + veth + iptables → CAP_NET_ADMIN
- `chroot` → CAP_SYS_CHROOT
- cgroup writes → CAP_SYS_ADMIN

Docker provides zero sandbox isolation. It's just packaging.

### Key Discoveries:
- All sandbox lifecycle logic lives in `shared/cgi-bin/common.sh` (shell impl) and is
  reimplemented in each compiled daemon
- Network egress filtering (veth + iptables) is ~120 lines in common.sh:309-454
- The secret proxy already prevents credential leakage regardless of network access
- Nix already builds every dependency: all 6 daemons, all modules, the Go proxy
- All 5 key replacement tools are in nixpkgs: squashfuse 0.6.1, fuse-overlayfs 1.16,
  bubblewrap 0.11.0, passt, slirp4netns 1.3.3

## Desired End State

- sq-sandbox runs as a regular unprivileged user process on bare Linux
- No Docker, no --privileged, no CAP_SYS_ADMIN, no CAP_NET_ADMIN
- Sandboxes mount squashfs layers via squashfuse (FUSE)
- Overlay merges via fuse-overlayfs
- Sandbox exec via bubblewrap (pivot_root, namespace isolation, capability dropping)
- Networking via host network or pasta (no veth/iptables)
- Secret proxy still intercepts and injects credentials
- Deployable via: `nix profile install`, `nix run`, static tarball, or NixOS module

### Verification:
- Full sandbox lifecycle (create/exec/snapshot/restore/destroy) works as unprivileged user
- `sq-test` integration tests pass without root
- Secret proxy credential injection works
- No capabilities or setuid needed

## What We're NOT Doing

- Per-sandbox network egress filtering (iptables) — dropped, secret proxy is the boundary
- Firecracker backend changes — stays as-is (already requires /dev/kvm, separate concern)
- Updating compiled daemon internals to call FUSE directly — they'll shell out to helpers
- GPU passthrough, live migration, multi-tenant hardening

## Implementation Approach

Create three new shared helper scripts that wrap the unprivileged tools. These become the
interface between all 6 daemon implementations and the kernel. Each helper detects the
available tool and falls back gracefully (squashfuse → kernel mount if root).

Then update common.sh (the reference implementation) to use them. Rip out the veth/iptables
networking stack. Update flake.nix deps. Remove Docker artifacts. Add Nix deployment.

---

## Phase 1: Unprivileged Mount Helpers

### Overview
Create `sq-mount-layer` and `sq-mount-overlay` — shared helper scripts that all
implementations can call instead of raw `mount` syscalls.

### Changes Required:

#### 1. `shared/bin/sq-mount-layer`
New file. Mounts a squashfs file at a mount point using squashfuse.

```sh
#!/bin/sh
# sq-mount-layer — mount a squashfs file read-only (unprivileged)
# Usage: sq-mount-layer <squashfs-file> <mount-point>
# Usage: sq-mount-layer --unmount <mount-point>
set -eu

case "${1:-}" in
  --unmount|-u)
    fusermount -u "$2" 2>/dev/null || umount "$2" 2>/dev/null || umount -l "$2"
    ;;
  *)
    sqfs="$1" mp="$2"
    mkdir -p "$mp"
    if command -v squashfuse >/dev/null 2>&1; then
      squashfuse -o ro "$sqfs" "$mp"
    else
      mount -o loop,ro -t squashfs "$sqfs" "$mp"
    fi
    ;;
esac
```

#### 2. `shared/bin/sq-mount-overlay`
New file. Creates a fuse-overlayfs merge from lowerdirs + upper + work → merged.

```sh
#!/bin/sh
# sq-mount-overlay — create overlay merge (unprivileged)
# Usage: sq-mount-overlay <lowerdir:...> <upperdir> <workdir> <merged>
# Usage: sq-mount-overlay --unmount <merged>
set -eu

case "${1:-}" in
  --unmount|-u)
    fusermount -u "$2" 2>/dev/null || umount "$2" 2>/dev/null || umount -l "$2"
    ;;
  *)
    lower="$1" upper="$2" work="$3" merged="$4"
    mkdir -p "$merged"
    if command -v fuse-overlayfs >/dev/null 2>&1; then
      fuse-overlayfs -o "lowerdir=$lower,upperdir=$upper,workdir=$work" "$merged"
    else
      mount -t overlay overlay \
        -o "lowerdir=$lower,upperdir=$upper,workdir=$work" "$merged"
    fi
    ;;
esac
```

### Success Criteria:

#### Automated:
- [ ] `sq-mount-layer test.squashfs /tmp/mnt` works as non-root user
- [ ] `sq-mount-overlay /tmp/mnt /tmp/upper /tmp/work /tmp/merged` works as non-root
- [ ] Writing to merged/ lands in upper/
- [ ] `sq-mount-layer --unmount /tmp/mnt` cleans up
- [ ] Falls back to kernel mount when running as root without FUSE tools

#### Manual:
- [ ] Mount 3-5 stacked layers and verify correct file precedence

---

## Phase 2: Bubblewrap Exec Helper

### Overview
Create `sq-exec` that replaces the `ip netns exec ... unshare ... chroot` chain.

### Changes Required:

#### 1. `shared/bin/sq-exec`
New file. Executes a command inside a sandbox using bubblewrap.

```sh
#!/bin/sh
# sq-exec — execute command in sandbox (unprivileged)
# Usage: sq-exec <merged-root> <cmd> [workdir] [timeout]
set -eu

root="$1" cmd="$2" workdir="${3:-/}" timeout_s="${4:-300}"

if command -v bwrap >/dev/null 2>&1; then
  timeout "$timeout_s" bwrap \
    --bind "$root" / \
    --dev /dev \
    --proc /proc \
    --tmpfs /tmp \
    --tmpfs /run \
    --ro-bind /etc/resolv.conf /etc/resolv.conf \
    --unshare-pid \
    --unshare-ipc \
    --unshare-uts \
    --die-with-parent \
    --chdir "$workdir" \
    /bin/sh -c "$cmd"
else
  # Fallback: classic unshare + chroot
  timeout "$timeout_s" \
    unshare --mount --pid --ipc --uts --fork --map-root-user \
    chroot "$root" \
    /bin/sh -c "cd \"$workdir\" 2>/dev/null || true; $cmd"
fi
```

### Success Criteria:

#### Automated:
- [ ] `sq-exec /tmp/merged "echo hello"` works as non-root
- [ ] Process isolation: PID 1 inside sandbox
- [ ] Mount isolation: sandbox cannot see host mounts
- [ ] Timeout works: long-running command killed after timeout
- [ ] Falls back to unshare+chroot when bwrap unavailable

---

## Phase 3: Simplify Networking

### Overview
Remove the entire veth/iptables/netns stack. Sandboxes use host networking.
Secret proxy remains the credential security boundary.

### Changes Required:

#### 1. `shared/cgi-bin/common.sh` — Remove networking functions
Delete these functions entirely:
- `_allocate_netns_index` (lines 309-325)
- `_chroot_setup_netns` (lines 327-380)
- `_apply_egress_rules` (lines 385-422)
- `_chroot_teardown_netns` (lines 424-454)

#### 2. `shared/cgi-bin/common.sh` — Simplify create
Remove netns setup call from `_chroot_create_sandbox`. Remove netns metadata writes
(netns_index, veth_host, veth_sandbox, netns_name).

#### 3. `shared/cgi-bin/common.sh` — Simplify exec
Remove `ip netns exec` wrapper from exec path. Just call `sq-exec`.

#### 4. `shared/cgi-bin/common.sh` — Simplify destroy
Remove netns/veth/iptables teardown from destroy path.

#### 5. Secret proxy address
Change proxy host from `10.200.<idx>.1` (veth gateway) to `127.0.0.1` (localhost).
The proxy listens on the same host, sandboxes inherit host networking, so localhost works.

### Success Criteria:

#### Automated:
- [ ] Create/exec/destroy cycle works without any ip/iptables commands
- [ ] No veth interfaces created
- [ ] No iptables rules created
- [ ] Secret proxy injection still works (placeholders replaced on outbound requests)

---

## Phase 4: Update common.sh to Use Helpers

### Overview
Wire the helpers from Phases 1-2 into common.sh, replacing direct mount/unshare calls.

### Changes Required:

#### 1. Mount layers via sq-mount-layer
Replace `mount -o loop,ro -t squashfs` at common.sh:501 with `sq-mount-layer`.

#### 2. Mount overlay via sq-mount-overlay
Replace `mount -t overlay` at common.sh:213-219 with `sq-mount-overlay`.

#### 3. Upper layer
Replace `mount -t tmpfs` at common.sh:480-481 with `mkdir -p` (plain directory).
The tmpfs was for size limiting — accept that size limiting requires root, or
use `fallocate` + loop mount as an optional enhancement later.

#### 4. Exec via sq-exec
Replace the `ip netns exec ... unshare ... chroot` chain at common.sh:575-587
with `sq-exec "$s/merged" "$cmd" "$workdir" "$timeout_s"`.

#### 5. Unmount via helpers
Replace `umount` and `umount -l` calls with `sq-mount-layer --unmount` and
`sq-mount-overlay --unmount`.

#### 6. Cgroups
Remove cgroup setup/teardown (`_cgroup_setup`, `_cgroup_teardown`). These require
root. Resource limiting without root is possible via systemd scope units or
`systemd-run --user --scope`, but that's a future enhancement.

### Success Criteria:

#### Automated:
- [ ] Full sandbox lifecycle: create → exec → snapshot → restore → destroy as non-root
- [ ] `sq-test` integration suite passes (adapted for non-root)
- [ ] Secret proxy placeholder injection works
- [ ] Snapshot produces valid squashfs file
- [ ] Restore remounts with snapshot as top layer

#### Manual:
- [ ] Run a Python script inside a 3-layer sandbox (base + python + snapshot)
- [ ] Verify snapshot/restore cycle preserves state

**Implementation Note**: Pause here for manual testing before Phase 5.

---

## Phase 5: Update flake.nix

### Overview
Add unprivileged tools to dependencies, add deployment targets.

### Changes Required:

#### 1. Runtime deps
Add to all devShells and images:
- `squashfuse`
- `fuse-overlayfs`
- `bubblewrap`
- `passt` (optional, for isolated networking mode)

Remove from required (keep in optional):
- `iproute2`
- `iptables`

#### 2. Systemd unit
Add a `systemd-unit` package that generates a systemd service file for squashd.

#### 3. NixOS module
Add `nix/nixos-module.nix` that provides `services.sq-sandbox = { enable, dataDir, port, ... }`.

#### 4. Static tarball
Add a `tarball` package using `pkgs.buildEnv` + `pkgs.runCommand` to create a
self-contained tarball with the daemon + all runtime deps.

### Success Criteria:

#### Automated:
- [ ] `nix build .#squashd-shell` includes squashfuse, fuse-overlayfs, bubblewrap in PATH
- [ ] `nix build .#tarball` produces a working self-contained archive
- [ ] `nix flake check` passes

---

## Phase 6: Remove Docker Artifacts

### Overview
Remove Docker-specific files and update documentation.

### Changes Required:

#### 1. Remove files
- `Dockerfile`
- `Earthfile`
- `shared/docker-compose.firecracker.yml`

#### 2. Replace entrypoints
Replace `shared/entrypoint-v3.sh` and `shared/entrypoint-v4.sh` with a single
`shared/bin/sq-start` that works on bare metal:
- Start secret proxy if secrets.json exists
- Start tailscale if TAILSCALE_AUTHKEY set
- Run sq-init
- Start sq-reaper
- Exec daemon

#### 3. Update README.md
Replace Quick Start from `docker run --privileged` to:
```sh
nix run github:pyrex41/sq-sandbox#squashd-shell
# or
nix profile install github:pyrex41/sq-sandbox#squashd-shell
sq-start
```

Remove `--privileged` references throughout. Document the unprivileged architecture.

### Success Criteria:

#### Automated:
- [ ] No Docker/Dockerfile/Earthfile references remain
- [ ] `sq-start` launches the full stack as non-root
- [ ] README examples work

---

## Testing Strategy

### Integration Tests:
- Adapt `shared/bin/sq-test` to run without root
- Test full lifecycle: create → exec → activate → snapshot → restore → exec → destroy
- Test secret proxy injection
- Test module building (mksquashfs still needs -all-root flag)

### Manual Testing:
1. Fresh `nix run .#squashd-shell` on bare Linux VM
2. Create sandbox with 3 layers
3. Execute Python/Node scripts inside
4. Snapshot, destroy, recreate, restore
5. Verify secret proxy works end-to-end

## References

- [squashfuse](https://github.com/vasi/squashfuse) — FUSE squashfs mounts
- [fuse-overlayfs](https://github.com/containers/fuse-overlayfs) — FUSE overlay filesystem
- [bubblewrap](https://github.com/containers/bubblewrap) — unprivileged sandboxing
- [passt/pasta](https://passt.top/passt/about/) — unprivileged networking
- Research: `thoughts/shared/research/2026-02-19-architecture-isolation-and-nix.md`
- Original plan: `thoughts/shared/plans/plan-firecracker-and-security.md`
