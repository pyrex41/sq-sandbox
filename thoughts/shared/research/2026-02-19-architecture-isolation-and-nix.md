---
date: 2026-02-19T19:31:09Z
researcher: reuben
git_commit: 35279731f798be2c1dc1211dd95892e15e04e68c
branch: feat/squash-nullclaw-integration
repository: sq-sandbox
topic: "Architecture, Isolation Mechanisms, Container Role, and Nix Infrastructure"
tags: [research, codebase, architecture, isolation, containers, nix, squashfs, overlayfs, namespaces, security]
status: complete
last_updated: 2026-02-19
last_updated_by: reuben
---

# Research: Architecture, Isolation Mechanisms, Container Role, and Nix Infrastructure

**Date**: 2026-02-19T19:31:09Z
**Researcher**: reuben
**Git Commit**: 35279731f798be2c1dc1211dd95892e15e04e68c
**Branch**: feat/squash-nullclaw-integration
**Repository**: sq-sandbox

## Research Question

1. What does the current architecture look like — how does squashfs mounting, overlayfs, and sandbox restoration work?
2. What role do containers actually play — are we getting real security/isolation benefits or just complexity?
3. How is Nix used for development and environment setup — what does the flake provide?
4. What isolation mechanisms exist beyond containers (namespaces, seccomp, landlock, etc.)?
5. What are the simplest/leanest approaches to providing per-user ephemeral sandboxes without requiring containers at all?

## Summary

sq-sandbox is a composable sandbox system built from stacked squashfs layers merged via overlayfs, with a writable tmpfs upper layer. The Docker container serves purely as a packaging and deployment vehicle — all actual isolation is performed by Linux kernel primitives (namespaces, cgroups, overlayfs, iptables) that the container must be `--privileged` to access. The container itself provides **zero sandbox isolation** — it's effectively a deployment tarball that happens to need root. Nix provides reproducible builds for six daemon implementations, squashfs module construction, OCI image assembly, and per-language dev shells. The entire isolation stack (mount/PID/IPC/UTS/net namespaces, cgroups v2, veth pairs, iptables egress rules) operates on raw Linux syscalls and could run identically on bare metal, a systemd service, or within a Nix profile — without Docker at all.

---

## Detailed Findings

### 1. Current Architecture: SquashFS + OverlayFS + Sandbox Lifecycle

#### Layer Mounting

Every squashfs module (e.g., `000-base-alpine.squashfs`, `100-python312.squashfs`) is loop-mounted read-only at sandbox creation time. Each module gets its own mount point at `$SANDBOXES/<id>/images/<mod>.squashfs/`.

**Key code**: `shared/cgi-bin/common.sh:501`
```sh
mount -o loop,ro -t squashfs "$(mod_path "$mod")" "$mp"
```

#### OverlayFS Merge

All read-only layers are combined with a writable upper layer via overlayfs:

**Key code**: `shared/cgi-bin/common.sh:213-219`
```sh
mount -t overlay overlay \
    -o "lowerdir=$lower,upperdir=$s/upper/data,workdir=$s/upper/work" \
    "$s/merged"
```

The `lowerdir` string is built by `_lowerdir()` (`common.sh:197-210`): snapshot first (highest priority), then module images in reverse sort order (higher-numbered modules win). The merged view at `$s/merged` is the chroot target.

#### Writable Upper Layer

The upper layer is a size-capped tmpfs (`common.sh:480-481`):
```sh
mount -t tmpfs -o "size=${upper_limit}M" tmpfs "$s/upper"
```
Default 512MB, configurable via `SQUASH_UPPER_LIMIT_MB`. All writes land here as copy-on-write deltas.

#### Snapshot

`mksquashfs "$s/upper/data" "$snapfile"` (`common.sh:651`) compresses the writable layer into a squashfs archive. Uses zstd if kernel supports it, gzip otherwise. Optionally pushes to S3.

#### Restore

Restore (`common.sh:664-707`):
1. Lazy-unmounts the merged overlayfs
2. Unmounts any active snapshot
3. Clears `upper/data` and `upper/work` (tmpfs stays)
4. Loop-mounts the snapshot squashfs at `_snapshot/`
5. Remounts overlay — `_lowerdir()` now includes `_snapshot/` as highest-priority layer
6. Result: fresh empty upper on top of the frozen snapshot

#### Sandbox Creation Flow

`_chroot_create_sandbox()` in `common.sh:458-542`:
1. Validate ID uniqueness and `SQUASH_MAX_SANDBOXES` limit
2. Create directory tree, mount tmpfs on `upper/`
3. Write metadata files (owner, task, layers, cpu, memory, etc.)
4. Set up cgroup (`/sys/fs/cgroup/squash-<id>` with `cpu.max` and `memory.max`)
5. Loop-mount each squashfs module read-only
6. Mount overlayfs
7. Set up network namespace (veth pair, iptables NAT, egress rules)
8. Seed `/etc/resolv.conf` pointing at veth gateway
9. Inject secret placeholder env vars + proxy CA cert
10. Optionally auto-restore from S3 in ephemeral mode

#### Exec Path

**Key code**: `common.sh:575-587`
```sh
ip netns exec "squash-$id" \
    unshare --mount --pid --ipc --uts --fork --map-root-user \
    chroot "$s/merged" \
    /bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$workdir" "$cmd"
```

Each exec enters the sandbox's persistent network namespace, creates fresh mount/PID/IPC/UTS namespaces, then chroots into the merged overlay.

---

### 2. What Role Containers Actually Play

#### What Docker Provides

The Docker container (`Dockerfile:1-38`, `nix/image.nix:30-66`) serves as:

1. **Packaging format** — bundles Alpine base + runtime deps (squashfs-tools, util-linux, iproute2, iptables, jq, socat, openssl, busybox) + the daemon binary + shared shell scripts
2. **Distribution mechanism** — OCI image pushed to `ghcr.io/pyrex41/sq-sandbox:latest`
3. **Process isolation from the host** — a Docker container provides its own PID/mount/IPC namespaces for the *daemon itself*

#### What Docker Does NOT Provide

The Docker container provides **no isolation for the sandboxes**. Specifically:

- **`--privileged` is mandatory** — the container runs with full root capabilities, disabling Docker's seccomp profile, AppArmor profile, and dropping all capability restrictions. Every `docker run` command in the README includes `--privileged`.
- **Sandbox isolation is entirely self-implemented** — the daemon uses `unshare`, `ip netns`, `chroot`, `mount`, cgroups, and iptables directly. These are the same syscalls available on bare metal.
- **The container boundary is transparent** — a sandbox process running inside the Docker container has the same kernel access as a process on the host (because `--privileged` grants `CAP_SYS_ADMIN`, `CAP_NET_ADMIN`, etc.)
- **No defense-in-depth from Docker** — Docker's seccomp filter, user namespace mapping, AppArmor/SELinux profiles, read-only rootfs, and capability dropping are all bypassed by `--privileged`.

#### Operations Requiring Privilege

Every core sandbox operation requires root or specific capabilities:

| Operation | Capability | Location |
|---|---|---|
| `mount -o loop -t squashfs` | `CAP_SYS_ADMIN` | `common.sh:501` |
| `mount -t overlay` | `CAP_SYS_ADMIN` | `common.sh:213` |
| `mount -t tmpfs` | `CAP_SYS_ADMIN` | `common.sh:480` |
| `ip netns add` | `CAP_NET_ADMIN` | `common.sh:337` |
| `ip link add veth` | `CAP_NET_ADMIN` | `common.sh:340` |
| `iptables` (NAT, FORWARD) | `CAP_NET_ADMIN` | `common.sh:365-421` |
| `chroot` | `CAP_SYS_CHROOT` | `common.sh:578` |
| cgroup directory creation | `CAP_SYS_ADMIN` | `common.sh:231` |
| `mksquashfs` | root | `common.sh:651` |

#### The Container as Deployment Convenience

The container is functioning as:
- A way to bundle ~15 Alpine packages without requiring them on the host
- A way to get a known-good filesystem layout (`/app/bin`, `/app/cgi-bin`, etc.)
- A volume mount convention for persistent data (`-v squash-data:/data`)
- A port mapping convention (`-p 8080:8080`)

All of these could be achieved with: a Nix profile, a systemd unit, a tarball, or even just `PATH` manipulation.

---

### 3. How Nix is Used

#### Flake Structure (`flake.nix:1-166`)

The flake targets three systems: `x86_64-linux`, `aarch64-linux`, `aarch64-darwin`. It produces three attribute sets merged into `packages`:

```
packages = modules // daemons // images;
```

#### Development Shells (`flake.nix:124-161`)

Seven `devShells`, each pinning exactly the toolchain for one implementation:

| Shell | Packages |
|---|---|
| `default` | `jq curl git shellcheck` |
| `shell` | `shellcheck jq curl busybox` |
| `rust` | `rustc cargo rust-analyzer clippy rustfmt` |
| `odin` | `odin` |
| `zig` | `zig zls` |
| `janet` | `janet jpm` |
| `cl` | `sbcl libev libffi` |

Some packages are gated with `pkgs.lib.optionals isLinux` (busybox, libev, libffi) so shells work on macOS.

#### Daemon Builds (`flake.nix:22-117`)

Six daemons + one Go proxy, all Linux-only:

| Package | Build method | Source |
|---|---|---|
| `sq-secret-proxy` | `stdenv.mkDerivation` + `go_1_22` | `shared/proxy/` |
| `squashd-rust` | `rustPlatform.buildRustPackage` | `impl/rust/` |
| `squashd-odin` | `stdenv.mkDerivation` + `odin` | `impl/odin/` |
| `squashd-zig` | `stdenv.mkDerivation` + `zig` | `impl/zig/` |
| `squashd-janet` | `writeShellApplication` (interpreted) | `impl/janet/main.janet` |
| `squashd-shell` | `writeShellApplication` | `shared/bin/start-api` |
| `squashd-cl` | `stdenv.mkDerivation` + `sbcl` (**impure**: downloads Quicklisp) | `impl/cl/` |

#### Squashfs Module Builds (`nix/lib-squashfs.nix`, `nix/modules.nix`, `modules/*.nix`)

`mkSquashfsModule` (`nix/lib-squashfs.nix:9-23`) is the core primitive:
- Takes `{ name, buildScript }`
- Creates a temp `$rootfs`, runs `buildScript` to populate it
- Calls `mksquashfs` with zstd-15 compression, 128K blocks, `-all-root`, `-repro-time 0`
- Outputs a single `.squashfs` file

Five modules are exported from `nix/modules.nix`:
- `module-base-alpine` — Alpine 3.21.3 minirootfs
- `module-python312` — python-build-standalone
- `module-nodejs22` — nodejs.org binary
- `module-golang` — Go 1.23.6
- `module-tailscale` — Tailscale 1.94.2

Two additional modules exist but are not exported: `sqlite-libs` (musl cross-compiled) and `nullclaw` (Zig cross-compiled, placeholder hash).

#### OCI Images (`nix/image.nix:30-100`)

`mkImage` wraps `pkgs.dockerTools.buildLayeredImage`. Six images are produced, each bundling: runtime deps (squashfs-tools, util-linux, iproute2, iptables, etc.) + one daemon binary + the Go secret proxy + shared shell scripts.

Only `image-cl` uses `entrypoint-v4.sh` (daemon replaces init/reaper/httpd). All others use `entrypoint-v3.sh`.

#### What Nix Could Replace

Nix already builds everything needed to run sq-sandbox without Docker:
- The daemon binary (any of the six implementations)
- All squashfs modules (reproducible, content-addressable)
- The Go secret proxy
- All runtime dependencies (squashfs-tools, iproute2, iptables, etc.)

A `nix profile` or `nix develop` shell could provide the complete runtime environment on bare metal.

---

### 4. Isolation Mechanisms That Exist Today

#### Namespace Isolation (per-exec)

Every sandbox exec uses `unshare` with these flags (`common.sh:577`):
- `--mount` — new mount namespace
- `--pid` — new PID namespace (process sees itself as PID 1)
- `--ipc` — new IPC namespace
- `--uts` — new UTS namespace
- `--fork` — required companion to `--pid`
- `--map-root-user` — creates a user namespace, maps calling UID to UID 0 inside

**Not used**: `--net` — network namespace is handled separately via persistent `ip netns`.

#### Network Namespace (per-sandbox, persistent)

Each sandbox gets a named netns `squash-<id>` (`common.sh:337`), a veth pair connecting it to the host, and a unique `/30` subnet from `10.200.1.0/30` through `10.200.254.0/30`.

#### Egress Filtering (iptables)

Per-sandbox iptables chains on the FORWARD table (`common.sh:389-421`):
- ICMP blocked unconditionally
- DNS rate-limited (10/s burst 20)
- Established/related connections accepted
- Per-hostname ACCEPT rules (resolved via `getent hosts`)
- Default DROP

#### Cgroups v2

Per-sandbox cgroup at `/sys/fs/cgroup/squash-<id>` (`common.sh:228-248`):
- `cpu.max` — quota/period format
- `memory.max` — hard memory limit (OOM kills on exceed)

#### Chroot

Standard `chroot "$s/merged"` (`common.sh:578`). Not a security boundary on its own, but combined with PID/mount namespaces it prevents most escape vectors.

#### Seccomp (optional, not applied by default)

The README mentions a reference `seccomp.json` in the repo and documents applying it via `docker run --security-opt seccomp=seccomp.json`. But this is Docker-level seccomp for the container process — not per-sandbox seccomp. No `seccomp()` or `prctl(PR_SET_SECCOMP)` calls exist in the codebase for individual sandboxes.

#### What Does NOT Exist

- **Landlock** — not referenced anywhere in the codebase
- **Per-sandbox seccomp** — no `seccomp_load()` or BPF filter application inside sandboxes
- **AppArmor/SELinux profiles** — not referenced
- **User namespace mapping** — `--map-root-user` creates a user namespace but only maps the single calling UID. No full uid_map/gid_map configuration.
- **Pivot root** — uses `chroot` not `pivot_root` (pivot_root is considered more secure as it fully detaches the old root)

#### Firecracker Backend (alternative isolation)

The Firecracker backend (`shared/bin/sq-firecracker`) provides VM-level isolation:
- Separate guest kernel (no shared kernel attack surface)
- Communication only via vsock (port 5000)
- Resource limits via VM config (not cgroups)
- Network via tap device instead of veth/netns
- Requires `/dev/kvm` access

---

### 5. What Exists for Containerless Operation

#### Already Container-Independent

The following components have **no Docker dependency** and operate on raw Linux syscalls:

- All sandbox lifecycle operations (`common.sh`) — `mount`, `unshare`, `chroot`, `ip netns`, `iptables`, cgroup writes
- All daemon implementations — listen on a TCP socket, fork/exec sandbox commands
- Module building (`sq-mkmod`) — downloads tarballs, calls `mksquashfs`
- S3 sync (`sq-s3`) — curl + openssl SigV4 or aws-cli
- Secret proxy — Go binary or socat-based shell proxy
- Reaper — simple `while true; sleep 10` loop
- Init/recovery (`sq-init`) — directory creation, surviving sandbox remount

#### What Docker Provides That Would Need Replacement

1. **Package bundling** — the ~15 Alpine packages. Nix already provides all of these as derivations.
2. **Process management** — `entrypoint.sh` starts proxy, tailscale, init, reaper, daemon. A systemd unit or a process supervisor (s6, runit) could do this.
3. **Volume semantics** — `VOLUME /data`. Just a directory path convention.
4. **Port binding** — `-p 8080:8080`. The daemon already binds directly to `0.0.0.0:8080`.

#### Unprivileged Alternatives Referenced in Codebase

**squashfuse**: Not referenced in the codebase. Would allow mounting squashfs without `CAP_SYS_ADMIN` by using FUSE. The `mount -o loop -t squashfs` calls (`common.sh:501`) would become `squashfuse <file> <mountpoint>`.

**Unprivileged overlayfs**: Linux 5.11+ supports overlayfs in user namespaces. Not used — the current code runs overlayfs mounts from a privileged context.

**bubblewrap**: Not referenced in the codebase. Could replace the `unshare + chroot` pattern with `bwrap --ro-bind ... --dev /dev --proc /proc --unshare-all`.

**systemd-nspawn**: Not referenced. Could provide container-like isolation without Docker, with native support for overlayfs and cgroups.

---

## Architecture Documentation

### Filesystem Layout

```
/data/                          (SQUASH_DATA, configurable)
├── modules/
│   ├── 000-base-alpine.squashfs
│   ├── 100-python312.squashfs
│   └── ...
├── sandboxes/
│   └── <id>/
│       ├── .meta/              (flat files: owner, task, layers, cpu, memory_mb, etc.)
│       │   ├── log/            (exec logs: 0001.json, 0002.json)
│       │   └── snapshots.jsonl
│       ├── images/             (loop mount points for each squashfs)
│       ├── upper/              (tmpfs: data/ + work/)
│       ├── merged/             (overlayfs mount, chroot target)
│       └── snapshots/          (squashfs checkpoint files)
├── secrets.json                (optional: placeholder → real credential mapping)
├── proxy-ca/                   (optional: MITM CA cert + key)
└── vm/                         (Firecracker: guest-rootfs.ext4, vmlinux, firecracker binary)
```

### Kernel Primitive Usage Map

```
squashfs loop mount ─── CAP_SYS_ADMIN ─── could use squashfuse (FUSE, unprivileged)
overlayfs mount ──────── CAP_SYS_ADMIN ─── Linux 5.11+ supports unprivileged in userns
tmpfs mount ──────────── CAP_SYS_ADMIN ─── unprivileged in user namespace
ip netns add ─────────── CAP_NET_ADMIN ─── requires real root or specific capability
veth pair ────────────── CAP_NET_ADMIN ─── requires real root or specific capability
iptables ─────────────── CAP_NET_ADMIN ─── requires real root or specific capability
chroot ───────────────── CAP_SYS_CHROOT ── bubblewrap/pivot_root as alternatives
cgroup writes ────────── CAP_SYS_ADMIN ─── systemd delegation or cgroupfs ownership
mksquashfs ───────────── root ──────────── works unprivileged with -all-root flag
unshare --map-root-user  unprivileged ──── already works without root
```

### Entrypoint Variants

| Entrypoint | Used by | Manages |
|---|---|---|
| `entrypoint-v3.sh` | Rust, Odin, Zig, Janet, Shell images | proxy → tailscale → sq-init → sq-reaper → start-api |
| `entrypoint-v4.sh` | CL image only | proxy → tailscale → `exec squashd` (daemon handles init+reaper+http) |

### Implementation Matrix

| Impl | LOC | Binary | HTTP | Init/Reaper | S3 | HTTPS Proxy |
|---|---|---|---|---|---|---|
| Rust | ~11k | 17MB stripped | Axum | Internal | Native (aws-sdk) | Native (rcgen+rustls) |
| CL | ~3.5k | ~20MB (SBCL image) | Woo/Ningle | Internal | Native (Ironclad) | Go sidecar |
| Zig | ~10k | 2.3MB stripped | std.http | Internal | Native (SigV4) | Go sidecar |
| Odin | ~5k | 838KB | Custom | Internal | Shell (sq-s3) | Go sidecar |
| Janet | ~1.9k | 71KB src | net/server | Internal | Shell (sq-s3) | Go sidecar |
| Shell | ~1.1k | N/A | busybox httpd+CGI | External scripts | Shell (sq-s3) | Go sidecar |

---

## Historical Context (from thoughts/)

- `thoughts/shared/plans/plan-firecracker-and-security.md` — The primary design document covering 7 phases: backend abstraction, resource limits (cgroups v2), network egress control (veth + iptables), guest VM image construction, Firecracker backend (vsock protocol), secret materialization proxy, and bare-metal deployment. All 7 phases are now implemented in the codebase.

- `thoughts/shared/plans/2026-02-16-nix-monorepo-declarative-builds.md` — Plan for consolidating all 6 implementations into a Nix flake monorepo with reproducible squashfs module builds, per-language dev shells, and OCI image construction. This plan is now implemented in `flake.nix`.

- `thoughts/shared/research/2026-02-15-main-vs-janet-vs-odin-review.md` — Comparative review of Shell, Janet, and Odin implementations across architecture, performance, reliability, security, and deployment.

- `thoughts/shared/research/2026-02-14-worktree-scorecard.md` — Scorecard of 5 implementations with post-Firecracker gap analysis.

## Related Research

- `thoughts/shared/research/2026-02-15-main-vs-janet-vs-odin-review.md`
- `thoughts/shared/research/2026-02-14-worktree-scorecard.md`
- `thoughts/shared/research/2026-02-15-odin-optimization-deep-dive.md`

## Open Questions

1. **squashfuse viability**: Could `squashfuse` replace kernel squashfs loop mounts to eliminate `CAP_SYS_ADMIN` for read operations? Performance implications for stacking 3-5 layers via FUSE?

2. **Unprivileged overlayfs**: Linux 5.11+ supports overlayfs in user namespaces. Does this work with squashfuse-mounted lowerdirs? What kernel version is the minimum deployment target?

3. **Network namespace without CAP_NET_ADMIN**: The veth + iptables setup is the hardest piece to make unprivileged. Options: slirp4netns (used by rootless Podman), pasta (newer alternative), or accepting that network isolation requires some privilege.

4. **bubblewrap vs raw unshare**: bubblewrap handles pivot_root, /proc remounting, and capability dropping in a single call. Could it replace the current `unshare + chroot` pattern while being more secure?

5. **Nix-native deployment**: Could `nix run .#squashd-<impl>` on a bare Linux host (with appropriate capabilities or setuid helpers) replace the entire Docker layer? What's the minimum set of capabilities needed?

6. **systemd integration**: Could systemd-nspawn or systemd's native cgroup delegation provide the privileged operations while keeping the daemon unprivileged?
