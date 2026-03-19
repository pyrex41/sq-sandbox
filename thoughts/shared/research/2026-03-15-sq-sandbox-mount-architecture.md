---
date: 2026-03-15T02:05:37+0000
researcher: reuben
git_commit: afdd91dd917e09eaf43e230cf183a05074063dfc
branch: feat/squash-nullclaw-integration
repository: sq-sandbox
topic: "sq-sandbox mount architecture: FUSE vs kernel mounts, privileged mode implications"
tags: [research, codebase, sq-sandbox, squashfs, fuse, overlayfs, kubernetes, privileged]
status: complete
last_updated: 2026-03-15
last_updated_by: reuben
---

# Research: sq-sandbox Mount Architecture

**Date**: 2026-03-15T02:05:37+0000
**Researcher**: reuben
**Git Commit**: afdd91dd917e09eaf43e230cf183a05074063dfc
**Branch**: feat/squash-nullclaw-integration
**Repository**: sq-sandbox

## Research Question

How does sq-sandbox mount squashfs layers and overlayfs? What are the FUSE vs kernel mount paths? Given that the sidecar runs privileged, can we eliminate FUSE entirely and use kernel mounts for better performance and to avoid FUSE visibility issues in containers?

## Summary

The mount architecture is entirely controlled by two shared shell scripts (`sq-mount-layer` and `sq-mount-overlay`), which all three compiled implementations (Rust, Zig, Odin) shell out to. These scripts **already have kernel mount fallback paths** — they prefer FUSE tools when available, but fall back to kernel mounts (`mount -t squashfs -o loop` and `mount -t overlay`) when FUSE tools are absent. The deployed container includes both squashfuse and fuse-overlayfs, so the FUSE path is always taken. Since the sidecar runs `privileged: true`, kernel mounts would work and would eliminate the FUSE mount visibility problem observed in the K8s deployment.

## Detailed Findings

### Mount Decision Flow

Two scripts make the FUSE-vs-kernel decision:

**`shared/bin/sq-mount-layer` (squashfs layers):**
- Line 27-28: If `squashfuse` is on PATH → `squashfuse -o ro,allow_other <sqfs> <mp>` (FUSE)
- Line 29-30: Else → `mount -o loop,ro -t squashfs <sqfs> <mp>` (kernel loop mount)

**`shared/bin/sq-mount-overlay` (overlay filesystem):**
- Line 27-29: If `fuse-overlayfs` is on PATH → `fuse-overlayfs -o lowerdir=...,upperdir=...,workdir=... <merged>` (FUSE)
- Line 31-33: Else → `mount -t overlay overlay -o lowerdir=...,upperdir=...,workdir=... <merged>` (kernel)

**No compiled implementation calls mount syscalls directly.** All three (Rust `impl/rust/src/sandbox/mounts.rs`, Zig `impl/zig/src/mounts.zig`, Odin `impl/odin/src/mounts.odin`) spawn `sq-mount-layer` and `sq-mount-overlay` as child processes.

### The FUSE Visibility Problem

In the current K8s deployment, FUSE mounts created inside the sidecar container are not reliably visible to other processes within the same container. This manifests as:
- squashfuse mount appears in `/proc/self/mountinfo`
- `ls` on the mount point returns `d?????????` or "No such file or directory"
- fuse-overlayfs creates an overlay that appears mounted but has empty contents
- `sq-exec` (which uses bwrap or chroot) fails with `/bin/sh: No such file or directory`

This is a known class of issues with FUSE in containers:
- FUSE mounts are tied to the daemon process's mount namespace
- [kubernetes/kubernetes#129550](https://github.com/kubernetes/kubernetes/issues/129550) tracks FUSE cleanup issues in emptyDir volumes
- [containers/podman#22558](https://github.com/containers/podman/issues/22558) tracks cross-container FUSE visibility

### Performance: FUSE vs Kernel

From [apptainer/apptainer#665](https://github.com/apptainer/apptainer/issues/665) benchmarks:

| Method | Random read IOPS | Metadata ops | CPU overhead |
|--------|-----------------|--------------|-------------|
| Kernel squashfs | ~16x squashfuse | Fast | Kernel threads |
| squashfuse (single-threaded) | Baseline | Very slow | 100% one core |
| squashfuse_ll (multi-threaded) | ~16x squashfuse | Better | Distributed |
| fuse-overlayfs | ~2x overhead vs kernel overlayfs | Similar | Per-I/O context switches |

For the rho-cli agent workload (heavy filesystem I/O across many files), kernel mounts would provide significantly better performance.

### What's Needed for Kernel Mounts

The sidecar already runs `privileged: true`, granting `CAP_SYS_ADMIN` and all other capabilities. For kernel squashfs loop mounts to work:

1. **Loop devices**: Need `/dev/loop*` accessible. Currently the container only mounts `/dev/fuse`. Options:
   - Bind-mount full `/dev` from host (simplest)
   - Or `mknod` loop devices inside the privileged container
   - Or use `losetup` which auto-creates them (works if the container sees the loop device subsystem)

2. **Kernel modules**: `squashfs` and `overlay` modules must be loaded on the EKS worker nodes (they are on standard Amazon Linux 2 / AL2023 AMIs)

3. **AppArmor**: Must be unconfined or not restricting mount syscalls (EKS doesn't use AppArmor by default)

### The Init System Already Uses Kernel Mounts

The Zig `init.zig` `remountOneSandbox` function (line 207-253) directly calls `mount -t squashfs -o ro` for crash recovery, **not** `sq-mount-layer`. Similarly, `remountOverlayViaShell` (line 287-310) calls `mount -t overlay overlay -o ...` directly. This confirms the kernel mount path works in the codebase — it's just not the default path for new sandbox creation because the scripts prefer FUSE tools.

### Existing Implementations

| Implementation | Source | Mount mechanism |
|----------------|--------|----------------|
| Rust | `impl/rust/src/sandbox/mounts.rs` | Shells out to `sq-mount-layer` / `sq-mount-overlay` |
| Zig | `impl/zig/src/mounts.zig` | Shells out to `sq-mount-layer` / `sq-mount-overlay` |
| Odin | `impl/odin/src/mounts.odin` | Shells out via `run_cmd("sq-mount-layer", ...)` |
| Shell/CGI | `shared/cgi-bin/common.sh` | Calls `sq-mount-layer` / `sq-mount-overlay` |
| Init recovery | `impl/zig/src/init.zig` | Direct `mount -t squashfs` and `mount -t overlay` |
| Firecracker | `shared/bin/sq-firecracker` | Guest kernel mounts; host never loop-mounts |

### Nix Module Build System

All squashfs modules are built via `nix/lib-squashfs.nix:mkSquashfsModule` using `mksquashfs` with zstd compression level 15, 128K block size. Modules are pure filesystem snapshots — no runtime dependency on FUSE. They work equally well with kernel mount or squashfuse.

### Dockerfile Runtime Packages

The current `Dockerfile.zig` installs both FUSE tools AND kernel mount prerequisites:
- `squashfs-tools` — `mksquashfs`/`unsquashfs` (creation only, not mount)
- `squashfuse` — FUSE-based squashfs mounting
- `fuse-overlayfs` — FUSE-based overlay
- `fuse3` — FUSE3 kernel interface
- `bubblewrap` — sandbox execution (uses `pivot_root`)
- `util-linux` — `mount`, `umount`, `losetup`, `unshare`, `nsenter`

The `util-linux` package provides all tools needed for kernel mounts. The FUSE packages (`squashfuse`, `fuse-overlayfs`, `fuse3`) would be unnecessary if kernel mounts are used.

## Architecture Options

### Option A: Remove FUSE tools from image

Remove `squashfuse`, `fuse-overlayfs`, and `fuse3` from the Dockerfile. The existing fallback paths in `sq-mount-layer` and `sq-mount-overlay` will automatically use kernel mounts. Additionally remove the `dev-fuse` hostPath volume from the deployment.

**Requires**: `/dev/loop*` accessible in the container (add `/dev` hostPath volume or `mknod` in init).

### Option B: Add `SQUASH_MOUNT_MODE` config

Add an environment variable like `SQUASH_MOUNT_MODE=kernel` that the scripts check before testing for FUSE tool presence. When set to `kernel`, skip the FUSE path entirely.

### Option C: Modify scripts to prefer kernel when privileged

Detect `id -u == 0` or check for `CAP_SYS_ADMIN` and prefer kernel mounts. Fall back to FUSE only when unprivileged.

## Code References

- `shared/bin/sq-mount-layer:27-31` — FUSE vs kernel decision for squashfs
- `shared/bin/sq-mount-overlay:27-34` — FUSE vs kernel decision for overlayfs
- `shared/bin/sq-exec:21-49` — bwrap/chroot execution inside merged root
- `impl/zig/src/mounts.zig:21-72` — SquashfsMount shells out to sq-mount-layer
- `impl/zig/src/mounts.zig:136-196` — OverlayMount shells out to sq-mount-overlay
- `impl/zig/src/init.zig:207-253` — Init recovery uses direct kernel mount
- `impl/zig/src/init.zig:287-310` — remountOverlayViaShell uses kernel overlayfs
- `impl/rust/src/sandbox/mounts.rs:9-101` — Rust SquashfsMount wraps sq-mount-layer
- `impl/odin/src/mounts.odin:32-97` — Odin mount wrappers
- `Dockerfile.zig:25-40` — Runtime packages including both FUSE and kernel tools
- `nix/lib-squashfs.nix:9-22` — mkSquashfsModule primitive (zstd, 128K blocks)

## Related Research

- [apptainer/apptainer#665](https://github.com/apptainer/apptainer/issues/665) — squashfuse vs kernel performance benchmarks
- [kubernetes/kubernetes#129550](https://github.com/kubernetes/kubernetes/issues/129550) — FUSE in emptyDir cleanup bug
- [moby/moby#27886](https://github.com/moby/moby/issues/27886) — loop device visibility in privileged containers
- [containers/fuse-overlayfs#306](https://github.com/containers/fuse-overlayfs/issues/306) — fuse-overlayfs/kernel incompatibility

## Open Questions

1. Are EKS AL2023 nodes guaranteed to have `squashfs` and `overlay` kernel modules loaded?
2. Are loop devices accessible inside privileged containers on EKS without explicitly mounting `/dev`?
3. If removing FUSE tools, should the Firecracker path (which doesn't use them) still be an option?
4. Should the unprivileged FUSE path be preserved as a fallback for non-K8s deployments (local dev, Fly.io)?
