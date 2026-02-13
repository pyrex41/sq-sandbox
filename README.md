# Squash Sandbox v3

Composable sandboxes from stacked squashfs layers. The PorteuX pattern,
distro-agnostic.

## How it works

A sandbox is a stack of read-only squashfs layers + a writable overlay:

```
┌─────────────────────────────┐
│ upper/  (writable, tmpfs)   │ ← all changes land here
├─────────────────────────────┤
│ 200-checkpoint.squashfs     │ ← optional: restored snapshot
├─────────────────────────────┤
│ 100-python312.squashfs      │ ← language runtime
├─────────────────────────────┤
│ 100-nodejs22.squashfs       │ ← another runtime
├─────────────────────────────┤
│ 000-base-alpine.squashfs    │ ← root filesystem
└─────────────────────────────┘
         ↓ overlayfs merge ↓
┌─────────────────────────────┐
│ merged/  (chroot target)    │
└─────────────────────────────┘
```

Every layer is a squashfs file containing a directory tree. Overlayfs merges
them bottom-up. The leftmost lowerdir wins on conflict (highest number prefix).

## Bases

The base layer is the root filesystem. Any Linux distro works:

| Base               | Size  | Package manager | libc  |
|--------------------|-------|-----------------|-------|
| base-alpine        | ~8MB  | apk             | musl  |
| base-debian-slim   | ~30MB | apt             | glibc |
| base-ubuntu-noble  | ~45MB | apt             | glibc |
| base-void          | ~20MB | xbps            | musl  |
| base-porteux-core  | ~200MB| installpkg      | glibc |

Default: `base-alpine`. Override per sandbox.

## Modules

Numbered layers stacked on the base:

| Range   | Purpose              | Examples                          |
|---------|----------------------|-----------------------------------|
| 000     | Base rootfs          | base-alpine, base-debian-slim     |
| 01x     | System config        | dns, locale, timezone             |
| 10x     | Language runtimes    | python312, nodejs22, golang       |
| 11x     | Build tools          | gcc, make, cmake, git             |
| 12x     | Libraries            | openssl, libffi, zlib             |
| 20x     | Services/daemons     | tailscale, nginx, postgres        |
| 9xx     | Checkpoints          | (auto-generated from upper layer) |

## Operations

```
create   → mount base + modules as overlayfs → chroot-ready sandbox
exec     → unshare + chroot into merged → run command → log result
activate → add module layer → remount overlayfs (upper preserved)
snapshot → mksquashfs upper/ → numbered checkpoint
restore  → mount checkpoint as layer → clear upper → remount
destroy  → unmount everything → rm -rf
```

## API

```
GET    /cgi-bin/health                    status
GET    /cgi-bin/api/sandboxes             list
POST   /cgi-bin/api/sandboxes             create
GET    /cgi-bin/api/sandboxes/:id         info
DELETE /cgi-bin/api/sandboxes/:id         destroy
POST   /cgi-bin/api/sandboxes/:id/exec    run command
POST   /cgi-bin/api/sandboxes/:id/activate  add module
POST   /cgi-bin/api/sandboxes/:id/snapshot  checkpoint
POST   /cgi-bin/api/sandboxes/:id/restore   restore
GET    /cgi-bin/api/sandboxes/:id/logs    history
GET    /cgi-bin/api/modules               available modules
POST   /cgi-bin/api/modules/build         build module from preset
```

## Files

```
bin/
  sq-init             first-boot: build base modules, init dirs
  sq-mkbase           build a base squashfs from a docker image or debootstrap
  sq-mkmod            build a module squashfs (from dir, preset, or sandbox)
  sq-ctl              CLI for the API
  start-api           launch busybox httpd
  setup-tailscale     join tailnet (optional)
cgi-bin/
  common.sh           shared functions (mount, overlay, exec, checkpoint)
  api/sandboxes       REST handler
  api/modules         module listing + build trigger
  health              health check
deploy/
  hetzner/            docker-compose + cloud-init (€12/mo)
  fly/                fly.toml ($62/mo)
  aws/                ECS task definition ($170/mo)
Dockerfile            Alpine host image
entrypoint.sh         init → api
```

## What this is NOT

- Not a VM. Namespace isolation (mount, pid, user), not hardware boundaries.
- Not for untrusted multi-tenant. Good for internal agents, CI, dev envs.
- Not PorteuX. Uses the same squashfs+overlayfs pattern, any distro as base.
