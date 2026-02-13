# Squash Sandbox v3

Composable sandboxes from stacked squashfs layers. The PorteuX pattern,
distro-agnostic. Dual backend: chroot (default) or Firecracker microVM.

## How it works

A sandbox is a stack of read-only squashfs layers + a writable overlay:

```
┌─────────────────────────────┐
│ upper/  (writable, tmpfs)   │ <- all changes land here
├─────────────────────────────┤
│ 200-checkpoint.squashfs     │ <- optional: restored snapshot
├─────────────────────────────┤
│ 100-python312.squashfs      │ <- language runtime
├─────────────────────────────┤
│ 100-nodejs22.squashfs       │ <- another runtime
├─────────────────────────────┤
│ 000-base-alpine.squashfs    │ <- root filesystem
└─────────────────────────────┘
         ↓ overlayfs merge ↓
┌─────────────────────────────┐
│ merged/  (chroot target)    │
└─────────────────────────────┘
```

Every layer is a squashfs file containing a directory tree. Overlayfs merges
them bottom-up. The leftmost lowerdir wins on conflict (highest number prefix).

## Backends

Selected via the `SQUASH_BACKEND` environment variable. The API is identical
regardless of backend.

| Backend       | Isolation              | Requires             | Best for                     |
|---------------|------------------------|----------------------|------------------------------|
| `chroot`      | overlayfs + unshare    | Privileged container | Fly, ECS, any VPS            |
| `firecracker` | microVM + vsock        | `/dev/kvm`           | Bare metal, stronger isolation|

**chroot** (default): Uses `unshare --mount --pid --fork` + `chroot` into the
merged overlay. Resource limits via cgroups v2. Network isolation via network
namespaces + nftables. Works on Fly.io, AWS ECS, Hetzner VPS, or any host that
supports privileged containers.

**firecracker**: Same squashfs layer model, but layers are attached as virtio
block devices to a Firecracker microVM. The guest boots a minimal init that
mounts layers and listens on vsock for commands. Native VM-level CPU/memory
limits. Network via tap devices + nftables. Requires bare metal with KVM access.

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
create   -> mount base + modules as overlayfs -> chroot-ready sandbox
exec     -> unshare + chroot into merged -> run command -> log result
activate -> add module layer -> remount overlayfs (upper preserved)
snapshot -> mksquashfs upper/ -> numbered checkpoint
restore  -> mount checkpoint as layer -> clear upper -> remount
destroy  -> unmount everything -> rm -rf
```

## Security Features

### Resource limits

Per-sandbox CPU, memory, and lifetime limits set at creation time:

- `cpu` — CPU cores (default: 2.0). Enforced via cgroups v2 in chroot mode,
  native VM config in firecracker mode.
- `memory_mb` — Memory in megabytes (default: 1024). OOM-killed on exceed.
- `max_lifetime_s` — Auto-destroy after N seconds (default: 0 = no limit).
  Enforced by the `sq-reaper` background process.

### Network egress control

Per-sandbox outbound network whitelisting via `allow_net`:

- `[]` (default) — allow all outbound traffic (backwards compatible)
- `["api.anthropic.com", "pypi.org"]` — allow only listed hosts
- `["none"]` — block all outbound traffic

Implemented via network namespaces (chroot) or tap devices (firecracker)
with nftables/iptables rules. DNS is always permitted for allowed hosts.

### Secret materialization

The `sq-secret-proxy` injects real credentials into outbound requests without
exposing them inside the sandbox. Sandboxes see only placeholder values.

Config at `$SQUASH_DATA/secrets.json`:

```json
{
    "secrets": {
        "ANTHROPIC_API_KEY": {
            "placeholder": "sk-placeholder-anthropic",
            "value": "sk-ant-real-key-here",
            "allowed_hosts": ["api.anthropic.com"]
        }
    }
}
```

The proxy runs on the host (port 8888), intercepts HTTP requests, replaces
placeholder values with real credentials only when the destination host is
in the secret's `allowed_hosts` list. Real keys never enter the sandbox.

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

### Create sandbox (POST)

```json
{
    "id": "dev",
    "owner": "alice",
    "layers": "000-base-alpine,100-python312",
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800,
    "allow_net": ["api.anthropic.com", "pypi.org"]
}
```

All fields except `id` and `layers` are optional.

## Files

```
bin/
  sq-init             first-boot: build base modules, provision VM (firecracker), init dirs
  sq-mkbase           build a base squashfs from a docker image or debootstrap
  sq-mkmod            build a module squashfs (from dir, preset, or sandbox)
  sq-mkvm             build Firecracker guest rootfs + fetch kernel + binary
  sq-firecracker      Firecracker VM lifecycle management (start, exec, stop)
  sq-reaper           auto-destroy expired sandboxes (background)
  sq-secret-proxy     credential-injecting HTTP proxy
  sq-ctl              CLI for the API
  start-api           launch busybox httpd
  setup-tailscale     join tailnet (optional)
vm/
  init                guest PID 1 — mounts squashfs layers, starts vsock listener
  sq-vsock-handler    vsock command handler inside Firecracker guest
cgi-bin/
  common.sh           shared functions (mount, overlay, exec, checkpoint, backend dispatch)
  api/sandboxes       REST handler
  api/modules         module listing + build trigger
  health              health check
deploy/
  hetzner/            docker-compose + cloud-init (chroot mode, ~12 EUR/mo)
  hetzner-baremetal/  docker-compose for bare metal with /dev/kvm (firecracker mode)
  fly/                fly.toml ($62/mo)
  aws/                ECS task definition ($170/mo)
Dockerfile            Alpine host image (chroot mode)
Dockerfile.firecracker  Extended image with Firecracker + KVM deps
entrypoint.sh         init -> reaper -> secret proxy -> api
```

## What this is NOT

- Not for untrusted multi-tenant. Firecracker mode provides VM isolation, but
  the system is designed for internal agents, CI, and dev environments.
- Not PorteuX. Uses the same squashfs+overlayfs pattern, any distro as base.
- No GPU passthrough or live migration between backends.
