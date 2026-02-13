# Squash Sandbox

Composable sandboxes from stacked squashfs layers. The PorteuX pattern,
distro-agnostic. Dual backend: chroot (default) or Firecracker microVM.

## Quick Start

### Option A: Docker (just run it)

```sh
docker run -d --privileged \
  -p 8080:8080 \
  -v squash-data:/data \
  ghcr.io/pyrex41/sq-sandbox:latest

# Create a sandbox
curl -X POST localhost:8080/cgi-bin/api/sandboxes \
  -d '{"id":"dev","layers":"000-base-alpine,100-python312"}'

# Run a command
curl -X POST localhost:8080/cgi-bin/api/sandboxes/dev/exec \
  -d '{"cmd":"python3 -c \"print(42)\""}'
```

### Option B: Clone and customize

```sh
git clone https://github.com/pyrex41/sq-sandbox.git my-sandbox
cd my-sandbox

# Add a custom module preset in bin/sq-mkmod
# Configure deploy/ for your provider
# Set env vars for auth, S3, etc.

docker build -t my-sandbox .
docker run -d --privileged -p 8080:8080 -v data:/data my-sandbox
```

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

| Backend       | Isolation              | Requires             | Best for                      |
|---------------|------------------------|----------------------|-------------------------------|
| `chroot`      | overlayfs + unshare    | Privileged container | Fly, ECS, any VPS             |
| `firecracker` | microVM + vsock        | `/dev/kvm`           | Bare metal, stronger isolation|

## Bases

| Base          | Size  | Package manager | libc  |
|---------------|-------|-----------------|-------|
| base-alpine   | ~8MB  | apk             | musl  |
| base-debian   | ~30MB | apt             | glibc |
| base-ubuntu   | ~45MB | apt             | glibc |
| base-void     | ~20MB | xbps            | musl  |

## Modules

| Range | Purpose           | Examples                          |
|-------|-------------------|-----------------------------------|
| 000   | Base rootfs       | base-alpine, base-debian          |
| 01x   | System config     | dns, locale, timezone             |
| 10x   | Language runtimes | python312, nodejs22, golang       |
| 11x   | Build tools       | gcc, make, cmake, git             |
| 12x   | Libraries         | openssl, libffi, zlib             |
| 20x   | Services/daemons  | tailscale, nginx, postgres        |
| 9xx   | Checkpoints       | (auto-generated from upper layer) |

See [docs/modules.md](docs/modules.md) for presets, custom modules, and build workflows.

## Operations

```
create   -> mount base + modules as overlayfs -> chroot-ready sandbox
exec     -> unshare + chroot into merged -> run command -> log result
activate -> add module layer -> remount overlayfs (upper preserved)
snapshot -> mksquashfs upper/ -> numbered checkpoint
restore  -> mount checkpoint as layer -> clear upper -> remount
destroy  -> unmount everything -> rm -rf
```

## API

```
GET    /cgi-bin/health                      status
GET    /cgi-bin/api/sandboxes               list
POST   /cgi-bin/api/sandboxes               create
GET    /cgi-bin/api/sandboxes/:id           info
DELETE /cgi-bin/api/sandboxes/:id           destroy
POST   /cgi-bin/api/sandboxes/:id/exec      run command
POST   /cgi-bin/api/sandboxes/:id/activate  add module
POST   /cgi-bin/api/sandboxes/:id/snapshot  checkpoint
POST   /cgi-bin/api/sandboxes/:id/restore   restore
GET    /cgi-bin/api/sandboxes/:id/logs      history
GET    /cgi-bin/api/modules                 available modules
```

See [docs/api.md](docs/api.md) for request/response formats and field reference.

## Environment Variables

| Variable              | Default                       | Purpose                              |
|-----------------------|-------------------------------|--------------------------------------|
| `SQUASH_BACKEND`      | `chroot`                      | Backend: `chroot` or `firecracker`   |
| `SQUASH_DATA`         | `/data`                       | Root directory for all state         |
| `SQUASH_PORT`         | `8080`                        | HTTP API listen port                 |
| `SQUASH_AUTH_TOKEN`   | `""` (no auth)                | Bearer token for API authentication  |
| `SQUASH_API`          | `http://localhost:$SQUASH_PORT` | API base URL (used by `sq-ctl`)    |
| `SQUASH_S3_BUCKET`    | `""` (disabled)               | S3 bucket for module/snapshot sync   |
| `SQUASH_S3_ENDPOINT`  | `""` (AWS default)            | Custom endpoint for R2/MinIO/B2      |
| `SQUASH_S3_REGION`    | `us-east-1`                   | AWS region                           |
| `SQUASH_S3_PREFIX`    | `""`                          | Key prefix (e.g. `prod/`)           |
| `SQUASH_EPHEMERAL`    | `""` (disabled)               | S3-backed ephemeral mode (set `1`)   |
| `TAILSCALE_AUTHKEY`   | —                             | Tailscale auth key (enables VPN)     |
| `TAILSCALE_HOSTNAME`  | `squash`                      | Hostname on tailnet                  |

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
  sq-s3               S3 sync helper (push, pull, list, sync)
  sq-ctl              CLI for the API
  start-api           launch busybox httpd
  setup-tailscale     join tailnet (optional)
vm/
  init                guest PID 1 — mounts squashfs layers, starts vsock listener
  sq-vsock-handler    vsock command handler inside Firecracker guest
cgi-bin/
  common.sh           shared functions (mount, overlay, exec, checkpoint, backend dispatch)
  api/sandboxes       REST handler
  api/modules         module listing
  health              health check
deploy/
  hetzner/            docker-compose + cloud-init (chroot mode, ~12 EUR/mo)
  hetzner-baremetal/  docker-compose for bare metal with /dev/kvm (firecracker mode)
  fly/                fly.toml ($62/mo)
  aws/                ECS task definition ($170/mo)
static/
  index.html          API landing page
docs/
  plan-firecracker-and-security.md  design doc for Firecracker + security features
Dockerfile            Alpine host image (chroot mode)
Dockerfile.firecracker  Extended image with Firecracker + KVM deps
entrypoint.sh         init -> reaper -> secret proxy -> api
```

## Security model

The security features — network egress whitelisting, secret materialization via
placeholder proxy, and per-sandbox resource limits — are inspired by
[Deno Sandbox](https://deno.com/blog/introducing-deno-sandbox) and
[coder/httpjail](https://github.com/coder/httpjail). Squash makes this pattern
self-hostable: the same outbound-proxy-as-chokepoint approach, but running on
your own infrastructure with either namespace isolation (chroot mode) or
Firecracker microVMs.

See [docs/security.md](docs/security.md) for details.

## What this is NOT

- Not for untrusted multi-tenant. Firecracker mode provides VM isolation, but
  the system is designed for internal agents, CI, and dev environments.
- Not PorteuX. Uses the same squashfs+overlayfs pattern, any distro as base.
- No GPU passthrough or live migration between backends.

## Documentation

| Doc | Description |
|-----|-------------|
| [docs/api.md](docs/api.md) | Full API reference with request/response examples |
| [docs/security.md](docs/security.md) | Resource limits, network egress, secret proxy |
| [docs/modules.md](docs/modules.md) | Presets, custom modules, build workflows |
| [docs/s3-sync.md](docs/s3-sync.md) | S3 sync architecture, providers, CLI |
| [docs/deployment.md](docs/deployment.md) | Per-provider deployment guides |
