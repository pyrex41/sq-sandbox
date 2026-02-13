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
| base-debian        | ~30MB | apt             | glibc |
| base-ubuntu        | ~45MB | apt             | glibc |
| base-void          | ~20MB | xbps            | musl  |

Default: `base-alpine`. Override per sandbox.

## Modules

Numbered layers stacked on the base:

| Range   | Purpose              | Examples                          |
|---------|----------------------|-----------------------------------|
| 000     | Base rootfs          | base-alpine, base-debian           |
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
```

### Create sandbox (POST)

```json
{
    "id": "dev",
    "owner": "alice",
    "layers": "000-base-alpine,100-python312",
    "task": "run tests",
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800,
    "allow_net": ["api.anthropic.com", "pypi.org"]
}
```

All fields except `id` and `layers` are optional. `layers` accepts a
comma-separated string or a JSON array.

### Sandbox info (GET /sandboxes/:id)

```json
{
    "id": "dev",
    "owner": "alice",
    "task": "run tests",
    "layers": ["000-base-alpine", "100-python312"],
    "created": "2025-01-01T00:00:00Z",
    "last_active": "2025-01-01T00:05:00Z",
    "mounted": true,
    "exec_count": 3,
    "upper_bytes": 4096,
    "snapshots": [],
    "active_snapshot": null,
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800,
    "allow_net": ["api.anthropic.com", "pypi.org"]
}
```

### Exec response (POST /sandboxes/:id/exec)

```json
{
    "exit_code": 0,
    "stdout": "hello\n",
    "stderr": "",
    "started": "2025-01-01T00:05:00Z",
    "ended": "2025-01-01T00:05:01Z",
    "duration_ms": 1000
}
```

Output is truncated to 64KB per stream.

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
| `TAILSCALE_AUTHKEY`   | —                             | Tailscale auth key (enables VPN)     |
| `TAILSCALE_HOSTNAME`  | `squash`                      | Hostname on tailnet                  |

## Module Presets

Build with `sq-mkmod preset <name>`:

| Preset        | Module name      | Source                              |
|---------------|------------------|-------------------------------------|
| `python3.12`  | `100-python312`  | python-build-standalone (GitHub)    |
| `nodejs22`    | `100-nodejs22`   | nodejs.org binary                   |
| `golang`      | `100-golang`     | go.dev binary                       |
| `tailscale`   | `200-tailscale`  | pkgs.tailscale.com binary           |

`rust` and `build-tools` presets exist as stubs — they print instructions to
install inside a running sandbox instead.

Custom modules: `sq-mkmod from-dir <path> <NNN-name>` or
`sq-mkmod from-sandbox <id> <NNN-name>` to package a sandbox's upper layer.

## S3 Sync

Optional S3 integration makes modules and snapshots durable and portable
across hosts. Local disk acts as a cache; S3 is the source of truth.
Works with any S3-compatible service (AWS S3, Cloudflare R2, MinIO, Backblaze B2).

**Disabled by default.** Set `SQUASH_S3_BUCKET` to enable.

```
Local (fast, ephemeral)          S3 (durable, universal)
┌──────────────────┐             ┌──────────────────┐
│ modules/ (cache)  │──push-bg──>│ modules/*.sqfs    │
│                   │<───pull────│                   │
│ snapshots/ (cache)│──push-bg──>│ sandboxes/*/snap/ │
│                   │<───pull────│                   │
└──────────────────┘             └──────────────────┘
```

### Auto sync behavior

- **After module build** (`sq-mkbase`, `sq-mkmod`): background push to S3
- **After snapshot**: background push to S3
- **On sandbox create**: if a requested module is missing locally, pull from S3
- **On restore**: if snapshot is missing locally, pull from S3
- **On startup** (`sq-init`): pull missing base/modules from S3 before building

### Manual CLI

```
sq-ctl push [modules|snapshots <id>]    push all to S3
sq-ctl pull [modules|snapshots <id>]    pull all from S3
sq-ctl sync [sandbox-id]                bi-directional sync
```

### Provider examples

**AWS S3:**
```sh
SQUASH_S3_BUCKET=my-squash-modules
SQUASH_S3_REGION=us-west-2
# Credentials via IAM role, env vars, or ~/.aws/credentials
```

**Cloudflare R2:**
```sh
SQUASH_S3_BUCKET=my-squash-modules
SQUASH_S3_ENDPOINT=https://ACCOUNT.r2.cloudflarestorage.com
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
```

**MinIO (self-hosted):**
```sh
SQUASH_S3_BUCKET=squash
SQUASH_S3_ENDPOINT=http://minio:9000
AWS_ACCESS_KEY_ID=minioadmin
AWS_SECRET_ACCESS_KEY=minioadmin
```

Transport: uses `aws` CLI if available, falls back to `curl` + AWS Signature V4
signing via `openssl`. No additional dependencies required beyond what's in the
base image.

## What this is NOT

- Not for untrusted multi-tenant. Firecracker mode provides VM isolation, but
  the system is designed for internal agents, CI, and dev environments.
- Not PorteuX. Uses the same squashfs+overlayfs pattern, any distro as base.
- No GPU passthrough or live migration between backends.
