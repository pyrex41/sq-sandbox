# Squash Sandbox (Experimental)

> **This project is experimental.** APIs, storage formats, and behavior may
> change without notice. Not recommended for production use.

Composable sandboxes from stacked squashfs layers. The PorteuX pattern,
distro-agnostic. Dual backend: chroot (default) or Firecracker microVM.

## Quick Start

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

## What this is

Squash Sandbox is a **single-container system** that is both the control plane
and the sandbox runtime. One Docker container runs:

- The HTTP API server (busybox httpd + CGI shell scripts)
- The sandbox lifecycle manager (create, exec, snapshot, restore, destroy)
- The sandbox host (overlayfs mounts, cgroups, network namespaces, or Firecracker VMs)
- Optional background services (reaper, secret proxy, Tailscale)

**HTTPS secret injection** — Set `SQUASH_PROXY_HTTPS=1` to enable transparent
HTTPS credential injection. The proxy generates per-host TLS certificates
signed by an auto-generated CA, allowing it to inspect and rewrite HTTPS
headers. Sandboxes automatically trust the CA. Real API keys never enter the
sandbox — only placeholders are visible. Works with curl, Python, Node.js, Go,
and any TLS client that trusts the system CA bundle.

There is no separate "sandbox agent" or remote execution node. The container
that serves the API is the same container that mounts squashfs layers and runs
sandboxed commands. This means:

- **Deploy one container** — that's the whole system
- **`--privileged` is required** — the container directly manipulates kernel
  resources (loop mounts, overlayfs, cgroups, network namespaces)
- **Sandbox escape = host compromise** in chroot mode — Firecracker mode
  provides a stronger VM boundary

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
│ 000-base-alpine.squashfs    │ <- root filesystem
└─────────────────────────────┘
         ↓ overlayfs merge ↓
┌─────────────────────────────┐
│ merged/  (chroot target)    │
└─────────────────────────────┘
```

Operations: create (mount layers as overlayfs), exec (unshare + chroot),
activate (add layer + remount), snapshot (mksquashfs upper/),
restore (mount snapshot as layer + clear upper), destroy (unmount + rm).

## Backends

| Backend       | Isolation              | Requires             |
|---------------|------------------------|----------------------|
| `chroot`      | overlayfs + unshare    | Privileged container |
| `firecracker` | microVM + vsock        | `/dev/kvm`           |

Set via `SQUASH_BACKEND` env var. The API is identical regardless of backend.

## API

All endpoints live under `/cgi-bin/`. If `SQUASH_AUTH_TOKEN` is set, include
`Authorization: Bearer <token>` on all requests.

```
GET    /cgi-bin/health                      {"status":"ok"}
GET    /cgi-bin/api/sandboxes               list all
POST   /cgi-bin/api/sandboxes               create
GET    /cgi-bin/api/sandboxes/:id           info
DELETE /cgi-bin/api/sandboxes/:id           destroy
POST   /cgi-bin/api/sandboxes/:id/exec      run command
POST   /cgi-bin/api/sandboxes/:id/activate  add module layer
POST   /cgi-bin/api/sandboxes/:id/snapshot  checkpoint upper layer
POST   /cgi-bin/api/sandboxes/:id/restore   restore from checkpoint
GET    /cgi-bin/api/sandboxes/:id/logs      execution history
GET    /cgi-bin/api/modules                 available modules
```

### Create

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

Only `id` and `layers` are required. Defaults: `cpu=2`, `memory_mb=1024`,
`max_lifetime_s=0` (unlimited), `allow_net=[]` (all egress allowed).

### Exec

```json
// POST /cgi-bin/api/sandboxes/dev/exec
{"cmd": "python3 -c \"print(42)\"", "workdir": "/", "timeout": 300}

// Response
{"exit_code": 0, "stdout": "42\n", "stderr": "", "started": "...", "finished": "..."}
```

### Activate, Snapshot, Restore

```json
// POST .../activate  — add a module to a running sandbox (upper preserved)
{"module": "100-nodejs22"}

// POST .../snapshot  — compress upper/ to squashfs checkpoint
{"label": "my-checkpoint"}

// POST .../restore   — mount snapshot as layer, clear upper, remount
{"label": "my-checkpoint"}
```

## Modules

| Range | Purpose           | Examples                          |
|-------|-------------------|-----------------------------------|
| 000   | Base rootfs       | base-alpine, base-debian          |
| 10x   | Language runtimes | python312, nodejs22, golang       |
| 11x   | Build tools       | gcc, make, cmake, git             |
| 20x   | Services/daemons  | tailscale, nginx, postgres        |
| 9xx   | Checkpoints       | (auto-generated from upper layer) |

Build presets: `sq-mkmod preset python3.12|nodejs22|golang|tailscale`

Build from directory: `sq-mkmod from-dir /path/to/tree 110-my-tools`

Build from running sandbox: `sq-mkmod from-sandbox my-sandbox 110-my-customizations`

Available bases: `base-alpine` (~8MB, musl), `base-debian` (~30MB, glibc),
`base-ubuntu` (~45MB, glibc), `base-void` (~20MB, musl).

## Security

**Resource limits** — `cpu` (cgroups v2 / VM config), `memory_mb` (OOM-killed
on exceed), `max_lifetime_s` (auto-destroyed by `sq-reaper`),
`SQUASH_UPPER_LIMIT_MB` (writable layer capped at 512MB tmpfs by default),
`SQUASH_MAX_SANDBOXES` (default 100 concurrent sandboxes).

**Namespace isolation** — Each sandbox exec runs in its own mount, PID, IPC,
and UTS namespaces (`unshare --mount --pid --ipc --uts`). Every sandbox also
gets a dedicated network namespace with veth pair, regardless of `allow_net`.

**Network egress** — `allow_net` field on create. `[]` = allow all (default),
`["api.anthropic.com"]` = whitelist, `["none"]` = block all. Every sandbox
gets its own network namespace (chroot) or tap device (firecracker). DNS
queries targeting the gateway IP are DNAT'd to the host's resolver. When
`allow_net` is specified, iptables egress rules are applied with ICMP blocking
and DNS rate limiting (10/s) to prevent tunneling.

**Secret proxy** — `sq-secret-proxy` injects real credentials into outbound
requests without exposing them inside the sandbox. Two modes:

- **HTTP mode** (default): Shell-based proxy replaces placeholders in plaintext
  HTTP headers. HTTPS connections bypass the proxy. Suitable when TLS is
  terminated externally (API gateway, reverse proxy, Firecracker backend).

- **HTTPS mode** (`SQUASH_PROXY_HTTPS=1`): Go-based MITM proxy generates
  per-host TLS certificates signed by a CA created at container startup. The CA
  is automatically injected into each sandbox's trust store (`ca-certificates.crt`,
  `NODE_EXTRA_CA_CERTS`, `SSL_CERT_FILE`, `REQUESTS_CA_BUNDLE`). All HTTPS API
  calls to `allowed_hosts` get transparent credential injection. Hosts not in
  any secret's `allowed_hosts` pass through without MITM (the proxy tunnels raw
  TCP). Network-level blocking is controlled separately by `allow_net`.

Configure `$SQUASH_DATA/secrets.json`:

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

Sandboxes see placeholder values. The proxy replaces them with real credentials
only when the destination host is in `allowed_hosts`.

## S3 Sync

Optional. Set `SQUASH_S3_BUCKET` to enable. Works with AWS S3, Cloudflare R2,
MinIO, Backblaze B2.

Modules and snapshots auto-sync: push after build/snapshot, pull on miss.
Manual: `sq-ctl push|pull|sync`.

**Ephemeral mode** (`SQUASH_EPHEMERAL=1`): S3 is sole durable storage, no
persistent volume needed. Sandboxes auto-snapshot to S3 on destroy and
auto-restore on create.

```sh
# Stateless container — no -v flag needed
docker run --rm --privileged \
  -e SQUASH_EPHEMERAL=1 \
  -e SQUASH_S3_BUCKET=my-squash \
  -p 8080:8080 \
  ghcr.io/pyrex41/sq-sandbox:latest
```

## Deployment

Runs as a privileged Docker container on any provider.

```sh
# Chroot backend (default)
docker compose up -d

# Firecracker backend (requires /dev/kvm)
docker compose -f docker-compose.firecracker.yml up -d
```

Set env vars in `.env` or your shell. Works on any VPS, bare metal, ECS, Fly,
etc. — anything that supports privileged containers.

## Environment Variables

| Variable              | Default                       | Purpose                              |
|-----------------------|-------------------------------|--------------------------------------|
| `SQUASH_BACKEND`      | `chroot`                      | `chroot` or `firecracker`            |
| `SQUASH_DATA`         | `/data`                       | Root directory for all state         |
| `SQUASH_PORT`         | `8080`                        | HTTP API listen port                 |
| `SQUASH_AUTH_TOKEN`   | `""` (no auth)                | Bearer token for API authentication  |
| `SQUASH_S3_BUCKET`    | `""` (disabled)               | S3 bucket for module/snapshot sync   |
| `SQUASH_S3_ENDPOINT`  | `""` (AWS default)            | Custom endpoint for R2/MinIO/B2      |
| `SQUASH_S3_REGION`    | `us-east-1`                   | AWS region                           |
| `SQUASH_S3_PREFIX`    | `""`                          | Key prefix (e.g. `prod/`)           |
| `SQUASH_EPHEMERAL`    | `""` (disabled)               | S3-backed ephemeral mode (set `1`)   |
| `SQUASH_UPPER_LIMIT_MB` | `512`                      | Max size of writable upper layer (tmpfs) |
| `SQUASH_MAX_SANDBOXES`  | `100`                      | Max concurrent sandboxes             |
| `SQUASH_PROXY_HTTPS`  | `""` (disabled)               | HTTPS MITM proxy (set `1`)           |
| `TAILSCALE_AUTHKEY`   | —                             | Tailscale auth key (enables VPN)     |

## Known limitations

- **Chroot: shared kernel** — a kernel exploit inside a sandbox could compromise
  the host. Firecracker mode mitigates this with a separate guest kernel.
- **Chroot: UID mapping in privileged containers** — `--map-root-user` helps but
  effective isolation depends on the container runtime's user namespace config.
  Use Firecracker mode for stronger isolation.
- **Secret proxy modes** — Default HTTP mode doesn't handle HTTPS. Set
  `SQUASH_PROXY_HTTPS=1` for full HTTPS support (generates a MITM CA).
- **Seccomp optional** — chroot relies on namespace isolation by default. Apply
  Docker's seccomp infrastructure at deployment time for additional syscall
  filtering: `docker run --privileged --security-opt seccomp=seccomp.json ...`
  A reference `seccomp.json` is included in the repo.
- **Single-container architecture** — API server and sandbox host share a process
  tree. Protect with `SQUASH_AUTH_TOKEN` and network-level access control.

Not for untrusted multi-tenant. No GPU passthrough. No live migration between backends.

## Language Implementations

The sandbox daemon is implemented in five languages on parallel branches. All
share the same core model (squashfs + overlayfs, cgroups, netns, secrets
injection) and expose the same HTTP API, but differ in runtime, dependencies,
and how much is implemented in-language vs. delegated to external tools.

| Implementation | Branch | LOC (src) | Runtime | HTTP Stack |
|----------------|--------|-----------|---------|------------|
| **Rust** | `feat/rust` | ~11k | Async (Tokio) | Axum + Tower |
| **Zig** | `feat/zig` | ~10k | Sync, thread pool | std.http (custom routing) |
| **Odin** | `feat/odin` | ~5k | Sync, thread pool | Custom (worker pool + bounded channels) |
| **Common Lisp** | `feat/cl` | ~3.5k | SBCL + bordeaux-threads | Clack/Woo (libev) + Ningle |
| **Janet** | `feat/janet` | ~1.9k | Fibers (ev/go) | net/server (stdlib) |

### Feature Matrix

| Feature | Rust | Zig | Odin | CL | Janet |
|---------|------|-----|------|----|-------|
| Chroot backend | ✓ | ✓ | ✓ | ✓ | ✓ |
| Firecracker backend | ✓ | ✓ | ✓ | ✓ | ✓ |
| Native S3 (in-language) | ✓ | ✓ | — | ✓ | — |
| S3 via shell (`sq-s3`) | — | — | ✓ | — | ✓ |
| Native HTTPS proxy | ✓ | — | — | — | — |
| HTTPS proxy (Go sidecar) | — | ✓ | ✓ | ✓ | ✓ |
| Auth middleware | ✓ | ✓ | ✓ | ✓ | ✓ |
| Cgroup v2 limits | ✓ | ✓ | ✓ | ✓ | ✓ |
| Network namespace | ✓ | ✓ | ✓ | ✓ | ✓ |
| Egress filtering | ✓ | ✓ | ✓ | ✓ | ✓ |
| Init/recovery | ✓ | ✓ | ✓ | ✓ | ✓ |
| Reaper | ✓ | ✓ | ✓ | ✓ | ✓ |
| Graceful shutdown | ✓ | ✓ | — | ✓ | — |
| Request body limits | ✓ | ✓ | — | ✓ | ✓ |
| SigV4 signing (in-lang) | — | ✓ | ✓ | ✓ | — |

### S3 Integration

| Impl | Approach | Notes |
|------|----------|-------|
| Rust | Native | aws-sdk-s3, async |
| Zig | Native | Custom SigV4 + std.http, atomic pulls, flock |
| CL | Native | Ironclad (SHA256/HMAC) + Dexador, SigV4 |
| Odin | Shell | Delegates to `sq-s3` script (sigv4.odin exists but unwired) |
| Janet | Shell | Delegates to `sq-s3` via `os/execute` / `os/spawn` |

### Trade-offs

| Criterion | Best fit |
|-----------|----------|
| Smallest codebase | Janet (~1.9k LOC) |
| Most self-contained | Rust (native S3 + native HTTPS proxy) |
| Lowest dependency surface | Zig, Odin |
| Fastest iteration / REPL | Common Lisp (Swank + live image) |
| Strongest typing | Rust, Zig |
| Simplest deployment | Janet (single interpreter) |
| Production hardening | Rust (tracing, error types, async, RAII) |
| Smallest binary | CL (~12-15MB compressed SBCL image) |
