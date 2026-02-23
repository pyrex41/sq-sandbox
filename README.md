# Squash Sandbox (Experimental)

> **This project is experimental.** APIs, storage formats, and behavior may
> change without notice. Not recommended for production use.

Composable sandboxes from stacked squashfs layers. The PorteuX pattern,
distro-agnostic. Dual backend: chroot (default) or Firecracker microVM.

## Quick Start

```sh
# Run directly via Nix (no Docker, no root)
nix run github:pyrex41/sq-sandbox#squashd-shell

# Or install and start
nix profile install github:pyrex41/sq-sandbox#squashd-shell
SQUASH_DATA=~/.sq-sandbox sq-start

# Create a sandbox
curl -X POST localhost:8080/cgi-bin/api/sandboxes \
  -d '{"id":"dev","layers":"000-base-alpine,100-python312"}'

# Run a command
curl -X POST localhost:8080/cgi-bin/api/sandboxes/dev/exec \
  -d '{"cmd":"python3 -c \"print(42)\""}'
```

## What this is

Squash Sandbox is a **single daemon** that runs as a regular unprivileged user.
No Docker, no root, no `--privileged`. It provides:

- The HTTP API server (busybox httpd + CGI, or a compiled daemon)
- The sandbox lifecycle manager (create, exec, snapshot, restore, destroy)
- The sandbox host (squashfuse + fuse-overlayfs + bubblewrap for isolation)
- Optional background services (reaper, secret proxy, Tailscale)

**Unprivileged by default** — sandboxes are isolated using squashfuse (FUSE-based
squashfs mounts), fuse-overlayfs (userspace overlay filesystem), and bubblewrap
(namespace isolation with `pivot_root`). No kernel modules, no capabilities, no
setuid binaries required.

**HTTPS secret injection** — Set `SQUASH_PROXY_HTTPS=1` to enable transparent
HTTPS credential injection. The proxy generates per-host TLS certificates
signed by an auto-generated CA, allowing it to inspect and rewrite HTTPS
headers. Sandboxes automatically trust the CA. Real API keys never enter the
sandbox — only placeholders are visible. Works with curl, Python, Node.js, Go,
and any TLS client that trusts the system CA bundle.

There is no separate "sandbox agent" or remote execution node. The daemon
that serves the API is the same process that mounts squashfs layers and runs
sandboxed commands. This means:

- **Deploy one binary** — that's the whole system (via Nix, tarball, or systemd)
- **No root required** — uses squashfuse + fuse-overlayfs + bubblewrap
- **Firecracker mode** — optional VM-level isolation (requires `/dev/kvm`)

## How it works

A sandbox is a stack of read-only squashfs layers + a writable overlay:

```
┌─────────────────────────────┐
│ upper/  (writable, plain dir)│ <- all changes land here
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

Operations: create (squashfuse + fuse-overlayfs), exec (bubblewrap sandbox),
activate (add layer + remount), snapshot (mksquashfs upper/),
restore (mount snapshot as layer + clear upper), destroy (unmount + rm).

## Architecture

All five implementations share a common architecture: they shell out to **shared
helper scripts** in `shared/bin/` rather than making direct syscalls:

| Helper | Purpose |
|--------|---------|
| `sq-mount-layer` | Mount/unmount squashfs via squashfuse |
| `sq-mount-overlay` | Mount/unmount overlayfs via fuse-overlayfs |
| `sq-exec` | Execute commands via bubblewrap (or unshare+chroot fallback) |

This means:
- **Consistent behavior** across all implementations
- **Easy updates** — fix a mount issue once, all impls benefit
- **Simpler code** — no FFI, no syscall bindings, just subprocess calls
- **Testable in isolation** — each helper can be tested standalone

Network namespaces and cgroups are **stubbed out** in unprivileged mode.
Isolation is handled entirely by `sq-exec` (bubblewrap), which provides
PID/IPC/UTS namespace isolation without requiring root or capabilities.

## Backends

| Backend       | Isolation                            | Requires                     |
|---------------|--------------------------------------|------------------------------|
| `chroot`      | squashfuse + fuse-overlayfs + bwrap  | FUSE access (unprivileged)   |
| `firecracker` | microVM + vsock + tap networking     | `/dev/kvm` + root            |

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
| 090   | Libs              | sqlite-libs (musl sqlite3.so)     |
| 200   | Services          | nullclaw (Zig AI agent runtime)   |
Build presets: `sq-mkmod preset python3.12|nodejs22|golang|tailscale|sqlite-libs|nullclaw`

Build from directory: `sq-mkmod from-dir /path/to/tree 110-my-tools`

Build from running sandbox: `sq-mkmod from-sandbox my-sandbox 110-my-customizations`

## Nullclaw Integration

Autonomous AI agent infrastructure (678KB Zig binary, [nullclaw/nullclaw](https://github.com/nullclaw/nullclaw)).

**Nix**: `nix build .#module-nullclaw` (optional `enableNullclaw=false`)

**Preset**: `sq-mkmod preset nullclaw` (200-nullclaw.squashfs + sqlite-libs)

**Runtime**:
```
curl -X POST localhost:8080/cgi-bin/api/sandboxes \\
  -d '{\"id\":\"ai\",\"layers\":\"000-base-alpine,090-sqlite-libs,200-nullclaw\"}'

curl -X POST localhost:8080/cgi-bin/api/sandboxes/ai/exec \\
  -d '{\"cmd\":\"/usr/local/bin/nullclaw --version\"}'
```

**Full agent**:
```
curl .../exec -d '{\"cmd\":\"/usr/local/bin/nullclaw onboard --api-key \\\"$SQUASH_ANTHROPIC_KEY\\\" --interactive\"}'
curl .../exec -d '{\"cmd\":\"/usr/local/bin/nullclaw agent -m \\\"Solve this LeetCode problem\\\" --skill coding\"}'
```

Secrets proxy injects `$SQUASH_ANTHROPIC_KEY` etc. Firecracker compatible.

**sq-sandbox as nullclaw backend**: Propose PR to nullclaw for `RuntimeAdapter \"squash\"`:
```zig
// src/runtime/squash.zig
const SquashAdapter = struct {
  api_base: []const u8 = \"http://host:8080/cgi-bin/api\";
  vtable: RuntimeAdapter.VTable = .{
    .name = \"squash\",
    .has_shell_access = true,
    .storage_path = \"/workspace/.nullclaw\",
    // exec: POST /sandboxes/{id}/exec {\"cmd\":...}
    // create: POST /sandboxes {\"layers\":\"090-sqlite-libs,000-base-alpine,200-nullclaw\"}
  };
};
```
Config: `\"runtime\": {\"kind\": \"squash\"}`
Available bases: `base-alpine` (~8MB, musl), `base-debian` (~30MB, glibc),
`base-ubuntu` (~45MB, glibc), `base-void` (~20MB, musl).

## Security

**Resource limits** — `max_lifetime_s` (auto-destroyed by `sq-reaper`),
`SQUASH_MAX_SANDBOXES` (default 100 concurrent sandboxes).

**Namespace isolation** — Each sandbox exec runs via bubblewrap with its own
mount, PID, IPC, and UTS namespaces. The sandbox root is a `pivot_root`
target, not a simple chroot. Processes inside cannot see host PIDs or mounts.
Falls back to `unshare --mount --pid --ipc --uts --fork --map-root-user`
+ chroot when bubblewrap is unavailable.

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
# Stateless deployment — no persistent volume needed
SQUASH_EPHEMERAL=1 SQUASH_S3_BUCKET=my-squash sq-start
```

## Deployment

Three deployment methods, all unprivileged:

```sh
# 1. Nix (recommended) — reproducible, all deps included
nix profile install github:pyrex41/sq-sandbox#squashd-shell
SQUASH_DATA=~/.sq-sandbox sq-start

# 2. NixOS module — declarative system service
services.sq-sandbox = { enable = true; dataDir = "/var/lib/sq-sandbox"; };

# 3. Static tarball — copy binaries + scripts, bring your own deps
tar xf sq-sandbox-static.tar.gz -C /opt/sq-sandbox
PATH=/opt/sq-sandbox/bin:$PATH sq-start

# Firecracker backend (requires /dev/kvm + root for tap networking)
SQUASH_BACKEND=firecracker sq-start
```

Requires: FUSE access (`/dev/fuse`), squashfuse, fuse-overlayfs, bubblewrap.
All included automatically with Nix. Set env vars in `.env` or your shell.

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
| `SQUASH_UPPER_LIMIT_MB` | `512`                      | Max size of writable upper layer     |
| `SQUASH_MAX_SANDBOXES`  | `100`                      | Max concurrent sandboxes             |
| `SQUASH_PROXY_HTTPS`  | `""` (disabled)               | HTTPS MITM proxy (set `1`)           |
| `TAILSCALE_AUTHKEY`   | —                             | Tailscale auth key (enables VPN)     |

## Known limitations

- **Shared kernel** — a kernel exploit inside a sandbox could compromise the
  host. Firecracker mode mitigates this with a separate guest kernel.
- **FUSE required** — the unprivileged path needs `/dev/fuse` access. Most
  Linux distributions enable this by default. If running inside a container,
  the container needs `--device /dev/fuse`.
- **Secret proxy modes** — Default HTTP mode doesn't handle HTTPS. Set
  `SQUASH_PROXY_HTTPS=1` for full HTTPS support (generates a MITM CA).
- **Single-process architecture** — API server and sandbox host share a process
  tree. Protect with `SQUASH_AUTH_TOKEN` and network-level access control.
- **No network egress filtering** — sandboxes share the host network namespace.
  Use the secret proxy for credential isolation. For network-level control,
  deploy behind a firewall or use Firecracker mode.

Not for untrusted multi-tenant. No GPU passthrough. No live migration between backends.

## Language Implementations

The sandbox daemon is implemented in five languages on parallel branches. All
share the same core model (squashfuse + fuse-overlayfs + bubblewrap, secrets
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

### Binary Sizes (measured)

| Language | Binary | Stripped | Notes |
|----------|--------|---------|-------|
| Odin | 838 KB | — | ELF aarch64-linux, not yet stripped |
| Zig | 2.7 MB | 2.3 MB | Mach-O arm64 |
| Rust | 22 MB | 17 MB | Mach-O arm64, many deps (tokio, aws-sdk, rustls) |
| CL | ~20 MB | N/A | SBCL image (runtime + compiler + libs, zstd compressed) |
| Janet | 71 KB src | N/A | Interpreted; requires ~1MB Janet runtime |

### Trade-offs

| Criterion | Best fit |
|-----------|----------|
| Smallest codebase | Janet (~1.9k LOC, 71KB source) |
| Smallest binary | Odin (838KB), Zig (2.3MB stripped) |
| Most self-contained | Rust (native S3 + native HTTPS proxy) |
| Lowest dependency surface | Zig, Odin |
| Fastest iteration / REPL | Common Lisp (Swank + live image) |
| Strongest typing | Rust, Zig |
| Simplest deployment | Janet (single interpreter) |
| Production hardening | Rust (tracing, error types, async, RAII) |
