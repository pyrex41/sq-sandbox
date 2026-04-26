# Squash Sandbox (Experimental)

> **This project is experimental.** APIs, storage formats, and behavior may
> change without notice. Not recommended for production use.

Composable sandboxes from stacked squashfs layers. The PorteuX pattern,
distro-agnostic. Three backends: chroot (default), gVisor (runsc), or Firecracker microVM.

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
- The sandbox host (overlayfs mounts, cgroups, network namespaces, gVisor containers, or Firecracker VMs)
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
- **Sandbox escape = host compromise** in chroot mode — gVisor mode provides
  user-space kernel isolation, Firecracker mode provides a full VM boundary

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

Operations: create (mount layers as overlayfs), exec (unshare + chroot, or
runsc exec, or vsock), activate (add layer + remount), snapshot (mksquashfs
upper/), restore (mount snapshot as layer + clear upper), destroy (unmount + rm).

## Backends

| Backend       | Isolation                        | Requires             |
|---------------|----------------------------------|----------------------|
| `chroot`      | overlayfs + unshare              | Privileged container |
| `gvisor`      | overlayfs + runsc (user-space kernel) | Privileged container + `runsc` binary |
| `firecracker` | microVM + vsock                  | `/dev/kvm`           |

Set via `SQUASH_BACKEND` env var. The API is identical regardless of backend.

**gVisor** sits between chroot and Firecracker: it reuses the same overlayfs
mount infrastructure as chroot (so snapshot/restore/activate work identically)
but isolates processes through gVisor's Sentry — a user-space kernel that
intercepts all syscalls without requiring `/dev/kvm`. This provides stronger
isolation than plain namespaces without the overhead of a full microVM.

## API

All endpoints live under `/cgi-bin/`. If `SQUASH_AUTH_TOKEN` is set, include
`Authorization: Bearer <token>` on all requests.

```
GET    /cgi-bin/health                              {"status":"ok"}
GET    /cgi-bin/api/sandboxes                       list all
POST   /cgi-bin/api/sandboxes                       create
GET    /cgi-bin/api/sandboxes/:id                   info
DELETE /cgi-bin/api/sandboxes/:id                   destroy
POST   /cgi-bin/api/sandboxes/:id/exec              run command (sync)
POST   /cgi-bin/api/sandboxes/:id/exec-bg           run command (async, returns jobId)
GET    /cgi-bin/api/sandboxes/:id/jobs/:jobId       job status
GET    /cgi-bin/api/sandboxes/:id/jobs/:jobId/logs  job logs (SSE)
DELETE /cgi-bin/api/sandboxes/:id/jobs/:jobId       kill job
POST   /cgi-bin/api/sandboxes/:id/activate          add module layer
POST   /cgi-bin/api/sandboxes/:id/snapshot          checkpoint upper layer
POST   /cgi-bin/api/sandboxes/:id/restore           restore from checkpoint
POST   /cgi-bin/api/sandboxes/:id/fork              clone sandbox state
GET    /cgi-bin/api/sandboxes/:id/diff              upper-layer diff
GET    /cgi-bin/api/sandboxes/:id/logs              execution history
POST   /cgi-bin/api/sandboxes/:id/task              start autonomous task
GET    /cgi-bin/api/sandboxes/:id/task              task status
GET    /cgi-bin/api/sandboxes/:id/task/events       SSE event stream
POST   /cgi-bin/api/sandboxes/:id/task/pause        pause turn loop
POST   /cgi-bin/api/sandboxes/:id/task/resume       resume turn loop
POST   /cgi-bin/api/sandboxes/:id/task/kill         kill task
POST   /cgi-bin/api/sandboxes/:id/task/snapshot     on-demand task snapshot
GET    /cgi-bin/api/modules                         available modules
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

### Autonomous Task Runner

Sandboxes can run a long-lived agent task with a built-in turn loop —
useful for driving Claude Code, rho-cli, or any agent CLI through a
multi-iteration plan without round-trips through the API per turn.

```json
// POST /cgi-bin/api/sandboxes/dev/task
{
    "agent": "claude",                  // "claude" | "rho"
    "plan": "/workspace/PLAN.md",
    "workdir": "/workspace/repo",
    "git_remote": "https://oauth2:${TOKEN}@gitlab.example.com/org/repo.git",
    "branch": "agent/issue-123",
    "issue_key": "PROJ-184",
    "gitlab_project": "org/repo",
    "max_turns": 40,
    "turns_per_batch": 8,
    "max_budget_usd": 5.0,
    "env_vars": {"ANTHROPIC_API_KEY": "sk-..."},
    "snapshot_policy": {
        "every_n_batches": 1,           // 0 = disabled, default 5
        "max_snapshots": 10
    }
}
```

Lifecycle: the runner clones the repo (if `workdir` is empty), prepares a
`.gitignore` for runner artifacts (`RHO.md`, `.stop`, `IMPLEMENTATION_PLAN.md`),
loops through batches of turns with rate-limit detection and exponential
backoff retry, auto-commits each batch, snapshots per `snapshot_policy`,
then pushes the branch and creates a draft MR (title pulled from the
agent's first conventional-commit message; description includes Jira link,
commit list, diff stats, and a test plan).

Stream events via SSE:

```sh
curl -N localhost:8080/cgi-bin/api/sandboxes/dev/task/events
# task_start | workdir_detected | git_clone | git_pull | git_branch |
# batch_start | session_start | retry | batch_complete | workspace_state |
# exit_with_commits | turn_limit | budget_limit | auto_commit | commit |
# no_changes | snapshot | snapshot_skip | snapshot_error |
# pushing | creating_mr | mr_created | mr_error |
# paused | resumed | cancelled | task_complete
```

Each event includes a structured payload (batch index, cost, files changed,
diff stats, etc.) so an orchestrator can drive multiple sandboxes
concurrently and surface progress in a dashboard.

Supported agents:

| Agent     | Adapter behavior                                                    |
|-----------|---------------------------------------------------------------------|
| `claude`  | `claude -p <plan> --permission-mode dontAsk --output-format stream-json` |
| `rho`     | `rho-cli loop --mode build --output-format stream-json` with bash tool |

Adding an adapter is a single struct in `impl/go/runner/agents.go`.

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
and UTS namespaces (`unshare --mount --pid --ipc --uts` for chroot, or via
OCI spec for gVisor). Every sandbox also gets a dedicated network namespace
with veth pair, regardless of `allow_net`.

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

# gVisor backend (requires runsc installed in the image)
SQUASH_BACKEND=gvisor docker compose up -d

# Firecracker backend (requires /dev/kvm)
docker compose -f docker-compose.firecracker.yml up -d
```

Set env vars in `.env` or your shell. Works on any VPS, bare metal, ECS, Fly,
etc. — anything that supports privileged containers.

## Environment Variables

| Variable              | Default                       | Purpose                              |
|-----------------------|-------------------------------|--------------------------------------|
| `SQUASH_BACKEND`      | `chroot`                      | `chroot`, `gvisor`, or `firecracker` |
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
| `SQUASH_SNAPSHOT_BACKEND` | `squashfs`                | Snapshot backend: `squashfs` or `irmin` |
| `SQUASH_STORE_SOCK`   | `$SQUASH_DATA/.sq-store.sock` | sq-store sidecar socket path         |
| `SQUASH_STORE_DIR`    | `$SQUASH_DATA/.sq-store/`     | Irmin pack data directory            |

### Irmin Snapshot Backend

When `SQUASH_SNAPSHOT_BACKEND=irmin`, snapshots use a content-addressed store
(Irmin-pack) instead of squashfs. This provides:

- **Incremental snapshots** — only changed files are stored (O(changed) vs O(total))
- **Cross-sandbox deduplication** — identical files stored once globally
- **O(1) fork** — cloning a sandbox's snapshot history is instant
- **Queryable diff** — compare any two snapshots to see changed files
- **Blob-level S3 sync** — push only new content, not entire snapshots

The `sq-store` sidecar runs as a separate process (OCaml binary using
irmin-pack). Build it from `store/` with `make build`.

```sh
# Start the sidecar
sq-store --daemon

# Create sandbox with irmin snapshots
export SQUASH_SNAPSHOT_BACKEND=irmin
curl -X POST localhost:8080/cgi-bin/api/sandboxes/dev/snapshot \
  -d '{"label":"checkpoint-1"}'

# Fork a sandbox (O(1), irmin-only)
curl -X POST localhost:8080/cgi-bin/api/sandboxes/dev/fork \
  -d '{"target_id":"dev-fork"}'

# Diff two snapshots (irmin-only)
curl 'localhost:8080/cgi-bin/api/sandboxes/dev/diff?from=cp1&to=cp2'
```

Legacy squashfs snapshots remain accessible. Mixed-mode is supported:
snapshots created with either backend can coexist for the same sandbox.

## Known limitations

- **Chroot: shared kernel** — a kernel exploit inside a sandbox could compromise
  the host. gVisor mode mitigates this with a user-space kernel (Sentry) that
  intercepts syscalls. Firecracker mode provides the strongest boundary with a
  separate guest kernel in a microVM.
- **Chroot: UID mapping in privileged containers** — `--map-root-user` helps but
  effective isolation depends on the container runtime's user namespace config.
  Use gVisor or Firecracker mode for stronger isolation.
- **gVisor: compatibility** — gVisor's Sentry does not implement every Linux
  syscall. Some programs (especially those using `io_uring`, eBPF, or niche
  `ioctl`s) may fail. Test your workload before deploying.
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
| gVisor backend | — | — | — | ✓ | — |
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

## Advanced Networking & Storage

### Hybrid Local-First Storage

By default, sandbox upper layers use `tmpfs` (RAM-backed). New env vars enable
persistent, faster storage backends and local caching:

| Env var | Default | Description |
|---------|---------|-------------|
| `SQUASH_UPPER_BACKEND` | `tmpfs` | Upper layer backend: `tmpfs`, `btrfs`, or `loop` |
| `SQUASH_LOCAL_CACHE_DIR` | `~/.cache/sq-sandbox` | Local cache for modules and snapshots |
| `SQUASH_UPPER_LIMIT_MB` | `512` | Size limit for tmpfs/loop backends |

#### Upper backends

- **tmpfs** (default): RAM-backed, zero-config. Fastest for ephemeral workloads. Data lost on reboot.
- **btrfs**: Creates a btrfs subvolume per sandbox under `/data/upper/`. Enables near-instant
  snapshots via `btrfs subvolume snapshot -r` (< 10ms). Requires the host volume to be btrfs-formatted.
- **loop**: Sparse file + ext4 loopback mount. Works on any filesystem. Persists across restarts.

```sh
# Use btrfs for instant snapshots
docker run -d --privileged \
  -e SQUASH_UPPER_BACKEND=btrfs \
  -v /path/to/btrfs-volume:/data \
  ghcr.io/pyrex41/sq-sandbox:latest

# Fast local snapshot (btrfs only, no mksquashfs overhead)
sq-ctl snapshot dev fast-snap --local
```

#### Local cache

Modules and snapshots are cached at `SQUASH_LOCAL_CACHE_DIR` even when S3 is enabled.
This eliminates redundant S3 downloads on restart:

```sh
# Pure local mode (no S3)
docker run -d --privileged \
  -v squash-data:/data \
  -v ~/.cache/sq-sandbox:/cache \
  -e SQUASH_LOCAL_CACHE_DIR=/cache \
  ghcr.io/pyrex41/sq-sandbox:latest
```

When `SQUASH_S3_BUCKET` is empty (or unset), all S3 logic is automatically disabled.
Existing behavior with `SQUASH_EPHEMERAL=1` is fully preserved.

#### Sync sidecar (`sq-sync`)

The sandbox process never touches S3 directly. Instead, a lightweight sidecar
(`sq-sync`) listens on a Unix domain socket at `/data/.sq-bus.sock` for
notifications. The sandbox writes snapshots to local disk and drops a small
JSON message on the bus — the sidecar handles S3 upload, retries, and caching
in the background. The sandbox never blocks on network I/O for durability.

```
Sandbox → writes squashfs → drops {"op":"push","path":"...","key":"..."} on bus → done
                                          ↓
                        sq-sync sidecar → reads bus → S3 push (with retry)
                                        → periodic module sync (every 5 min)
                                        → file-based spool for offline resilience
```

| Env var | Default | Description |
|---------|---------|-------------|
| `SQUASH_BUS_SOCK` | `/data/.sq-bus.sock` | Unix socket path for sidecar IPC |
| `SQUASH_SYNC_INTERVAL` | `300` | Seconds between periodic module syncs |

The sidecar starts automatically via the entrypoint. Manual control:

```sh
sq-sync                     # start in foreground
sq-sync --daemon            # start in background (writes PID file)
sq-sync --status            # check if running
sq-sync --stop              # stop the sidecar
sq-sync --notify '{"op":"sync_modules"}'   # trigger manual sync
sq-ctl sync --background    # drain file-based spool
sq-ctl sync sandbox-id      # bi-directional sync (existing behavior)
```

If the sidecar is down, notifications are spooled to
`$SQUASH_LOCAL_CACHE_DIR/spool/` and drained when the sidecar restarts.

### Kernel-Level WireGuard

First-class WireGuard support for sandbox-to-sandbox networking. Uses the kernel
wireguard module (not userspace wireguard-go) for maximum throughput.

#### Setup

1. Activate the wireguard module:
   ```sh
   sq-ctl activate dev 030-wireguard
   ```

2. Add peers via API:
   ```sh
   curl -X POST localhost:8080/cgi-bin/api/sandboxes/dev/wg/peers \
     -d '[{"publicKey":"PEER_PUBKEY","endpoint":"1.2.3.4:51820","allowedIPs":"10.0.0.0/24"}]'
   ```

3. Or via CLI:
   ```sh
   sq-ctl wg genkey                            # generate keypair
   sq-ctl wg add-peer dev PUBKEY 1.2.3.4:51820 10.0.0.0/24
   ```

#### API: `POST /cgi-bin/api/sandboxes/:id/wg/peers`

Request body (JSON array of peers):
```json
[
  {
    "publicKey": "base64-encoded-public-key",
    "endpoint": "1.2.3.4:51820",
    "allowedIPs": "10.0.0.0/24",
    "presharedKey": "optional-base64-psk"
  }
]
```

Response:
```json
{
  "status": "ok",
  "publicKey": "sandbox-public-key",
  "listenPort": 51820,
  "peersAdded": 1
}
```

#### Backend support

| Backend | WireGuard support | Notes |
|---------|-------------------|-------|
| chroot | Kernel wg0 in sandbox netns | Full performance, coexists with Tailscale |
| Firecracker | Guest kernel wireguard.ko | Config passed via vsock/cloud-init |
| gVisor | Userspace only (document only) | Not implemented; use Tailscale instead |

#### Security

- Private keys **never leave the sandbox** — written to a temp file, passed to `wg set`, immediately deleted.
- Keys are **never logged**.
- If `SQUASH_SECRET_PROXY=1`, keys are stored encrypted at rest.
- Existing `allow_net` iptables rules apply in front of wg0.

#### Example: two-sandbox WireGuard tunnel

```sh
# Create two sandboxes
sq-ctl create sb-a alice 000-base-alpine,030-wireguard
sq-ctl create sb-b bob   000-base-alpine,030-wireguard

# Generate keypairs (privkey on stderr, pubkey on stdout)
KEY_A=$(sq-ctl wg genkey 2>/tmp/priv-a)
KEY_B=$(sq-ctl wg genkey 2>/tmp/priv-b)

# Set up WireGuard on each
sq-wg setup sb-a "$(cat /tmp/priv-a)" 51820
sq-wg setup sb-b "$(cat /tmp/priv-b)" 51821

# Assign IPs
ip netns exec squash-sb-a ip addr add 10.0.0.1/24 dev wg0
ip netns exec squash-sb-b ip addr add 10.0.0.2/24 dev wg0

# Add peers (cross-connect)
sq-wg add-peer sb-a "$KEY_B" "$(ip netns exec squash-sb-b wg show wg0 | grep endpoint)" 10.0.0.2/32
sq-wg add-peer sb-b "$KEY_A" "" 10.0.0.1/32

# Test throughput
sq-ctl exec sb-a "iperf3 -s -D"
sq-ctl exec sb-b "iperf3 -c 10.0.0.1 -t 5"
```

### How to Test

**Storage backends:**
```sh
# Tmpfs (default) — no special setup
docker run -d --privileged -p 8080:8080 ghcr.io/pyrex41/sq-sandbox:latest
sq-ctl create dev alice && sq-ctl snapshot dev test-snap

# Btrfs backend
mkfs.btrfs /dev/sdX && mount /dev/sdX /mnt/btrfs
docker run -d --privileged -e SQUASH_UPPER_BACKEND=btrfs -v /mnt/btrfs:/data ...
sq-ctl create dev alice && sq-ctl snapshot dev fast --local  # < 10ms

# Loop backend
docker run -d --privileged -e SQUASH_UPPER_BACKEND=loop ...

# Local cache (no S3)
docker run -d --privileged -e SQUASH_LOCAL_CACHE_DIR=/cache -v /tmp/cache:/cache ...

# S3 fallback test
docker run -d --privileged -e SQUASH_S3_BUCKET=my-bucket ...
sq-ctl snapshot dev snap1
sq-ctl sync --background  # push to S3
```

**WireGuard:**
```sh
# Requires: wireguard kernel module loaded on host
modprobe wireguard

docker run -d --privileged -p 8080:8080 ghcr.io/pyrex41/sq-sandbox:latest
sq-ctl create sb1 alice 000-base-alpine,030-wireguard
curl -X POST localhost:8080/cgi-bin/api/sandboxes/sb1/wg/peers \
  -d '[{"publicKey":"test","allowedIPs":"10.0.0.0/24"}]'
```
