# sq-sandbox — Janet Implementation

A Janet implementation of the sq-sandbox container runtime daemon.

## Requirements

- [Janet](https://janet-lang.org/) 1.23+ (for FFI)
- Linux (x86_64 or aarch64) — mount/umount syscalls
- Root or CAP_SYS_ADMIN for mount operations

## Install Janet

```bash
# macOS
brew install janet

# From source
git clone https://github.com/janet-lang/janet.git
cd janet && make && sudo make install
```

## Install Dependencies

```bash
jpm deps   # installs json from project.janet
```

## Run

```bash
# From project root (ensure bin/ is in PATH for sq-s3, sq-secret-proxy)
export PATH="$(pwd)/bin:$PATH"
JANET_PATH="$(pwd)/src:/opt/homebrew/lib/janet" janet main.janet

# With temp data dir (for testing without root)
SQUASH_DATA=/tmp/squash-janet PATH="$(pwd)/bin:$PATH" JANET_PATH="$(pwd)/src:/opt/homebrew/lib/janet" janet main.janet

# Test health
curl http://localhost:8080/cgi-bin/health
# -> {"status":"ok","sandboxes":0,"modules":0,"base_ready":false}
```

## Project Structure

```
sq-sandbox-janet/
├── main.janet           # Entry point
├── project.janet        # jpm project config
├── src/
│   ├── config.janet     # Config from env
│   ├── validate.janet   # Input validation
│   ├── syscalls.janet   # FFI: mount, umount2
│   ├── mounts.janet     # squashfs, tmpfs, overlay
│   ├── sandbox.janet    # Create, destroy
│   ├── manager.janet    # Sandbox registry
│   ├── api.janet        # HTTP handlers
│   ├── s3.janet         # S3 sync (shell out to bin/sq-s3)
│   ├── secrets.janet    # Secret injection into sandbox
│   └── proxy.janet      # Secret proxy (shell or Go binary)
├── bin/                 # Shared scripts (sq-mkbase, sq-s3, sq-secret-proxy, etc.)
└── static/              # Web UI
```

## Implemented

- [x] Config from environment
- [x] Input validation (valid-id, valid-label)
- [x] Mount/unmount (squashfs, tmpfs, overlay) via FFI
- [x] Sandbox create/destroy with netns, cgroups
- [x] HTTP API: health, sandboxes, exec, snapshot, restore, activate, logs
- [x] Manager with fiber-safe locking
- [x] **S3 sync** — shells out to `bin/sq-s3` for push, pull, sync-modules, sync-snapshots
- [x] **Secret proxy** — spawns `sq-secret-proxy` (shell) or `sq-secret-proxy-https` (Go) when `secrets.json` exists
- [x] **Secret injection** — placeholders + proxy env in `/etc/profile.d/squash-secrets.sh`
- [x] Reaper
- [x] Init/recovery

## S3 Sync

Set `SQUASH_S3_BUCKET` (and optionally `SQUASH_S3_ENDPOINT`, `SQUASH_S3_REGION`, `SQUASH_S3_PREFIX`). The daemon uses the shared `bin/sq-s3` script:

- **Startup**: `sync-modules` to pull missing modules
- **Snapshot**: `push-bg` to upload in background
- **Restore / activate**: `pull` when file not found locally

## Secret Proxy

When `$SQUASH_DATA/secrets.json` exists:

- **HTTP mode** (default): spawns `sq-secret-proxy` (shell script from `bin/`)
- **HTTPS mode** (`SQUASH_PROXY_HTTPS=1`): spawns `sq-secret-proxy-https` (Go binary from `proxy/`). Build with `cd proxy && go build -o sq-secret-proxy-https .` and ensure it's in `PATH` or set `SQ_PROXY_BIN`.

The daemon generates a CA at `$SQUASH_DATA/proxy-ca/` if missing (HTTPS mode). Sandboxes get placeholder env vars and `http_proxy`/`https_proxy` pointing to the gateway. With HTTPS mode, the CA cert is injected for TLS verification.

## Comparison to Other Implementations

| Feature        | Rust | Zig | Odin | CL  | Janet |
|----------------|------|-----|------|-----|-------|
| LOC            | ~11k | ~9k | ~3k  | ~3k | ~1.5k |
| FFI            | nix  | @cImport | c_* | CFFI | ffi/defbind |
| HTTP           | axum | std.http | custom | Woo | net/server |
| Proxy          | native | stub | Go | Go | Go/shell |
| S3             | native | shell | shell | shell | shell |
