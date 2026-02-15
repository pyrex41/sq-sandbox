# sq-sandbox Rust Rewrite — Complete Implementation Prompt

## Project Summary

Rewrite the `sq-sandbox` container runtime from shell scripts + a Go MITM proxy into **two Rust binaries**:

1. **`squashd`** — the main daemon. Replaces all of `cgi-bin/`, `bin/` (except module-building shell scripts), and `proxy/main.go`. This is the HTTP API server, sandbox lifecycle manager, secret proxy, reaper, S3 sync engine, and init/recovery system — all in one process.

2. **`sq-guest-agent`** — the Firecracker guest agent. Replaces `vm/init` and `vm/sq-vsock-handler`. Runs as PID 1 inside a microVM, mounts squashfs layers as overlayfs, and listens on vsock for commands.

The **module-building scripts** (`sq-mkbase`, `sq-mkmod`, `sq-mkvm`) remain as shell scripts. They download tarballs, run `chroot`/`apk add`, and call `mksquashfs`. There is no value in rewriting these in Rust — they run once during setup, not on the hot path. `squashd` shells out to `mksquashfs` for snapshots (there is no Rust squashfs creation library worth using). The CLI tool `sq-ctl` also remains as a shell script — it's just curl wrappers around the API.

The existing repo is at https://github.com/pyrex41/sq-sandbox. Read every file before starting. This prompt references specific line numbers and functions from that codebase.

---

## Architecture Principles

### 1. Drop-based resource lifecycle

Every kernel resource gets a Rust type that cleans up in `Drop`:

```
MountHandle    → umount2(path, MNT_DETACH) on drop
LoopDevice     → (if we manage loop explicitly) release on drop
CgroupHandle   → rmdir cgroup path on drop
NetnsHandle    → ip netns delete on drop
VethPair       → ip link delete host end on drop (peer auto-deletes)
IptablesChain  → flush + delete chain on drop
TmpfsMount     → umount on drop
```

A `Sandbox` struct owns all of these. When the sandbox is dropped (explicitly destroyed, or the process exits, or a panic unwinds), all kernel resources are cleaned up in reverse order. This is the primary correctness advantage over the shell implementation, which has `|| true` and `2>/dev/null` suppressing cleanup failures throughout `common.sh`.

### 2. Per-sandbox mutex, not global lock

```rust
struct SandboxManager {
    sandboxes: DashMap<String, Arc<Mutex<Sandbox>>>,
    config: Arc<Config>,
}
```

Operations on different sandboxes are fully concurrent. Operations on the same sandbox (exec, snapshot, activate, destroy) are serialized by the per-sandbox mutex. This fixes the race conditions in the shell implementation where simultaneous exec + activate could unmount the overlay mid-execution.

### 3. Sandbox state machine as enum

```rust
enum SandboxState {
    Creating,
    Ready {
        mounts: SandboxMounts,
        netns: Option<NetnsHandle>,
        cgroup: Option<CgroupHandle>,
    },
    Executing {
        pid: Pid,
        started: Instant,
    },
    Snapshotting,
    Destroying,
}
```

State transitions are enforced at compile time. You cannot call `exec` on a `Creating` sandbox because the function signature requires `&mut SandboxState::Ready`.

### 4. Structured errors, not string parsing

```rust
#[derive(thiserror::Error, Debug)]
enum SandboxError {
    #[error("mount failed: {fstype} on {target}")]
    Mount { fstype: String, target: PathBuf, source: nix::Error },
    #[error("overlay mount failed")]
    Overlay(nix::Error),
    #[error("namespace setup failed")]
    Netns(nix::Error),
    #[error("cgroup write failed: {param}")]
    Cgroup { param: String, source: io::Error },
    #[error("exec failed in sandbox {id}")]
    Exec { id: String, source: nix::Error },
    #[error("module not found: {0}")]
    ModuleNotFound(String),
    #[error("sandbox not found: {0}")]
    NotFound(String),
    #[error("sandbox already exists: {0}")]
    AlreadyExists(String),
    #[error("sandbox limit reached ({0})")]
    LimitReached(usize),
    #[error("invalid id: {0}")]
    InvalidId(String),
    #[error("invalid label: {0}")]
    InvalidLabel(String),
    #[error("wrong state: expected {expected}, got {actual}")]
    WrongState { expected: &'static str, actual: &'static str },
    #[error("timeout after {0}s")]
    Timeout(u64),
    #[error("s3 error: {0}")]
    S3(String),
}
```

Every error carries context. No more `echo "mount failed" >&2; return 3`.

---

## Crate Dependencies

```toml
[dependencies]
# Async runtime
tokio = { version = "1", features = ["full"] }

# HTTP server
axum = { version = "0.8", features = ["ws"] }
tower = "0.5"
tower-http = { version = "0.6", features = ["cors", "trace"] }

# Linux syscalls
nix = { version = "0.29", features = [
    "mount", "sched", "signal", "process", "fs",
    "ioctl", "net", "user", "hostname"
]}
# Netlink (for veth, addr, route, netns)
rtnetlink = "0.14"
netlink-packet-route = "0.21"

# TLS / MITM proxy
rustls = "0.23"
tokio-rustls = "0.26"
rcgen = "0.13"          # dynamic cert generation
x509-parser = "0.16"    # CA cert loading

# S3
aws-sdk-s3 = "1"
aws-config = "1"

# Serialization
serde = { version = "1", features = ["derive"] }
serde_json = "1"

# Misc
dashmap = "6"
thiserror = "2"
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["json"] }
clap = { version = "4", features = ["derive"] }
chrono = { version = "0.4", features = ["serde"] }
uuid = { version = "1", features = ["v4"] }

# For the guest agent only (separate binary, minimal deps)
[target.'cfg(feature = "guest")'.dependencies]
tokio-vsock = "0.5"
```

Use `nix` for all direct syscalls (mount, unshare, chroot, setns, clone, kill, waitpid). Use `rtnetlink` for netlink operations (veth creation, IP address assignment, route addition, netns operations). Use `std::process::Command` to shell out to `iptables` and `mksquashfs` — writing a netfilter library is not worth it for the ~10 rules we manage, and there's no production-quality Rust squashfs creator.

Build with:
```
RUSTFLAGS='-C target-feature=+crt-static' cargo build --release --target x86_64-unknown-linux-musl
```

---

## Module Structure

```
src/
├── main.rs                 # squashd entrypoint, arg parsing, startup
├── config.rs               # Config struct from env vars
├── api/
│   ├── mod.rs              # axum router setup
│   ├── sandboxes.rs        # CRUD + exec/activate/snapshot/restore/logs
│   ├── modules.rs          # list modules
│   └── health.rs           # health endpoint
├── sandbox/
│   ├── mod.rs              # Sandbox struct, SandboxState enum
│   ├── manager.rs          # SandboxManager (DashMap of sandboxes)
│   ├── mounts.rs           # MountHandle, OverlayMount, SquashfsMount, TmpfsMount
│   ├── exec.rs             # fork/unshare/chroot/exec logic
│   ├── netns.rs            # NetnsHandle, VethPair, egress rules
│   ├── cgroup.rs           # CgroupHandle (cgroups v2)
│   ├── snapshot.rs         # snapshot (mksquashfs) and restore
│   └── meta.rs             # metadata read/write (filesystem-backed)
├── modules/
│   ├── mod.rs              # Module registry, mod_exists, list_modules
│   └── builder.rs          # shell-out to sq-mkbase/sq-mkmod (thin wrapper)
├── proxy/
│   ├── mod.rs              # Secret proxy (HTTPS MITM + HTTP rewrite)
│   ├── secrets.rs          # secrets.json loading
│   └── certs.rs            # CA loading, dynamic cert generation, cache
├── s3/
│   └── mod.rs              # S3 sync: push, pull, list, push_bg, sync, ephemeral
├── reaper.rs               # Background task: destroy expired sandboxes
├── init.rs                 # First-boot: build base if missing, remount surviving sandboxes
└── validate.rs             # Input validation (sandbox IDs, labels, module names)

guest/
└── main.rs                 # sq-guest-agent: PID 1, mount layers, vsock listener
```

---

## Detailed Implementation Specifications

### Config (`config.rs`)

Load from environment variables, matching the existing defaults exactly:

```rust
pub struct Config {
    pub backend: Backend,              // SQUASH_BACKEND, default "chroot"
    pub data_dir: PathBuf,             // SQUASH_DATA, default "/data"
    pub port: u16,                     // SQUASH_PORT, default 8080
    pub auth_token: Option<String>,    // SQUASH_AUTH_TOKEN, default None
    pub s3_bucket: Option<String>,     // SQUASH_S3_BUCKET
    pub s3_endpoint: Option<String>,   // SQUASH_S3_ENDPOINT
    pub s3_region: String,             // SQUASH_S3_REGION, default "us-east-1"
    pub s3_prefix: String,             // SQUASH_S3_PREFIX, default ""
    pub ephemeral: bool,               // SQUASH_EPHEMERAL == "1"
    pub upper_limit_mb: u64,           // SQUASH_UPPER_LIMIT_MB, default 512
    pub max_sandboxes: usize,          // SQUASH_MAX_SANDBOXES, default 100
    pub proxy_https: bool,             // SQUASH_PROXY_HTTPS == "1"
    pub tailscale_authkey: Option<String>, // TAILSCALE_AUTHKEY
    pub tailscale_hostname: String,    // TAILSCALE_HOSTNAME, default "squash"
}

pub enum Backend { Chroot, Firecracker }
```

Derived paths (same layout as existing):
- `config.modules_dir()` → `{data_dir}/modules`
- `config.sandboxes_dir()` → `{data_dir}/sandboxes`
- `config.secrets_path()` → `{data_dir}/secrets.json`
- `config.proxy_ca_dir()` → `{data_dir}/proxy-ca`
- `config.vm_dir()` → `{data_dir}/vm`

### Input Validation (`validate.rs`)

Must match existing shell validation exactly:

```rust
/// Sandbox IDs: [a-zA-Z0-9_-]+
pub fn valid_id(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

/// Snapshot labels and module names: [a-zA-Z0-9_.-]+
pub fn valid_label(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-')
}

// valid_module is the same as valid_label
pub fn valid_module(s: &str) -> bool { valid_label(s) }
```

Reject any input that fails validation with 400 Bad Request, same as the shell implementation.

### Sandbox Mounts (`sandbox/mounts.rs`)

This is the most critical file for correctness. Every mount type implements `Drop`.

**SquashfsMount** — loop-mounts a .squashfs file read-only:
```rust
pub struct SquashfsMount {
    mount_point: PathBuf,
    active: bool,  // set to false when ownership transfers to OverlayMount
}

impl SquashfsMount {
    pub fn mount(squashfs_path: &Path, mount_point: &Path) -> Result<Self> {
        std::fs::create_dir_all(mount_point)?;
        nix::mount::mount(
            Some(squashfs_path),
            mount_point,
            Some("squashfs"),
            MsFlags::MS_RDONLY,
            None::<&str>,
        )?;
        Ok(Self { mount_point: mount_point.to_owned(), active: true })
    }
}

impl Drop for SquashfsMount {
    fn drop(&mut self) {
        if self.active {
            let _ = nix::mount::umount2(&self.mount_point, MntFlags::MNT_DETACH);
        }
    }
}
```

**TmpfsMount** — size-limited writable layer:
```rust
pub struct TmpfsMount {
    mount_point: PathBuf,
}

impl TmpfsMount {
    pub fn mount(path: &Path, size_mb: u64) -> Result<Self> {
        std::fs::create_dir_all(path)?;
        let opts = format!("size={}m", size_mb);
        nix::mount::mount(
            Some("tmpfs"), path, Some("tmpfs"),
            MsFlags::empty(), Some(opts.as_str()),
        )?;
        // Create the data/ and work/ subdirectories overlayfs needs
        std::fs::create_dir_all(path.join("data"))?;
        std::fs::create_dir_all(path.join("work"))?;
        Ok(Self { mount_point: path.to_owned() })
    }
}

impl Drop for TmpfsMount {
    fn drop(&mut self) {
        let _ = nix::mount::umount2(&self.mount_point, MntFlags::MNT_DETACH);
    }
}
```

**OverlayMount** — the final merged view:
```rust
pub struct OverlayMount {
    merged_path: PathBuf,
}

impl OverlayMount {
    /// lowerdir_components: ordered highest-priority first.
    /// The existing code puts _snapshot first, then modules in descending numeric order.
    pub fn mount(
        lower_components: &[&Path],
        upper_data: &Path,
        work: &Path,
        merged: &Path,
    ) -> Result<Self> {
        std::fs::create_dir_all(merged)?;
        let lowerdir = lower_components.iter()
            .map(|p| p.to_str().unwrap())
            .collect::<Vec<_>>()
            .join(":");
        let opts = format!(
            "lowerdir={},upperdir={},workdir={}",
            lowerdir,
            upper_data.display(),
            work.display(),
        );
        nix::mount::mount(
            Some("overlay"), merged, Some("overlay"),
            MsFlags::empty(), Some(opts.as_str()),
        )?;
        Ok(Self { merged_path: merged.to_owned() })
    }

    /// Remount with updated lower layers (used by activate_module and restore)
    pub fn remount(&mut self, lower_components: &[&Path], upper_data: &Path, work: &Path) -> Result<()> {
        // Unmount existing
        nix::mount::umount2(&self.merged_path, MntFlags::MNT_DETACH)?;
        // Remount with new lowerdir
        let lowerdir = lower_components.iter()
            .map(|p| p.to_str().unwrap())
            .collect::<Vec<_>>()
            .join(":");
        let opts = format!(
            "lowerdir={},upperdir={},workdir={}",
            lowerdir, upper_data.display(), work.display()
        );
        nix::mount::mount(
            Some("overlay"), &self.merged_path, Some("overlay"),
            MsFlags::empty(), Some(opts.as_str()),
        )?;
        Ok(())
    }
}

impl Drop for OverlayMount {
    fn drop(&mut self) {
        let _ = nix::mount::umount2(&self.merged_path, MntFlags::MNT_DETACH);
    }
}
```

**SandboxMounts** — owns everything:
```rust
pub struct SandboxMounts {
    pub squashfs_mounts: Vec<SquashfsMount>,  // module layers
    pub snapshot_mount: Option<SquashfsMount>, // active snapshot layer
    pub tmpfs: TmpfsMount,                     // writable upper
    pub overlay: OverlayMount,                 // merged view
}
```

When `SandboxMounts` is dropped, Rust drops fields in declaration order. But we need reverse order (overlay first, then tmpfs, then squashfs layers). Implement `Drop` for `SandboxMounts` explicitly:

```rust
impl Drop for SandboxMounts {
    fn drop(&mut self) {
        // Manually drop in reverse mount order
        // 1. Overlay (merged)
        let _ = nix::mount::umount2(&self.overlay.merged_path, MntFlags::MNT_DETACH);
        self.overlay.active = false; // prevent double-unmount in OverlayMount::drop

        // 2. Snapshot layer
        if let Some(ref mut snap) = self.snapshot_mount {
            let _ = nix::mount::umount2(&snap.mount_point, MntFlags::MNT_DETACH);
            snap.active = false;
        }

        // 3. Tmpfs
        let _ = nix::mount::umount2(&self.tmpfs.mount_point, MntFlags::MNT_DETACH);
        // mark inactive...

        // 4. Squashfs layers in reverse
        for m in self.squashfs_mounts.iter_mut().rev() {
            let _ = nix::mount::umount2(&m.mount_point, MntFlags::MNT_DETACH);
            m.active = false;
        }
    }
}
```

### Network Namespace (`sandbox/netns.rs`)

Use `rtnetlink` for netlink operations. Use `std::process::Command` for iptables (not worth a netfilter library for ~10 rules).

```rust
pub struct NetnsHandle {
    name: String,           // "squash-{id}"
    index: u8,              // 1-254, for 10.200.{index}.0/30 subnet
    veth_host: String,      // "sq-{id}-h"
    iptables_chain: Option<String>,  // "squash-{id}" if egress rules applied
}
```

**Index allocation** — replicate the flock-based sequential scan from `_allocate_netns_index()` (common.sh:309-325). Use `flock` via `nix::fcntl::flock()` on `{data_dir}/.netns-index.lock`. Scan `{sandboxes_dir}/*/.meta/netns_index` files to find the first unused index 1-254.

**Setup sequence** (must match common.sh:327-383 exactly):
1. `ip netns add squash-{id}`
2. `ip link add sq-{id}-h type veth peer name sq-{id}-s`
3. `ip link set sq-{id}-s netns squash-{id}`
4. `ip addr add 10.200.{index}.1/30 dev sq-{id}-h`
5. `ip link set sq-{id}-h up`
6. Inside netns: `ip addr add 10.200.{index}.2/30 dev sq-{id}-s`, `ip link set sq-{id}-s up`, `ip link set lo up`, `ip route add default via 10.200.{index}.1`
7. `echo 1 > /proc/sys/net/ipv4/ip_forward`
8. `iptables -t nat -A POSTROUTING -s 10.200.{index}.0/30 -j MASQUERADE`
9. DNS DNAT: redirect UDP/TCP port 53 from sandbox→gateway to host resolver (parse first nameserver from /etc/resolv.conf)
10. If `allow_net` is non-empty, apply egress filtering chain

**Egress filtering** (must match common.sh:385-422):
1. Create iptables chain `squash-{id}`
2. Jump from FORWARD on interface `sq-{id}-h` to chain
3. Drop ICMP (prevent tunneling)
4. Rate-limit DNS to 10/s burst 20
5. Accept ESTABLISHED,RELATED
6. For each host in `allow_net`: resolve via `getent hosts` (or `libc::getaddrinfo`), accept dest IP
7. Wildcards: log warning (same as existing behavior)
8. Default: DROP

**Teardown** (in `Drop`, must match common.sh:424-454):
1. Remove FORWARD jump rule
2. Flush and delete iptables chain
3. Remove NAT POSTROUTING rule
4. Remove DNS DNAT rules
5. Delete host veth (`ip link delete sq-{id}-h` — peer auto-deletes)
6. Delete netns (`ip netns delete squash-{id}`)

Every iptables command: use `Command::new("iptables")`. Ignore errors during teardown (the shell version does `2>/dev/null || true`). Log them at `warn` level.

### Cgroups v2 (`sandbox/cgroup.rs`)

```rust
pub struct CgroupHandle {
    path: PathBuf,  // /sys/fs/cgroup/squash-{id}
}

impl CgroupHandle {
    pub fn create(id: &str, cpu: f64, memory_mb: u64) -> Result<Self> {
        let path = PathBuf::from(format!("/sys/fs/cgroup/squash-{}", id));
        std::fs::create_dir_all(&path)?;

        // CPU: quota = cpu * 100000, period = 100000
        let quota = (cpu * 100_000.0) as u64;
        std::fs::write(path.join("cpu.max"), format!("{} 100000", quota))?;

        // Memory
        let mem_bytes = memory_mb * 1024 * 1024;
        std::fs::write(path.join("memory.max"), mem_bytes.to_string())?;

        Ok(Self { path })
    }

    /// Add a process to this cgroup
    pub fn add_process(&self, pid: u32) -> Result<()> {
        std::fs::write(self.path.join("cgroup.procs"), pid.to_string())?;
        Ok(())
    }
}

impl Drop for CgroupHandle {
    fn drop(&mut self) {
        // Move all processes out first (otherwise rmdir fails)
        let _ = std::fs::write(
            PathBuf::from("/sys/fs/cgroup/cgroup.procs"),
            std::fs::read_to_string(self.path.join("cgroup.procs")).unwrap_or_default()
        );
        let _ = std::fs::remove_dir(&self.path);
    }
}
```

### Sandbox Execution (`sandbox/exec.rs`)

This replaces common.sh:546-607 (`_chroot_exec_in_sandbox`).

**The current approach**: `timeout $s ip netns exec $netns unshare --mount --pid --ipc --uts --fork --map-root-user chroot $merged /bin/sh -c 'cd "$1"; eval "$2"' _ "$wd" "$cmd"`

**The Rust approach**: fork, enter namespaces, chroot, exec. No shell `eval`. Use `nix::unistd::fork()`, `nix::sched::setns()`, `nix::sched::unshare()`, `nix::unistd::chroot()`, `nix::unistd::chdir()`, `nix::unistd::execve()`.

```rust
pub struct ExecRequest {
    pub cmd: String,
    pub workdir: String,  // default "/"
    pub timeout: u64,     // default 300 seconds
}

pub struct ExecResult {
    pub exit_code: i32,
    pub stdout: String,      // capped at 64KB (matching existing head -c 65536)
    pub stderr: String,      // capped at 64KB
    pub started: DateTime<Utc>,
    pub finished: DateTime<Utc>,
    pub seq: u32,            // execution sequence number
}
```

**Execution flow:**

1. Verify sandbox is in `Ready` state
2. Update `last_active` timestamp
3. Allocate log sequence number
4. Create stdout/stderr pipes
5. Fork:
   - **Child**: Enter network namespace (if present) via `setns(fd, CLONE_NEWNET)`. Then `unshare(CLONE_NEWNS | CLONE_NEWPID | CLONE_NEWIPC | CLONE_NEWUTS)`. Then `chroot(merged_path)`. Then `chdir(workdir)`. Then `exec /bin/sh -c "{cmd}"`.
   - **Parent**: Read from stdout/stderr pipes with timeout. Use `tokio::time::timeout` wrapping `tokio::process::Child::wait_with_output()` — or, since the child uses raw fork, use `nix::sys::wait::waitpid` with `WNOHANG` in a poll loop.
6. If timeout expires, kill child with SIGKILL
7. Collect exit code (124 if timed out, matching `timeout` command behavior)
8. Truncate stdout/stderr to 65536 bytes
9. Write log entry as JSON to `{sandbox_dir}/.meta/log/{seq:04}.json`
10. Return ExecResult

**Important**: The child process must enter the cgroup before exec. In the fork child, before `unshare`, write own PID to the cgroup's `cgroup.procs`.

**Important**: The cmd is passed to `/bin/sh -c` just like the existing implementation. Do NOT try to parse or split the command yourself. The sandbox's `/bin/sh` handles pipes, redirects, semicolons, etc.

### Secret Proxy (`proxy/`)

Port the Go MITM proxy (`proxy/main.go`, 526 lines) to Rust. The behavior must be identical.

**Three modes of operation:**

1. **Plain HTTP** — Rewrite auth headers (replace placeholder → real value), strip hop-by-hop headers, forward via hyper client.

2. **HTTPS CONNECT to allowed host** — Accept CONNECT, send `200 Connection Established`, perform TLS handshake with dynamically-generated cert signed by the CA, read plaintext HTTP requests, rewrite auth headers, forward to real upstream over TLS.

3. **HTTPS CONNECT to non-allowed host** — Accept CONNECT, send `200 Connection Established`, blind bidirectional TCP tunnel (no inspection).

**Replaceable headers** (allowlist, must match Go exactly):
```
Authorization, X-Api-Key, Api-Key, X-Auth-Token, X-Access-Token, Proxy-Authorization
```

**Constants** (must match Go):
```
maxCertCacheSize   = 1000
maxResponseBody    = 512 MB
maxRequestBody     = 64 MB
maxConcurrentConns = 512
connIdleTimeout    = 2 minutes
connReadTimeout    = 30 seconds
tunnelTimeout      = 10 minutes
```

**Certificate generation**: Use `rcgen` to create leaf certs signed by the CA. Cache in a `DashMap<String, Arc<rustls::sign::CertifiedKey>>` with the same eviction policy as Go (clear all when full — the cache is bounded by `allowed_hosts` count, which is typically < 10).

**CA generation**: At startup, if `{data_dir}/proxy-ca/ca.crt` doesn't exist, generate a self-signed EC (P-256) CA cert with `rcgen`, write `ca.crt` and `ca.key` in PEM format. The existing shell code in `entrypoint.sh:12-18` uses `openssl req` to generate the CA — the Rust version should produce compatible output (PEM-encoded, EC P-256, CN=sq-secret-proxy CA, 10-year validity).

**Secret injection into sandboxes** (must match common.sh:253-305):
After overlay mount, before the sandbox is marked Ready:
1. Create `{upper}/data/etc/profile.d/squash-secrets.sh` containing:
   - `export {SECRET_NAME}={placeholder}` for each secret in secrets.json
   - `export http_proxy=http://{gateway_ip}:8888` (where gateway_ip is `10.200.{index}.1`)
   - `export https_proxy=http://{gateway_ip}:8888`
   - Same for `HTTP_PROXY`, `HTTPS_PROXY`
2. If HTTPS proxy mode and CA cert exists:
   - Copy CA cert to `{upper}/data/usr/local/share/ca-certificates/sq-proxy-ca.crt`
   - Append CA cert to `{upper}/data/etc/ssl/certs/ca-certificates.crt` (copy from merged first)
   - Add `NODE_EXTRA_CA_CERTS`, `REQUESTS_CA_BUNDLE`, `SSL_CERT_FILE` env vars

**Proxy listener**: Bind to `0.0.0.0:8888`. Run as a tokio task within `squashd`, not a separate process.

### S3 Sync (`s3/`)

Use `aws-sdk-s3`. Support custom endpoints (for R2, MinIO, B2) via `aws_sdk_s3::config::Builder::endpoint_url()`.

**Operations** (matching `bin/sq-s3` exactly):
- `push(local_path, s3_key)` — upload file
- `pull(s3_key, local_path)` — download file. Atomic: write to `{path}.s3tmp` then rename. Use flock to prevent concurrent pulls of the same file (matching sq-s3:264-281).
- `exists(s3_key)` — HEAD object
- `list(prefix)` — ListObjectsV2
- `push_bg(local_path, s3_key)` — spawn a tokio task that copies the file to a temp location and uploads asynchronously (matching the shell `(sq-s3 push ... &)` pattern)

**Module sync** (`sync_modules`, matching sq-s3:312-334):
1. Push local-only modules (local exists, not in S3)
2. Pull remote-only modules (in S3, not local)

**Snapshot sync** (`sync_snapshots`, matching sq-s3:336-363): same bidirectional pattern for `sandboxes/{id}/snapshots/`.

**Ephemeral mode** (matching common.sh:83-89):
- On destroy: auto-snapshot to S3, push manifest
- On create: check S3 for latest snapshot, auto-restore if found
- Manifest is a JSON file at `sandboxes/{id}/manifest.json` containing sandbox config (matching common.sh:35-66)

### API Server (`api/`)

Use axum. The API must be wire-compatible with the existing CGI endpoints.

**Router:**
```rust
Router::new()
    .route("/cgi-bin/health", get(health))
    .route("/cgi-bin/api/sandboxes", get(list_sandboxes).post(create_sandbox))
    .route("/cgi-bin/api/sandboxes/{id}", get(get_sandbox).delete(destroy_sandbox))
    .route("/cgi-bin/api/sandboxes/{id}/exec", post(exec_in_sandbox))
    .route("/cgi-bin/api/sandboxes/{id}/activate", post(activate_module))
    .route("/cgi-bin/api/sandboxes/{id}/snapshot", post(snapshot_sandbox))
    .route("/cgi-bin/api/sandboxes/{id}/restore", post(restore_sandbox))
    .route("/cgi-bin/api/sandboxes/{id}/logs", get(get_logs))
    .route("/cgi-bin/api/modules", get(list_modules))
    // Serve static files from /app/static/ at /
    .fallback_service(ServeDir::new("/app/static"))
```

**Authentication middleware**: If `SQUASH_AUTH_TOKEN` is set, require `Authorization: Bearer {token}` on all `/cgi-bin/api/` routes. Return 401 if missing/wrong.

**Request/response formats** — must exactly match existing JSON shapes:

Create request (POST /cgi-bin/api/sandboxes):
```json
{
    "id": "dev",
    "owner": "alice",
    "layers": "000-base-alpine,100-python312",  // string or array
    "task": "run tests",
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800,
    "allow_net": ["api.anthropic.com", "pypi.org"]
}
```

Only `id` is required (the shell checks `layers` but defaults to `000-base-alpine`). Defaults: owner="anon", layers="000-base-alpine", task="", cpu=2, memory_mb=1024, max_lifetime_s=0, allow_net=[] (allow all).

Note: `layers` can be either a comma-separated string OR a JSON array. The shell code handles both (common.sh:22 / cgi-bin/api/sandboxes:22). The Rust deserializer should accept both.

Sandbox info response (GET, POST create response):
```json
{
    "id": "dev",
    "owner": "alice",
    "task": "run tests",
    "layers": ["000-base-alpine", "100-python312"],
    "created": "2025-01-15T10:30:00+00:00",
    "last_active": "2025-01-15T10:35:00+00:00",
    "mounted": true,
    "exec_count": 5,
    "upper_bytes": 1048576,
    "snapshots": [],
    "active_snapshot": null,
    "cpu": 2,
    "memory_mb": 1024,
    "max_lifetime_s": 0,
    "allow_net": null
}
```

Exec request/response (POST .../exec):
```json
// Request
{"cmd": "python3 -c \"print(42)\"", "workdir": "/", "timeout": 300}

// Response
{
    "seq": 1,
    "cmd": "python3 -c \"print(42)\"",
    "workdir": "/",
    "exit_code": 0,
    "started": "2025-01-15T10:35:00+00:00",
    "finished": "2025-01-15T10:35:01+00:00",
    "stdout": "42\n",
    "stderr": ""
}
```

Error responses: `{"error": "message string"}` with appropriate HTTP status codes (400, 401, 404, 409, 415, 500).

**Content-Type enforcement**: POST endpoints require `Content-Type: application/json`. Return 415 if missing (matching common.sh:113-118).

### Sandbox Create (Full Flow)

This is the most complex operation. Must match `_chroot_create_sandbox` (common.sh:458-542):

1. Validate ID (alphanumeric + dash + underscore)
2. Check sandbox doesn't already exist
3. Check sandbox count < `max_sandboxes`
4. Validate all layers exist (check local, then try S3 pull)
5. Create directory tree: `{sandboxes_dir}/{id}/images/`, `upper/`, `merged/`, `.meta/log/`
6. Mount tmpfs on `upper/` with size limit
7. Create `upper/data/` and `upper/work/` inside tmpfs
8. Write metadata files to `.meta/` (owner, task, layers, created, last_active, cpu, memory_mb, max_lifetime_s, allow_net)
9. Set up cgroup (may fail silently — existing code does `|| true`)
10. Loop-mount each squashfs module at `images/{module_name}.squashfs/`
11. Mount overlay (lowerdir = modules in descending numeric order)
12. Set up network namespace + veth + iptables
13. Seed `/etc/resolv.conf` in upper (point to gateway IP)
14. Inject secret placeholders + proxy config
15. If ephemeral mode: check S3 for latest snapshot, auto-restore
16. Return sandbox info

If any step after tmpfs mount fails, the whole Sandbox struct should be dropped, triggering cleanup via Drop implementations.

### Module Activation (must match common.sh:611-632)

1. Verify module exists
2. Check not already active
3. Loop-mount new squashfs at `images/{module}.squashfs/`
4. Unmount overlay
5. Remount overlay with updated lowerdir (including new module)
6. Append module to `.meta/layers`

**Critical**: The overlay unmount+remount must be atomic from the sandbox's perspective — this is why per-sandbox mutex matters. No exec can run during this operation.

### Snapshot (must match common.sh:636-659)

1. Validate label
2. Check snapshot doesn't already exist
3. Shell out to `mksquashfs {upper}/data {snapdir}/{label}.squashfs` with compression args:
   - Check `/proc/config.gz` for `CONFIG_SQUASHFS_ZSTD=y`
   - If zstd: `-comp zstd -Xcompression-level 3 -b 128K`
   - If not: `-comp gzip -b 256K`
   - Always: `-noappend -quiet`
4. Write snapshot entry to `.meta/snapshots.jsonl`
5. If S3 enabled, push snapshot in background
6. Return snapshot filename

### Restore (must match common.sh:664-707)

1. Validate label
2. Find snapshot file (local, or pull from S3)
3. Unmount overlay
4. Unmount previous snapshot layer if any
5. Clear `upper/data/*` and `upper/work/*` (preserve tmpfs mount)
6. Loop-mount snapshot at `images/_snapshot/`
7. Write label to `.meta/active_snapshot`
8. Remount overlay with snapshot as highest-priority lower layer
9. Re-inject secret placeholders (they were in the cleared upper)

### Destroy (must match common.sh:708-740)

1. If ephemeral mode: auto-snapshot + push to S3
2. Drop the Sandbox struct (triggers all Drop impls)
3. Remove sandbox directory tree
4. Remove from DashMap

### Reaper (`reaper.rs`)

Background tokio task. Every 10 seconds (matching sq-reaper), scan all sandboxes:
- Read `.meta/max_lifetime_s` and `.meta/created`
- If `now - created > max_lifetime_s` and `max_lifetime_s > 0`, destroy the sandbox
- Log each reap at `info` level

### Init / Recovery (`init.rs`)

Run at startup before accepting API requests (matching `bin/sq-init`):

1. Create `modules/` and `sandboxes/` directories
2. Check for `000-base-alpine.squashfs`:
   - Check `.version` file (current version = 2)
   - If missing or outdated: try S3 pull, then fall back to shelling out to `sq-mkbase alpine`
3. If Firecracker backend: check for VM components, shell out to `sq-mkvm all` if missing
4. If NOT ephemeral mode: scan `sandboxes/*/` and remount surviving sandboxes:
   - For each sandbox with `.meta/layers`:
     - Skip if already in /proc/mounts
     - Remount each squashfs module
     - Remount active snapshot if any
     - Rebuild overlay
     - Log success/failure

### Firecracker Backend

The chroot backend is the priority. Implement the Firecracker backend as a separate `Backend` trait implementation that can be filled in later. For now, implement the trait with `todo!()` or `unimplemented!()` for Firecracker methods.

The trait boundary:

```rust
#[async_trait]
trait SandboxBackend: Send + Sync {
    async fn create(&self, id: &str, config: &SandboxConfig) -> Result<Sandbox>;
    async fn exec(&self, sandbox: &mut Sandbox, req: ExecRequest) -> Result<ExecResult>;
    async fn activate(&self, sandbox: &mut Sandbox, module: &str) -> Result<()>;
    async fn snapshot(&self, sandbox: &mut Sandbox, label: &str) -> Result<PathBuf>;
    async fn restore(&self, sandbox: &mut Sandbox, label: &str) -> Result<()>;
    async fn destroy(&self, sandbox: &mut Sandbox) -> Result<()>;
    fn is_mounted(&self, sandbox: &Sandbox) -> bool;
}
```

When Firecracker is implemented later, the guest agent (`sq-guest-agent`) handles mount/overlay/exec inside the VM, and the host-side backend communicates via vsock + Firecracker HTTP API.

### Guest Agent (`guest/main.rs`)

Separate binary, minimal dependencies. This is PID 1 inside the Firecracker VM.

**Startup sequence** (matching `vm/init`):
1. Mount /proc, /sys, /dev, /dev/pts, /dev/shm
2. Parse kernel cmdline for `squash.layers=N` and `squash.allow_net=...`
3. Mount squashfs layers from /dev/vd[b-z] (vda = this rootfs)
4. Set up overlayfs (same upper/work/merged pattern)
5. Seed /etc/resolv.conf with `nameserver 10.0.0.1`
6. Configure network: lo up, eth0 10.0.0.2/30, default route via 10.0.0.1
7. Listen on vsock port 5000 for commands

**Command protocol** (matching `vm/sq-vsock-handler`):
One JSON object per connection:
```json
// Request
{"cmd": "uname -a", "workdir": "/", "timeout": 300}

// Response
{"exit_code": 0, "stdout": "...", "stderr": "..."}
```

Special command `__squash_remount`: rebuild overlayfs from current block devices (for hot-add).

Use `tokio-vsock` for the listener. Each connection: read JSON request, fork+chroot+exec inside `/sandbox/merged`, collect output, write JSON response.

---

## Startup Sequence (`main.rs`)

```
1. Parse config from env vars
2. Run init/recovery (build base if needed, remount surviving sandboxes)
3. Start secret proxy on :8888 (if secrets.json exists and proxy_https is enabled)
4. Start shell-based secret proxy (if secrets.json exists and proxy_https is NOT enabled)
   → for HTTP-only mode, port the 4.5KB shell proxy (bin/sq-secret-proxy) or just keep it as a child process
5. Start Tailscale (if authkey set) — shell out to setup-tailscale
6. Start reaper background task
7. Start HTTP API server on :${port}
8. Log "squash v4 ready" with module/sandbox counts
```

---

## Filesystem Layout (Must Be Identical)

The on-disk layout must be byte-compatible with the existing shell version so that a running shell-based system can be upgraded to the Rust version without data loss. The Rust binary must be able to read and remount sandboxes created by the shell version, and vice versa.

```
/data/
├── modules/
│   ├── 000-base-alpine.squashfs
│   ├── 000-base-alpine.version      # "2"
│   ├── 100-python312.squashfs
│   └── ...
├── sandboxes/
│   └── {id}/
│       ├── .meta/
│       │   ├── owner                 # plain text
│       │   ├── task                  # plain text
│       │   ├── layers                # comma-separated: "000-base-alpine,100-python312"
│       │   ├── created               # ISO 8601
│       │   ├── last_active           # ISO 8601
│       │   ├── cpu                   # "2"
│       │   ├── memory_mb             # "1024"
│       │   ├── max_lifetime_s        # "0"
│       │   ├── allow_net             # JSON array or absent
│       │   ├── active_snapshot       # label string or absent
│       │   ├── netns_index           # "1" through "254"
│       │   ├── veth_host             # "sq-{id}-h"
│       │   ├── veth_sandbox          # "sq-{id}-s"
│       │   ├── netns_name            # "squash-{id}"
│       │   ├── snapshots.jsonl       # one JSON object per line
│       │   └── log/
│       │       ├── 0001.json
│       │       └── ...
│       ├── images/
│       │   ├── 000-base-alpine.squashfs/   # mount point
│       │   ├── 100-python312.squashfs/     # mount point
│       │   └── _snapshot/                  # mount point (if restored)
│       ├── upper/                          # tmpfs mount
│       │   ├── data/                       # overlayfs upperdir
│       │   └── work/                       # overlayfs workdir
│       ├── merged/                         # overlayfs merge target
│       └── snapshots/
│           └── {label}.squashfs
├── secrets.json                            # optional
├── proxy-ca/                               # generated at startup
│   ├── ca.crt
│   └── ca.key
├── vm/                                     # Firecracker only
│   ├── firecracker
│   ├── vmlinux
│   └── guest-rootfs.ext4
├── .netns-index.lock                       # flock file
└── .s3-push.log                            # background push log
```

---

## Dockerfile

```dockerfile
# Build squashd
FROM rust:1.83-alpine AS build
RUN apk add --no-cache musl-dev openssl-dev openssl-libs-static pkgconf
WORKDIR /build
COPY Cargo.toml Cargo.lock ./
COPY src/ src/
COPY guest/ guest/
RUN RUSTFLAGS='-C target-feature=+crt-static' \
    cargo build --release --target x86_64-unknown-linux-musl
RUN RUSTFLAGS='-C target-feature=+crt-static' \
    cargo build --release --target x86_64-unknown-linux-musl --bin sq-guest-agent

FROM alpine:3.21
RUN apk add --no-cache \
    jq squashfs-tools util-linux curl wget coreutils \
    socat openssl busybox-extras iproute2 iptables \
    && apk add --no-cache aws-cli 2>/dev/null || true \
    && apk add --no-cache tailscale 2>/dev/null || true

COPY --from=build /build/target/x86_64-unknown-linux-musl/release/squashd /app/bin/squashd
COPY --from=build /build/target/x86_64-unknown-linux-musl/release/sq-guest-agent /app/bin/sq-guest-agent
# Keep shell scripts for module building + CLI
COPY bin/sq-mkbase bin/sq-mkmod bin/sq-mkvm bin/sq-ctl bin/setup-tailscale /app/bin/
COPY static/ /app/static/

RUN chmod +x /app/bin/*

VOLUME /data
EXPOSE 8080

ENV SQUASH_DATA=/data \
    SQUASH_PORT=8080 \
    SQUASH_BACKEND=chroot \
    SQUASH_AUTH_TOKEN="" \
    SQUASH_S3_BUCKET="" \
    SQUASH_S3_ENDPOINT="" \
    SQUASH_S3_REGION="us-east-1" \
    SQUASH_S3_PREFIX="" \
    SQUASH_EPHEMERAL="" \
    SQUASH_UPPER_LIMIT_MB=512 \
    SQUASH_MAX_SANDBOXES=100 \
    SQUASH_PROXY_HTTPS="" \
    TAILSCALE_AUTHKEY="" \
    TAILSCALE_HOSTNAME="squash"

WORKDIR /app
ENTRYPOINT ["/app/bin/squashd"]
```

---

## Testing Strategy

### Unit tests (in each module)
- Validation functions (valid_id, valid_label, valid_module)
- Config parsing (all env vars, defaults, edge cases)
- Secrets loading + placeholder replacement logic
- Overlay lowerdir ordering (snapshot first, then descending numeric)
- S3 key construction

### Integration tests (require privileged container)

Port the existing `sq-test` (bin/sq-test, 278 lines) to Rust integration tests. Each test from that file becomes a `#[tokio::test]`:

1. **Input validation**: reject path traversal in ID (`../../etc`), reject path traversal in snapshot label, reject missing Content-Type
2. **Filesystem isolation**: create + exec, writes land in upper only, overlayfs COW works
3. **Snapshot/restore cycle**: create, exec (write file), snapshot, exec (delete file), restore, verify file exists again
4. **Module activation**: create with base, activate python module, verify python3 works
5. **Network namespace**: sandbox gets isolated netns, can resolve DNS, egress filtering works
6. **Resource limits**: memory limit triggers OOM, CPU limit works (approximate), max_lifetime_s triggers reaper
7. **Secret proxy**: placeholder visible inside sandbox, real key reaches upstream (mock)
8. **Concurrent operations**: parallel exec on same sandbox serializes correctly, parallel create on different sandboxes works
9. **Cleanup correctness**: destroy leaves no orphaned mounts, no orphaned iptables rules, no orphaned netns

### Stress tests
- Create and destroy 100 sandboxes rapidly — verify no resource leaks
- Concurrent exec on 50 sandboxes simultaneously
- Kill squashd mid-create — restart and verify recovery works

---

## Implementation Order

Build and test incrementally. Each phase should result in passing tests.

1. **Phase 1: Config + Validation + API skeleton** — axum server with health endpoint, config loading, input validation. No sandbox operations. Just verify the HTTP server works.

2. **Phase 2: Mounts** — SquashfsMount, TmpfsMount, OverlayMount with Drop. Unit-test mount/unmount in a privileged container. This is the foundation everything else depends on.

3. **Phase 3: Sandbox create + destroy** — Full create flow (minus netns and cgroup). Exec with basic unshare+chroot. Verify create/exec/destroy cycle works. Test cleanup on partial failure.

4. **Phase 4: Cgroups + Network namespaces** — Add CgroupHandle and NetnsHandle. Verify resource limits and network isolation. Test egress filtering.

5. **Phase 5: Snapshot + Restore + Activate** — mksquashfs shell-out, restore flow with overlay remount, module activation. Test the full snapshot/restore cycle.

6. **Phase 6: Secret proxy** — Port the Go MITM proxy. Test with a mock upstream.

7. **Phase 7: S3 sync** — aws-sdk-s3 integration. Test with MinIO in Docker.

8. **Phase 8: Init/recovery + Reaper + Ephemeral** — Startup recovery, background reaper, ephemeral mode. Test crash recovery.

9. **Phase 9: Wire compatibility** — Verify a sandbox created by the shell version can be remounted by the Rust version. Run the existing `sq-test` suite against the Rust binary.

---

## Non-Goals (explicitly out of scope for this rewrite)

- **Rewriting sq-mkbase/sq-mkmod/sq-mkvm in Rust** — these are setup-time scripts that download tarballs and call mksquashfs. Shell is fine.
- **Rewriting sq-ctl in Rust** — it's a thin curl wrapper. Shell is fine.
- **Implementing the Firecracker backend** — design the trait boundary, implement chroot backend fully, stub Firecracker. The guest agent binary should be implemented but the host-side Firecracker management can wait.
- **WebSocket streaming for exec** — the existing API is request/response. Streaming exec output is a future enhancement.
- **Multi-node distribution** — that's the Elixir orchestrator layer, not this binary.
- **GPU passthrough** — acknowledged as a limitation in the existing README.
- **Live migration between backends** — same.
