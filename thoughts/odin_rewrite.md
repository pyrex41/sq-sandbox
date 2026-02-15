# sq-sandbox Odin Rewrite — Complete Implementation Prompt

## Project Summary

Rewrite the `sq-sandbox` container runtime from shell scripts into **two Odin binaries** plus a **Go sidecar**:

1. **`squashd`** — the main daemon. Replaces all of `cgi-bin/`, most of `bin/`. HTTP API server, sandbox lifecycle manager, reaper, S3 sync, init/recovery. Written entirely in Odin using the standard library (`core:net`, `core:encoding/json`, `core:os`, `core:sys/linux`, `core:thread`, `core:sync`, `core:crypto`).

2. **`sq-guest-agent`** — the Firecracker guest agent. Replaces `vm/init` and `vm/sq-vsock-handler`. Runs as PID 1 inside a microVM. Also Odin.

3. **`sq-secret-proxy-https`** — the existing Go binary (`proxy/main.go`, 526 lines), kept as-is. Managed as a child process by `squashd`. Rewriting TLS MITM in Odin would require binding to a C TLS library and reimplementing Go's `net/http` CONNECT handling. Not worth it.

Shell scripts (`sq-mkbase`, `sq-mkmod`, `sq-mkvm`, `sq-ctl`) remain as-is.

The existing repo is at https://github.com/pyrex41/sq-sandbox. Read every file before starting.

---

## Why Odin for This

Odin is C's philosophy with modern ergonomics. For a project that's 90% Linux syscall sequencing, the value proposition is:

**`defer` is the right cleanup primitive for kernel state.** Unlike Go where `defer` runs at function exit, Odin's `defer` runs at scope exit, giving finer-grained control. Unlike Rust's `Drop`, it requires explicit annotation — but unlike C's `goto cleanup`, the compiler enforces it at the point of acquisition. The pattern `handle, err := acquire(); if err != nil { return }; defer release(handle)` is idiomatic and correct.

**`core:sys/linux` gives typed syscall wrappers.** `mount`, `umount2`, `unshare`, `chroot`, `setns`, `clone`, `fork`, `waitpid`, `pipe`, `poll` — all available with proper types. No binding library to version-manage. For anything missing, `foreign import` gives zero-cost C FFI.

**Arena allocators per sandbox.** Odin's implicit context system passes allocators through the call chain. Each sandbox gets a `mem.Arena` — when the sandbox is destroyed, free the arena and every allocation the sandbox ever made disappears at once. No individual `free()` calls, no possibility of per-sandbox memory leaks.

**Tagged unions for state machines.** `Sandbox_State :: union { Creating, Ready, Executing, Snapshotting, Destroying }` — exhaustive switching enforced by the compiler.

**`core:encoding/json` with struct tags.** Marshal/unmarshal directly to/from Odin structs. No manual JSON construction.

**`core:net` for the HTTP server.** TCP listen, accept, send, recv — enough to build a simple HTTP/1.1 server for 10 JSON endpoints.

**`or_return` for error propagation.** `result := try_thing() or_return` — clean error forwarding without `?` syntax or exception overhead.

**Binary size:** ~1-3MB static. Smaller than Rust (3-5MB), much smaller than Go (8-12MB). Larger than Zig (500KB-1.5MB) because Odin links the LLVM-compiled runtime, but still very reasonable for Firecracker guest images.

**Build time:** ~2-5 seconds for the whole project. Odin compiles fast.

---

## Architecture Principles

### 1. Arena-per-sandbox memory model

```odin
Managed_Sandbox :: struct {
    sandbox: Sandbox,
    arena:   mem.Arena,
    lock:    sync.Mutex,
}
```

When creating a sandbox, initialize a `mem.Arena`. Pass `mem.arena_allocator(&arena)` as the allocator for all sandbox-related allocations (path construction, metadata strings, mount point arrays). When destroying, `mem.arena_destroy(&arena)` frees everything at once.

This is idiomatic Odin. The implicit `context.allocator` means library code automatically uses whatever allocator the caller provides — no explicit threading of allocators through function signatures.

### 2. Explicit cleanup with `defer`

Every resource acquisition is immediately followed by `defer` cleanup:

```odin
sandbox_create :: proc(mgr: ^Sandbox_Manager, id: string, opts: Create_Options) -> (Sandbox, Error) {
    // 1. Mount tmpfs
    tmpfs := tmpfs_mount(upper_path, config.upper_limit_mb) or_return
    defer if err != nil { tmpfs_unmount(&tmpfs) }

    // 2. Mount squashfs layers
    sqfs_mounts := make([dynamic]Squashfs_Mount, allocator = context.allocator)
    defer if err != nil {
        for &m in sqfs_mounts { squashfs_unmount(&m) }
        delete(sqfs_mounts)
    }
    for layer in layers {
        m := squashfs_mount(mod_path, mp_path) or_return
        append(&sqfs_mounts, m)
    }

    // 3. Mount overlay
    overlay := overlay_mount(lower_components, upper_data, work, merged) or_return
    defer if err != nil { overlay_unmount(&overlay) }

    // 4. Cgroup (may fail, ok)
    cgroup, cg_err := cgroup_create(id, opts.cpu, opts.memory_mb)
    defer if err != nil && cg_err == nil { cgroup_destroy(&cgroup) }

    // 5. Network namespace
    netns := netns_setup(config, id, opts.allow_net) or_return
    defer if err != nil { netns_teardown(&netns) }

    // 6. Seed resolv.conf, inject secrets
    seed_resolv_conf(config, id, netns.index) or_return
    inject_secret_placeholders(config, id, netns.index) or_return

    // 7. Write metadata
    write_meta(config, id, layers, opts) or_return

    // All succeeded — return without triggering defers
    return Sandbox{
        id          = id,
        tmpfs       = tmpfs,
        sqfs_mounts = sqfs_mounts[:],
        overlay     = overlay,
        cgroup      = cgroup if cg_err == nil else nil,
        netns       = netns,
    }, nil
}
```

The `defer if err != nil` pattern means: if the function returns an error (any later step fails), run this cleanup. If the function returns successfully, the defers don't fire and ownership transfers to the caller. This is Odin's equivalent of Zig's `errdefer`.

**The destruction path** requires explicit `sandbox_destroy()` calls — same discipline requirement as Zig, same mitigation: the `Sandbox_Manager` always calls `destroy()` before removing from the map.

### 3. Per-sandbox mutex, not global lock

```odin
Sandbox_Manager :: struct {
    sandboxes:   map[string]^Managed_Sandbox,
    global_lock: sync.Mutex,  // protects the map itself
    config:      ^Config,
    allocator:   mem.Allocator,
}
```

Operations on different sandboxes are concurrent. Operations on the same sandbox are serialized by the per-sandbox mutex.

### 4. Tagged union state machine

```odin
Sandbox_State :: union {
    Creating,
    Ready,
    Executing,
    Snapshotting,
    Destroying,
}

Creating :: struct {}

Ready :: struct {
    mounts: Sandbox_Mounts,
    netns:  Maybe(Netns_Handle),
    cgroup: Maybe(Cgroup_Handle),
}

Executing :: struct {
    pid:     i32,
    started: time.Time,
}

Snapshotting :: struct {}
Destroying :: struct {}
```

Odin's `#partial switch` warns if you don't handle all variants. State transitions are enforced by only allowing operations on the correct variant.

---

## Build Configuration

```
# Build squashd (static musl binary)
odin build src/ -target:linux_amd64 -o:speed -out:squashd -extra-linker-flags:"-static"

# Build guest agent
odin build guest/ -target:linux_amd64 -o:size -out:sq-guest-agent -extra-linker-flags:"-static"

# Run tests
odin test src/ -target:linux_amd64
```

Odin uses LLVM as its backend and can produce static binaries when targeting musl. The `-o:speed` flag enables optimizations; `-o:size` minimizes binary size for the guest agent.

---

## Package Structure

```
src/
├── main.odin           # squashd entrypoint, startup sequence
├── config.odin         # Config struct from env vars
├── validate.odin       # Input validation (IDs, labels, module names)
├── api.odin            # HTTP server, routing, request/response handling
├── sandbox.odin        # Sandbox struct, state union, create/destroy
├── manager.odin        # Sandbox_Manager (map + per-sandbox locks)
├── mounts.odin         # Squashfs_Mount, Tmpfs_Mount, Overlay_Mount
├── exec.odin           # fork/unshare/chroot/exec logic
├── netns.odin          # Network namespace, veth, iptables (shells out)
├── cgroup.odin         # cgroups v2
├── snapshot.odin       # snapshot (mksquashfs) and restore
├── meta.odin           # Metadata read/write (filesystem-backed)
├── modules.odin        # Module registry
├── s3.odin             # S3 sync (HTTP + SigV4 signing)
├── sigv4.odin          # AWS Signature V4 signing
├── reaper.odin         # Background thread: destroy expired sandboxes
├── init.odin           # First-boot: build base, remount survivors
├── secrets.odin        # secrets.json loading, placeholder injection
├── http.odin           # Minimal HTTP/1.1 server built on core:net
├── linux_extra.odin    # Any syscall wrappers not in core:sys/linux

guest/
├── main.odin           # sq-guest-agent: PID 1, mount layers
├── vsock.odin          # vsock listener (foreign import for AF_VSOCK)
└── overlay.odin        # guest-side overlayfs setup
```

All files are `package squashd` (or `package guest_agent` for the guest). Odin compiles all files in a directory as a single package — no explicit module declarations needed.

---

## Detailed Implementation Specifications

### Config (`config.odin`)

```odin
Backend :: enum {
    Chroot,
    Firecracker,
}

Config :: struct {
    backend:            Backend,
    data_dir:           string,
    port:               u16,
    auth_token:         Maybe(string),
    s3_bucket:          Maybe(string),
    s3_endpoint:        Maybe(string),
    s3_region:          string,
    s3_prefix:          string,
    ephemeral:          bool,
    upper_limit_mb:     u64,
    max_sandboxes:      int,
    proxy_https:        bool,
    tailscale_authkey:  Maybe(string),
    tailscale_hostname: string,
}

config_from_env :: proc(allocator := context.allocator) -> Config {
    get :: proc(key: string, fallback: string) -> string {
        if val, ok := os.lookup_env(key); ok {
            return val
        }
        return fallback
    }

    return Config{
        backend        = .Firecracker if get("SQUASH_BACKEND", "") == "firecracker" else .Chroot,
        data_dir       = get("SQUASH_DATA", "/data"),
        port           = u16(strconv.atoi(get("SQUASH_PORT", "8080"))),
        auth_token     = auth if auth := get("SQUASH_AUTH_TOKEN", ""); len(auth) > 0 else nil,
        s3_bucket      = bucket if bucket := get("SQUASH_S3_BUCKET", ""); len(bucket) > 0 else nil,
        s3_endpoint    = ep if ep := get("SQUASH_S3_ENDPOINT", ""); len(ep) > 0 else nil,
        s3_region      = get("SQUASH_S3_REGION", "us-east-1"),
        s3_prefix      = get("SQUASH_S3_PREFIX", ""),
        ephemeral      = get("SQUASH_EPHEMERAL", "") == "1",
        upper_limit_mb = u64(strconv.atoi(get("SQUASH_UPPER_LIMIT_MB", "512"))),
        max_sandboxes  = strconv.atoi(get("SQUASH_MAX_SANDBOXES", "100")),
        proxy_https    = get("SQUASH_PROXY_HTTPS", "") == "1",
        tailscale_authkey  = key if key := get("TAILSCALE_AUTHKEY", ""); len(key) > 0 else nil,
        tailscale_hostname = get("TAILSCALE_HOSTNAME", "squash"),
    }
}

modules_dir :: proc(c: ^Config) -> string { return fmt.tprintf("%s/modules", c.data_dir) }
sandboxes_dir :: proc(c: ^Config) -> string { return fmt.tprintf("%s/sandboxes", c.data_dir) }
secrets_path :: proc(c: ^Config) -> string { return fmt.tprintf("%s/secrets.json", c.data_dir) }
proxy_ca_dir :: proc(c: ^Config) -> string { return fmt.tprintf("%s/proxy-ca", c.data_dir) }
```

### Input Validation (`validate.odin`)

```odin
// Sandbox IDs: [a-zA-Z0-9_-]+
valid_id :: proc(s: string) -> bool {
    if len(s) == 0 do return false
    for ch in s {
        if !unicode.is_alpha(ch) && !unicode.is_digit(ch) && ch != '_' && ch != '-' {
            return false
        }
    }
    return true
}

// Snapshot labels and module names: [a-zA-Z0-9_.-]+
valid_label :: proc(s: string) -> bool {
    if len(s) == 0 do return false
    for ch in s {
        if !unicode.is_alpha(ch) && !unicode.is_digit(ch) && ch != '_' && ch != '.' && ch != '-' {
            return false
        }
    }
    return true
}

valid_module :: valid_label  // same rules
```

### Mount Operations (`mounts.odin`)

Uses `core:sys/linux` for `mount`/`umount2` syscalls. If `core:sys/linux` doesn't expose them directly (check the package — it may vary by Odin version), use `foreign import` to call libc:

```odin
import linux "core:sys/linux"
import "core:c"

// If not in core:sys/linux, import from libc
when !#defined(linux.mount) {
    foreign import libc "system:c"

    foreign libc {
        @(link_name="mount")
        c_mount :: proc(source: cstring, target: cstring, fstype: cstring, flags: c.ulong, data: rawptr) -> c.int ---
        @(link_name="umount2")
        c_umount2 :: proc(target: cstring, flags: c.int) -> c.int ---
        @(link_name="unshare")
        c_unshare :: proc(flags: c.int) -> c.int ---
        @(link_name="chroot")
        c_chroot :: proc(path: cstring) -> c.int ---
        @(link_name="setns")
        c_setns :: proc(fd: c.int, nstype: c.int) -> c.int ---
    }
}

MS_RDONLY    :: 1
MNT_DETACH   :: 2
CLONE_NEWNET :: 0x40000000
CLONE_NEWNS  :: 0x00020000
CLONE_NEWPID :: 0x20000000
CLONE_NEWIPC :: 0x08000000
CLONE_NEWUTS :: 0x04000000

Mount_Error :: enum {
    None,
    Create_Dir_Failed,
    Mount_Failed,
    Unmount_Failed,
}

Squashfs_Mount :: struct {
    mount_point: string,
    active:      bool,
}

squashfs_mount :: proc(squashfs_path: string, mount_point: string) -> (Squashfs_Mount, Mount_Error) {
    if !os.make_directory(mount_point) && !os.exists(mount_point) {
        return {}, .Create_Dir_Failed
    }

    src := strings.clone_to_cstring(squashfs_path, context.temp_allocator)
    tgt := strings.clone_to_cstring(mount_point, context.temp_allocator)

    rc := c_mount(src, tgt, "squashfs", MS_RDONLY, nil)
    if rc != 0 {
        return {}, .Mount_Failed
    }

    return Squashfs_Mount{
        mount_point = strings.clone(mount_point),
        active      = true,
    }, .None
}

squashfs_unmount :: proc(m: ^Squashfs_Mount) {
    if m.active {
        tgt := strings.clone_to_cstring(m.mount_point, context.temp_allocator)
        c_umount2(tgt, MNT_DETACH)
        m.active = false
    }
}

Tmpfs_Mount :: struct {
    mount_point: string,
    active:      bool,
}

tmpfs_mount :: proc(mount_point: string, size_mb: u64) -> (Tmpfs_Mount, Mount_Error) {
    if !os.make_directory(mount_point) && !os.exists(mount_point) {
        return {}, .Create_Dir_Failed
    }

    tgt := strings.clone_to_cstring(mount_point, context.temp_allocator)
    opts := fmt.ctprintf("size=%dm", size_mb)

    rc := c_mount("tmpfs", tgt, "tmpfs", 0, rawptr(opts))
    if rc != 0 {
        return {}, .Mount_Failed
    }

    // Create overlay subdirectories
    os.make_directory(fmt.tprintf("%s/data", mount_point))
    os.make_directory(fmt.tprintf("%s/work", mount_point))

    return Tmpfs_Mount{ mount_point = strings.clone(mount_point), active = true }, .None
}

tmpfs_unmount :: proc(m: ^Tmpfs_Mount) {
    if m.active {
        tgt := strings.clone_to_cstring(m.mount_point, context.temp_allocator)
        c_umount2(tgt, MNT_DETACH)
        m.active = false
    }
}

Overlay_Mount :: struct {
    merged_path: string,
    active:      bool,
}

// lower_components: ordered highest-priority first (snapshot, then descending numeric modules)
overlay_mount :: proc(
    lower_components: []string,
    upper_data:       string,
    work:             string,
    merged:           string,
) -> (Overlay_Mount, Mount_Error) {
    if !os.make_directory(merged) && !os.exists(merged) {
        return {}, .Create_Dir_Failed
    }

    // Build options string
    buf: [4096]byte
    b := strings.builder_from_bytes(buf[:])
    strings.write_string(&b, "lowerdir=")
    for comp, i in lower_components {
        if i > 0 do strings.write_byte(&b, ':')
        strings.write_string(&b, comp)
    }
    fmt.sbprintf(&b, ",upperdir=%s,workdir=%s", upper_data, work)

    tgt := strings.clone_to_cstring(merged, context.temp_allocator)
    opts := strings.clone_to_cstring(strings.to_string(b), context.temp_allocator)

    rc := c_mount("overlay", tgt, "overlay", 0, rawptr(opts))
    if rc != 0 {
        return {}, .Mount_Failed
    }

    return Overlay_Mount{ merged_path = strings.clone(merged), active = true }, .None
}

overlay_unmount :: proc(m: ^Overlay_Mount) {
    if m.active {
        tgt := strings.clone_to_cstring(m.merged_path, context.temp_allocator)
        c_umount2(tgt, MNT_DETACH)
        m.active = false
    }
}

Sandbox_Mounts :: struct {
    sqfs_mounts:    [dynamic]Squashfs_Mount,
    snapshot_mount: Maybe(Squashfs_Mount),
    tmpfs:          Tmpfs_Mount,
    overlay:        Overlay_Mount,
}

// Explicit teardown in reverse mount order
sandbox_mounts_destroy :: proc(mounts: ^Sandbox_Mounts) {
    overlay_unmount(&mounts.overlay)
    if snap, ok := &mounts.snapshot_mount.?; ok {
        squashfs_unmount(snap)
    }
    tmpfs_unmount(&mounts.tmpfs)
    // Reverse order for squashfs layers
    #reverse for &m in mounts.sqfs_mounts {
        squashfs_unmount(&m)
    }
    delete(mounts.sqfs_mounts)
}
```

### Network Namespace (`netns.odin`)

Shell out to `ip` and `iptables` — same pragmatic choice as the Zig prompt. Use `os.process_exec` (or `libc.system` / `fork+exec`) for command execution.

```odin
Netns_Handle :: struct {
    name:       string,  // "squash-{id}"
    index:      u8,      // 1-254
    veth_host:  string,  // "sq-{id}-h"
    chain_name: Maybe(string),
    host_dns:   Maybe(string),
}

// Run a shell command, return success/failure
run_cmd :: proc(args: ..string) -> bool {
    // Build command string for system()
    cmd := strings.join(args[:], " ", context.temp_allocator)
    cstr := strings.clone_to_cstring(cmd, context.temp_allocator)
    return libc.system(cstr) == 0
}

netns_setup :: proc(
    config: ^Config,
    id:     string,
    allow_net: Maybe([]string),
) -> (Netns_Handle, Error) {
    index := allocate_netns_index(config) or_return

    name := fmt.aprintf("squash-%s", id)
    veth_host := fmt.aprintf("sq-%s-h", id)
    veth_sandbox := fmt.aprintf("sq-%s-s", id)

    // Create netns
    if !run_cmd("ip", "netns", "add", name) {
        return {}, .Netns_Create_Failed
    }

    // Create veth pair
    if !run_cmd("ip", "link", "add", veth_host, "type", "veth", "peer", "name", veth_sandbox) {
        run_cmd("ip", "netns", "delete", name)  // rollback
        return {}, .Veth_Create_Failed
    }

    // Move sandbox end into netns
    run_cmd("ip", "link", "set", veth_sandbox, "netns", name)

    // Configure host side
    host_addr := fmt.tprintf("10.200.%d.1/30", index)
    run_cmd("ip", "addr", "add", host_addr, "dev", veth_host)
    run_cmd("ip", "link", "set", veth_host, "up")

    // Configure sandbox side (inside netns)
    sandbox_addr := fmt.tprintf("10.200.%d.2/30", index)
    gateway := fmt.tprintf("10.200.%d.1", index)
    run_cmd("ip", "netns", "exec", name, "ip", "addr", "add", sandbox_addr, "dev", veth_sandbox)
    run_cmd("ip", "netns", "exec", name, "ip", "link", "set", veth_sandbox, "up")
    run_cmd("ip", "netns", "exec", name, "ip", "link", "set", "lo", "up")
    run_cmd("ip", "netns", "exec", name, "ip", "route", "add", "default", "via", gateway)

    // IP forwarding
    os.write_entire_file("/proc/sys/net/ipv4/ip_forward", transmute([]byte)string("1"))

    // NAT
    subnet := fmt.tprintf("10.200.%d.0/30", index)
    run_cmd("iptables", "-t", "nat", "-A", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE")

    // DNS DNAT
    host_dns := parse_first_nameserver("/etc/resolv.conf")
    if dns, ok := host_dns.?; ok {
        run_cmd("iptables", "-t", "nat", "-A", "PREROUTING",
            "-s", subnet, "-d", gateway, "-p", "udp", "--dport", "53",
            "-j", "DNAT", "--to-destination", dns)
        run_cmd("iptables", "-t", "nat", "-A", "PREROUTING",
            "-s", subnet, "-d", gateway, "-p", "tcp", "--dport", "53",
            "-j", "DNAT", "--to-destination", dns)
    }

    // Egress filtering
    chain_name: Maybe(string)
    if nets, ok := allow_net.?; ok && len(nets) > 0 {
        chain_name = apply_egress_rules(id, veth_host, nets)
    }

    return Netns_Handle{
        name       = name,
        index      = index,
        veth_host  = veth_host,
        chain_name = chain_name,
        host_dns   = host_dns,
    }, nil
}

netns_teardown :: proc(h: ^Netns_Handle) {
    // Reverse order, ignore all errors (matching shell || true)
    if chain, ok := h.chain_name.?; ok {
        run_cmd("iptables", "-D", "FORWARD", "-i", h.veth_host, "-j", chain)
        run_cmd("iptables", "-F", chain)
        run_cmd("iptables", "-X", chain)
    }
    subnet := fmt.tprintf("10.200.%d.0/30", h.index)
    run_cmd("iptables", "-t", "nat", "-D", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE")
    // DNS DNAT cleanup...
    run_cmd("ip", "link", "delete", h.veth_host)
    run_cmd("ip", "netns", "delete", h.name)
}
```

**Egress filtering** (matching common.sh:385-422): create iptables chain, ICMP drop, DNS rate limit, ESTABLISHED/RELATED accept, resolve allowed hosts, default DROP.

**Index allocation** (matching common.sh:309-325): `flock` on `{data_dir}/.netns-index.lock`, scan `.meta/netns_index` files for first unused 1-254.

### Cgroups v2 (`cgroup.odin`)

```odin
Cgroup_Handle :: struct {
    path: string,
}

cgroup_create :: proc(id: string, cpu: f64, memory_mb: u64) -> (Cgroup_Handle, Error) {
    path := fmt.aprintf("/sys/fs/cgroup/squash-%s", id)
    os.make_directory(path)

    // CPU: quota = cpu * 100000, period = 100000
    quota := u64(cpu * 100_000.0)
    os.write_entire_file(
        fmt.tprintf("%s/cpu.max", path),
        transmute([]byte)fmt.tprintf("%d 100000", quota),
    )

    // Memory
    os.write_entire_file(
        fmt.tprintf("%s/memory.max", path),
        transmute([]byte)fmt.tprintf("%d", memory_mb * 1024 * 1024),
    )

    return Cgroup_Handle{ path = path }, nil
}

cgroup_add_process :: proc(h: ^Cgroup_Handle, pid: i32) {
    os.write_entire_file(
        fmt.tprintf("%s/cgroup.procs", h.path),
        transmute([]byte)fmt.tprintf("%d", pid),
    )
}

cgroup_destroy :: proc(h: ^Cgroup_Handle) {
    // Move processes out, then rmdir
    if procs, ok := os.read_entire_file(fmt.tprintf("%s/cgroup.procs", h.path)); ok {
        os.write_entire_file("/sys/fs/cgroup/cgroup.procs", procs)
    }
    os.remove(h.path)
}
```

### Sandbox Execution (`exec.odin`)

Replaces common.sh:546-607. Uses `fork`, `unshare`, `chroot`, `execve` via foreign imports.

```odin
foreign import libc "system:c"
foreign libc {
    c_fork    :: proc() -> c.int ---
    c_execve  :: proc(path: cstring, argv: [^]cstring, envp: [^]cstring) -> c.int ---
    c_chdir   :: proc(path: cstring) -> c.int ---
    c_dup2    :: proc(oldfd, newfd: c.int) -> c.int ---
    c_pipe    :: proc(fds: [^]c.int) -> c.int ---
    c_close   :: proc(fd: c.int) -> c.int ---
    c_read    :: proc(fd: c.int, buf: [^]byte, count: c.size_t) -> c.ssize_t ---
    c_write   :: proc(fd: c.int, buf: [^]byte, count: c.size_t) -> c.ssize_t ---
    c_poll    :: proc(fds: [^]Poll_Fd, nfds: c.ulong, timeout: c.int) -> c.int ---
    c_waitpid :: proc(pid: c.int, status: ^c.int, options: c.int) -> c.int ---
    c_kill    :: proc(pid: c.int, sig: c.int) -> c.int ---
    c_getpid  :: proc() -> c.int ---
    _exit     :: proc(status: c.int) ---
}

SIGKILL :: 9
STDOUT_FILENO :: 1
STDERR_FILENO :: 2
WNOHANG :: 1

Poll_Fd :: struct {
    fd:      c.int,
    events:  c.short,
    revents: c.short,
}
POLLIN  :: c.short(0x0001)
POLLHUP :: c.short(0x0010)

Exec_Request :: struct {
    cmd:     string,
    workdir: string,
    timeout: u64,
}

Exec_Result :: struct {
    exit_code: i32,
    stdout:    string,
    stderr:    string,
    started:   time.Time,
    finished:  time.Time,
    seq:       u32,
}

exec_in_sandbox :: proc(
    sandbox: ^Sandbox,
    req: Exec_Request,
    allocator := context.allocator,
) -> (Exec_Result, Error) {
    started := time.now()

    // Create pipes
    stdout_fds: [2]c.int
    stderr_fds: [2]c.int
    c_pipe(&stdout_fds[0])
    c_pipe(&stderr_fds[0])
    defer {
        c_close(stdout_fds[0])
        c_close(stderr_fds[0])
    }

    // Open netns fd
    netns_fd: c.int = -1
    if netns, ok := sandbox.netns.?; ok {
        ns_path := fmt.ctprintf("/var/run/netns/%s", netns.name)
        netns_fd = c.int(linux.open(ns_path, {.RDONLY}, {}))
    }
    defer if netns_fd >= 0 { c_close(netns_fd) }

    pid := c_fork()

    if pid == 0 {
        // ═══ CHILD ═══
        c_close(stdout_fds[0])
        c_close(stderr_fds[0])
        c_dup2(stdout_fds[1], STDOUT_FILENO)
        c_dup2(stderr_fds[1], STDERR_FILENO)
        c_close(stdout_fds[1])
        c_close(stderr_fds[1])

        // Enter cgroup
        if cg, ok := sandbox.cgroup.?; ok {
            cgroup_add_process(&cg, c_getpid())
        }

        // Enter network namespace
        if netns_fd >= 0 {
            c_setns(netns_fd, CLONE_NEWNET)
        }

        // New mount/PID/IPC/UTS namespaces
        c_unshare(CLONE_NEWNS | CLONE_NEWPID | CLONE_NEWIPC | CLONE_NEWUTS)

        // chroot into merged overlay
        merged_c := strings.clone_to_cstring(sandbox.mounts.overlay.merged_path, context.temp_allocator)
        c_chroot(merged_c)
        workdir_c := strings.clone_to_cstring(req.workdir, context.temp_allocator)
        c_chdir(workdir_c)

        // exec /bin/sh -c "{cmd}"
        cmd_c := strings.clone_to_cstring(req.cmd, context.temp_allocator)
        argv := [4]cstring{ "/bin/sh", "-c", cmd_c, nil }
        envp := [1]cstring{ nil }
        c_execve("/bin/sh", &argv[0], &envp[0])
        _exit(127)
    }

    // ═══ PARENT ═══
    c_close(stdout_fds[1])
    c_close(stderr_fds[1])

    // Read stdout/stderr with timeout via poll()
    stdout_buf: [65536]byte
    stderr_buf: [65536]byte
    stdout_len: int = 0
    stderr_len: int = 0

    timeout_ms := i64(req.timeout) * 1000
    deadline := time.tick_now()

    stdout_eof := false
    stderr_eof := false

    for !stdout_eof || !stderr_eof {
        elapsed := time.tick_diff(deadline, time.tick_now())
        remaining := timeout_ms - time.duration_milliseconds(elapsed)
        if remaining <= 0 {
            c_kill(pid, SIGKILL)
            break
        }

        fds := [2]Poll_Fd{
            { fd = stdout_fds[0], events = POLLIN },
            { fd = stderr_fds[0], events = POLLIN },
        }
        c_poll(&fds[0], 2, c.int(min(remaining, i64(max(c.int)))))

        if fds[0].revents & POLLIN != 0 && stdout_len < len(stdout_buf) {
            n := c_read(stdout_fds[0], &stdout_buf[stdout_len], c.size_t(len(stdout_buf) - stdout_len))
            if n <= 0 { stdout_eof = true } else { stdout_len += int(n) }
        }
        if fds[0].revents & POLLHUP != 0 { stdout_eof = true }

        if fds[1].revents & POLLIN != 0 && stderr_len < len(stderr_buf) {
            n := c_read(stderr_fds[0], &stderr_buf[stderr_len], c.size_t(len(stderr_buf) - stderr_len))
            if n <= 0 { stderr_eof = true } else { stderr_len += int(n) }
        }
        if fds[1].revents & POLLHUP != 0 { stderr_eof = true }
    }

    // Wait for child
    status: c.int
    c_waitpid(pid, &status, 0)

    exit_code: i32
    timed_out := time.duration_milliseconds(time.tick_diff(deadline, time.tick_now())) >= timeout_ms
    if timed_out {
        exit_code = 124
    } else if status & 0x7f == 0 {  // WIFEXITED
        exit_code = i32((status >> 8) & 0xff)  // WEXITSTATUS
    } else {
        exit_code = 128 + i32(status & 0x7f)  // 128 + signal
    }

    finished := time.now()

    // Write log entry
    seq := sandbox_next_log_seq(sandbox)
    write_exec_log(sandbox, seq, req, exit_code, started, finished,
        stdout_buf[:stdout_len], stderr_buf[:stderr_len])

    return Exec_Result{
        exit_code = exit_code,
        stdout    = strings.clone_from_bytes(stdout_buf[:stdout_len], allocator),
        stderr    = strings.clone_from_bytes(stderr_buf[:stderr_len], allocator),
        started   = started,
        finished  = finished,
        seq       = seq,
    }, nil
}
```

### HTTP Server (`http.odin`)

Build a minimal HTTP/1.1 server on `core:net`. This API has 10 endpoints, all JSON, no streaming, no WebSocket. A hand-rolled HTTP server is ~300 lines.

```odin
import "core:net"

Http_Handler :: #type proc(req: ^Http_Request, resp: ^Http_Response)

Http_Request :: struct {
    method:       string,
    path:         string,
    headers:      map[string]string,
    body:         []byte,
    content_type: string,
}

Http_Response :: struct {
    status:  int,
    headers: map[string]string,
    body:    []byte,
}

Route :: struct {
    method:  string,
    path:    string,  // exact match or prefix with "/*"
    handler: Http_Handler,
}

serve :: proc(port: u16, routes: []Route, config: ^Config, manager: ^Sandbox_Manager) {
    endpoint := net.Endpoint{
        address = net.IP4_Any,
        port    = int(port),
    }

    socket, err := net.listen_tcp(endpoint)
    if err != nil {
        fmt.eprintfln("listen failed: %v", err)
        return
    }

    for {
        client, _, accept_err := net.accept_tcp(socket)
        if accept_err != nil do continue

        // Spawn thread per connection
        thread.create_and_start_with_poly_data2(
            client,
            &Thread_Context{ routes = routes, config = config, manager = manager },
            handle_connection,
        )
    }
}

handle_connection :: proc(client: net.TCP_Socket, ctx: ^Thread_Context) {
    defer net.close(client)

    // Read request (simple HTTP/1.1 parsing)
    buf: [65536]byte
    n, _ := net.recv_tcp(client, buf[:])
    if n <= 0 do return

    req := parse_http_request(buf[:n])

    // Auth check
    if token, ok := ctx.config.auth_token.?; ok {
        auth_header := req.headers["Authorization"] or_else ""
        expected := fmt.tprintf("Bearer %s", token)
        if auth_header != expected && strings.has_prefix(req.path, "/cgi-bin/api/") {
            send_json_error(client, 401, "unauthorized")
            return
        }
    }

    // Route matching
    for route in ctx.routes {
        if req.method == route.method && match_path(req.path, route.path) {
            resp := Http_Response{ status = 200 }
            route.handler(&req, &resp)
            send_response(client, &resp)
            return
        }
    }

    send_json_error(client, 404, "not found")
}
```

### JSON API (`api.odin`)

Use `core:encoding/json` for marshal/unmarshal with struct tags.

```odin
import "core:encoding/json"

// Wire-compatible with existing API

Create_Request :: struct {
    id:             string          `json:"id"`,
    owner:          Maybe(string)   `json:"owner"`,
    layers:         json.Value      `json:"layers"`,   // string or array
    task:           Maybe(string)   `json:"task"`,
    cpu:            Maybe(f64)      `json:"cpu"`,
    memory_mb:      Maybe(u64)      `json:"memory_mb"`,
    max_lifetime_s: Maybe(u64)      `json:"max_lifetime_s"`,
    allow_net:      Maybe([]string) `json:"allow_net"`,
}

Sandbox_Info :: struct {
    id:              string         `json:"id"`,
    owner:           string         `json:"owner"`,
    task:            string         `json:"task"`,
    layers:          []string       `json:"layers"`,
    created:         string         `json:"created"`,
    last_active:     string         `json:"last_active"`,
    mounted:         bool           `json:"mounted"`,
    exec_count:      int            `json:"exec_count"`,
    upper_bytes:     u64            `json:"upper_bytes"`,
    snapshots:       []string       `json:"snapshots"`,
    active_snapshot: Maybe(string)  `json:"active_snapshot"`,
    cpu:             f64            `json:"cpu"`,
    memory_mb:       u64            `json:"memory_mb"`,
    max_lifetime_s:  u64            `json:"max_lifetime_s"`,
    allow_net:       Maybe([]string)`json:"allow_net"`,
}

Exec_Request_Json :: struct {
    cmd:     string        `json:"cmd"`,
    workdir: Maybe(string) `json:"workdir"`,
    timeout: Maybe(u64)    `json:"timeout"`,
}

Exec_Result_Json :: struct {
    seq:       u32    `json:"seq"`,
    cmd:       string `json:"cmd"`,
    workdir:   string `json:"workdir"`,
    exit_code: i32    `json:"exit_code"`,
    started:   string `json:"started"`,
    finished:  string `json:"finished"`,
    stdout:    string `json:"stdout"`,
    stderr:    string `json:"stderr"`,
}

Error_Response :: struct {
    error: string `json:"error"`,
}

// Parse layers field — accept string or array
parse_layers :: proc(val: json.Value) -> []string {
    #partial switch v in val {
    case json.String:
        return strings.split(v, ",")
    case json.Array:
        result := make([]string, len(v))
        for item, i in v {
            if s, ok := item.(json.String); ok {
                result[i] = s
            }
        }
        return result
    }
    return {"000-base-alpine"}
}

// API handler registration
register_routes :: proc() -> []Route {
    return {
        { "GET",    "/cgi-bin/health",                      handle_health },
        { "GET",    "/cgi-bin/api/sandboxes",               handle_list_sandboxes },
        { "POST",   "/cgi-bin/api/sandboxes",               handle_create_sandbox },
        { "GET",    "/cgi-bin/api/sandboxes/*",             handle_get_sandbox },
        { "DELETE", "/cgi-bin/api/sandboxes/*",             handle_destroy_sandbox },
        { "POST",   "/cgi-bin/api/sandboxes/*/exec",        handle_exec },
        { "POST",   "/cgi-bin/api/sandboxes/*/activate",    handle_activate },
        { "POST",   "/cgi-bin/api/sandboxes/*/snapshot",    handle_snapshot },
        { "POST",   "/cgi-bin/api/sandboxes/*/restore",     handle_restore },
        { "GET",    "/cgi-bin/api/sandboxes/*/logs",        handle_get_logs },
        { "GET",    "/cgi-bin/api/modules",                 handle_list_modules },
    }
}
```

**Content-Type enforcement**: POST endpoints check for `Content-Type: application/json`. Return 415 if missing.

### S3 Client (`s3.odin` + `sigv4.odin`)

Same approach as the Zig prompt: implement minimal S3 operations with raw HTTP + AWS SigV4 signing. Use `core:net` for TCP, `core:crypto/hash` for SHA256, `core:crypto/hmac` for HMAC-SHA256.

```odin
import "core:crypto/hash"

S3_Client :: struct {
    bucket:     string,
    endpoint:   Maybe(string),
    region:     string,
    prefix:     string,
    access_key: string,
    secret_key: string,
}

s3_push    :: proc(client: ^S3_Client, local_path, key: string) -> Error { ... }
s3_pull    :: proc(client: ^S3_Client, key, local_path: string) -> Error { ... }
s3_exists  :: proc(client: ^S3_Client, key: string) -> (bool, Error) { ... }
s3_list    :: proc(client: ^S3_Client, prefix: string, allocator := context.allocator) -> ([]string, Error) { ... }
s3_push_bg :: proc(client: ^S3_Client, local_path, key: string) {
    // Spawn detached thread
    thread.create_and_start_with_poly_data2(
        strings.clone(local_path), strings.clone(key),
        proc(path: string, key: string) {
            s3_push(client, path, key)
        },
    )
}
```

**SigV4 signing** (`sigv4.odin`): use `core:crypto/hash.Algorithm.SHA256` and `core:crypto/hmac`. Port the signing logic from the existing shell `sq-s3` script.

**Pull atomicity**: write to `.s3tmp`, then `os.rename`. Use `flock` via foreign import.

### Secrets (`secrets.odin`)

```odin
Secret :: struct {
    placeholder:   string   `json:"placeholder"`,
    value:         string   `json:"value"`,
    allowed_hosts: []string `json:"allowed_hosts"`,
}

Secrets_Config :: struct {
    secrets: map[string]Secret `json:"secrets"`,
}

load_secrets :: proc(path: string, allocator := context.allocator) -> (Secrets_Config, bool) {
    data, ok := os.read_entire_file(path, allocator)
    if !ok do return {}, false

    config: Secrets_Config
    err := json.unmarshal(data, &config, allocator = allocator)
    return config, err == nil
}
```

### Reaper (`reaper.odin`)

```odin
import "core:thread"

reaper_thread :: proc(manager: ^Sandbox_Manager) {
    for {
        time.sleep(10 * time.Second)

        expired: [dynamic]string
        defer delete(expired)

        sync.lock(&manager.global_lock)
        for id, ms in manager.sandboxes {
            sync.lock(&ms.lock)
            max_lifetime := ms.sandbox.max_lifetime_s
            if max_lifetime > 0 {
                age := time.diff(ms.sandbox.created, time.now())
                if time.duration_seconds(age) > f64(max_lifetime) {
                    append(&expired, id)
                }
            }
            sync.unlock(&ms.lock)
        }
        sync.unlock(&manager.global_lock)

        for id in expired {
            fmt.printfln("[reaper] destroying expired sandbox %s", id)
            manager_destroy_sandbox(manager, id)
        }
    }
}
```

### Init / Recovery (`init.odin`)

Matching `bin/sq-init`:

```odin
init_run :: proc(config: ^Config) -> Error {
    os.make_directory(modules_dir(config))
    os.make_directory(sandboxes_dir(config))

    // Check base module
    base_path := fmt.tprintf("%s/000-base-alpine.squashfs", modules_dir(config))
    version_path := fmt.tprintf("%s/000-base-alpine.version", modules_dir(config))

    current_version :: 2
    existing_version := 0
    if data, ok := os.read_entire_file(version_path, context.temp_allocator); ok {
        existing_version = strconv.atoi(string(data))
    }

    if !os.exists(base_path) || existing_version < current_version {
        // Try S3, then build
        if bucket, ok := config.s3_bucket.?; ok {
            // ... try s3_pull
        }
        if !os.exists(base_path) {
            fmt.println("[init] building Alpine base")
            run_cmd("sq-mkbase", "alpine")
        }
    }

    // Firecracker components
    if config.backend == .Firecracker {
        if !os.exists(fmt.tprintf("%s/vm/firecracker", config.data_dir)) {
            run_cmd("sq-mkvm", "all")
        }
    }

    // Remount surviving sandboxes (unless ephemeral)
    if !config.ephemeral {
        remount_surviving_sandboxes(config)
    }

    return nil
}
```

### Snapshot / Restore / Activate (`snapshot.odin`)

**Snapshot** — shell out to `mksquashfs`:

```odin
snapshot_sandbox :: proc(sandbox: ^Sandbox, label: string) -> (string, Error) {
    if !valid_label(label) do return "", .Invalid_Label

    snap_dir := fmt.tprintf("%s/snapshots", sandbox.dir)
    os.make_directory(snap_dir)
    snap_path := fmt.aprintf("%s/%s.squashfs", snap_dir, label)

    if os.exists(snap_path) do return "", .Already_Exists

    upper_data := fmt.tprintf("%s/upper/data", sandbox.dir)

    // Detect compression
    use_zstd := detect_zstd_support()
    comp_args: string
    if use_zstd {
        comp_args = "-comp zstd -Xcompression-level 3 -b 128K"
    } else {
        comp_args = "-comp gzip -b 256K"
    }

    cmd := fmt.tprintf("mksquashfs %s %s %s -noappend -quiet", upper_data, snap_path, comp_args)
    if !run_cmd_str(cmd) do return "", .Mksquashfs_Failed

    return snap_path, nil
}
```

**Restore** (matching common.sh:664-707): unmount overlay, unmount previous snapshot, clear upper, mount new snapshot, remount overlay, re-inject secrets.

**Activate** (matching common.sh:611-632): mount new squashfs, unmount overlay, remount with updated lowerdir, append to .meta/layers.

### Sandbox Destroy

```odin
sandbox_destroy :: proc(sandbox: ^Sandbox) {
    // Reverse creation order
    sandbox_mounts_destroy(&sandbox.mounts)
    if cg, ok := &sandbox.cgroup.?; ok { cgroup_destroy(cg) }
    if ns, ok := &sandbox.netns.?; ok { netns_teardown(ns) }

    // Remove directory tree
    remove_dir_recursive(sandbox.dir)
}
```

### Guest Agent (`guest/main.odin`)

Separate binary. PID 1 in Firecracker VM. Uses vsock via `foreign import` for `AF_VSOCK`.

```odin
package guest_agent

foreign import libc "system:c"
foreign libc {
    c_socket  :: proc(domain, type, protocol: c.int) -> c.int ---
    c_bind    :: proc(fd: c.int, addr: rawptr, len: c.uint) -> c.int ---
    c_listen  :: proc(fd: c.int, backlog: c.int) -> c.int ---
    c_accept  :: proc(fd: c.int, addr: rawptr, len: ^c.uint) -> c.int ---
}

AF_VSOCK :: 40
SOCK_STREAM :: 1
VMADDR_CID_ANY :: 0xFFFFFFFF

Sockaddr_VM :: struct #packed {
    svm_family:  u16,
    svm_reserved1: u16,
    svm_port:    u32,
    svm_cid:     u32,
    svm_flags:   u8,
    svm_zero:    [3]u8,
}

main :: proc() {
    // 1. Mount /proc, /sys, /dev, etc.
    mount_essential_filesystems()

    // 2. Parse kernel cmdline
    layer_count := parse_cmdline_int("squash.layers=") or_else 99

    // 3. Mount squashfs layers from /dev/vd[b-z]
    lower_components: [24]string
    lower_count := 0
    for dev_letter := u8('b'); dev_letter <= 'z' && lower_count < layer_count; dev_letter += 1 {
        dev_path := fmt.tprintf("/dev/vd%c", rune(dev_letter))
        mp := fmt.tprintf("/layers/%d", lower_count)
        os.make_directory(mp)
        m, err := squashfs_mount(dev_path, mp)
        if err == .None {
            lower_components[lower_count] = mp
            lower_count += 1
        }
    }

    // 4. Overlay
    setup_guest_overlay(lower_components[:lower_count])

    // 5. Network
    setup_guest_network()

    // 6. resolv.conf
    os.write_entire_file("/sandbox/merged/etc/resolv.conf", transmute([]byte)string("nameserver 10.0.0.1\n"))

    fmt.printfln("[guest] ready — layers=%d", lower_count)

    // 7. Vsock listener on port 5000
    vsock_listen(5000)
}

vsock_listen :: proc(port: u32) {
    fd := c_socket(AF_VSOCK, SOCK_STREAM, 0)
    assert(fd >= 0)

    addr := Sockaddr_VM{
        svm_family = AF_VSOCK,
        svm_cid    = VMADDR_CID_ANY,
        svm_port   = port,
    }
    c_bind(fd, &addr, size_of(Sockaddr_VM))
    c_listen(fd, 16)

    for {
        conn_fd := c_accept(fd, nil, nil)
        if conn_fd < 0 do continue
        thread.create_and_start_with_poly_data(conn_fd, handle_vsock_connection)
    }
}

handle_vsock_connection :: proc(conn_fd: c.int) {
    defer c_close(conn_fd)

    buf: [4096]byte
    n := c_read(conn_fd, &buf[0], size_of(buf))
    if n <= 0 do return

    req: Exec_Request_Json
    if json.unmarshal(buf[:n], &req) != nil do return

    result := exec_in_guest(req)
    resp_bytes, _ := json.marshal(result, allocator = context.temp_allocator)
    c_write(conn_fd, &resp_bytes[0], c.size_t(len(resp_bytes)))
}
```

---

## Startup Sequence (`main.odin`)

```odin
package squashd

import "core:fmt"
import "core:os"
import "core:thread"

main :: proc() {
    config := config_from_env()

    fmt.println("[squash] v4 (odin)")

    // 1. Init / recovery
    if err := init_run(&config); err != nil {
        fmt.eprintfln("[init] failed: %v", err)
        os.exit(1)
    }

    // 2. Start Go secret proxy as child process
    proxy_pid: Maybe(i32)
    if secrets_exist(&config) {
        if config.proxy_https {
            ensure_proxy_ca(&config)
            proxy_pid = start_go_proxy(&config)
        } else {
            proxy_pid = start_shell_proxy(&config)
        }
    }
    defer if pid, ok := proxy_pid.?; ok { c_kill(pid, 15) }  // SIGTERM

    // 3. Tailscale
    if _, ok := config.tailscale_authkey.?; ok {
        thread.create_and_start_with_poly_data(&config, start_tailscale)
    }

    // 4. Sandbox manager
    manager := manager_init(&config)
    defer manager_destroy_all(&manager)

    // 5. Reaper thread
    thread.create_and_start_with_poly_data(&manager, reaper_thread)

    // 6. HTTP server (blocks)
    mod_count := count_modules(&config)
    sb_count := len(manager.sandboxes)
    fmt.printfln("[squash] ready — modules: %d, sandboxes: %d", mod_count, sb_count)

    routes := register_routes()
    serve(config.port, routes[:], &config, &manager)
}
```

---

## Filesystem Layout

Byte-compatible with the existing shell version. Identical to the Rust and Zig prompts. A running shell-based system can be upgraded to the Odin version without data loss.

```
/data/
├── modules/
│   ├── 000-base-alpine.squashfs
│   ├── 000-base-alpine.version      # "2"
│   └── ...
├── sandboxes/{id}/
│   ├── .meta/ (owner, task, layers, created, last_active, cpu, memory_mb, ...)
│   ├── images/{module}.squashfs/     # mount points
│   ├── upper/ (tmpfs: data/ + work/)
│   ├── merged/                       # overlayfs
│   └── snapshots/{label}.squashfs
├── secrets.json
├── proxy-ca/ (ca.crt, ca.key)
└── .netns-index.lock
```

---

## Dockerfile

```dockerfile
# Build squashd (Odin)
FROM alpine:3.21 AS odin-build
RUN apk add --no-cache curl clang musl-dev llvm18-dev
# Install Odin
RUN curl -L https://github.com/odin-lang/Odin/releases/download/dev-2026-01/odin-linux-amd64-dev-2026-01.tar.gz \
    | tar xz -C /opt
ENV PATH="/opt/odin-linux-amd64-dev-2026-01:$PATH"
WORKDIR /build
COPY src/ src/
COPY guest/ guest/
RUN odin build src/ -target:linux_amd64 -o:speed -out:squashd -extra-linker-flags:"-static"
RUN odin build guest/ -target:linux_amd64 -o:size -out:sq-guest-agent -extra-linker-flags:"-static"

# Build Go proxy (keep existing)
FROM golang:1.22-alpine AS proxy-build
COPY proxy/ /build/
RUN cd /build && CGO_ENABLED=0 go build -ldflags='-s -w' -o /sq-secret-proxy-https .

FROM alpine:3.21
RUN apk add --no-cache \
    jq squashfs-tools util-linux curl wget coreutils \
    socat openssl busybox-extras iproute2 iptables \
    && apk add --no-cache aws-cli 2>/dev/null || true \
    && apk add --no-cache tailscale 2>/dev/null || true

COPY --from=odin-build /build/squashd /app/bin/squashd
COPY --from=odin-build /build/sq-guest-agent /app/bin/sq-guest-agent
COPY --from=proxy-build /sq-secret-proxy-https /app/bin/sq-secret-proxy-https
COPY bin/sq-mkbase bin/sq-mkmod bin/sq-mkvm bin/sq-ctl bin/sq-secret-proxy bin/setup-tailscale /app/bin/
COPY static/ /app/static/

RUN chmod +x /app/bin/*

VOLUME /data
EXPOSE 8080

ENV SQUASH_DATA=/data \
    SQUASH_PORT=8080 \
    SQUASH_BACKEND=chroot \
    SQUASH_UPPER_LIMIT_MB=512 \
    SQUASH_MAX_SANDBOXES=100

WORKDIR /app
ENTRYPOINT ["/app/bin/squashd"]
```

---

## Key Differences from Other Prompts

| Concern | Rust | Zig | Odin |
|---|---|---|---|
| Cleanup | `Drop` (automatic) | `errdefer` + manual `deinit()` | `defer` + manual destroy |
| Error handling | `Result<T,E>` + `?` | Error unions + `try` | Multiple returns + `or_return` |
| Syscalls | `nix` crate | `@cImport` headers | `core:sys/linux` + `foreign import` |
| HTTP server | `axum` (async, production) | `std.http.Server` or libmicrohttpd | Hand-rolled on `core:net` (~300 lines) |
| JSON | `serde_json` (derive) | `std.json` | `core:encoding/json` (struct tags) |
| Memory model | Ownership + borrowing | Manual + arena | **Arena per sandbox via context** |
| Async | `tokio` | Thread pool | Thread pool |
| MITM proxy | Rewrite in Rust | Keep Go sidecar | Keep Go sidecar |
| Binary size | 3-5MB | 500KB-1.5MB | **1-3MB** |
| Build time | 30-60s | 3-5s | **2-5s** |
| Maturity | Post-1.0, huge ecosystem | Pre-1.0, thin ecosystem | Pre-1.0, growing ecosystem |
| Production use | Ubiquitous | Growing | JangaFX (EmberGen) |

**Odin's unique advantage: the context system.** The implicit `context` that carries the allocator through every call means arena-per-sandbox works without threading allocators through every function signature. In Zig you'd pass the allocator explicitly. In Rust you'd use `Arc<Allocator>` or global allocator overrides. In Odin, you set `context.allocator = arena_allocator(&sandbox.arena)` and every allocation in scope uses it automatically — including `core:encoding/json`, `core:fmt`, and any library code.

---

## Testing Strategy

### Unit tests (using Odin's test runner)

```odin
@(test)
test_valid_id :: proc(t: ^testing.T) {
    testing.expect(t, valid_id("my-sandbox-123"))
    testing.expect(t, !valid_id("../../etc"))
    testing.expect(t, !valid_id(""))
    testing.expect(t, !valid_id("has space"))
}

@(test)
test_sigv4_signing :: proc(t: ^testing.T) {
    // Test vector from AWS documentation
    // ...
}
```

Run with `odin test src/`.

### Integration tests

Keep `bin/sq-test` as the integration test suite — it's a shell script hitting the HTTP API, so it's language-agnostic.

Additional tests:
1. **Cleanup after kill**: start squashd, create sandbox, `kill -9 squashd`, restart, verify no orphaned mounts/iptables/netns
2. **Concurrent operations**: parallel exec on same sandbox serializes
3. **Arena leak detection**: in debug builds, track arena sizes and verify they're freed on destroy

---

## Implementation Order

1. **Phase 1: Config + Validation + HTTP skeleton** — Hand-rolled HTTP server with health endpoint.
2. **Phase 2: Mounts** — Squashfs, Tmpfs, Overlay with `defer` cleanup. Test in privileged container.
3. **Phase 3: Sandbox create + destroy + exec** — Full lifecycle minus netns/cgroup.
4. **Phase 4: Cgroups + Network namespaces** — Shell out to `ip`/`iptables`.
5. **Phase 5: Snapshot + Restore + Activate** — mksquashfs, overlay remount.
6. **Phase 6: S3 sync** — SigV4 signing, HTTP client.
7. **Phase 7: Go proxy integration** — Child process management.
8. **Phase 8: Init/recovery + Reaper** — Startup, background thread, ephemeral mode.
9. **Phase 9: Wire compatibility** — Run `sq-test` against Odin binary.

---

## Discipline Checklist

Same as Zig — no automatic destructors means explicit discipline:

- [ ] Every type with kernel state has a matching `_destroy`/`_unmount`/`_teardown` proc
- [ ] Every such proc is idempotent (safe to call twice)
- [ ] `manager_destroy_sandbox` ALWAYS calls `sandbox_destroy` before map removal
- [ ] `main()` has `defer manager_destroy_all(&manager)`
- [ ] Reaper calls `manager_destroy_sandbox`, not just map removal
- [ ] Every test creating mounts has `defer` cleanup
- [ ] Arena allocator set in context before sandbox operations
- [ ] `defer if err != nil` used on every resource acquisition in multi-step creation

---

## Non-Goals

Same as all other prompts:
- Rewriting shell setup scripts
- Implementing Firecracker backend (stub it)
- WebSocket streaming for exec
- Multi-node distribution
- Rewriting the Go MITM proxy
