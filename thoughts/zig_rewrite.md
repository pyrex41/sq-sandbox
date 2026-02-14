# sq-sandbox Zig Rewrite — Complete Implementation Prompt

## Project Summary

Rewrite the `sq-sandbox` container runtime from shell scripts into **two Zig binaries** plus a **Go sidecar** for the MITM proxy:

1. **`squashd`** — the main daemon (~500KB-1.5MB static binary). Replaces all of `cgi-bin/`, most of `bin/`. This is the HTTP API server, sandbox lifecycle manager, reaper, S3 sync engine, and init/recovery system. Does NOT include the MITM proxy.

2. **`sq-guest-agent`** — the Firecracker guest agent (~100-300KB). Replaces `vm/init` and `vm/sq-vsock-handler`. Runs as PID 1 inside a microVM.

3. **`sq-secret-proxy-https`** — the existing Go binary (`proxy/main.go`), kept as-is. Managed as a child process by `squashd`. The Go MITM proxy is 526 lines of working, tested code that handles dynamic TLS cert generation, connection hijacking, and HTTP/2 edge cases. Rewriting it in Zig would require binding to a C TLS library, writing ASN.1/X.509 cert generation, and reimplementing Go's `net/http` CONNECT handling — roughly 2000+ lines of Zig for equivalent functionality, with more bug surface area. Keep it.

The **module-building scripts** (`sq-mkbase`, `sq-mkmod`, `sq-mkvm`) and the **CLI** (`sq-ctl`) remain as shell scripts. They're setup/admin tools, not hot path.

The existing repo is at https://github.com/pyrex41/sq-sandbox. Read every file before starting.

---

## Why Zig for This

The core value proposition: this binary is 90% Linux syscall sequencing. Zig lets you `@cImport` the kernel headers directly and call `mount()`, `unshare()`, `clone3()`, `chroot()` as the kernel authors intended — no wrapper crate, no binding layer, no version mismatch. The `errdefer` primitive is purpose-built for the "set up 6 things in order, roll back if step 4 fails" pattern that dominates sandbox lifecycle management.

The binary will be ~500KB-1.5MB static. The Firecracker guest agent will be ~100-300KB. For comparison, Rust produces 3-5MB, Go produces 8-12MB. Inside a Firecracker guest rootfs where the entire image should be a few MB, this matters.

---

## Architecture Principles

### 1. Explicit lifecycle with errdefer

Zig has no destructors. Every resource has an explicit `deinit()` method. The `errdefer` keyword ensures cleanup on the creation path (if a later step fails, earlier steps are rolled back). The destruction path requires explicit `deinit()` calls — there is no safety net if you forget.

This is the fundamental tradeoff vs Rust's `Drop`. The creation path is cleaner in Zig. The destruction path requires more discipline.

**Mitigation**: every type that manages kernel state has a `deinit()` that is idempotent (safe to call twice). The `Sandbox` struct has a single `destroy()` method that calls `deinit()` on all sub-resources in reverse order. The `SandboxManager` ensures `destroy()` is always called when removing a sandbox — in normal destruction, in reaper expiry, and in a `defer` block in `main()` that runs on process exit.

### 2. Direct C header imports for syscalls

```zig
const c = @cImport({
    @cInclude("sys/mount.h");
    @cInclude("sched.h");
    @cInclude("linux/sched.h");
    @cInclude("unistd.h");
    @cInclude("sys/wait.h");
    @cInclude("linux/netlink.h");
    @cInclude("linux/rtnetlink.h");
    @cInclude("linux/if.h");
    @cInclude("linux/veth.h");
    @cInclude("sys/socket.h");
    @cInclude("linux/vm_sockets.h");  // vsock for guest agent
});
```

No wrapper library. The types and constants come directly from the kernel headers. When you read `man 2 mount`, the code you write matches the documentation.

### 3. Per-sandbox mutex, arena allocator per sandbox

```zig
const SandboxManager = struct {
    sandboxes: std.StringHashMap(*ManagedSandbox),
    global_lock: std.Thread.Mutex,  // protects the hashmap itself
    config: *const Config,
    allocator: std.mem.Allocator,

    const ManagedSandbox = struct {
        sandbox: Sandbox,
        lock: std.Thread.Mutex,  // protects this sandbox's operations
    };
};
```

Operations on different sandboxes are concurrent. Operations on the same sandbox are serialized by the per-sandbox mutex.

Each sandbox gets its own `ArenaAllocator` backed by the page allocator. When the sandbox is destroyed, the entire arena is freed at once — no individual `free()` calls, no possibility of memory leaks within a sandbox's lifetime.

### 4. Blocking I/O with thread pool

Zig's standard library does not have an async runtime like tokio. Use a thread-pool model:

- The HTTP server runs on the main thread, accepting connections and dispatching to a thread pool.
- Each API request runs on a pool thread, holding the per-sandbox mutex for the duration of the operation.
- The reaper runs on a dedicated thread in a `sleep(10)` loop.
- Background S3 pushes are dispatched to the pool.
- The Go proxy runs as a child process.

Use `std.http.Server` for the HTTP server. It's basic but sufficient — this API has 10 endpoints with JSON request/response. No WebSocket, no streaming, no HTTP/2.

Alternatively, if `std.http.Server` proves too limited (it has had stability issues across Zig versions), bind to `libmicrohttpd` via `@cImport` — it's a small, stable, well-tested C HTTP server library that handles connection management, threading, and HTTP parsing. ~50KB added to the binary.

---

## Build Configuration

**build.zig:**
```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{
        .default_target = .{
            .cpu_arch = .x86_64,
            .os_tag = .linux,
            .abi = .musl,
        },
    });
    const optimize = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseSafe,  // keep safety checks, still fast
    });

    // Main daemon
    const squashd = b.addExecutable(.{
        .name = "squashd",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    squashd.linkLibC();  // needed for @cImport of Linux headers

    // Guest agent
    const guest = b.addExecutable(.{
        .name = "sq-guest-agent",
        .root_source_file = b.path("guest/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    guest.linkLibC();

    b.installArtifact(squashd);
    b.installArtifact(guest);

    // Tests
    const tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    tests.linkLibC();
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_tests.step);
}
```

Build: `zig build -Doptimize=ReleaseSafe`

This produces a fully static musl binary. No runtime dependencies. No dynamic linker.

---

## Module Structure

```
src/
├── main.zig                # squashd entrypoint, startup sequence
├── config.zig              # Config struct from env vars
├── validate.zig            # Input validation (IDs, labels, module names)
├── api.zig                 # HTTP server, routing, JSON request/response
├── sandbox.zig             # Sandbox struct, state enum, create/destroy
├── manager.zig             # SandboxManager (hashmap + per-sandbox locks)
├── mounts.zig              # SquashfsMount, TmpfsMount, OverlayMount
├── exec.zig                # fork/unshare/chroot/exec logic
├── netns.zig               # Network namespace, veth, iptables
├── cgroup.zig              # cgroups v2
├── snapshot.zig            # snapshot (mksquashfs) and restore
├── meta.zig                # Metadata read/write (filesystem-backed)
├── modules.zig             # Module registry
├── s3.zig                  # S3 sync (HTTP + SigV4 signing)
├── reaper.zig              # Background thread: destroy expired sandboxes
├── init.zig                # First-boot: build base, remount survivors
├── secrets.zig             # secrets.json loading, placeholder injection
├── json.zig                # JSON helpers (serialize sandbox info, exec results)
├── netlink.zig             # Raw netlink socket operations (veth, addr, route)
└── sigv4.zig               # AWS Signature V4 signing for S3

guest/
├── main.zig                # sq-guest-agent: PID 1, mount layers
├── vsock.zig               # vsock listener
└── overlay.zig             # guest-side overlayfs setup
```

---

## Detailed Implementation Specifications

### Config (`config.zig`)

```zig
pub const Backend = enum { chroot, firecracker };

pub const Config = struct {
    backend: Backend = .chroot,
    data_dir: []const u8 = "/data",
    port: u16 = 8080,
    auth_token: ?[]const u8 = null,
    s3_bucket: ?[]const u8 = null,
    s3_endpoint: ?[]const u8 = null,
    s3_region: []const u8 = "us-east-1",
    s3_prefix: []const u8 = "",
    ephemeral: bool = false,
    upper_limit_mb: u64 = 512,
    max_sandboxes: usize = 100,
    proxy_https: bool = false,
    tailscale_authkey: ?[]const u8 = null,
    tailscale_hostname: []const u8 = "squash",

    pub fn modulesDir(self: *const Config) [*:0]const u8 { ... }
    pub fn sandboxesDir(self: *const Config) [*:0]const u8 { ... }
    // etc.

    pub fn fromEnv(allocator: std.mem.Allocator) !Config {
        var cfg = Config{};
        if (std.posix.getenv("SQUASH_BACKEND")) |v| {
            cfg.backend = if (std.mem.eql(u8, v, "firecracker")) .firecracker else .chroot;
        }
        if (std.posix.getenv("SQUASH_DATA")) |v| cfg.data_dir = v;
        if (std.posix.getenv("SQUASH_PORT")) |v| cfg.port = std.fmt.parseInt(u16, v, 10) catch 8080;
        if (std.posix.getenv("SQUASH_AUTH_TOKEN")) |v| {
            cfg.auth_token = if (v.len > 0) v else null;
        }
        // ... same pattern for all env vars
        if (std.posix.getenv("SQUASH_EPHEMERAL")) |v| cfg.ephemeral = std.mem.eql(u8, v, "1");
        if (std.posix.getenv("SQUASH_PROXY_HTTPS")) |v| cfg.proxy_https = std.mem.eql(u8, v, "1");
        return cfg;
    }
};
```

### Input Validation (`validate.zig`)

```zig
/// Sandbox IDs: [a-zA-Z0-9_-]+
pub fn validId(s: []const u8) bool {
    if (s.len == 0) return false;
    for (s) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '-') return false;
    }
    return true;
}

/// Snapshot labels and module names: [a-zA-Z0-9_.-]+
pub fn validLabel(s: []const u8) bool {
    if (s.len == 0) return false;
    for (s) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '.' and ch != '-') return false;
    }
    return true;
}

pub const validModule = validLabel;
```

### Mount Operations (`mounts.zig`)

This is the core of the system. Every mount operation returns a handle. Every handle has a `deinit()` that unmounts. The `errdefer` pattern ensures rollback on partial failure.

```zig
const c = @cImport({
    @cInclude("sys/mount.h");
});

pub const SquashfsMount = struct {
    mount_point: [*:0]const u8,
    active: bool,
    allocator: std.mem.Allocator,

    pub fn mount(
        allocator: std.mem.Allocator,
        squashfs_path: [*:0]const u8,
        mount_point: [*:0]const u8,
    ) !SquashfsMount {
        // Create mount point directory
        try std.fs.makeDirAbsolute(std.mem.span(mount_point));

        const rc = c.mount(squashfs_path, mount_point, "squashfs", c.MS_RDONLY, null);
        if (rc != 0) return error.MountFailed;

        return .{
            .mount_point = mount_point,
            .active = true,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SquashfsMount) void {
        if (self.active) {
            // MNT_DETACH = 2
            _ = c.umount2(self.mount_point, 2);
            self.active = false;
        }
    }
};

pub const TmpfsMount = struct {
    mount_point: [*:0]const u8,
    active: bool,

    pub fn mount(mount_point: [*:0]const u8, size_mb: u64) !TmpfsMount {
        try std.fs.makeDirAbsolute(std.mem.span(mount_point));

        var opts_buf: [64]u8 = undefined;
        const opts = std.fmt.bufPrintZ(&opts_buf, "size={d}m", .{size_mb}) catch return error.FormatFailed;

        const rc = c.mount("tmpfs", mount_point, "tmpfs", 0, opts.ptr);
        if (rc != 0) return error.MountFailed;

        // Create overlay subdirectories
        const mp_span = std.mem.span(mount_point);
        var data_buf: [512]u8 = undefined;
        const data_path = std.fmt.bufPrint(&data_buf, "{s}/data", .{mp_span}) catch return error.FormatFailed;
        try std.fs.makeDirAbsolute(data_path);

        var work_buf: [512]u8 = undefined;
        const work_path = std.fmt.bufPrint(&work_buf, "{s}/work", .{mp_span}) catch return error.FormatFailed;
        try std.fs.makeDirAbsolute(work_path);

        return .{ .mount_point = mount_point, .active = true };
    }

    pub fn deinit(self: *TmpfsMount) void {
        if (self.active) {
            _ = c.umount2(self.mount_point, 2); // MNT_DETACH
            self.active = false;
        }
    }
};

pub const OverlayMount = struct {
    merged_path: [*:0]const u8,
    active: bool,

    /// lower_components: ordered highest-priority first.
    /// Snapshot first, then modules in descending numeric order.
    pub fn mount(
        lower_components: []const []const u8,
        upper_data: [*:0]const u8,
        work: [*:0]const u8,
        merged: [*:0]const u8,
    ) !OverlayMount {
        try std.fs.makeDirAbsolute(std.mem.span(merged));

        // Build options string: "lowerdir=A:B:C,upperdir=X,workdir=Y"
        var opts_buf: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&opts_buf);
        const writer = stream.writer();
        try writer.writeAll("lowerdir=");
        for (lower_components, 0..) |comp, i| {
            if (i > 0) try writer.writeByte(':');
            try writer.writeAll(comp);
        }
        try writer.print(",upperdir={s},workdir={s}", .{
            std.mem.span(upper_data), std.mem.span(work),
        });
        try writer.writeByte(0); // null terminator

        const opts_z: [*:0]const u8 = @ptrCast(opts_buf[0..stream.pos]);

        const rc = c.mount("overlay", merged, "overlay", 0, opts_z);
        if (rc != 0) return error.MountFailed;

        return .{ .merged_path = merged, .active = true };
    }

    pub fn deinit(self: *OverlayMount) void {
        if (self.active) {
            _ = c.umount2(self.merged_path, 2);
            self.active = false;
        }
    }
};
```

**The sandbox creation errdefer chain** — this is where Zig shines:

```zig
pub fn createSandbox(
    allocator: std.mem.Allocator,
    config: *const Config,
    id: []const u8,
    layers: []const []const u8,
    opts: SandboxOptions,
) !Sandbox {
    // 1. Mount tmpfs for writable upper
    var tmpfs = try TmpfsMount.mount(upper_path_z, config.upper_limit_mb);
    errdefer tmpfs.deinit();  // rolls back if anything below fails

    // 2. Loop-mount each squashfs layer
    var squashfs_mounts = std.ArrayList(SquashfsMount).init(allocator);
    errdefer {
        // Roll back in reverse order
        var i = squashfs_mounts.items.len;
        while (i > 0) {
            i -= 1;
            squashfs_mounts.items[i].deinit();
        }
        squashfs_mounts.deinit();
    }
    for (layers) |layer| {
        const sqfs = try SquashfsMount.mount(allocator, mod_path, mp_path);
        try squashfs_mounts.append(sqfs);
    }

    // 3. Mount overlay
    var overlay = try OverlayMount.mount(lower_components, upper_data_z, work_z, merged_z);
    errdefer overlay.deinit();

    // 4. Set up cgroup (may fail, that's okay)
    var cgroup: ?CgroupHandle = CgroupHandle.create(id, opts.cpu, opts.memory_mb) catch null;
    errdefer if (cgroup) |*cg| cg.deinit();

    // 5. Set up network namespace
    var netns = try NetnsHandle.setup(allocator, config, id, opts.allow_net);
    errdefer netns.deinit();

    // 6. Seed resolv.conf, inject secrets
    try seedResolvConf(config, id, netns.index);
    try injectSecretPlaceholders(config, id, netns.index);

    // 7. Write metadata
    try writeMeta(config, id, layers, opts);

    // If we got here, everything succeeded. Return the sandbox.
    // None of the errdefers fire.
    return Sandbox{
        .id = id,
        .tmpfs = tmpfs,
        .squashfs_mounts = squashfs_mounts,
        .overlay = overlay,
        .cgroup = cgroup,
        .netns = netns,
        .allocator = allocator,
    };
}
```

If step 5 (netns setup) fails, the errdefers fire in reverse: cgroup deinit, overlay unmount, squashfs unmounts (reverse), tmpfs unmount. Exactly the right behavior, expressed in 6 `errdefer` lines rather than a manual cleanup function.

**The destruction path** — this is where discipline is needed:

```zig
pub const Sandbox = struct {
    // ... fields ...

    /// Explicit destruction. Must be called. No safety net.
    pub fn destroy(self: *Sandbox) void {
        // Reverse order of creation
        // 1. Unmount overlay (must be first — it references lower layers)
        self.overlay.deinit();

        // 2. Unmount snapshot if any
        if (self.snapshot_mount) |*snap| snap.deinit();

        // 3. Unmount squashfs layers (reverse order)
        var i = self.squashfs_mounts.items.len;
        while (i > 0) {
            i -= 1;
            self.squashfs_mounts.items[i].deinit();
        }
        self.squashfs_mounts.deinit();

        // 4. Unmount tmpfs
        self.tmpfs.deinit();

        // 5. Teardown cgroup
        if (self.cgroup) |*cg| cg.deinit();

        // 6. Teardown network namespace + iptables
        self.netns.deinit();

        // 7. Remove directory tree
        std.fs.deleteTreeAbsolute(self.sandbox_dir) catch |err| {
            log.warn("failed to remove sandbox dir {s}: {}", .{ self.sandbox_dir, err });
        };
    }
};
```

**Critical rule**: `SandboxManager.removeSandbox()` ALWAYS calls `sandbox.destroy()` before removing from the hashmap. The reaper calls `removeSandbox()`. The API destroy handler calls `removeSandbox()`. The shutdown hook iterates all sandboxes and calls `destroy()`. There is no code path that removes a sandbox without `destroy()`.

### Netlink Operations (`netlink.zig`)

This is the file you have to write from scratch. No library. Use raw netlink sockets with the kernel header structs imported via `@cImport`.

The operations needed (matching common.sh:327-454):

1. **Create network namespace**: `unshare(CLONE_NEWNET)` in a child process, bind-mount `/proc/{pid}/ns/net` to `/var/run/netns/squash-{id}`. Or use `ip netns add` via `Command` — pragmatically equivalent and less error-prone.

2. **Create veth pair**: Netlink `RTM_NEWLINK` message with `IFLA_INFO_KIND=veth` and `IFLA_INFO_DATA` containing peer info.

3. **Move veth to netns**: Netlink `RTM_NEWLINK` with `IFLA_NET_NS_NAME`.

4. **Add IP address**: Netlink `RTM_NEWADDR` message.

5. **Set link up**: Netlink `RTM_NEWLINK` with `IFF_UP` flag.

6. **Add route**: Netlink `RTM_NEWROUTE` message.

**Pragmatic alternative**: shell out to `ip` for all of these. The performance cost of 7-8 `fork/exec` calls during sandbox creation is negligible (~10-20ms total). The correctness benefit of not writing raw netlink is significant. The existing shell code does exactly this.

**Recommendation: shell out to `ip` and `iptables`.** The netlink approach is more "correct" in a Zig purist sense, but the `ip` command is part of `iproute2` which is always present in the container (it's in the Dockerfile). The binary size savings of avoiding netlink code (~300-400 lines) is irrelevant because the `ip` binary is already in the container image anyway.

```zig
pub const NetnsHandle = struct {
    name: []const u8,       // "squash-{id}"
    index: u8,              // 1-254
    veth_host: []const u8,  // "sq-{id}-h"
    chain_name: ?[]const u8, // "squash-{id}" if egress rules
    host_dns: ?[]const u8,
    allocator: std.mem.Allocator,

    pub fn setup(
        allocator: std.mem.Allocator,
        config: *const Config,
        id: []const u8,
        allow_net: ?[]const []const u8,
    ) !NetnsHandle {
        const index = try allocateNetnsIndex(config);
        errdefer releaseNetnsIndex(config, index);

        const name = try std.fmt.allocPrint(allocator, "squash-{s}", .{id});
        errdefer allocator.free(name);

        // ip netns add squash-{id}
        try runCommand(allocator, &.{ "ip", "netns", "add", name });
        errdefer _ = runCommand(allocator, &.{ "ip", "netns", "delete", name }) catch {};

        const veth_host = try std.fmt.allocPrint(allocator, "sq-{s}-h", .{id});
        const veth_sandbox = try std.fmt.allocPrint(allocator, "sq-{s}-s", .{id});

        // ip link add sq-{id}-h type veth peer name sq-{id}-s
        try runCommand(allocator, &.{ "ip", "link", "add", veth_host, "type", "veth", "peer", "name", veth_sandbox });
        errdefer _ = runCommand(allocator, &.{ "ip", "link", "delete", veth_host }) catch {};

        // ip link set sq-{id}-s netns squash-{id}
        try runCommand(allocator, &.{ "ip", "link", "set", veth_sandbox, "netns", name });

        // Configure host side
        const host_addr = try std.fmt.allocPrint(allocator, "10.200.{d}.1/30", .{index});
        try runCommand(allocator, &.{ "ip", "addr", "add", host_addr, "dev", veth_host });
        try runCommand(allocator, &.{ "ip", "link", "set", veth_host, "up" });

        // Configure sandbox side (inside netns)
        const sandbox_addr = try std.fmt.allocPrint(allocator, "10.200.{d}.2/30", .{index});
        const gateway = try std.fmt.allocPrint(allocator, "10.200.{d}.1", .{index});
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "addr", "add", sandbox_addr, "dev", veth_sandbox });
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "link", "set", veth_sandbox, "up" });
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "link", "set", "lo", "up" });
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "route", "add", "default", "via", gateway });

        // Enable IP forwarding
        writeFile("/proc/sys/net/ipv4/ip_forward", "1") catch {};

        // NAT
        const subnet = try std.fmt.allocPrint(allocator, "10.200.{d}.0/30", .{index});
        try runCommand(allocator, &.{ "iptables", "-t", "nat", "-A", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE" });

        // DNS forwarding
        const host_dns = parseFirstNameserver("/etc/resolv.conf");
        if (host_dns) |dns| {
            try runCommand(allocator, &.{
                "iptables", "-t", "nat", "-A", "PREROUTING",
                "-s", subnet, "-d", gateway, "-p", "udp", "--dport", "53",
                "-j", "DNAT", "--to-destination", dns,
            });
            try runCommand(allocator, &.{
                "iptables", "-t", "nat", "-A", "PREROUTING",
                "-s", subnet, "-d", gateway, "-p", "tcp", "--dport", "53",
                "-j", "DNAT", "--to-destination", dns,
            });
        }

        // Egress filtering
        var chain_name: ?[]const u8 = null;
        if (allow_net) |nets| {
            if (nets.len > 0) {
                chain_name = try applyEgressRules(allocator, id, veth_host, nets);
            }
        }

        return .{
            .name = name,
            .index = index,
            .veth_host = veth_host,
            .chain_name = chain_name,
            .host_dns = host_dns,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *NetnsHandle) void {
        // Reverse order. Ignore all errors (matching shell || true pattern)
        if (self.chain_name) |chain| {
            _ = runCommand(self.allocator, &.{ "iptables", "-D", "FORWARD", "-i", self.veth_host, "-j", chain }) catch {};
            _ = runCommand(self.allocator, &.{ "iptables", "-F", chain }) catch {};
            _ = runCommand(self.allocator, &.{ "iptables", "-X", chain }) catch {};
        }
        const subnet = std.fmt.allocPrint(self.allocator, "10.200.{d}.0/30", .{self.index}) catch return;
        defer self.allocator.free(subnet);
        _ = runCommand(self.allocator, &.{ "iptables", "-t", "nat", "-D", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE" }) catch {};
        // DNS DNAT cleanup...
        _ = runCommand(self.allocator, &.{ "ip", "link", "delete", self.veth_host }) catch {};
        _ = runCommand(self.allocator, &.{ "ip", "netns", "delete", self.name }) catch {};
    }
};
```

**Egress filtering** (matching common.sh:385-422): use `iptables` commands. Create chain, add ICMP drop, DNS rate limit, ESTABLISHED/RELATED accept, resolve each allowed host with `getaddrinfo`, add accept rules, default drop.

**Index allocation** (matching common.sh:309-325): use `flock` on `{data_dir}/.netns-index.lock`. Scan `.meta/netns_index` files. Find first unused index 1-254.

### Cgroups v2 (`cgroup.zig`)

```zig
pub const CgroupHandle = struct {
    path_buf: [256]u8,
    path_len: usize,

    pub fn create(id: []const u8, cpu: f64, memory_mb: u64) !CgroupHandle {
        var handle = CgroupHandle{ .path_buf = undefined, .path_len = 0 };
        const path = std.fmt.bufPrint(&handle.path_buf, "/sys/fs/cgroup/squash-{s}", .{id})
            catch return error.PathTooLong;
        handle.path_len = path.len;

        std.fs.makeDirAbsolute(path) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        // CPU: quota = cpu * 100000, period = 100000
        const quota: u64 = @intFromFloat(cpu * 100_000.0);
        var cpu_buf: [64]u8 = undefined;
        const cpu_str = std.fmt.bufPrint(&cpu_buf, "{d} 100000", .{quota}) catch unreachable;
        try writeFileInDir(path, "cpu.max", cpu_str);

        // Memory
        var mem_buf: [64]u8 = undefined;
        const mem_str = std.fmt.bufPrint(&mem_buf, "{d}", .{memory_mb * 1024 * 1024}) catch unreachable;
        try writeFileInDir(path, "memory.max", mem_str);

        return handle;
    }

    pub fn addProcess(self: *const CgroupHandle, pid: std.posix.pid_t) !void {
        var pid_buf: [16]u8 = undefined;
        const pid_str = std.fmt.bufPrint(&pid_buf, "{d}", .{pid}) catch unreachable;
        try writeFileInDir(self.path(), "cgroup.procs", pid_str);
    }

    fn path(self: *const CgroupHandle) []const u8 {
        return self.path_buf[0..self.path_len];
    }

    pub fn deinit(self: *CgroupHandle) void {
        // Move processes out, then rmdir
        const procs = readFileAlloc(self.path(), "cgroup.procs") catch return;
        defer std.heap.page_allocator.free(procs);
        writeFileInDir("/sys/fs/cgroup", "cgroup.procs", procs) catch {};
        std.fs.deleteDirAbsolute(self.path()) catch {};
    }
};
```

### Sandbox Execution (`exec.zig`)

Replaces common.sh:546-607. This is the most security-sensitive code.

```zig
pub const ExecRequest = struct {
    cmd: []const u8,
    workdir: []const u8 = "/",
    timeout: u64 = 300,  // seconds
};

pub const ExecResult = struct {
    exit_code: i32,
    stdout: []const u8,  // capped at 64KB
    stderr: []const u8,  // capped at 64KB
    started: i64,        // unix timestamp
    finished: i64,
    seq: u32,
};
```

**Execution flow:**

```zig
pub fn execInSandbox(
    allocator: std.mem.Allocator,
    sandbox: *Sandbox,
    req: ExecRequest,
) !ExecResult {
    const started = std.time.timestamp();

    // Create pipes for stdout/stderr
    const stdout_pipe = try std.posix.pipe();
    defer std.posix.close(stdout_pipe[0]);
    const stderr_pipe = try std.posix.pipe();
    defer std.posix.close(stderr_pipe[0]);

    // Open netns fd for setns (if netns exists)
    const netns_fd = if (sandbox.netns.name.len > 0) blk: {
        var ns_path_buf: [256]u8 = undefined;
        const ns_path = std.fmt.bufPrint(&ns_path_buf, "/var/run/netns/{s}", .{sandbox.netns.name})
            catch return error.PathTooLong;
        break :blk try std.posix.open(ns_path, .{ .ACCMODE = .RDONLY }, 0);
    } else null;
    defer if (netns_fd) |fd| std.posix.close(fd);

    const pid = try std.posix.fork();

    if (pid == 0) {
        // ═══ CHILD ═══

        // Close read ends of pipes
        std.posix.close(stdout_pipe[0]);
        std.posix.close(stderr_pipe[0]);

        // Redirect stdout/stderr to pipes
        std.posix.dup2(stdout_pipe[1], std.posix.STDOUT_FILENO) catch std.posix.exit(126);
        std.posix.dup2(stderr_pipe[1], std.posix.STDERR_FILENO) catch std.posix.exit(126);

        // Enter cgroup
        if (sandbox.cgroup) |*cg| {
            cg.addProcess(std.posix.getpid()) catch {};
        }

        // Enter network namespace
        if (netns_fd) |fd| {
            // setns(fd, CLONE_NEWNET)
            const rc = c.setns(fd, c.CLONE_NEWNET);
            if (rc != 0) std.posix.exit(126);
        }

        // Create new mount, PID, IPC, UTS namespaces
        const ns_flags = c.CLONE_NEWNS | c.CLONE_NEWPID | c.CLONE_NEWIPC | c.CLONE_NEWUTS;
        const rc = c.unshare(ns_flags);
        if (rc != 0) std.posix.exit(126);

        // chroot into the merged overlayfs
        std.posix.chroot(sandbox.merged_path) catch std.posix.exit(126);
        std.posix.chdir(req.workdir) catch {};

        // exec /bin/sh -c "{cmd}"
        const argv = [_:null]?[*:0]const u8{
            "/bin/sh",
            "-c",
            req.cmd_z,  // must be null-terminated
            null,
        };
        const envp = [_:null]?[*:0]const u8{null};
        _ = std.posix.execveZ("/bin/sh", &argv, &envp);
        std.posix.exit(127);
    }

    // ═══ PARENT ═══

    // Close write ends of pipes
    std.posix.close(stdout_pipe[1]);
    std.posix.close(stderr_pipe[1]);

    // Read stdout/stderr with timeout
    // Use poll() with timeout on both pipe fds
    var stdout_buf: [65536]u8 = undefined;
    var stderr_buf: [65536]u8 = undefined;
    var stdout_len: usize = 0;
    var stderr_len: usize = 0;

    const deadline = std.time.milliTimestamp() + @as(i64, @intCast(req.timeout)) * 1000;

    // Poll loop reading from both pipes until both are closed or timeout
    var stdout_eof = false;
    var stderr_eof = false;
    while (!stdout_eof or !stderr_eof) {
        const remaining_ms = deadline - std.time.milliTimestamp();
        if (remaining_ms <= 0) {
            // Timeout — kill child
            std.posix.kill(pid, std.posix.SIG.KILL) catch {};
            break;
        }

        var fds: [2]std.posix.pollfd = .{
            .{ .fd = stdout_pipe[0], .events = std.posix.POLL.IN, .revents = 0 },
            .{ .fd = stderr_pipe[0], .events = std.posix.POLL.IN, .revents = 0 },
        };

        _ = std.posix.poll(&fds, @intCast(@min(remaining_ms, std.math.maxInt(i32)))) catch break;

        if (fds[0].revents & std.posix.POLL.IN != 0) {
            const n = std.posix.read(stdout_pipe[0], stdout_buf[stdout_len..]) catch 0;
            if (n == 0) stdout_eof = true else stdout_len += n;
        }
        if (fds[0].revents & std.posix.POLL.HUP != 0) stdout_eof = true;

        if (fds[1].revents & std.posix.POLL.IN != 0) {
            const n = std.posix.read(stderr_pipe[0], stderr_buf[stderr_len..]) catch 0;
            if (n == 0) stderr_eof = true else stderr_len += n;
        }
        if (fds[1].revents & std.posix.POLL.HUP != 0) stderr_eof = true;
    }

    // Wait for child
    const wait_result = std.posix.waitpid(pid, 0);
    const exit_code = if (std.posix.W.IFEXITED(wait_result.status))
        std.posix.W.EXITSTATUS(wait_result.status)
    else if (std.posix.W.IFSIGNALED(wait_result.status))
        @as(i32, 128) + @as(i32, @intCast(std.posix.W.TERMSIG(wait_result.status)))
    else
        -1;

    // Check if we timed out (killed the child)
    const timed_out = std.time.milliTimestamp() >= deadline;
    const final_exit_code: i32 = if (timed_out) 124 else exit_code;

    const finished = std.time.timestamp();

    // Write log entry
    const seq = sandbox.nextLogSeq();
    try writeExecLog(sandbox, seq, req, final_exit_code, started, finished,
        stdout_buf[0..stdout_len], stderr_buf[0..stderr_len]);

    return ExecResult{
        .exit_code = final_exit_code,
        .stdout = try allocator.dupe(u8, stdout_buf[0..stdout_len]),
        .stderr = try allocator.dupe(u8, stderr_buf[0..stderr_len]),
        .started = started,
        .finished = finished,
        .seq = seq,
    };
}
```

### S3 Sync (`s3.zig` + `sigv4.zig`)

The existing shell code (`bin/sq-s3`) implements AWS SigV4 signing with `openssl` and `curl`. Port this to Zig.

**Do NOT use an AWS SDK.** There is no production Zig AWS SDK. Instead, implement the minimal S3 operations with raw HTTP + SigV4 signing, exactly as the shell version does with curl.

`sigv4.zig` — implements AWS Signature V4:
```zig
pub fn sign(
    method: []const u8,
    path: []const u8,
    query: []const u8,
    payload_hash: [64]u8,  // hex-encoded SHA256
    access_key: []const u8,
    secret_key: []const u8,
    region: []const u8,
    datetime: []const u8,   // "20250115T103000Z"
    datestamp: []const u8,   // "20250115"
) SignResult { ... }
```

Use `std.crypto.hash.sha2.Sha256` for hashing and `std.crypto.auth.hmac.sha2.HmacSha256` for HMAC. These are in the Zig standard library — no external dependency.

`s3.zig` — the S3 client:
```zig
pub const S3Client = struct {
    bucket: []const u8,
    endpoint: ?[]const u8,
    region: []const u8,
    prefix: []const u8,
    access_key: []const u8,
    secret_key: []const u8,
    allocator: std.mem.Allocator,

    pub fn push(self: *S3Client, local_path: []const u8, key: []const u8) !void { ... }
    pub fn pull(self: *S3Client, key: []const u8, local_path: []const u8) !void { ... }
    pub fn exists(self: *S3Client, key: []const u8) !bool { ... }
    pub fn list(self: *S3Client, prefix: []const u8) ![][]const u8 { ... }
    pub fn pushBg(self: *S3Client, local_path: []const u8, key: []const u8) void {
        // Spawn a thread that copies the file and uploads
        const thread = std.Thread.spawn(.{}, pushThreadFn, .{ self, local_path, key }) catch return;
        thread.detach();
    }
};
```

Use `std.http.Client` for HTTP requests. It supports HTTPS via the system CA bundle.

**Pull atomicity** (matching sq-s3:258-284): write to `.s3tmp`, then rename. Use `flock` to prevent concurrent pulls of the same file.

### HTTP API Server (`api.zig`)

Use `std.http.Server`. The API has 10 endpoints, all JSON request/response, no streaming. This is well within what the standard library can handle.

```zig
pub fn startServer(config: *const Config, manager: *SandboxManager) !void {
    const address = std.net.Address.initIp4(.{ 0, 0, 0, 0 }, config.port);
    var server = try std.http.Server.init(.{
        .address = address,
        .reuse_address = true,
    });
    defer server.deinit();

    while (true) {
        var response = try server.accept();
        // Dispatch to thread pool
        const thread = try std.Thread.spawn(.{}, handleRequest, .{ &response, config, manager });
        thread.detach();
    }
}

fn handleRequest(
    response: *std.http.Server.Response,
    config: *const Config,
    manager: *SandboxManager,
) void {
    defer response.deinit();

    // Auth check
    if (config.auth_token) |token| {
        const auth_header = response.request.headers.getFirstValue("Authorization") orelse {
            sendJsonError(response, .unauthorized, "unauthorized");
            return;
        };
        if (!std.mem.eql(u8, auth_header, std.fmt.allocPrint(allocator, "Bearer {s}", .{token}))) {
            sendJsonError(response, .unauthorized, "unauthorized");
            return;
        }
    }

    // Route
    const path = response.request.target;
    const method = response.request.method;

    if (std.mem.eql(u8, path, "/cgi-bin/health")) {
        return handleHealth(response);
    }
    if (std.mem.startsWith(u8, path, "/cgi-bin/api/sandboxes")) {
        return routeSandboxes(response, path, method, config, manager);
    }
    if (std.mem.eql(u8, path, "/cgi-bin/api/modules")) {
        return handleListModules(response, config);
    }

    sendJsonError(response, .not_found, "not found");
}
```

**Content-Type enforcement**: POST endpoints check for `Content-Type: application/json`. Return 415 if missing.

**Request/response JSON formats**: must exactly match the existing API. See the Rust prompt for detailed JSON schemas — they are identical regardless of implementation language.

### JSON Helpers (`json.zig`)

Use `std.json` for parsing and serialization. Define structs matching the API shapes:

```zig
pub const CreateRequest = struct {
    id: []const u8,
    owner: ?[]const u8 = null,
    layers: ?std.json.Value = null,  // string or array — handle both
    task: ?[]const u8 = null,
    cpu: ?f64 = null,
    memory_mb: ?u64 = null,
    max_lifetime_s: ?u64 = null,
    allow_net: ?[]const []const u8 = null,
};

pub const ExecRequestJson = struct {
    cmd: []const u8,
    workdir: ?[]const u8 = null,
    timeout: ?u64 = null,
};
```

For `layers`: accept either `"000-base-alpine,100-python312"` (string, split on comma) or `["000-base-alpine", "100-python312"]` (array). The shell code handles both (cgi-bin/api/sandboxes:22).

### Secrets (`secrets.zig`)

```zig
pub const Secret = struct {
    placeholder: []const u8,
    value: []const u8,
    allowed_hosts: []const []const u8,
};

pub const SecretsConfig = struct {
    secrets: std.StringHashMap(Secret),

    pub fn load(allocator: std.mem.Allocator, path: []const u8) !SecretsConfig { ... }
};
```

**Placeholder injection** (matching common.sh:253-305): write `squash-secrets.sh` into the sandbox's `/etc/profile.d/` via the upper layer. This runs in the sandbox's shell profile, exporting placeholder values and proxy config.

### Reaper (`reaper.zig`)

```zig
pub fn reaperThread(manager: *SandboxManager) void {
    while (true) {
        std.time.sleep(10 * std.time.ns_per_s);

        var expired = std.ArrayList([]const u8).init(manager.allocator);
        defer expired.deinit();

        // Scan all sandboxes
        var iter = manager.sandboxes.iterator();
        while (iter.next()) |entry| {
            const sandbox = entry.value_ptr.*.sandbox;
            const max_lifetime = sandbox.max_lifetime_s;
            if (max_lifetime == 0) continue;

            const age = std.time.timestamp() - sandbox.created_ts;
            if (age > max_lifetime) {
                expired.append(entry.key_ptr.*) catch continue;
            }
        }

        // Destroy expired sandboxes (outside iterator)
        for (expired.items) |id| {
            log.info("reaper: destroying expired sandbox {s}", .{id});
            manager.destroySandbox(id) catch |err| {
                log.warn("reaper: failed to destroy {s}: {}", .{ id, err });
            };
        }
    }
}
```

### Init / Recovery (`init.zig`)

Runs at startup (matching bin/sq-init):

1. Create `modules/` and `sandboxes/` directories
2. Check for `000-base-alpine.squashfs`:
   - If missing: try S3 pull, then shell out to `sq-mkbase alpine`
3. If Firecracker backend: check VM components, shell out to `sq-mkvm all`
4. If NOT ephemeral: remount surviving sandboxes (scan dirs, loop-mount modules, rebuild overlayfs)

### Snapshot (`snapshot.zig`)

Shell out to `mksquashfs`:

```zig
pub fn snapshotSandbox(
    allocator: std.mem.Allocator,
    sandbox: *Sandbox,
    label: []const u8,
) ![]const u8 {
    if (!validLabel(label)) return error.InvalidLabel;

    const snap_dir = try std.fmt.allocPrint(allocator, "{s}/snapshots", .{sandbox.dir});
    try std.fs.makeDirAbsolute(snap_dir);

    const snap_path = try std.fmt.allocPrint(allocator, "{s}/{s}.squashfs", .{ snap_dir, label });

    // Check if snapshot already exists
    std.fs.accessAbsolute(snap_path, .{}) catch |err| switch (err) {
        error.FileNotFound => {},  // good, doesn't exist
        else => return err,
    };

    // Detect compression support
    const use_zstd = detectZstdSupport();
    const comp_args = if (use_zstd)
        &[_][]const u8{ "-comp", "zstd", "-Xcompression-level", "3", "-b", "128K" }
    else
        &[_][]const u8{ "-comp", "gzip", "-b", "256K" };

    const upper_data = try std.fmt.allocPrint(allocator, "{s}/upper/data", .{sandbox.dir});

    var args = std.ArrayList([]const u8).init(allocator);
    try args.append("mksquashfs");
    try args.append(upper_data);
    try args.append(snap_path);
    try args.appendSlice(comp_args);
    try args.appendSlice(&.{ "-noappend", "-quiet" });

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = args.items,
    });
    if (result.term.Exited != 0) return error.MksquashfsFailed;

    return snap_path;
}
```

### Restore (`snapshot.zig`)

Matching common.sh:664-707:

1. Find snapshot file (local or S3 pull)
2. Unmount overlay
3. Unmount previous snapshot layer if any
4. Clear `upper/data/*` and `upper/work/*` (keep tmpfs mount)
5. Loop-mount snapshot at `images/_snapshot/`
6. Remount overlay with snapshot as highest-priority lower layer
7. Re-inject secret placeholders

### Guest Agent (`guest/main.zig`)

Separate binary. PID 1 in Firecracker VM. Minimal — no S3, no HTTP server, no secrets.

```zig
pub fn main() !void {
    // 1. Mount essential filesystems
    try mountProc();
    try mountSys();
    try mountDev();
    try mountDevPts();
    try mountShm();

    // 2. Parse kernel cmdline
    const cmdline = try readFile("/proc/cmdline");
    const layer_count = parseCmdlineInt(cmdline, "squash.layers=") orelse 99;

    // 3. Mount squashfs layers from /dev/vd[b-z]
    var lower_components: [24][]const u8 = undefined;
    var lower_count: usize = 0;
    var dev_letter: u8 = 'b';
    while (dev_letter <= 'z' and lower_count < layer_count) : (dev_letter += 1) {
        var dev_path_buf: [16]u8 = undefined;
        const dev_path = std.fmt.bufPrint(&dev_path_buf, "/dev/vd{c}", .{dev_letter})
            catch continue;
        var mp_buf: [32]u8 = undefined;
        const mp = std.fmt.bufPrint(&mp_buf, "/layers/{d}", .{lower_count}) catch continue;

        std.fs.makeDirAbsolute(mp) catch {};
        const rc = c.mount(dev_path.ptr, mp.ptr, "squashfs", c.MS_RDONLY, null);
        if (rc == 0) {
            lower_components[lower_count] = mp;
            lower_count += 1;
        }
    }

    // 4. Set up overlayfs
    try setupGuestOverlay(lower_components[0..lower_count]);

    // 5. Network
    try setupGuestNetwork();

    // 6. Seed resolv.conf
    writeFile("/sandbox/merged/etc/resolv.conf", "nameserver 10.0.0.1\n") catch {};

    std.log.info("squash-guest ready — layers={d}", .{lower_count});

    // 7. Listen on vsock port 5000
    try vsockListen(5000);
}
```

**Vsock listener** (`guest/vsock.zig`):

```zig
const VMADDR_CID_ANY: u32 = 0xFFFFFFFF;

fn vsockListen(port: u32) !void {
    const fd = try std.posix.socket(
        c.AF_VSOCK,
        std.posix.SOCK.STREAM | std.posix.SOCK.CLOEXEC,
        0,
    );
    defer std.posix.close(fd);

    var addr = c.sockaddr_vm{
        .svm_family = c.AF_VSOCK,
        .svm_cid = VMADDR_CID_ANY,
        .svm_port = port,
        .svm_flags = 0,
        .svm_zero = .{0} ** 4,
    };
    try std.posix.bind(fd, @ptrCast(&addr), @sizeOf(c.sockaddr_vm));
    try std.posix.listen(fd, 16);

    while (true) {
        const conn_fd = try std.posix.accept(fd, null, null, 0);
        const thread = std.Thread.spawn(.{}, handleVsockConnection, .{conn_fd}) catch {
            std.posix.close(conn_fd);
            continue;
        };
        thread.detach();
    }
}

fn handleVsockConnection(conn_fd: std.posix.fd_t) void {
    defer std.posix.close(conn_fd);

    // Read JSON request
    var buf: [4096]u8 = undefined;
    const n = std.posix.read(conn_fd, &buf) catch return;
    const req_json = buf[0..n];

    // Parse
    const parsed = std.json.parseFromSlice(ExecRequestJson, std.heap.page_allocator, req_json, .{}) catch return;
    defer parsed.deinit();

    // Execute in chroot
    const result = execInGuest(parsed.value) catch |err| {
        const err_resp = std.fmt.allocPrint(std.heap.page_allocator,
            "{{\"exit_code\":-1,\"stdout\":\"\",\"stderr\":\"{s}\"}}", .{@errorName(err)}) catch return;
        _ = std.posix.write(conn_fd, err_resp) catch {};
        return;
    };

    // Write JSON response
    _ = std.posix.write(conn_fd, result) catch {};
}
```

Special command `__squash_remount`: rebuild overlayfs from current block devices (matching vm/sq-vsock-handler:62-83).

---

## Startup Sequence (`main.zig`)

```zig
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const config = try Config.fromEnv(allocator);

    std.log.info("squash v4 (zig)", .{});

    // 1. Init / recovery
    try init.run(allocator, &config);

    // 2. Start Go secret proxy as child process (if secrets.json exists)
    var proxy_child: ?std.process.Child = null;
    if (config.proxy_https and secretsExist(&config)) {
        // Generate CA if needed
        try ensureProxyCa(allocator, &config);
        proxy_child = try startGoProxy(allocator, &config);
    } else if (secretsExist(&config)) {
        // HTTP-only shell proxy
        proxy_child = try startShellProxy(allocator, &config);
    }
    defer if (proxy_child) |*child| {
        _ = child.kill() catch {};
        _ = child.wait() catch {};
    };

    // 3. Start Tailscale if configured
    if (config.tailscale_authkey) |_| {
        _ = std.Thread.spawn(.{}, startTailscale, .{ allocator, &config }) catch {};
    }

    // 4. Create sandbox manager
    var manager = try SandboxManager.init(allocator, &config);
    defer {
        // Cleanup all sandboxes on exit
        manager.destroyAll();
        manager.deinit();
    }

    // 5. Start reaper thread
    const reaper_thread = try std.Thread.spawn(.{}, reaper.reaperThread, .{&manager});
    reaper_thread.detach();

    // 6. Start HTTP server (blocks)
    const mod_count = countModules(&config);
    const sb_count = manager.sandboxes.count();
    std.log.info("ready — modules: {d}, sandboxes: {d}", .{ mod_count, sb_count });

    try api.startServer(&config, &manager);
}
```

---

## Filesystem Layout

Byte-compatible with the existing shell version. A running shell-based system can be upgraded to the Zig version without data loss. See the Rust prompt for the full directory tree — it is identical.

---

## Dockerfile

```dockerfile
# Build squashd (Zig)
FROM alpine:3.21 AS zig-build
RUN apk add --no-cache curl xz
# Download Zig
RUN curl -L https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz | tar xJ -C /opt
ENV PATH="/opt/zig-linux-x86_64-0.13.0:$PATH"
WORKDIR /build
COPY build.zig build.zig.zon ./
COPY src/ src/
COPY guest/ guest/
RUN zig build -Doptimize=ReleaseSafe

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

COPY --from=zig-build /build/zig-out/bin/squashd /app/bin/squashd
COPY --from=zig-build /build/zig-out/bin/sq-guest-agent /app/bin/sq-guest-agent
COPY --from=proxy-build /sq-secret-proxy-https /app/bin/sq-secret-proxy-https
COPY bin/sq-mkbase bin/sq-mkmod bin/sq-mkvm bin/sq-ctl bin/sq-secret-proxy bin/setup-tailscale /app/bin/
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

## Key Differences from the Rust Prompt

| Concern | Rust approach | Zig approach |
|---|---|---|
| **Resource cleanup** | `Drop` trait — automatic, compiler-enforced | `deinit()` — explicit, discipline-enforced. `errdefer` covers creation path. |
| **Error handling** | `Result<T, E>` + `?` + `thiserror` | Error unions + `try` + `errdefer`. More explicit, less boilerplate. |
| **Syscalls** | `nix` crate wrapping libc | `@cImport` of kernel headers — zero abstraction layer |
| **HTTP server** | `axum` (production-grade, async) | `std.http.Server` or `libmicrohttpd` binding. Simpler, synchronous. |
| **TLS/MITM proxy** | Rewrite in Rust with `rustls` + `rcgen` | Keep Go binary as sidecar — pragmatic, proven |
| **S3 client** | `aws-sdk-s3` crate | Custom HTTP + SigV4 signing (~200 lines) |
| **JSON** | `serde_json` (derive macros) | `std.json` (manual struct mapping) |
| **Async model** | `tokio` runtime, async/await | Thread pool, blocking I/O |
| **Binary size** | ~3-5MB static musl | **~500KB-1.5MB static musl** |
| **Guest agent** | ~1-2MB | **~100-300KB** |
| **Concurrency** | `DashMap` + `Arc<Mutex<T>>` | `std.StringHashMap` + `std.Thread.Mutex` |
| **Netlink** | `rtnetlink` crate | Shell out to `ip` command (pragmatic choice) |
| **Build time** | ~30-60s full | **~3-5s full** |

---

## Testing Strategy

### Unit tests (in-file, using `test` blocks)

```zig
test "valid_id accepts alphanumeric" {
    try std.testing.expect(validId("my-sandbox-123"));
    try std.testing.expect(!validId("../../etc"));
    try std.testing.expect(!validId(""));
    try std.testing.expect(!validId("has space"));
}

test "sigv4 signing produces correct signature" {
    // Test vector from AWS documentation
    // ...
}
```

### Integration tests (require privileged container)

Port the existing `sq-test` (bin/sq-test, 278 lines). Can remain as a shell script hitting the HTTP API — the tests are black-box against the API, so the implementation language doesn't matter.

Tests to add beyond the existing suite:
1. **Cleanup correctness**: create sandbox, kill `squashd`, restart, verify no orphaned mounts (check `/proc/mounts`), no orphaned iptables rules (`iptables -L`), no orphaned netns (`ip netns list`)
2. **Concurrent operations**: parallel exec on same sandbox serializes correctly
3. **Stress**: create/destroy 100 sandboxes rapidly, verify no resource leaks

### Memory safety

Build with `ReleaseSafe` (not `ReleaseFast`) to keep bounds checking, null pointer checks, and undefined behavior detection active. The performance difference is negligible for this workload — syscall latency dominates, not computation.

Run integration tests under Valgrind to catch memory leaks. Zig's `GeneralPurposeAllocator` has leak detection built in — use it in debug/test builds:

```zig
var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
defer std.debug.assert(gpa.deinit() == .ok);  // fails if any memory leaked
```

---

## Implementation Order

1. **Phase 1: Config + Validation + HTTP skeleton** — `std.http.Server` with health endpoint. Verify it builds and runs.

2. **Phase 2: Mounts** — SquashfsMount, TmpfsMount, OverlayMount with `deinit()`. Test mount/unmount in privileged container. Verify `errdefer` rollback works (deliberately fail mid-creation, check no orphaned mounts).

3. **Phase 3: Sandbox create + destroy + exec** — Full create flow minus netns/cgroup. Basic fork+unshare+chroot exec. Test full lifecycle.

4. **Phase 4: Cgroups + Network namespaces** — Shell out to `ip` and `iptables`. Test isolation.

5. **Phase 5: Snapshot + Restore + Activate** — mksquashfs shell-out, overlay remount. Test snapshot/restore cycle.

6. **Phase 6: S3 sync** — SigV4 signing, HTTP client. Test with MinIO.

7. **Phase 7: Go proxy integration** — Child process management, CA generation.

8. **Phase 8: Init/recovery + Reaper + Ephemeral** — Startup recovery, background reaper. Test crash recovery.

9. **Phase 9: Wire compatibility** — Run existing `sq-test` against Zig binary.

---

## Discipline Checklist

Because Zig has no `Drop`, enforce these rules in code review:

- [ ] Every type with a `deinit()` is documented as requiring explicit cleanup
- [ ] Every `deinit()` is idempotent (safe to call twice)
- [ ] `SandboxManager.destroySandbox()` ALWAYS calls `sandbox.destroy()`
- [ ] `main()` has a `defer manager.destroyAll()` for clean shutdown
- [ ] The reaper calls `destroySandbox()`, not just removing from the hashmap
- [ ] No sandbox is removed from the hashmap without `destroy()` being called
- [ ] Every test that creates mounts has a `defer mount.deinit()` or explicit cleanup
- [ ] `errdefer` is used on EVERY resource acquisition in multi-step creation sequences

---

## Non-Goals

Same as Rust prompt:
- Rewriting shell setup scripts in Zig
- Implementing Firecracker backend (stub the trait)
- WebSocket streaming
- Multi-node distribution
- GPU passthrough
- Rewriting the Go MITM proxy in Zig
