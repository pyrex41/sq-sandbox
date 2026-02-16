const std = @import("std");
const posix = std.posix;

/// Handle for a sandbox network namespace with veth pair, NAT, DNS forwarding,
/// and optional egress filtering. Shells out to `ip` and `iptables` per the
/// architecture spec recommendation (pragmatic, ~10-20ms total fork/exec cost).
pub const NetnsHandle = struct {
    name: []const u8, // "squash-{id}"
    index: u8, // 1-254
    veth_host: []const u8, // "sq-{id}-h"
    chain_name: ?[]const u8, // "squash-{id}" if egress rules applied
    host_dns: ?[]const u8, // first nameserver from /etc/resolv.conf
    subnet: []const u8, // "10.200.{index}.0/30"
    gateway: []const u8, // "10.200.{index}.1"
    allocator: std.mem.Allocator,

    pub fn setup(
        allocator: std.mem.Allocator,
        data_dir: []const u8,
        id: []const u8,
        allow_net: ?[]const []const u8,
    ) !NetnsHandle {
        const index = try allocateNetnsIndex(allocator, data_dir);
        errdefer releaseNetnsIndex(data_dir, index);

        const name = try std.fmt.allocPrint(allocator, "squash-{s}", .{id});
        errdefer allocator.free(name);

        // ip netns add squash-{id}
        try runCommand(allocator, &.{ "ip", "netns", "add", name });
        errdefer runCommand(allocator, &.{ "ip", "netns", "delete", name }) catch {};

        const veth_host = try std.fmt.allocPrint(allocator, "sq-{s}-h", .{id});
        errdefer allocator.free(veth_host);
        const veth_sandbox = try std.fmt.allocPrint(allocator, "sq-{s}-s", .{id});
        defer allocator.free(veth_sandbox);

        // ip link add sq-{id}-h type veth peer name sq-{id}-s
        try runCommand(allocator, &.{ "ip", "link", "add", veth_host, "type", "veth", "peer", "name", veth_sandbox });
        errdefer runCommand(allocator, &.{ "ip", "link", "delete", veth_host }) catch {};

        // ip link set sq-{id}-s netns squash-{id}
        try runCommand(allocator, &.{ "ip", "link", "set", veth_sandbox, "netns", name });

        // Configure host side
        const host_addr = try std.fmt.allocPrint(allocator, "10.200.{d}.1/30", .{index});
        defer allocator.free(host_addr);
        try runCommand(allocator, &.{ "ip", "addr", "add", host_addr, "dev", veth_host });
        try runCommand(allocator, &.{ "ip", "link", "set", veth_host, "up" });

        // Configure sandbox side (inside netns)
        const sandbox_addr = try std.fmt.allocPrint(allocator, "10.200.{d}.2/30", .{index});
        defer allocator.free(sandbox_addr);
        const gateway = try std.fmt.allocPrint(allocator, "10.200.{d}.1", .{index});
        errdefer allocator.free(gateway);
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "addr", "add", sandbox_addr, "dev", veth_sandbox });
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "link", "set", veth_sandbox, "up" });
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "link", "set", "lo", "up" });
        try runCommand(allocator, &.{ "ip", "netns", "exec", name, "ip", "route", "add", "default", "via", gateway });

        // Enable IP forwarding
        writeFile("/proc/sys/net/ipv4/ip_forward", "1") catch {};

        // NAT
        const subnet = try std.fmt.allocPrint(allocator, "10.200.{d}.0/30", .{index});
        errdefer allocator.free(subnet);
        try runCommand(allocator, &.{ "iptables", "-t", "nat", "-A", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE" });

        // DNS forwarding — redirect queries aimed at gateway to host's real resolver
        const host_dns = parseFirstNameserver(allocator) catch null;
        errdefer if (host_dns) |d| allocator.free(d);
        if (host_dns) |dns| {
            try runCommand(allocator, &.{
                "iptables", "-t", "nat", "-A", "PREROUTING",
                "-s",      subnet,  "-d", gateway,
                "-p",      "udp",   "--dport", "53",
                "-j",      "DNAT",  "--to-destination", dns,
            });
            try runCommand(allocator, &.{
                "iptables", "-t", "nat", "-A", "PREROUTING",
                "-s",      subnet,  "-d", gateway,
                "-p",      "tcp",   "--dport", "53",
                "-j",      "DNAT",  "--to-destination", dns,
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
            .subnet = subnet,
            .gateway = gateway,
            .allocator = allocator,
        };
    }

    /// Idempotent cleanup. Tears down in reverse order, ignoring all errors
    /// (matching the shell `|| true` pattern).
    pub fn deinit(self: *NetnsHandle) void {
        const allocator = self.allocator;

        // 1. Remove egress chain
        if (self.chain_name) |chain| {
            runCommand(allocator, &.{ "iptables", "-D", "FORWARD", "-i", self.veth_host, "-j", chain }) catch {};
            runCommand(allocator, &.{ "iptables", "-F", chain }) catch {};
            runCommand(allocator, &.{ "iptables", "-X", chain }) catch {};
            allocator.free(chain);
        }

        // 2. Remove NAT MASQUERADE
        runCommand(allocator, &.{ "iptables", "-t", "nat", "-D", "POSTROUTING", "-s", self.subnet, "-j", "MASQUERADE" }) catch {};

        // 3. Remove DNS DNAT rules
        if (self.host_dns) |dns| {
            runCommand(allocator, &.{
                "iptables", "-t", "nat", "-D", "PREROUTING",
                "-s",      self.subnet, "-d", self.gateway,
                "-p",      "udp",       "--dport", "53",
                "-j",      "DNAT",      "--to-destination", dns,
            }) catch {};
            runCommand(allocator, &.{
                "iptables", "-t", "nat", "-D", "PREROUTING",
                "-s",      self.subnet, "-d", self.gateway,
                "-p",      "tcp",       "--dport", "53",
                "-j",      "DNAT",      "--to-destination", dns,
            }) catch {};
            allocator.free(dns);
        }

        // 4. Delete veth pair (host end — peer is auto-deleted)
        runCommand(allocator, &.{ "ip", "link", "delete", self.veth_host }) catch {};

        // 5. Delete network namespace
        runCommand(allocator, &.{ "ip", "netns", "delete", self.name }) catch {};

        // Free allocations
        allocator.free(self.subnet);
        allocator.free(self.gateway);
        allocator.free(self.veth_host);
        allocator.free(self.name);
    }
};

// ── Egress filtering ─────────────────────────────────────────────────

/// Create an iptables chain with egress rules matching common.sh:385-422.
/// Returns the chain name (owned by caller) on success.
fn applyEgressRules(
    allocator: std.mem.Allocator,
    id: []const u8,
    veth_host: []const u8,
    allow_net: []const []const u8,
) ![]const u8 {
    const chain = try std.fmt.allocPrint(allocator, "squash-{s}", .{id});
    errdefer allocator.free(chain);

    // Create chain (ignore error if it already exists, matching -N || true)
    runCommand(allocator, &.{ "iptables", "-N", chain }) catch {};

    // Jump from FORWARD to our chain for traffic from the veth
    try runCommand(allocator, &.{ "iptables", "-A", "FORWARD", "-i", veth_host, "-j", chain });

    // Block ICMP (prevents tunneling)
    try runCommand(allocator, &.{ "iptables", "-A", chain, "-p", "icmp", "-j", "DROP" });

    // Rate-limited DNS (prevents DNS tunneling)
    try runCommand(allocator, &.{
        "iptables", "-A", chain, "-p", "udp", "--dport", "53",
        "-m",       "limit", "--limit", "10/s", "--limit-burst", "20",
        "-j",       "ACCEPT",
    });
    try runCommand(allocator, &.{
        "iptables", "-A", chain, "-p", "tcp", "--dport", "53",
        "-m",       "limit", "--limit", "10/s", "--limit-burst", "20",
        "-j",       "ACCEPT",
    });

    // Allow established/related connections
    try runCommand(allocator, &.{
        "iptables", "-A", chain, "-m", "state", "--state", "ESTABLISHED,RELATED", "-j", "ACCEPT",
    });

    // Resolve each allowed host and add accept rules
    for (allow_net) |host| {
        if (std.mem.eql(u8, host, "none")) continue;
        if (std.mem.indexOfScalar(u8, host, '*') != null) {
            // Wildcards need proxy mode — log warning, skip
            std.log.warn("[net] wildcard {s} requires proxy mode", .{host});
            continue;
        }
        // Resolve host via getaddrinfo
        const addrs = resolveHost(allocator, host) catch continue;
        defer allocator.free(addrs);
        for (addrs) |addr| {
            defer allocator.free(addr);
            try runCommand(allocator, &.{ "iptables", "-A", chain, "-d", addr, "-j", "ACCEPT" });
        }
    }

    // Default: drop
    try runCommand(allocator, &.{ "iptables", "-A", chain, "-j", "DROP" });

    return chain;
}

/// Resolve a hostname to IP addresses via getaddrinfo. Returns a slice of
/// allocated IP strings (caller must free both the slice and each element).
fn resolveHost(allocator: std.mem.Allocator, host: []const u8) ![][]const u8 {
    var list: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (list.items) |item| allocator.free(item);
        list.deinit(allocator);
    }

    const addr_list = std.net.getAddressList(allocator, host, 0) catch return error.ResolveFailed;
    defer addr_list.deinit();

    for (addr_list.addrs) |addr| {
        if (addr.any.family != posix.AF.INET) continue;
        const addr_bytes: *const [4]u8 = @ptrCast(&addr.in.sa.addr);
        const ip_str = try std.fmt.allocPrint(allocator, "{d}.{d}.{d}.{d}", .{
            addr_bytes[0],
            addr_bytes[1],
            addr_bytes[2],
            addr_bytes[3],
        });
        try list.append(allocator, ip_str);
    }

    if (list.items.len == 0) return error.ResolveFailed;
    return list.toOwnedSlice(allocator);
}

// ── Netns index allocation ───────────────────────────────────────────

/// Allocate a unique netns index (1-254) using flock-based locking.
/// Scans {data_dir}/sandboxes/*/.meta/netns_index to find first unused index.
fn allocateNetnsIndex(_: std.mem.Allocator, data_dir: []const u8) !u8 {
    // Open or create the lock file
    var lock_path_buf: [512]u8 = undefined;
    const lock_path = try std.fmt.bufPrint(&lock_path_buf, "{s}/.netns-index.lock", .{data_dir});

    const lock_fd = try openOrCreateFile(lock_path);
    defer posix.close(lock_fd);

    // Acquire exclusive flock
    try posix.flock(lock_fd, posix.LOCK.EX);
    defer posix.flock(lock_fd, posix.LOCK.UN) catch {};

    // Scan all sandbox .meta/netns_index files to find used indices
    var used = [_]bool{false} ** 255; // index 0 unused, 1-254 valid

    var sandboxes_path_buf: [512]u8 = undefined;
    const sandboxes_path = try std.fmt.bufPrint(&sandboxes_path_buf, "{s}/sandboxes", .{data_dir});

    var dir = std.fs.openDirAbsolute(sandboxes_path, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => {
            // No sandboxes dir yet — index 1 is free
            return 1;
        },
        else => return err,
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .directory) continue;

        // Read {sandbox_dir}/.meta/netns_index
        var idx_path_buf: [1024]u8 = undefined;
        const idx_path = std.fmt.bufPrint(&idx_path_buf, "{s}/{s}/.meta/netns_index", .{
            sandboxes_path, entry.name,
        }) catch continue;

        const content = readSmallFile(idx_path) catch continue;
        const trimmed = std.mem.trim(u8, &content.data, &std.ascii.whitespace);
        const idx = std.fmt.parseInt(u8, trimmed[0..@min(trimmed.len, content.len)], 10) catch continue;
        if (idx >= 1 and idx <= 254) {
            used[idx] = true;
        }
    }

    // Find first unused index
    for (1..255) |i| {
        if (!used[i]) return @intCast(i);
    }

    return error.SubnetExhausted;
}

/// Release a netns index. Currently a no-op because the index is tracked via
/// the .meta/netns_index file which is cleaned up with the sandbox directory.
/// The allocation scan will no longer find it after sandbox destruction.
fn releaseNetnsIndex(_: []const u8, _: u8) void {}

// ── Resolv.conf seeding ──────────────────────────────────────────────

/// Write /etc/resolv.conf in the sandbox upper layer pointing DNS at the
/// netns gateway IP (where DNAT will redirect to the host's real resolver).
pub fn seedResolvConf(data_dir: []const u8, id: []const u8, index: u8) !void {
    var path_buf: [512]u8 = undefined;
    const dir_path = try std.fmt.bufPrint(&path_buf, "{s}/sandboxes/{s}/upper/data/etc", .{ data_dir, id });

    std.fs.makeDirAbsolute(dir_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var resolv_path_buf: [512]u8 = undefined;
    const resolv_path = try std.fmt.bufPrint(&resolv_path_buf, "{s}/resolv.conf", .{dir_path});

    var content_buf: [128]u8 = undefined;
    const content = try std.fmt.bufPrint(&content_buf, "nameserver 10.200.{d}.1\n", .{index});

    const file = try std.fs.createFileAbsolute(resolv_path, .{});
    defer file.close();
    try file.writeAll(content);
}

// ── Helper functions ─────────────────────────────────────────────────

/// Run an external command synchronously. Returns error on non-zero exit.
fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Close;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // We don't need stdout/stderr content, but must drain pipes to avoid deadlock
    var stdout_buf: [4096]u8 = undefined;
    var stderr_buf: [4096]u8 = undefined;
    _ = child.stdout.?.read(&stdout_buf) catch 0;
    _ = child.stderr.?.read(&stderr_buf) catch 0;

    const term = try child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) return error.CommandFailed;
        },
        else => return error.CommandFailed,
    }
}

/// Write content to a file, creating it if needed, truncating if it exists.
fn writeFile(file_path: []const u8, content: []const u8) !void {
    const file = try std.fs.openFileAbsolute(file_path, .{ .mode = .write_only });
    defer file.close();
    try file.writeAll(content);
}

/// Open or create a file for locking purposes.
fn openOrCreateFile(file_path: []const u8) !posix.fd_t {
    var buf: [512]u8 = undefined;
    const z_path = std.fmt.bufPrintZ(&buf, "{s}", .{file_path}) catch return error.PathTooLong;
    return posix.openZ(
        z_path,
        .{ .ACCMODE = .RDWR, .CREAT = true, .CLOEXEC = true },
        0o644,
    );
}

const SmallFileResult = struct {
    data: [64]u8,
    len: usize,
};

/// Read a small file (up to 64 bytes) into a stack buffer.
fn readSmallFile(file_path: []const u8) !SmallFileResult {
    const file = std.fs.openFileAbsolute(file_path, .{ .mode = .read_only }) catch return error.FileNotFound;
    defer file.close();
    var result = SmallFileResult{ .data = undefined, .len = 0 };
    result.len = file.read(&result.data) catch return error.ReadFailed;
    return result;
}

/// Parse the first nameserver from /etc/resolv.conf.
fn parseFirstNameserver(allocator: std.mem.Allocator) ![]const u8 {
    const file = std.fs.openFileAbsolute("/etc/resolv.conf", .{ .mode = .read_only }) catch return error.NoResolv;
    defer file.close();

    var buf: [4096]u8 = undefined;
    const n = file.read(&buf) catch return error.ReadFailed;
    const content = buf[0..n];

    var lines = std.mem.splitScalar(u8, content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
        if (std.mem.startsWith(u8, trimmed, "nameserver ")) {
            const ns = std.mem.trim(u8, trimmed["nameserver ".len..], &std.ascii.whitespace);
            if (ns.len > 0) {
                return try allocator.dupe(u8, ns);
            }
        }
    }
    return error.NoNameserver;
}

// ── Tests ────────────────────────────────────────────────────────────

test "parseFirstNameserver from test content" {
    // We can't mock /etc/resolv.conf in a unit test, but we can test the
    // parsing logic directly by testing the line parsing pattern.
    const content = "# comment\nnameserver 8.8.8.8\nnameserver 1.1.1.1\n";
    var lines = std.mem.splitScalar(u8, content, '\n');
    var found: ?[]const u8 = null;
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
        if (std.mem.startsWith(u8, trimmed, "nameserver ")) {
            found = std.mem.trim(u8, trimmed["nameserver ".len..], &std.ascii.whitespace);
            break;
        }
    }
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("8.8.8.8", found.?);
}

test "resolv.conf parsing skips comments" {
    const content = "# This is a comment\nsearch example.com\nnameserver 10.0.0.1\n";
    var lines = std.mem.splitScalar(u8, content, '\n');
    var found: ?[]const u8 = null;
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
        if (std.mem.startsWith(u8, trimmed, "nameserver ")) {
            found = std.mem.trim(u8, trimmed["nameserver ".len..], &std.ascii.whitespace);
            break;
        }
    }
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("10.0.0.1", found.?);
}

test "subnet formatting" {
    var buf: [64]u8 = undefined;
    const s = try std.fmt.bufPrint(&buf, "10.200.{d}.0/30", .{@as(u8, 42)});
    try std.testing.expectEqualStrings("10.200.42.0/30", s);
}

test "gateway formatting" {
    var buf: [64]u8 = undefined;
    const s = try std.fmt.bufPrint(&buf, "10.200.{d}.1", .{@as(u8, 1)});
    try std.testing.expectEqualStrings("10.200.1.1", s);
}

test "netns name formatting" {
    const allocator = std.testing.allocator;
    const name = try std.fmt.allocPrint(allocator, "squash-{s}", .{"my-sandbox"});
    defer allocator.free(name);
    try std.testing.expectEqualStrings("squash-my-sandbox", name);
}

test "veth name formatting" {
    const allocator = std.testing.allocator;
    const host = try std.fmt.allocPrint(allocator, "sq-{s}-h", .{"abc"});
    defer allocator.free(host);
    const sandbox = try std.fmt.allocPrint(allocator, "sq-{s}-s", .{"abc"});
    defer allocator.free(sandbox);
    try std.testing.expectEqualStrings("sq-abc-h", host);
    try std.testing.expectEqualStrings("sq-abc-s", sandbox);
}

test "chain name formatting" {
    const allocator = std.testing.allocator;
    const chain = try std.fmt.allocPrint(allocator, "squash-{s}", .{"test-id"});
    defer allocator.free(chain);
    try std.testing.expectEqualStrings("squash-test-id", chain);
}
