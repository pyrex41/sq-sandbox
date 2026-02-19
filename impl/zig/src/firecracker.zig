// firecracker.zig — Firecracker VM backend: CID allocation, tap networking,
// VM lifecycle via sq-firecracker CLI, and vsock-based exec via socat.
//
// All VM operations shell out to the `sq-firecracker` CLI tool.
// Guest communication uses `socat` to speak JSON over vsock port 5000.

const std = @import("std");
const posix = std.posix;
const builtin = @import("builtin");
const is_linux = builtin.os.tag == .linux;
const log = std.log.scoped(.firecracker);

// ── CID Allocation ───────────────────────────────────────────────────

/// Allocate a unique CID for a Firecracker VM using flock-based counter.
/// CIDs start at 100 and increment. The counter file is at
/// {data_dir}/.fc-cid-counter.
pub fn allocateCid(data_dir: []const u8, allocator: std.mem.Allocator) !u32 {
    _ = allocator;
    var lock_path_buf: [512]u8 = undefined;
    const lock_path = try std.fmt.bufPrint(&lock_path_buf, "{s}/.fc-cid-counter", .{data_dir});

    const lock_fd = try openOrCreateFile(lock_path);
    defer posix.close(lock_fd);

    // Acquire exclusive flock
    try posix.flock(lock_fd, posix.LOCK.EX);
    defer posix.flock(lock_fd, posix.LOCK.UN) catch {};

    // Read current value
    var read_buf: [32]u8 = undefined;
    const file = std.fs.openFileAbsolute(lock_path, .{ .mode = .read_write }) catch return 100;
    defer file.close();

    const n = file.read(&read_buf) catch 0;
    const trimmed = std.mem.trim(u8, read_buf[0..n], &std.ascii.whitespace);
    var cid: u32 = 100;
    if (trimmed.len > 0) {
        cid = std.fmt.parseInt(u32, trimmed, 10) catch 100;
    }

    const next_cid = cid + 1;

    // Write next value
    file.seekTo(0) catch {};
    var write_buf: [32]u8 = undefined;
    const next_str = std.fmt.bufPrint(&write_buf, "{d}\n", .{next_cid}) catch "101\n";
    file.writeAll(next_str) catch {};

    return cid;
}

// ── Network Setup (tap + NAT) ────────────────────────────────────────

/// Set up tap device + NAT for a Firecracker VM.
/// Uses 10.0.{index}.1/30 subnet. The tap device is named sq-{id}-tap.
pub fn setupNetwork(id: []const u8, index: u8, allow_net: ?[]const u8, allocator: std.mem.Allocator) !void {
    var tap_name_buf: [64]u8 = undefined;
    const tap_name = try std.fmt.bufPrint(&tap_name_buf, "sq-{s}-tap", .{id});

    // ip tuntap add dev sq-{id}-tap mode tap
    try runCommand(allocator, &.{ "ip", "tuntap", "add", "dev", tap_name, "mode", "tap" });

    // ip addr add 10.0.{index}.1/30 dev sq-{id}-tap
    var addr_buf: [64]u8 = undefined;
    const addr = try std.fmt.bufPrint(&addr_buf, "10.0.{d}.1/30", .{index});
    try runCommand(allocator, &.{ "ip", "addr", "add", addr, "dev", tap_name });

    // ip link set sq-{id}-tap up
    try runCommand(allocator, &.{ "ip", "link", "set", tap_name, "up" });

    // Enable IP forwarding
    writeFile("/proc/sys/net/ipv4/ip_forward", "1") catch {};

    // iptables -t nat -A POSTROUTING -s 10.0.{index}.0/30 -j MASQUERADE
    var subnet_buf: [64]u8 = undefined;
    const subnet = try std.fmt.bufPrint(&subnet_buf, "10.0.{d}.0/30", .{index});
    try runCommand(allocator, &.{ "iptables", "-t", "nat", "-A", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE" });

    _ = allow_net; // TODO: egress filtering for firecracker
}

/// Tear down tap device + NAT rules for a Firecracker VM.
pub fn teardownNetwork(id: []const u8, index: u8, allocator: std.mem.Allocator) void {
    // Remove NAT rule
    var subnet_buf: [64]u8 = undefined;
    const subnet = std.fmt.bufPrint(&subnet_buf, "10.0.{d}.0/30", .{index}) catch return;
    runCommand(allocator, &.{ "iptables", "-t", "nat", "-D", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE" }) catch {};

    // Delete tap device
    var tap_name_buf: [64]u8 = undefined;
    const tap_name = std.fmt.bufPrint(&tap_name_buf, "sq-{s}-tap", .{id}) catch return;
    runCommand(allocator, &.{ "ip", "link", "delete", tap_name }) catch {};
}

// ── VM Lifecycle ─────────────────────────────────────────────────────

/// Start a Firecracker VM via sq-firecracker CLI.
pub fn startVm(
    id: []const u8,
    cpu: f64,
    memory_mb: u64,
    squashfs_paths: []const []const u8,
    cid: u32,
    meta_dir: []const u8,
    allocator: std.mem.Allocator,
) !void {
    // Build argv: sq-firecracker start {id} {cpu} {mem} {sqfs1} {sqfs2} ...
    var argv_list: std.ArrayList([]const u8) = .empty;
    defer {
        // Free formatted strings we allocated
        for (argv_list.items) |item| {
            // Only free items we allocated (skip static strings and borrowed slices)
            if (isAllocated(item, id, squashfs_paths, meta_dir)) {
                allocator.free(item);
            }
        }
        argv_list.deinit(allocator);
    }

    try argv_list.append(allocator, "sq-firecracker");
    try argv_list.append(allocator, "start");
    try argv_list.append(allocator, id);

    const cpu_str = try std.fmt.allocPrint(allocator, "{d}", .{cpu});
    try argv_list.append(allocator, cpu_str);

    const mem_str = try std.fmt.allocPrint(allocator, "{d}", .{memory_mb});
    try argv_list.append(allocator, mem_str);

    for (squashfs_paths) |path| {
        try argv_list.append(allocator, path);
    }

    // Pass --cid and --meta-dir
    try argv_list.append(allocator, "--cid");
    const cid_str = try std.fmt.allocPrint(allocator, "{d}", .{cid});
    try argv_list.append(allocator, cid_str);

    try argv_list.append(allocator, "--meta-dir");
    try argv_list.append(allocator, meta_dir);

    try runCommand(allocator, argv_list.items);

    log.info("VM started: id={s} cid={d} cpu={d} mem={d}MB", .{ id, cid, cpu, memory_mb });
}

/// Stop a Firecracker VM.
pub fn stopVm(id: []const u8, meta_dir: []const u8, allocator: std.mem.Allocator) void {
    runCommand(allocator, &.{ "sq-firecracker", "stop", id, "--meta-dir", meta_dir }) catch |err| {
        log.warn("failed to stop VM {s}: {}", .{ id, err });
    };
    log.info("VM stopped: id={s}", .{id});
}

// ── Exec via vsock ───────────────────────────────────────────────────

/// Result from a vsock exec command.
pub const VsockExecResult = struct {
    exit_code: i32,
    stdout: []const u8,
    stderr: []const u8,

    pub fn deinit(self: *VsockExecResult, allocator: std.mem.Allocator) void {
        if (self.stdout.len > 0) allocator.free(self.stdout);
        if (self.stderr.len > 0) allocator.free(self.stderr);
        self.* = undefined;
    }
};

/// Execute a command inside the VM via vsock+socat.
/// Spawns `socat -T{timeout} - VSOCK-CONNECT:{cid}:5000`,
/// writes a JSON request on stdin, reads a JSON response from stdout.
pub fn execVsock(
    cid: u32,
    cmd: []const u8,
    workdir: []const u8,
    timeout_s: u64,
    allocator: std.mem.Allocator,
) !VsockExecResult {
    // Build socat args
    var timeout_buf: [32]u8 = undefined;
    const timeout_arg = try std.fmt.bufPrint(&timeout_buf, "-T{d}", .{timeout_s});

    var vsock_buf: [64]u8 = undefined;
    const vsock_arg = try std.fmt.bufPrint(&vsock_buf, "VSOCK-CONNECT:{d}:5000", .{cid});

    var child = std.process.Child.init(
        &.{ "socat", timeout_arg, "-", vsock_arg },
        allocator,
    );
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Write JSON request to stdin
    const request_json = try std.fmt.allocPrint(
        allocator,
        "{{\"cmd\":\"{s}\",\"workdir\":\"{s}\",\"timeout\":{d}}}",
        .{ cmd, workdir, timeout_s },
    );
    defer allocator.free(request_json);

    child.stdin.?.writeAll(request_json) catch {};
    // Close stdin to signal EOF
    child.stdin.?.close();
    child.stdin = null;

    // Read stdout (the JSON response)
    var stdout_buf: [65536]u8 = undefined;
    var stderr_buf: [4096]u8 = undefined;
    const stdout_n = child.stdout.?.read(&stdout_buf) catch 0;
    const stderr_n = child.stderr.?.read(&stderr_buf) catch 0;

    const term = try child.wait();
    const socat_exit = switch (term) {
        .Exited => |code| code,
        else => 1,
    };

    // Parse the JSON response from stdout
    if (stdout_n > 0) {
        const parsed = std.json.parseFromSlice(VsockResponse, allocator, stdout_buf[0..stdout_n], .{
            .ignore_unknown_fields = true,
            .allocate = .alloc_always,
        }) catch {
            // Failed to parse — return raw stdout as output
            return VsockExecResult{
                .exit_code = @as(i32, @intCast(socat_exit)),
                .stdout = try allocator.dupe(u8, stdout_buf[0..stdout_n]),
                .stderr = if (stderr_n > 0) try allocator.dupe(u8, stderr_buf[0..stderr_n]) else &[_]u8{},
            };
        };
        defer parsed.deinit();

        return VsockExecResult{
            .exit_code = parsed.value.exit_code,
            .stdout = if (parsed.value.stdout.len > 0) try allocator.dupe(u8, parsed.value.stdout) else &[_]u8{},
            .stderr = if (parsed.value.stderr.len > 0) try allocator.dupe(u8, parsed.value.stderr) else &[_]u8{},
        };
    }

    return VsockExecResult{
        .exit_code = @as(i32, @intCast(socat_exit)),
        .stdout = &[_]u8{},
        .stderr = if (stderr_n > 0) try allocator.dupe(u8, stderr_buf[0..stderr_n]) else &[_]u8{},
    };
}

/// JSON response from the guest agent over vsock.
const VsockResponse = struct {
    exit_code: i32 = -1,
    stdout: []const u8 = "",
    stderr: []const u8 = "",
};

// ── Drive Hot-Add ────────────────────────────────────────────────────

/// Hot-add a drive to a running Firecracker VM, then trigger guest remount.
pub fn addDrive(
    id: []const u8,
    drive_id: []const u8,
    squashfs_path: []const u8,
    cid: u32,
    meta_dir: []const u8,
    allocator: std.mem.Allocator,
) !void {
    // sq-firecracker add-drive {id} {drive-id} {path} --meta-dir {meta_dir}
    try runCommand(allocator, &.{
        "sq-firecracker", "add-drive", id, drive_id, squashfs_path,
        "--meta-dir",     meta_dir,
    });

    // Tell the guest to remount via vsock
    var result = try execVsock(cid, "__squash_remount", "/", 30, allocator);
    result.deinit(allocator);

    log.info("drive added: id={s} drive={s}", .{ id, drive_id });
}

// ── CID File Helpers ─────────────────────────────────────────────────

/// Read the CID from a sandbox's .meta/fc.cid file.
pub fn readCid(sandbox_path: []const u8) !u32 {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cid_path = try std.fmt.bufPrint(&path_buf, "{s}/.meta/fc.cid", .{sandbox_path});

    const file = try std.fs.openFileAbsolute(cid_path, .{ .mode = .read_only });
    defer file.close();

    var buf: [32]u8 = undefined;
    const n = try file.read(&buf);
    const trimmed = std.mem.trim(u8, buf[0..n], &std.ascii.whitespace);
    return std.fmt.parseInt(u32, trimmed, 10) catch error.InvalidCharacter;
}

/// Write the CID to a sandbox's .meta/fc.cid file.
pub fn writeCid(sandbox_path: []const u8, cid: u32) void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cid_path = std.fmt.bufPrint(&path_buf, "{s}/.meta/fc.cid", .{sandbox_path}) catch return;

    const file = std.fs.createFileAbsolute(cid_path, .{}) catch return;
    defer file.close();

    var cid_buf: [32]u8 = undefined;
    const cid_str = std.fmt.bufPrint(&cid_buf, "{d}", .{cid}) catch return;
    file.writeAll(cid_str) catch {};
}

/// Read the network index from a sandbox's .meta/fc.net_index file.
pub fn readNetIndex(sandbox_path: []const u8) !u8 {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try std.fmt.bufPrint(&path_buf, "{s}/.meta/fc.net_index", .{sandbox_path});

    const file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
    defer file.close();

    var buf: [32]u8 = undefined;
    const n = try file.read(&buf);
    const trimmed = std.mem.trim(u8, buf[0..n], &std.ascii.whitespace);
    return std.fmt.parseInt(u8, trimmed, 10) catch error.InvalidCharacter;
}

/// Write the network index to a sandbox's .meta/fc.net_index file.
pub fn writeNetIndex(sandbox_path: []const u8, index: u8) void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/.meta/fc.net_index", .{sandbox_path}) catch return;

    const file = std.fs.createFileAbsolute(path, .{}) catch return;
    defer file.close();

    var idx_buf: [8]u8 = undefined;
    const idx_str = std.fmt.bufPrint(&idx_buf, "{d}", .{index}) catch return;
    file.writeAll(idx_str) catch {};
}

// ── Internal Helpers ─────────────────────────────────────────────────

/// Run an external command synchronously. Returns error on non-zero exit.
fn runCommand(allocator: std.mem.Allocator, argv: []const []const u8) !void {
    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Close;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    // Drain pipes to avoid deadlock
    var stdout_buf: [4096]u8 = undefined;
    var stderr_buf: [4096]u8 = undefined;
    _ = child.stdout.?.read(&stdout_buf) catch 0;
    _ = child.stderr.?.read(&stderr_buf) catch 0;

    const term = try child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                log.warn("{s} exited with code {d}", .{ argv[0], code });
                return error.CommandFailed;
            }
        },
        else => {
            log.warn("{s} terminated abnormally", .{argv[0]});
            return error.CommandFailed;
        },
    }
}

/// Write content to a file.
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

/// Check if a string is one we allocated (vs a borrowed static/slice).
/// Used to avoid double-freeing borrowed strings in argv cleanup.
fn isAllocated(item: []const u8, id: []const u8, paths: []const []const u8, meta_dir: []const u8) bool {
    // Skip known static/borrowed strings
    if (std.mem.eql(u8, item, "sq-firecracker")) return false;
    if (std.mem.eql(u8, item, "start")) return false;
    if (std.mem.eql(u8, item, "--cid")) return false;
    if (std.mem.eql(u8, item, "--meta-dir")) return false;
    if (item.ptr == id.ptr) return false;
    if (item.ptr == meta_dir.ptr) return false;
    for (paths) |p| {
        if (item.ptr == p.ptr) return false;
    }
    return true;
}

// ── Tests ────────────────────────────────────────────────────────────

test "CID file round-trip" {
    const test_dir = "/tmp/sq-fc-test-cid";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    std.fs.makeDirAbsolute(test_dir) catch {};
    var meta_buf: [256]u8 = undefined;
    const meta_path = std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{test_dir}) catch unreachable;
    std.fs.makeDirAbsolute(meta_path) catch {};

    writeCid(test_dir, 142);
    const cid = try readCid(test_dir);
    try std.testing.expectEqual(@as(u32, 142), cid);
}

test "net index file round-trip" {
    const test_dir = "/tmp/sq-fc-test-idx";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    std.fs.makeDirAbsolute(test_dir) catch {};
    var meta_buf: [256]u8 = undefined;
    const meta_path = std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{test_dir}) catch unreachable;
    std.fs.makeDirAbsolute(meta_path) catch {};

    writeNetIndex(test_dir, 7);
    const idx = try readNetIndex(test_dir);
    try std.testing.expectEqual(@as(u8, 7), idx);
}

test "readCid fails on missing file" {
    const result = readCid("/tmp/sq-fc-nonexistent");
    try std.testing.expectError(error.FileNotFound, result);
}

test "readNetIndex fails on missing file" {
    const result = readNetIndex("/tmp/sq-fc-nonexistent");
    try std.testing.expectError(error.FileNotFound, result);
}

test "VsockExecResult deinit handles empty output" {
    var result = VsockExecResult{
        .exit_code = 0,
        .stdout = &[_]u8{},
        .stderr = &[_]u8{},
    };
    result.deinit(std.testing.allocator);
}

test "VsockExecResult deinit frees allocations" {
    const allocator = std.testing.allocator;
    const stdout = try allocator.dupe(u8, "hello");
    const stderr = try allocator.dupe(u8, "world");

    var result = VsockExecResult{
        .exit_code = 0,
        .stdout = stdout,
        .stderr = stderr,
    };
    result.deinit(allocator);
}

test "isAllocated identifies borrowed strings" {
    const id = "test-id";
    const paths = [_][]const u8{ "/path/a", "/path/b" };
    const meta = "/meta/dir";

    try std.testing.expect(!isAllocated("sq-firecracker", id, &paths, meta));
    try std.testing.expect(!isAllocated("start", id, &paths, meta));
    try std.testing.expect(!isAllocated("--cid", id, &paths, meta));
    try std.testing.expect(!isAllocated("--meta-dir", id, &paths, meta));
    try std.testing.expect(!isAllocated(id, id, &paths, meta));
    try std.testing.expect(!isAllocated(meta, id, &paths, meta));

    // An independently allocated string should return true
    const allocator = std.testing.allocator;
    const allocated = try allocator.dupe(u8, "123");
    defer allocator.free(allocated);
    try std.testing.expect(isAllocated(allocated, id, &paths, meta));
}
