const std = @import("std");

/// Handle for a cgroup v2 sandbox group under /sys/fs/cgroup/squash-{id}.
///
/// Manages CPU and memory limits. On deinit, moves all processes back to
/// the root cgroup before removing the directory.
pub const CgroupHandle = struct {
    path_buf: [256]u8,
    path_len: usize,

    pub fn create(id: []const u8, cpu: f64, memory_mb: u64) !CgroupHandle {
        var handle = CgroupHandle{ .path_buf = undefined, .path_len = 0 };
        const cgroup_path = std.fmt.bufPrint(&handle.path_buf, "/sys/fs/cgroup/squash-{s}", .{id}) catch
            return error.PathTooLong;
        handle.path_len = cgroup_path.len;

        std.fs.makeDirAbsolute(cgroup_path) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };
        errdefer std.fs.deleteDirAbsolute(cgroup_path) catch {};

        // CPU: quota = cpu * 100000, period = 100000
        const quota: u64 = @intFromFloat(cpu * 100_000.0);
        var cpu_buf: [64]u8 = undefined;
        const cpu_str = std.fmt.bufPrint(&cpu_buf, "{d} 100000", .{quota}) catch unreachable;
        try writeFileInDir(cgroup_path, "cpu.max", cpu_str);

        // Memory limit in bytes
        var mem_buf: [64]u8 = undefined;
        const mem_str = std.fmt.bufPrint(&mem_buf, "{d}", .{memory_mb * 1024 * 1024}) catch unreachable;
        try writeFileInDir(cgroup_path, "memory.max", mem_str);

        return handle;
    }

    /// Add a process to this cgroup by writing its PID to cgroup.procs.
    pub fn addProcess(self: *const CgroupHandle, pid: std.posix.pid_t) !void {
        var pid_buf: [16]u8 = undefined;
        const pid_str = std.fmt.bufPrint(&pid_buf, "{d}", .{pid}) catch unreachable;
        try writeFileInDir(self.path(), "cgroup.procs", pid_str);
    }

    pub fn path(self: *const CgroupHandle) []const u8 {
        return self.path_buf[0..self.path_len];
    }

    /// Idempotent cleanup: move all processes to root cgroup, then rmdir.
    /// Ignores all errors (matching shell `|| true` pattern).
    pub fn deinit(self: *CgroupHandle) void {
        // Read all PIDs in this cgroup
        var procs_path_buf: [512]u8 = undefined;
        const procs_path = std.fmt.bufPrint(&procs_path_buf, "{s}/cgroup.procs", .{self.path()}) catch return;

        const procs_content = readFileAlloc(procs_path) catch return;
        defer std.heap.page_allocator.free(procs_content);

        if (procs_content.len > 0) {
            // Move each PID individually to the root cgroup.
            // Writing all PIDs at once can fail if any single PID is dead,
            // so we write them one-by-one and ignore individual failures.
            var iter = std.mem.splitScalar(u8, procs_content, '\n');
            while (iter.next()) |line| {
                const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
                if (trimmed.len == 0) continue;
                writeFileInDir("/sys/fs/cgroup", "cgroup.procs", trimmed) catch {};
            }
        }

        std.fs.deleteDirAbsolute(self.path()) catch {};
    }
};

/// Write `content` to `dir/filename`, truncating any existing content.
fn writeFileInDir(dir_path: []const u8, filename: []const u8, content: []const u8) !void {
    var path_buf: [512]u8 = undefined;
    const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ dir_path, filename }) catch
        return error.PathTooLong;

    const file = try std.fs.openFileAbsolute(full_path, .{ .mode = .write_only });
    defer file.close();
    try file.writeAll(content);
}

/// Read entire file contents using the page allocator.
fn readFileAlloc(file_path: []const u8) ![]u8 {
    const file = try std.fs.openFileAbsolute(file_path, .{ .mode = .read_only });
    defer file.close();
    return file.readToEndAlloc(std.heap.page_allocator, 64 * 1024);
}

// ── Tests ────────────────────────────────────────────────────────────

test "CgroupHandle path formatting" {
    var handle = CgroupHandle{ .path_buf = undefined, .path_len = 0 };
    const p = std.fmt.bufPrint(&handle.path_buf, "/sys/fs/cgroup/squash-{s}", .{"test-123"}) catch unreachable;
    handle.path_len = p.len;
    try std.testing.expectEqualStrings("/sys/fs/cgroup/squash-test-123", handle.path());
}

test "CgroupHandle path too long" {
    // 256-byte path_buf should reject IDs that make the path exceed 256
    const long_id = "a" ** 250;
    const result = CgroupHandle.create(long_id, 1.0, 512);
    try std.testing.expectError(error.PathTooLong, result);
}

test "writeFileInDir path formatting" {
    // Just verify the path buffer overflow detection works
    const long_dir = "/" ++ "x" ** 500;
    const result = writeFileInDir(long_dir, "test", "data");
    // Can be either PathTooLong (from our check) or NameTooLong (from OS)
    if (result) |_| {
        try std.testing.expect(false);
    } else |err| {
        try std.testing.expect(err == error.PathTooLong or err == error.NameTooLong);
    }
}

test "CgroupHandle deinit is safe on zero-length path" {
    // deinit should not crash even with an empty handle
    var handle = CgroupHandle{ .path_buf = undefined, .path_len = 0 };
    handle.deinit(); // should not panic
}
