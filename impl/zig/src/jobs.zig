// jobs.zig — Background job registry for long-running exec commands.
//
// Tracks background processes spawned via exec-bg. Each job has:
// - A unique ID (incrementing counter)
// - The sandbox it belongs to
// - A child process PID
// - Status (running/completed/failed/timed_out)
// - Paths to stdout/stderr JSONL files on disk
//
// Thread-safe: all access goes through a mutex-protected registry.

const std = @import("std");
const log = std.log.scoped(.jobs);

pub const JobStatus = enum {
    running,
    completed,
    failed,
    timed_out,
};

pub const JobEntry = struct {
    id: u32,
    sandbox_id_buf: [64]u8,
    sandbox_id_len: usize,
    cmd_buf: [1024]u8,
    cmd_len: usize,
    status: JobStatus,
    exit_code: ?i32,
    pid: ?std.posix.pid_t,
    started: i64,
    finished: ?i64,
    stdout_path_buf: [std.fs.max_path_bytes]u8,
    stdout_path_len: usize,
    stderr_path_buf: [std.fs.max_path_bytes]u8,
    stderr_path_len: usize,

    pub fn sandboxId(self: *const JobEntry) []const u8 {
        return self.sandbox_id_buf[0..self.sandbox_id_len];
    }

    pub fn cmd(self: *const JobEntry) []const u8 {
        return self.cmd_buf[0..self.cmd_len];
    }

    pub fn stdoutPath(self: *const JobEntry) []const u8 {
        return self.stdout_path_buf[0..self.stdout_path_len];
    }

    pub fn stderrPath(self: *const JobEntry) []const u8 {
        return self.stderr_path_buf[0..self.stderr_path_len];
    }
};

/// Thread-safe registry of background jobs.
pub const JobRegistry = struct {
    mutex: std.Thread.Mutex,
    entries: std.AutoHashMap(u32, JobEntry),
    next_id: std.atomic.Value(u32),

    pub fn init(allocator: std.mem.Allocator) JobRegistry {
        return .{
            .mutex = .{},
            .entries = std.AutoHashMap(u32, JobEntry).init(allocator),
            .next_id = std.atomic.Value(u32).init(1),
        };
    }

    pub fn deinit(self: *JobRegistry) void {
        self.entries.deinit();
    }

    /// Create a new job entry and return its ID.
    pub fn create(
        self: *JobRegistry,
        sandbox_id: []const u8,
        cmd_str: []const u8,
        sandbox_path: []const u8,
    ) !u32 {
        const id = self.next_id.fetchAdd(1, .monotonic);

        var entry = JobEntry{
            .id = id,
            .sandbox_id_buf = undefined,
            .sandbox_id_len = @min(sandbox_id.len, 64),
            .cmd_buf = undefined,
            .cmd_len = @min(cmd_str.len, 1024),
            .status = .running,
            .exit_code = null,
            .pid = null,
            .started = std.time.timestamp(),
            .finished = null,
            .stdout_path_buf = undefined,
            .stdout_path_len = 0,
            .stderr_path_buf = undefined,
            .stderr_path_len = 0,
        };

        @memcpy(entry.sandbox_id_buf[0..entry.sandbox_id_len], sandbox_id[0..entry.sandbox_id_len]);
        @memcpy(entry.cmd_buf[0..entry.cmd_len], cmd_str[0..entry.cmd_len]);

        // Build stdout/stderr paths: {sandbox_path}/.meta/jobs/{id}/stdout.jsonl
        const stdout_path = std.fmt.bufPrint(&entry.stdout_path_buf, "{s}/.meta/jobs/{d}/stdout.jsonl", .{ sandbox_path, id }) catch
            return error.PathTooLong;
        entry.stdout_path_len = stdout_path.len;

        const stderr_path = std.fmt.bufPrint(&entry.stderr_path_buf, "{s}/.meta/jobs/{d}/stderr.jsonl", .{ sandbox_path, id }) catch
            return error.PathTooLong;
        entry.stderr_path_len = stderr_path.len;

        // Create job directory hierarchy: {sandbox_path}/.meta/jobs/{id}
        // Create each level since makeDirAbsolute is non-recursive.
        var meta_buf: [std.fs.max_path_bytes]u8 = undefined;
        var jobs_buf: [std.fs.max_path_bytes]u8 = undefined;
        var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
        const meta_path = std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{sandbox_path}) catch return error.PathTooLong;
        const jobs_path = std.fmt.bufPrint(&jobs_buf, "{s}/.meta/jobs", .{sandbox_path}) catch return error.PathTooLong;
        const dir_path = std.fmt.bufPrint(&dir_buf, "{s}/.meta/jobs/{d}", .{ sandbox_path, id }) catch return error.PathTooLong;

        for ([_][]const u8{ meta_path, jobs_path, dir_path }) |path| {
            std.fs.makeDirAbsolute(path) catch |err| switch (err) {
                error.PathAlreadyExists => {},
                else => {
                    log.warn("failed to create dir {s}: {}", .{ path, err });
                    return error.IoError;
                },
            };
        }

        self.mutex.lock();
        defer self.mutex.unlock();
        self.entries.put(id, entry) catch return error.OutOfMemory;

        return id;
    }

    /// Get a copy of a job entry by ID.
    pub fn get(self: *JobRegistry, id: u32) ?JobEntry {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.entries.get(id);
    }

    /// Update job status and exit code.
    pub fn complete(self: *JobRegistry, id: u32, exit_code: i32, status: JobStatus) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.entries.getPtr(id)) |entry| {
            entry.status = status;
            entry.exit_code = exit_code;
            entry.finished = std.time.timestamp();
        }
    }

    /// Set the PID of a running job.
    pub fn setPid(self: *JobRegistry, id: u32, pid: std.posix.pid_t) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.entries.getPtr(id)) |entry| {
            entry.pid = pid;
        }
    }

    /// List all jobs for a given sandbox.
    pub fn list(self: *JobRegistry, allocator: std.mem.Allocator, sandbox_id: []const u8) ![]JobEntry {
        self.mutex.lock();
        defer self.mutex.unlock();

        var result: std.ArrayList(JobEntry) = .empty;
        errdefer result.deinit(allocator);

        var iter = self.entries.valueIterator();
        while (iter.next()) |entry| {
            if (std.mem.eql(u8, entry.sandboxId(), sandbox_id)) {
                try result.append(allocator, entry.*);
            }
        }

        return result.toOwnedSlice(allocator);
    }

    /// Remove all jobs for a sandbox.
    pub fn cleanup(self: *JobRegistry, sandbox_id: []const u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Collect IDs to remove
        var to_remove: [256]u32 = undefined;
        var count: usize = 0;

        var iter = self.entries.iterator();
        while (iter.next()) |kv| {
            if (std.mem.eql(u8, kv.value_ptr.sandboxId(), sandbox_id)) {
                if (count < 256) {
                    to_remove[count] = kv.key_ptr.*;
                    count += 1;
                }
            }
        }

        for (to_remove[0..count]) |id| {
            _ = self.entries.remove(id);
        }
    }
};

// ── Global instance ─────────────────────────────────────────────────

var global_registry: ?*JobRegistry = null;

pub fn setGlobal(registry: *JobRegistry) void {
    global_registry = registry;
}

pub fn getGlobal() ?*JobRegistry {
    return global_registry;
}

// ── Tests ───────────────────────────────────────────────────────────

fn ensureTestDir(path: []const u8) void {
    std.fs.makeDirAbsolute(path) catch {};
}

test "JobRegistry create and get" {
    var registry = JobRegistry.init(std.testing.allocator);
    defer registry.deinit();

    ensureTestDir("/tmp/test-jobs");
    defer std.fs.deleteTreeAbsolute("/tmp/test-jobs") catch {};

    const id = try registry.create("test-sandbox", "echo hello", "/tmp/test-jobs");

    try std.testing.expectEqual(@as(u32, 1), id);

    const entry = registry.get(id).?;
    try std.testing.expectEqualStrings("test-sandbox", entry.sandboxId());
    try std.testing.expectEqualStrings("echo hello", entry.cmd());
    try std.testing.expectEqual(JobStatus.running, entry.status);
    try std.testing.expectEqual(@as(?i32, null), entry.exit_code);
}

test "JobRegistry complete updates status" {
    var registry = JobRegistry.init(std.testing.allocator);
    defer registry.deinit();

    ensureTestDir("/tmp/test-jobs-complete");
    defer std.fs.deleteTreeAbsolute("/tmp/test-jobs-complete") catch {};

    const id = try registry.create("sb1", "ls", "/tmp/test-jobs-complete");

    registry.complete(id, 0, .completed);

    const entry = registry.get(id).?;
    try std.testing.expectEqual(JobStatus.completed, entry.status);
    try std.testing.expectEqual(@as(?i32, 0), entry.exit_code);
    try std.testing.expect(entry.finished != null);
}

test "JobRegistry list filters by sandbox" {
    var registry = JobRegistry.init(std.testing.allocator);
    defer registry.deinit();

    ensureTestDir("/tmp/test-jobs-list");
    defer std.fs.deleteTreeAbsolute("/tmp/test-jobs-list") catch {};

    _ = try registry.create("sb-a", "cmd1", "/tmp/test-jobs-list");
    _ = try registry.create("sb-b", "cmd2", "/tmp/test-jobs-list");
    _ = try registry.create("sb-a", "cmd3", "/tmp/test-jobs-list");

    const jobs = try registry.list(std.testing.allocator, "sb-a");
    defer std.testing.allocator.free(jobs);

    try std.testing.expectEqual(@as(usize, 2), jobs.len);
}

test "JobRegistry cleanup removes sandbox jobs" {
    var registry = JobRegistry.init(std.testing.allocator);
    defer registry.deinit();

    ensureTestDir("/tmp/test-jobs-cleanup");
    defer std.fs.deleteTreeAbsolute("/tmp/test-jobs-cleanup") catch {};

    _ = try registry.create("sb-x", "cmd1", "/tmp/test-jobs-cleanup");
    _ = try registry.create("sb-y", "cmd2", "/tmp/test-jobs-cleanup");
    _ = try registry.create("sb-x", "cmd3", "/tmp/test-jobs-cleanup");

    registry.cleanup("sb-x");

    const jobs = try registry.list(std.testing.allocator, "sb-x");
    defer std.testing.allocator.free(jobs);
    try std.testing.expectEqual(@as(usize, 0), jobs.len);

    // sb-y should still exist
    const jobs_y = try registry.list(std.testing.allocator, "sb-y");
    defer std.testing.allocator.free(jobs_y);
    try std.testing.expectEqual(@as(usize, 1), jobs_y.len);
}

test "JobRegistry ID increments" {
    var registry = JobRegistry.init(std.testing.allocator);
    defer registry.deinit();

    ensureTestDir("/tmp/test-jobs-inc");
    defer std.fs.deleteTreeAbsolute("/tmp/test-jobs-inc") catch {};

    const id1 = try registry.create("sb", "a", "/tmp/test-jobs-inc");
    const id2 = try registry.create("sb", "b", "/tmp/test-jobs-inc");
    const id3 = try registry.create("sb", "c", "/tmp/test-jobs-inc");

    try std.testing.expectEqual(@as(u32, 1), id1);
    try std.testing.expectEqual(@as(u32, 2), id2);
    try std.testing.expectEqual(@as(u32, 3), id3);
}
