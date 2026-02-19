// exec.zig — Sandbox execution via sq-exec helper script.
//
// Shells out to `sq-exec <merged-root> <cmd> [workdir] [timeout]` which
// handles PID/IPC/UTS isolation via bubblewrap or unshare+chroot.

const std = @import("std");
const posix = std.posix;
const log = std.log.scoped(.exec);
const json_mod = @import("json.zig");

/// Maximum bytes captured from stdout/stderr (64KB each).
pub const max_output_bytes: usize = 65536;

/// Request to execute a command inside a sandbox.
pub const ExecRequest = struct {
    /// Command string passed to /bin/sh -c.
    cmd: []const u8,
    /// Working directory inside the chroot. Defaults to "/".
    workdir: []const u8 = "/",
    /// Timeout in seconds. 0 = no timeout.
    timeout_s: u64 = 300,
};

/// Result of a sandbox execution.
pub const ExecResult = struct {
    exit_code: i32,
    stdout: []const u8,
    stderr: []const u8,
    started: i64,
    finished: i64,
    timed_out: bool,
    seq: u32,

    pub fn deinit(self: *ExecResult, allocator: std.mem.Allocator) void {
        if (self.stdout.len > 0) allocator.free(self.stdout);
        if (self.stderr.len > 0) allocator.free(self.stderr);
        self.* = undefined;
    }
};

/// Atomic counter for exec log sequence numbering.
/// One instance per sandbox, shared across all exec calls for that sandbox.
pub const SeqCounter = struct {
    value: std.atomic.Value(u32) = std.atomic.Value(u32).init(0),

    /// Atomically increment and return the new sequence number (1-based).
    pub fn next(self: *SeqCounter) u32 {
        return self.value.fetchAdd(1, .monotonic) + 1;
    }

    /// Get the current count without incrementing.
    pub fn current(self: *const SeqCounter) u32 {
        return self.value.load(.monotonic);
    }
};

/// Parameters describing the sandbox environment for exec.
/// Other modules (sandbox.zig, manager.zig) populate this from their state.
pub const SandboxContext = struct {
    /// Absolute path to the merged overlayfs directory (sandbox root).
    merged_path: []const u8,
    /// Absolute path to the exec log directory for this sandbox. Null to skip logging.
    log_dir: ?[]const u8 = null,
    /// Atomic sequence counter shared across execs for this sandbox.
    seq_counter: ?*SeqCounter = null,
};

pub const ExecError = error{
    ForkFailed,
    PipeCreationFailed,
    PollFailed,
    WaitFailed,
    OutputAlloc,
};

/// Execute a command inside a sandboxed environment by shelling out to sq-exec.
///
/// sq-exec <merged-root> <cmd> [workdir] [timeout]
pub fn execInSandbox(
    allocator: std.mem.Allocator,
    ctx: SandboxContext,
    req: ExecRequest,
) ExecError!ExecResult {
    const started = std.time.timestamp();

    // Format timeout as string
    var timeout_buf: [16]u8 = undefined;
    const timeout_str = std.fmt.bufPrint(&timeout_buf, "{d}", .{req.timeout_s}) catch
        return ExecError.ForkFailed;

    var child = std.process.Child.init(
        &.{ "sq-exec", ctx.merged_path, req.cmd, req.workdir, timeout_str },
        allocator,
    );
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.stdin_behavior = .Close;

    child.spawn() catch return ExecError.ForkFailed;

    // Read stdout/stderr from pipes
    var stdout_buf: [max_output_bytes]u8 = undefined;
    var stderr_buf: [max_output_bytes]u8 = undefined;
    var stdout_len: usize = 0;
    var stderr_len: usize = 0;

    // Read stdout
    if (child.stdout) |*stdout_stream| {
        while (stdout_len < max_output_bytes) {
            const n = stdout_stream.read(stdout_buf[stdout_len..]) catch break;
            if (n == 0) break;
            stdout_len += n;
        }
    }

    // Read stderr
    if (child.stderr) |*stderr_stream| {
        while (stderr_len < max_output_bytes) {
            const n = stderr_stream.read(stderr_buf[stderr_len..]) catch break;
            if (n == 0) break;
            stderr_len += n;
        }
    }

    // Wait for child to exit
    const term = child.wait() catch return ExecError.WaitFailed;
    const finished = std.time.timestamp();

    const exit_code: i32 = switch (term) {
        .Exited => |code| @intCast(code),
        .Signal => |sig| 128 + @as(i32, @intCast(sig)),
        else => -1,
    };

    // sq-exec uses timeout(1) which returns 124 on timeout
    const timed_out = exit_code == 124;
    const final_exit_code = exit_code;

    // Get sequence number and write exec log
    const seq: u32 = if (ctx.seq_counter) |counter| counter.next() else 0;

    if (ctx.log_dir != null and ctx.seq_counter != null) {
        writeExecLog(
            ctx.log_dir.?,
            seq,
            req,
            final_exit_code,
            started,
            finished,
            stdout_buf[0..stdout_len],
            stderr_buf[0..stderr_len],
            timed_out,
        );
    }

    // Copy output to caller-owned memory
    const stdout_copy = if (stdout_len > 0)
        allocator.dupe(u8, stdout_buf[0..stdout_len]) catch return ExecError.OutputAlloc
    else
        &[_]u8{};

    const stderr_copy = if (stderr_len > 0)
        (allocator.dupe(u8, stderr_buf[0..stderr_len]) catch {
            if (stdout_len > 0) allocator.free(stdout_copy);
            return ExecError.OutputAlloc;
        })
    else
        &[_]u8{};

    return ExecResult{
        .exit_code = final_exit_code,
        .stdout = stdout_copy,
        .stderr = stderr_copy,
        .started = started,
        .finished = finished,
        .timed_out = timed_out,
        .seq = seq,
    };
}

// ── Exec Logging ────────────────────────────────────────────────────────

/// Write an exec log entry as a JSON file to the sandbox's log directory.
/// File naming: {seq:0>4}.json (e.g. 0001.json, 0002.json).
/// Best-effort — log failures are warned but don't fail the exec.
fn writeExecLog(
    log_dir: []const u8,
    seq: u32,
    req: ExecRequest,
    exit_code: i32,
    started: i64,
    finished: i64,
    stdout_data: []const u8,
    stderr_data: []const u8,
    timed_out: bool,
) void {
    // Format timestamps as ISO 8601
    var started_buf: [32]u8 = undefined;
    var finished_buf: [32]u8 = undefined;
    const started_str = json_mod.formatTimestamp(&started_buf, started) catch "?";
    const finished_str = json_mod.formatTimestamp(&finished_buf, finished) catch "?";

    // Build the log file path: {log_dir}/{seq:0>4}.json
    var path_buf: [4096]u8 = undefined;
    const file_path = std.fmt.bufPrint(&path_buf, "{s}/{d:0>4}.json", .{ log_dir, seq }) catch {
        log.warn("exec log path too long for seq {d}", .{seq});
        return;
    };

    // Build JSON content — use a fixed buffer to avoid allocation.
    var buf: [max_output_bytes * 6 * 2 + 4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    writeLogJson(writer, seq, req, exit_code, started_str, finished_str, stdout_data, stderr_data, timed_out) catch {
        log.warn("exec log JSON write failed for seq {d}", .{seq});
        return;
    };

    const json_bytes = stream.getWritten();

    // Ensure log directory exists
    std.fs.makeDirAbsolute(log_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            log.warn("failed to create log dir {s}: {}", .{ log_dir, err });
            return;
        },
    };

    // Write the file
    const file = std.fs.createFileAbsolute(file_path, .{}) catch |err| {
        log.warn("failed to create exec log {s}: {}", .{ file_path, err });
        return;
    };
    defer file.close();

    file.writeAll(json_bytes) catch |err| {
        log.warn("failed to write exec log {s}: {}", .{ file_path, err });
    };
}

/// Write the exec log JSON structure to a writer.
/// Separated from writeExecLog for testability.
pub fn writeLogJson(
    writer: anytype,
    seq: u32,
    req: ExecRequest,
    exit_code: i32,
    started_str: []const u8,
    finished_str: []const u8,
    stdout_data: []const u8,
    stderr_data: []const u8,
    timed_out: bool,
) !void {
    try writer.writeAll("{");
    try writer.print("\"seq\":{d},", .{seq});
    try writeJsonString(writer, "cmd", req.cmd);
    try writer.writeAll(",");
    try writeJsonString(writer, "workdir", req.workdir);
    try writer.print(",\"exit_code\":{d},", .{exit_code});
    try writeJsonString(writer, "started", started_str);
    try writer.writeAll(",");
    try writeJsonString(writer, "finished", finished_str);
    try writer.writeAll(",");
    try writeJsonString(writer, "stdout", stdout_data);
    try writer.writeAll(",");
    try writeJsonString(writer, "stderr", stderr_data);
    try writer.print(",\"timed_out\":{}", .{timed_out});
    try writer.writeAll("}");
}

/// Write a JSON key-value pair where the value is a string that needs escaping.
fn writeJsonString(writer: anytype, key: []const u8, value: []const u8) !void {
    try writer.print("\"{s}\":\"", .{key});
    for (value) |byte| {
        switch (byte) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x00...0x08, 0x0b, 0x0c, 0x0e...0x1f => {
                try writer.print("\\u{x:0>4}", .{byte});
            },
            else => try writer.writeByte(byte),
        }
    }
    try writer.writeAll("\"");
}

/// Read all exec log entries from a sandbox's log directory.
/// Returns entries sorted by sequence number. Caller owns returned slice and
/// must free with freeExecLogs.
pub fn readExecLogs(allocator: std.mem.Allocator, log_dir: []const u8) ![]json_mod.ExecResult {
    var dir = std.fs.openDirAbsolute(log_dir, .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => return &[_]json_mod.ExecResult{},
        else => return err,
    };
    defer dir.close();

    var entries: std.ArrayList(json_mod.ExecResult) = .empty;
    errdefer entries.deinit(allocator);

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".json")) continue;
        if (std.mem.endsWith(u8, entry.name, ".json.tmp")) continue;

        const content = dir.readFileAlloc(allocator, entry.name, max_output_bytes * 6 * 2 + 4096) catch continue;
        defer allocator.free(content);

        const parsed = std.json.parseFromSlice(json_mod.ExecResult, allocator, content, .{
            .ignore_unknown_fields = true,
            .allocate = .alloc_always,
        }) catch continue;
        defer parsed.deinit();

        // Dupe strings for caller ownership
        const result = json_mod.ExecResult{
            .seq = parsed.value.seq,
            .cmd = allocator.dupe(u8, parsed.value.cmd) catch continue,
            .workdir = allocator.dupe(u8, parsed.value.workdir) catch continue,
            .exit_code = parsed.value.exit_code,
            .started = allocator.dupe(u8, parsed.value.started) catch continue,
            .finished = allocator.dupe(u8, parsed.value.finished) catch continue,
            .stdout = allocator.dupe(u8, parsed.value.stdout) catch continue,
            .stderr = allocator.dupe(u8, parsed.value.stderr) catch continue,
        };
        try entries.append(allocator, result);
    }

    const items = try entries.toOwnedSlice(allocator);
    std.mem.sort(json_mod.ExecResult, items, {}, struct {
        fn lessThan(_: void, a: json_mod.ExecResult, b: json_mod.ExecResult) bool {
            return a.seq < b.seq;
        }
    }.lessThan);

    return items;
}

/// Free exec log entries returned by readExecLogs.
pub fn freeExecLogs(allocator: std.mem.Allocator, logs: []json_mod.ExecResult) void {
    for (logs) |entry| {
        allocator.free(entry.cmd);
        allocator.free(entry.workdir);
        allocator.free(entry.started);
        allocator.free(entry.finished);
        allocator.free(entry.stdout);
        allocator.free(entry.stderr);
    }
    allocator.free(logs);
}

// ── Tests ───────────────────────────────────────────────────────────────

test "ExecRequest defaults" {
    const req = ExecRequest{ .cmd = "echo hello" };
    try std.testing.expectEqualStrings("/", req.workdir);
    try std.testing.expectEqual(@as(u64, 300), req.timeout_s);
}

test "ExecResult deinit frees allocations" {
    const allocator = std.testing.allocator;
    const stdout_data = try allocator.dupe(u8, "hello");
    const stderr_data = try allocator.dupe(u8, "world");

    var result = ExecResult{
        .exit_code = 0,
        .stdout = stdout_data,
        .stderr = stderr_data,
        .started = 100,
        .finished = 200,
        .timed_out = false,
        .seq = 1,
    };
    result.deinit(allocator);
}

test "ExecResult deinit handles empty output" {
    const allocator = std.testing.allocator;
    var result = ExecResult{
        .exit_code = 0,
        .stdout = &[_]u8{},
        .stderr = &[_]u8{},
        .started = 100,
        .finished = 200,
        .timed_out = false,
        .seq = 1,
    };
    result.deinit(allocator);
}

test "SandboxContext defaults" {
    const ctx = SandboxContext{ .merged_path = "/tmp/merged" };
    try std.testing.expectEqual(@as(?[]const u8, null), ctx.log_dir);
    try std.testing.expectEqual(@as(?*SeqCounter, null), ctx.seq_counter);
}

test "max_output_bytes is 64KB" {
    try std.testing.expectEqual(@as(usize, 65536), max_output_bytes);
}

test "SeqCounter starts at 0 and increments" {
    var counter = SeqCounter{};
    try std.testing.expectEqual(@as(u32, 0), counter.current());
    try std.testing.expectEqual(@as(u32, 1), counter.next());
    try std.testing.expectEqual(@as(u32, 2), counter.next());
    try std.testing.expectEqual(@as(u32, 3), counter.next());
    try std.testing.expectEqual(@as(u32, 3), counter.current());
}

test "writeLogJson produces valid JSON" {
    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const req = ExecRequest{ .cmd = "echo hello", .workdir = "/tmp", .timeout_s = 60 };

    try writeLogJson(writer, 1, req, 0, "2025-01-15T00:00:00+00:00", "2025-01-15T00:00:05+00:00", "hello\n", "", false);

    const json_str = stream.getWritten();

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 1), obj.get("seq").?.integer);
    try std.testing.expectEqualStrings("echo hello", obj.get("cmd").?.string);
    try std.testing.expectEqualStrings("/tmp", obj.get("workdir").?.string);
    try std.testing.expectEqual(@as(i64, 0), obj.get("exit_code").?.integer);
    try std.testing.expectEqualStrings("hello\n", obj.get("stdout").?.string);
    try std.testing.expectEqualStrings("", obj.get("stderr").?.string);
    try std.testing.expectEqual(false, obj.get("timed_out").?.bool);
}

test "writeLogJson escapes special characters" {
    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const req = ExecRequest{ .cmd = "echo \"hello\\nworld\"" };

    try writeLogJson(writer, 1, req, 0, "t0", "t1", "line1\nline2\ttab", "err\"msg", false);

    const json_str = stream.getWritten();

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("echo \"hello\\nworld\"", obj.get("cmd").?.string);
    try std.testing.expectEqualStrings("line1\nline2\ttab", obj.get("stdout").?.string);
    try std.testing.expectEqualStrings("err\"msg", obj.get("stderr").?.string);
}

test "writeLogJson with timed_out true" {
    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    const req = ExecRequest{ .cmd = "sleep 100", .timeout_s = 1 };

    try writeLogJson(writer, 5, req, 124, "t0", "t1", "", "timed out", true);

    const json_str = stream.getWritten();

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 5), obj.get("seq").?.integer);
    try std.testing.expectEqual(@as(i64, 124), obj.get("exit_code").?.integer);
    try std.testing.expectEqual(true, obj.get("timed_out").?.bool);
}

test "writeExecLog creates log file" {
    const test_dir = "/tmp/squash-test-exec-log";

    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    const req = ExecRequest{ .cmd = "echo test", .workdir = "/" };

    writeExecLog(test_dir, 1, req, 0, 1736899200, 1736899205, "test output\n", "", false);

    const content = std.fs.cwd().readFileAlloc(std.testing.allocator, test_dir ++ "/0001.json", 8192) catch |err| {
        std.debug.print("Failed to read log file: {}\n", .{err});
        return err;
    };
    defer std.testing.allocator.free(content);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, content, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 1), obj.get("seq").?.integer);
    try std.testing.expectEqualStrings("echo test", obj.get("cmd").?.string);
    try std.testing.expectEqual(@as(i64, 0), obj.get("exit_code").?.integer);
    try std.testing.expectEqualStrings("test output\n", obj.get("stdout").?.string);
}

test "writeExecLog sequence numbering in filenames" {
    const test_dir = "/tmp/squash-test-exec-log-seq";

    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    const req = ExecRequest{ .cmd = "echo test" };

    writeExecLog(test_dir, 1, req, 0, 100, 101, "out1", "", false);
    writeExecLog(test_dir, 2, req, 1, 102, 103, "out2", "err2", false);
    writeExecLog(test_dir, 10, req, 0, 104, 105, "out3", "", false);

    const f1 = std.fs.cwd().readFileAlloc(std.testing.allocator, test_dir ++ "/0001.json", 8192) catch return error.SkipZigTest;
    defer std.testing.allocator.free(f1);
    const f2 = std.fs.cwd().readFileAlloc(std.testing.allocator, test_dir ++ "/0002.json", 8192) catch return error.SkipZigTest;
    defer std.testing.allocator.free(f2);
    const f3 = std.fs.cwd().readFileAlloc(std.testing.allocator, test_dir ++ "/0010.json", 8192) catch return error.SkipZigTest;
    defer std.testing.allocator.free(f3);

    const parsed2 = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, f2, .{});
    defer parsed2.deinit();
    try std.testing.expectEqual(@as(i64, 1), parsed2.value.object.get("exit_code").?.integer);
}

test "readExecLogs returns sorted entries" {
    const test_dir = "/tmp/squash-test-read-logs";

    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    const req1 = ExecRequest{ .cmd = "first" };
    const req2 = ExecRequest{ .cmd = "second" };
    const req3 = ExecRequest{ .cmd = "third" };

    writeExecLog(test_dir, 3, req3, 0, 300, 301, "three", "", false);
    writeExecLog(test_dir, 1, req1, 0, 100, 101, "one", "", false);
    writeExecLog(test_dir, 2, req2, 1, 200, 201, "two", "err", false);

    const logs = try readExecLogs(std.testing.allocator, test_dir);
    defer freeExecLogs(std.testing.allocator, logs);

    try std.testing.expectEqual(@as(usize, 3), logs.len);

    try std.testing.expectEqual(@as(u32, 1), logs[0].seq);
    try std.testing.expectEqualStrings("first", logs[0].cmd);
    try std.testing.expectEqualStrings("one", logs[0].stdout);

    try std.testing.expectEqual(@as(u32, 2), logs[1].seq);
    try std.testing.expectEqualStrings("second", logs[1].cmd);
    try std.testing.expectEqual(@as(i32, 1), logs[1].exit_code);

    try std.testing.expectEqual(@as(u32, 3), logs[2].seq);
    try std.testing.expectEqualStrings("third", logs[2].cmd);
}

test "readExecLogs returns empty for missing directory" {
    const logs = try readExecLogs(std.testing.allocator, "/tmp/squash-nonexistent-dir-12345");
    try std.testing.expectEqual(@as(usize, 0), logs.len);
}

test "writeJsonString handles control characters" {
    var buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    try writeJsonString(writer, "val", "a\x00b\x01c");

    const output = stream.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, output, "\\u0000") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "\\u0001") != null);
}
