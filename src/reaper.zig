// reaper.zig — Background thread that periodically scans for expired
// sandboxes and destroys them based on max_lifetime_s.
//
// Runs on a dedicated thread with a sleep(10) scan loop.
// Each sandbox has an optional max_lifetime_s; if the sandbox has been
// alive longer than that, the reaper destroys it.

const std = @import("std");
const config = @import("config.zig");
const log = std.log.scoped(.reaper);

/// Metadata for a tracked sandbox used by the reaper.
pub const SandboxMeta = struct {
    id: []const u8,
    created_ts: i64,
    max_lifetime_s: u64, // 0 = no expiry
};

/// Callback interface for sandbox destruction.
/// The reaper doesn't own the sandbox manager directly — it uses this
/// function pointer to call back into the manager's destroy method.
/// This avoids a circular dependency between reaper and manager modules.
pub const DestroyFn = *const fn (id: []const u8) void;

/// Callback interface for listing sandboxes and their metadata.
/// Returns a snapshot of sandbox metadata for the reaper to evaluate.
/// The returned slice is owned by the caller and must be freed.
pub const ListFn = *const fn (allocator: std.mem.Allocator) ?[]SandboxMeta;

/// Configuration for the reaper thread.
pub const ReaperConfig = struct {
    /// Scan interval in seconds.
    interval_s: u64 = 10,
    /// Callback to list sandbox metadata.
    list_fn: ListFn,
    /// Callback to destroy a sandbox by ID.
    destroy_fn: DestroyFn,
};

/// Entry point for the reaper background thread.
/// This function never returns — it loops forever scanning for expired sandboxes.
///
/// Usage from main.zig:
///   const reaper_thread = try std.Thread.spawn(.{}, reaper.run, .{reaper_cfg});
///   reaper_thread.detach();
pub fn run(reaper_cfg: ReaperConfig) void {
    const interval_ns = reaper_cfg.interval_s * std.time.ns_per_s;

    log.info("reaper started — scan interval {d}s", .{reaper_cfg.interval_s});

    while (true) {
        std.Thread.sleep(interval_ns);
        scanAndReap(reaper_cfg);
    }
}

/// Perform a single scan-and-reap cycle.
/// Separated from the loop for testability.
pub fn scanAndReap(reaper_cfg: ReaperConfig) void {
    const now = std.time.timestamp();

    // Get a snapshot of all sandbox metadata
    const metas = reaper_cfg.list_fn(std.heap.page_allocator) orelse return;
    defer std.heap.page_allocator.free(metas);

    for (metas) |meta| {
        if (meta.max_lifetime_s == 0) continue; // no expiry

        const age: i64 = now - meta.created_ts;
        if (age < 0) continue; // clock skew protection

        if (@as(u64, @intCast(age)) > meta.max_lifetime_s) {
            log.info("sandbox {s} expired (age={d}s, max={d}s) — destroying", .{
                meta.id,
                @as(u64, @intCast(age)),
                meta.max_lifetime_s,
            });
            reaper_cfg.destroy_fn(meta.id);
        }
    }
}

/// Convenience: scan a sandboxes directory on disk to find expired sandboxes
/// by reading .meta/created_ts and .meta/max_lifetime_s files.
/// This is the filesystem-backed implementation used when no in-memory
/// sandbox manager is available (e.g., during recovery).
pub fn scanFilesystem(data_dir: []const u8) void {
    var sandboxes_buf: [256]u8 = undefined;
    const sandboxes_dir = std.fmt.bufPrint(&sandboxes_buf, "{s}/sandboxes", .{data_dir}) catch return;

    var dir = std.fs.openDirAbsolute(sandboxes_dir, .{ .iterate = true }) catch return;
    defer dir.close();

    const now = std.time.timestamp();
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;

        const meta = readSandboxMeta(sandboxes_dir, entry.name) orelse continue;
        if (meta.max_lifetime_s == 0) continue;

        const age: i64 = now - meta.created_ts;
        if (age < 0) continue;

        if (@as(u64, @intCast(age)) > meta.max_lifetime_s) {
            log.info("filesystem scan: sandbox {s} expired (age={d}s, max={d}s)", .{
                entry.name,
                @as(u64, @intCast(age)),
                meta.max_lifetime_s,
            });
        }
    }
}

/// Read sandbox metadata from disk (.meta/created_ts and .meta/max_lifetime_s).
fn readSandboxMeta(sandboxes_dir: []const u8, id: []const u8) ?SandboxMeta {
    // Read created_ts
    var ts_path_buf: [512]u8 = undefined;
    const ts_path = std.fmt.bufPrint(&ts_path_buf, "{s}/{s}/.meta/created_ts", .{ sandboxes_dir, id }) catch return null;
    const created_ts = readIntFile(i64, ts_path) orelse return null;

    // Read max_lifetime_s
    var lt_path_buf: [512]u8 = undefined;
    const lt_path = std.fmt.bufPrint(&lt_path_buf, "{s}/{s}/.meta/max_lifetime_s", .{ sandboxes_dir, id }) catch return null;
    const max_lifetime_s = readIntFile(u64, lt_path) orelse 0;

    return SandboxMeta{
        .id = id,
        .created_ts = created_ts,
        .max_lifetime_s = max_lifetime_s,
    };
}

/// Read a small file and parse its contents as an integer.
fn readIntFile(comptime T: type, path: []const u8) ?T {
    const file = std.fs.openFileAbsolute(path, .{ .mode = .read_only }) catch return null;
    defer file.close();
    var buf: [64]u8 = undefined;
    const n = file.read(&buf) catch return null;
    const trimmed = std.mem.trim(u8, buf[0..n], &std.ascii.whitespace);
    return std.fmt.parseInt(T, trimmed, 10) catch null;
}

// ── Tests ────────────────────────────────────────────────────────────

test "SandboxMeta defaults" {
    const meta = SandboxMeta{
        .id = "test-sandbox",
        .created_ts = 1000,
        .max_lifetime_s = 3600,
    };
    try std.testing.expectEqualStrings("test-sandbox", meta.id);
    try std.testing.expectEqual(@as(i64, 1000), meta.created_ts);
    try std.testing.expectEqual(@as(u64, 3600), meta.max_lifetime_s);
}

test "SandboxMeta zero lifetime means no expiry" {
    const meta = SandboxMeta{
        .id = "no-expiry",
        .created_ts = 1000,
        .max_lifetime_s = 0,
    };
    try std.testing.expectEqual(@as(u64, 0), meta.max_lifetime_s);
}

test "scanAndReap calls destroy for expired sandboxes" {
    // Track which sandbox IDs were destroyed
    const State = struct {
        var destroyed_count: usize = 0;

        fn destroy(_: []const u8) void {
            destroyed_count += 1;
        }

        fn listMetas(allocator: std.mem.Allocator) ?[]SandboxMeta {
            const now = std.time.timestamp();
            const metas = allocator.alloc(SandboxMeta, 3) catch return null;
            // Expired: created 2 hours ago with 1 hour max lifetime
            metas[0] = .{
                .id = "expired-1",
                .created_ts = now - 7200,
                .max_lifetime_s = 3600,
            };
            // Not expired: created 30 min ago with 1 hour max lifetime
            metas[1] = .{
                .id = "alive-1",
                .created_ts = now - 1800,
                .max_lifetime_s = 3600,
            };
            // No expiry
            metas[2] = .{
                .id = "no-expiry",
                .created_ts = now - 999999,
                .max_lifetime_s = 0,
            };
            return metas;
        }
    };

    State.destroyed_count = 0;

    const reaper_cfg = ReaperConfig{
        .interval_s = 10,
        .list_fn = State.listMetas,
        .destroy_fn = State.destroy,
    };

    scanAndReap(reaper_cfg);

    // Only the expired sandbox should have been destroyed
    try std.testing.expectEqual(@as(usize, 1), State.destroyed_count);
}

test "scanAndReap handles no sandboxes" {
    const State = struct {
        fn listEmpty(_: std.mem.Allocator) ?[]SandboxMeta {
            return null;
        }
        fn noop(_: []const u8) void {}
    };

    const reaper_cfg = ReaperConfig{
        .list_fn = State.listEmpty,
        .destroy_fn = State.noop,
    };

    // Should not crash
    scanAndReap(reaper_cfg);
}

test "scanAndReap handles all non-expiring sandboxes" {
    const State = struct {
        var destroyed_count: usize = 0;

        fn listNoExpiry(allocator: std.mem.Allocator) ?[]SandboxMeta {
            const metas = allocator.alloc(SandboxMeta, 2) catch return null;
            const now = std.time.timestamp();
            metas[0] = .{ .id = "a", .created_ts = now - 100000, .max_lifetime_s = 0 };
            metas[1] = .{ .id = "b", .created_ts = now - 200000, .max_lifetime_s = 0 };
            return metas;
        }
        fn destroy(_: []const u8) void {
            destroyed_count += 1;
        }
    };

    State.destroyed_count = 0;

    scanAndReap(.{
        .list_fn = State.listNoExpiry,
        .destroy_fn = State.destroy,
    });

    try std.testing.expectEqual(@as(usize, 0), State.destroyed_count);
}

test "readIntFile with valid file" {
    // Create a temp file with a number
    const path = "/tmp/sq-reaper-test-int";
    {
        const f = try std.fs.createFileAbsolute(path, .{});
        defer f.close();
        try f.writeAll("42\n");
    }
    defer std.fs.deleteFileAbsolute(path) catch {};

    const result = readIntFile(u64, path);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(u64, 42), result.?);
}

test "readIntFile returns null for missing file" {
    const result = readIntFile(u64, "/nonexistent-path-12345/file");
    try std.testing.expect(result == null);
}

test "readIntFile returns null for non-numeric content" {
    const path = "/tmp/sq-reaper-test-nan";
    {
        const f = try std.fs.createFileAbsolute(path, .{});
        defer f.close();
        try f.writeAll("not-a-number\n");
    }
    defer std.fs.deleteFileAbsolute(path) catch {};

    const result = readIntFile(u64, path);
    try std.testing.expect(result == null);
}
