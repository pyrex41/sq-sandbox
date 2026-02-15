// manager.zig — In-memory sandbox registry with thread-safe access.
//
// Tracks active sandboxes in a hash map protected by a mutex.
// Provides the list/destroy callbacks needed by the reaper, and
// a count accessor for the health endpoint.
//
// Does NOT own the actual mount/netns/cgroup resources — those belong
// to the individual sandbox structs (future sandbox.zig). The manager
// tracks metadata (id, owner, created_ts, max_lifetime_s) and delegates
// teardown to a configurable destroy function.

const std = @import("std");
const config = @import("config.zig");
const reaper = @import("reaper.zig");
const validate = @import("validate.zig");
const api = @import("api.zig");
const log = std.log.scoped(.manager);

/// Per-sandbox metadata stored in the manager.
pub const SandboxEntry = struct {
    id_buf: [64]u8,
    id_len: usize,
    owner_buf: [64]u8,
    owner_len: usize,
    created_ts: i64,
    max_lifetime_s: u64,
    mounted: bool,

    pub fn id(self: *const SandboxEntry) []const u8 {
        return self.id_buf[0..self.id_len];
    }

    pub fn owner(self: *const SandboxEntry) []const u8 {
        return self.owner_buf[0..self.owner_len];
    }
};

/// Thread-safe sandbox manager.
/// All public methods acquire the mutex internally.
pub const SandboxManager = struct {
    mutex: std.Thread.Mutex,
    entries: std.StringHashMap(SandboxEntry),
    allocator: std.mem.Allocator,
    max_sandboxes: usize,

    pub fn init(allocator: std.mem.Allocator, max_sandboxes: usize) SandboxManager {
        return .{
            .mutex = .{},
            .entries = std.StringHashMap(SandboxEntry).init(allocator),
            .allocator = allocator,
            .max_sandboxes = max_sandboxes,
        };
    }

    pub fn deinit(self: *SandboxManager) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.entries.deinit();
    }

    /// Register a new sandbox. Returns error if at capacity or ID already exists.
    pub fn register(
        self: *SandboxManager,
        id: []const u8,
        owner_name: []const u8,
        max_lifetime_s: u64,
    ) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        try self.putEntry(id, owner_name, max_lifetime_s, std.time.timestamp(), true);
    }

    /// Register a sandbox with explicit timestamps, used for disk recovery.
    pub fn registerRecovered(
        self: *SandboxManager,
        id: []const u8,
        owner_name: []const u8,
        max_lifetime_s: u64,
        created_ts: i64,
        mounted: bool,
    ) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.putEntry(id, owner_name, max_lifetime_s, created_ts, mounted);
    }

    fn putEntry(
        self: *SandboxManager,
        id: []const u8,
        owner_name: []const u8,
        max_lifetime_s: u64,
        created_ts: i64,
        mounted: bool,
    ) !void {
        if (self.entries.count() >= self.max_sandboxes) {
            return error.AtCapacity;
        }
        if (self.entries.contains(id)) {
            return error.AlreadyExists;
        }

        var entry = SandboxEntry{
            .id_buf = undefined,
            .id_len = @min(id.len, 64),
            .owner_buf = undefined,
            .owner_len = @min(owner_name.len, 64),
            .created_ts = created_ts,
            .max_lifetime_s = max_lifetime_s,
            .mounted = mounted,
        };
        @memcpy(entry.id_buf[0..entry.id_len], id[0..entry.id_len]);
        @memcpy(entry.owner_buf[0..entry.owner_len], owner_name[0..entry.owner_len]);

        const key = try self.allocator.dupe(u8, id);
        errdefer self.allocator.free(key);
        try self.entries.put(key, entry);
    }

    /// Remove a sandbox from the registry. Idempotent — no error if not found.
    pub fn unregister(self: *SandboxManager, id: []const u8) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.entries.fetchRemove(id)) |kv| {
            self.allocator.free(kv.key);
        }
    }

    /// Number of currently tracked sandboxes.
    pub fn count(self: *SandboxManager) u32 {
        self.mutex.lock();
        defer self.mutex.unlock();
        return @intCast(self.entries.count());
    }

    /// Check if a sandbox exists.
    pub fn contains(self: *SandboxManager, id: []const u8) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.entries.contains(id);
    }

    /// Get a copy of sandbox entry (if it exists).
    pub fn get(self: *SandboxManager, id: []const u8) ?SandboxEntry {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.entries.get(id);
    }

    /// Re-register existing sandbox directories into the in-memory manager.
    /// This keeps reaper and health semantics consistent after daemon restart.
    pub fn recoverFromDisk(self: *SandboxManager, data_dir: []const u8) void {
        var sandboxes_buf: [std.fs.max_path_bytes]u8 = undefined;
        const sandboxes_dir = std.fmt.bufPrint(&sandboxes_buf, "{s}/sandboxes", .{data_dir}) catch return;

        var dir = std.fs.openDirAbsolute(sandboxes_dir, .{ .iterate = true }) catch return;
        defer dir.close();

        var recovered: usize = 0;
        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind != .directory) continue;
            if (!validate.validId(entry.name)) continue;
            if (self.contains(entry.name)) continue;

            var sandbox_buf: [std.fs.max_path_bytes]u8 = undefined;
            const sandbox_path = std.fmt.bufPrint(&sandbox_buf, "{s}/{s}", .{ sandboxes_dir, entry.name }) catch continue;

            const owner_opt = readMetaString(self.allocator, sandbox_path, "owner");
            defer if (owner_opt) |owner| self.allocator.free(owner);
            const owner = owner_opt orelse "anon";

            const max_lifetime_s = readMetaU64(sandbox_path, "max_lifetime_s") orelse 0;
            const mounted = readMetaBool(sandbox_path, "mounted") orelse true;
            const created_ts = readMetaI64(sandbox_path, "created_ts") orelse std.time.timestamp();

            self.registerRecovered(entry.name, owner, max_lifetime_s, created_ts, mounted) catch |err| {
                log.warn("recovery skip {s}: {}", .{ entry.name, err });
                continue;
            };
            recovered += 1;
        }

        if (recovered > 0) {
            log.info("recovered {d} sandbox(es) into manager registry", .{recovered});
        }
    }

    // ── Reaper integration ──────────────────────────────────────────────

    /// Global manager pointer set during startup for reaper callbacks.
    /// This is safe because the manager outlives the reaper thread.
    var global_instance: ?*SandboxManager = null;
    var global_data_dir: []const u8 = "/data";

    /// Set the global instance pointer. Called once from main.zig startup.
    pub fn setGlobal(self: *SandboxManager) void {
        global_instance = self;
    }

    pub fn setDataDir(self: *SandboxManager, data_dir: []const u8) void {
        _ = self;
        global_data_dir = data_dir;
    }

    /// List callback for the reaper. Returns a snapshot of sandbox metadata.
    pub fn reaperListFn(allocator: std.mem.Allocator) ?[]reaper.SandboxMeta {
        const mgr = global_instance orelse return null;
        mgr.mutex.lock();
        defer mgr.mutex.unlock();

        const n = mgr.entries.count();
        if (n == 0) return null;

        const metas = allocator.alloc(reaper.SandboxMeta, n) catch return null;
        var i: usize = 0;
        var iter = mgr.entries.iterator();
        while (iter.next()) |entry| {
            metas[i] = .{
                .id = entry.key_ptr.*,
                .created_ts = entry.value_ptr.created_ts,
                .max_lifetime_s = entry.value_ptr.max_lifetime_s,
            };
            i += 1;
        }
        return metas[0..i];
    }

    /// Destroy callback for the reaper. Tears down kernel resources
    /// (mounts, cgroups, netns) and then removes on-disk state.
    pub fn reaperDestroyFn(id: []const u8) void {
        const mgr = global_instance orelse return;
        const alloc = std.heap.page_allocator;
        const id_copy = alloc.dupe(u8, id) catch return;
        defer alloc.free(id_copy);
        log.info("reaper destroying sandbox {s}", .{id_copy});
        mgr.unregister(id_copy);

        // Build sandbox path for resource teardown
        var sandbox_buf: [std.fs.max_path_bytes]u8 = undefined;
        const sandbox_path = std.fmt.bufPrint(&sandbox_buf, "{s}/sandboxes/{s}", .{ global_data_dir, id_copy }) catch {
            teardownSandboxFiles(id_copy);
            return;
        };
        api.teardownSandboxResources(alloc, sandbox_path, id_copy) catch {};
        teardownSandboxFiles(id_copy);
    }

    /// Build a ReaperConfig that uses this manager's callbacks.
    pub fn reaperConfig(self: *SandboxManager) reaper.ReaperConfig {
        _ = self; // callbacks use global_instance
        return .{
            .list_fn = reaperListFn,
            .destroy_fn = reaperDestroyFn,
        };
    }
};

fn readMetaString(allocator: std.mem.Allocator, sandbox_path: []const u8, name: []const u8) ?[]u8 {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fmt.bufPrint(&path_buf, "{s}/.meta/{s}", .{ sandbox_path, name }) catch return null;

    const file = std.fs.openFileAbsolute(path, .{ .mode = .read_only }) catch return null;
    defer file.close();

    var buf: [512]u8 = undefined;
    const n = file.readAll(&buf) catch return null;
    const trimmed = std.mem.trim(u8, buf[0..n], &std.ascii.whitespace);
    if (trimmed.len == 0) return null;
    return allocator.dupe(u8, trimmed) catch null;
}

fn readMetaU64(sandbox_path: []const u8, name: []const u8) ?u64 {
    const raw = readMetaString(std.heap.page_allocator, sandbox_path, name) orelse return null;
    defer std.heap.page_allocator.free(raw);
    return std.fmt.parseInt(u64, raw, 10) catch null;
}

fn readMetaI64(sandbox_path: []const u8, name: []const u8) ?i64 {
    const raw = readMetaString(std.heap.page_allocator, sandbox_path, name) orelse return null;
    defer std.heap.page_allocator.free(raw);
    return std.fmt.parseInt(i64, raw, 10) catch null;
}

fn readMetaBool(sandbox_path: []const u8, name: []const u8) ?bool {
    const raw = readMetaString(std.heap.page_allocator, sandbox_path, name) orelse return null;
    defer std.heap.page_allocator.free(raw);
    if (std.mem.eql(u8, raw, "1") or std.ascii.eqlIgnoreCase(raw, "true")) return true;
    if (std.mem.eql(u8, raw, "0") or std.ascii.eqlIgnoreCase(raw, "false")) return false;
    return null;
}

// ── Destroy All ──────────────────────────────────────────────────────────

/// Best-effort cleanup of all tracked sandboxes.
/// Called from main.zig defer to ensure cleanup on shutdown.
pub fn destroyAll(mgr: *SandboxManager) void {
    mgr.mutex.lock();
    const n = mgr.entries.count();
    mgr.mutex.unlock();

    if (n == 0) return;
    log.info("destroying {d} sandbox(es) on shutdown", .{n});

    // Collect IDs under lock, then destroy outside lock to avoid
    // deadlock with reaperDestroyFn (which also takes the lock).
    var ids_buf: [256][]const u8 = undefined;
    const id_count = blk: {
        mgr.mutex.lock();
        defer mgr.mutex.unlock();
        var iter = mgr.entries.iterator();
        var i: usize = 0;
        while (iter.next()) |entry| {
            if (i >= ids_buf.len) break;
            ids_buf[i] = entry.key_ptr.*;
            i += 1;
        }
        break :blk i;
    };

    const alloc = std.heap.page_allocator;
    for (ids_buf[0..id_count]) |id| {
        log.info("shutdown: destroying sandbox {s}", .{id});
        var sandbox_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fmt.bufPrint(&sandbox_buf, "{s}/sandboxes/{s}", .{ SandboxManager.global_data_dir, id })) |sandbox_path| {
            api.teardownSandboxResources(alloc, sandbox_path, id) catch {};
        } else |_| {}
        teardownSandboxFiles(id);
        mgr.unregister(id);
    }
}

fn runSilent(argv: []const []const u8) void {
    const result = std.process.Child.run(.{
        .allocator = std.heap.page_allocator,
        .argv = argv,
    }) catch return;
    std.heap.page_allocator.free(result.stdout);
    std.heap.page_allocator.free(result.stderr);
}

fn teardownSandboxFiles(id: []const u8) void {
    var sandbox_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_buf, "{s}/sandboxes/{s}", .{ SandboxManager.global_data_dir, id }) catch return;

    var merged_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_path = std.fmt.bufPrint(&merged_buf, "{s}/merged", .{sandbox_path}) catch "";
    if (merged_path.len > 0) runSilent(&.{ "umount", "-l", merged_path });

    var snap_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path = std.fmt.bufPrint(&snap_buf, "{s}/images/_snapshot", .{sandbox_path}) catch "";
    if (snap_path.len > 0) runSilent(&.{ "umount", "-l", snap_path });

    var upper_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_path = std.fmt.bufPrint(&upper_buf, "{s}/upper", .{sandbox_path}) catch "";
    if (upper_path.len > 0) runSilent(&.{ "umount", "-l", upper_path });

    std.fs.deleteTreeAbsolute(sandbox_path) catch {};
}

// ── Tests ────────────────────────────────────────────────────────────────

test "register and unregister" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    try mgr.register("sb-1", "alice", 3600);
    try std.testing.expectEqual(@as(u32, 1), mgr.count());
    try std.testing.expect(mgr.contains("sb-1"));

    mgr.unregister("sb-1");
    try std.testing.expectEqual(@as(u32, 0), mgr.count());
    try std.testing.expect(!mgr.contains("sb-1"));
}

test "register duplicate returns error" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    try mgr.register("sb-1", "alice", 0);
    try std.testing.expectError(error.AlreadyExists, mgr.register("sb-1", "bob", 0));
}

test "register at capacity returns error" {
    var mgr = SandboxManager.init(std.testing.allocator, 2);
    defer mgr.deinit();

    try mgr.register("a", "alice", 0);
    try mgr.register("b", "bob", 0);
    try std.testing.expectError(error.AtCapacity, mgr.register("c", "charlie", 0));
}

test "unregister is idempotent" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    try mgr.register("sb-1", "alice", 0);
    mgr.unregister("sb-1");
    mgr.unregister("sb-1"); // no error
    try std.testing.expectEqual(@as(u32, 0), mgr.count());
}

test "get returns entry for existing sandbox" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    try mgr.register("sb-1", "alice", 7200);
    const entry = mgr.get("sb-1");
    try std.testing.expect(entry != null);
    try std.testing.expectEqualStrings("sb-1", entry.?.id());
    try std.testing.expectEqualStrings("alice", entry.?.owner());
    try std.testing.expectEqual(@as(u64, 7200), entry.?.max_lifetime_s);
    try std.testing.expect(entry.?.mounted);
}

test "get returns null for missing sandbox" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    try std.testing.expect(mgr.get("nonexistent") == null);
}

test "destroyAll cleans up all entries" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    try mgr.register("a", "alice", 0);
    try mgr.register("b", "bob", 0);
    try mgr.register("c", "charlie", 0);

    destroyAll(&mgr);
    try std.testing.expectEqual(@as(u32, 0), mgr.count());
}

test "reaperListFn returns null when no global instance" {
    SandboxManager.global_instance = null;
    const result = SandboxManager.reaperListFn(std.testing.allocator);
    try std.testing.expect(result == null);
}

test "reaperListFn returns metadata snapshot" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    mgr.setGlobal();
    defer {
        SandboxManager.global_instance = null;
    }

    try mgr.register("sb-1", "alice", 3600);
    try mgr.register("sb-2", "bob", 0);

    const metas = SandboxManager.reaperListFn(std.testing.allocator);
    try std.testing.expect(metas != null);
    defer std.testing.allocator.free(metas.?);

    try std.testing.expectEqual(@as(usize, 2), metas.?.len);
}

test "reaperDestroyFn unregisters sandbox" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    mgr.setGlobal();
    defer {
        SandboxManager.global_instance = null;
    }

    try mgr.register("sb-1", "alice", 3600);
    try std.testing.expectEqual(@as(u32, 1), mgr.count());

    SandboxManager.reaperDestroyFn("sb-1");
    try std.testing.expectEqual(@as(u32, 0), mgr.count());
}

test "reaperConfig builds valid config" {
    var mgr = SandboxManager.init(std.testing.allocator, 10);
    defer mgr.deinit();

    const cfg = mgr.reaperConfig();
    try std.testing.expect(cfg.list_fn == SandboxManager.reaperListFn);
    try std.testing.expect(cfg.destroy_fn == SandboxManager.reaperDestroyFn);
    try std.testing.expectEqual(@as(u64, 10), cfg.interval_s);
}
