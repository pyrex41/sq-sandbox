// json.zig — API request/response structs and JSON helpers.
//
// Defines all 10 endpoint schemas matching the existing wire format for
// backward compatibility. Handles the layers field as both comma-separated
// string and JSON array per PRD.

const std = @import("std");
const validate = @import("validate.zig");

// ── Request Types ───────────────────────────────────────────────────────

/// POST /cgi-bin/api/sandboxes — create a new sandbox.
pub const CreateRequest = struct {
    id: []const u8,
    owner: ?[]const u8 = null,
    /// Raw layers value — parsed via parseLayers() to handle both
    /// comma-separated string and JSON array formats.
    layers: ?std.json.Value = null,
    task: ?[]const u8 = null,
    cpu: ?f64 = null,
    memory_mb: ?u64 = null,
    max_lifetime_s: ?u64 = null,
    allow_net: ?[]const []const u8 = null,

    /// Resolve the layers field into a string slice.
    /// Accepts: "a,b,c" (comma-separated) or ["a","b","c"] (JSON array).
    /// Caller owns the returned slice and its string elements.
    pub fn parseLayers(self: *const CreateRequest, allocator: std.mem.Allocator) ![][]const u8 {
        const val = self.layers orelse return &[_][]const u8{};

        switch (val) {
            .string => |s| {
                if (s.len == 0) return &[_][]const u8{};
                return splitCsv(allocator, s);
            },
            .array => |arr| {
                var result = try allocator.alloc([]const u8, arr.items.len);
                var i: usize = 0;
                for (arr.items) |item| {
                    switch (item) {
                        .string => |s| {
                            result[i] = try allocator.dupe(u8, s);
                            i += 1;
                        },
                        else => {
                            // Free already-allocated strings on error
                            for (result[0..i]) |prev| allocator.free(prev);
                            allocator.free(result);
                            return error.InvalidLayerFormat;
                        },
                    }
                }
                return result[0..i];
            },
            else => return error.InvalidLayerFormat,
        }
    }

    pub fn effectiveOwner(self: *const CreateRequest) []const u8 {
        return self.owner orelse "anon";
    }

    pub fn effectiveCpu(self: *const CreateRequest) f64 {
        return self.cpu orelse 2.0;
    }

    pub fn effectiveMemoryMb(self: *const CreateRequest) u64 {
        return self.memory_mb orelse 1024;
    }

    pub fn effectiveMaxLifetimeS(self: *const CreateRequest) u64 {
        return self.max_lifetime_s orelse 0;
    }
};

/// POST /cgi-bin/api/sandboxes/:id/exec — execute a command.
pub const ExecRequestJson = struct {
    cmd: []const u8,
    workdir: ?[]const u8 = null,
    timeout: ?u64 = null,

    pub fn effectiveWorkdir(self: *const ExecRequestJson) []const u8 {
        return self.workdir orelse "/";
    }

    pub fn effectiveTimeout(self: *const ExecRequestJson) u64 {
        return self.timeout orelse 300;
    }
};

/// POST /cgi-bin/api/sandboxes/:id/snapshot — create a checkpoint.
pub const SnapshotRequest = struct {
    label: []const u8,
};

/// POST /cgi-bin/api/sandboxes/:id/restore — restore from checkpoint.
pub const RestoreRequest = struct {
    label: []const u8,
};

/// POST /cgi-bin/api/sandboxes/:id/activate — add a module layer.
pub const ActivateRequest = struct {
    module: []const u8,
};

// ── Response Types ──────────────────────────────────────────────────────

/// Snapshot entry within sandbox info.
pub const SnapshotInfo = struct {
    label: []const u8,
    created: []const u8,
    size: u64,
};

/// Full sandbox state — returned by GET, POST create, POST restore, POST activate.
pub const SandboxInfo = struct {
    id: []const u8,
    owner: []const u8,
    task: ?[]const u8 = null,
    layers: []const []const u8,
    created: []const u8,
    last_active: []const u8,
    mounted: bool,
    exec_count: u32,
    upper_bytes: u64,
    snapshots: []const SnapshotInfo,
    active_snapshot: ?[]const u8 = null,
    cpu: f64,
    memory_mb: u64,
    max_lifetime_s: u64,
    allow_net: []const []const u8,
};

/// Exec result — returned by POST exec, also used in logs endpoint.
pub const ExecResult = struct {
    seq: u32,
    cmd: []const u8,
    workdir: []const u8,
    exit_code: i32,
    started: []const u8,
    finished: []const u8,
    stdout: []const u8,
    stderr: []const u8,
};

/// Module entry — returned by GET /cgi-bin/api/modules.
pub const ModuleInfo = struct {
    name: []const u8,
    size: u64,
    location: []const u8,
};

/// Health check response — returned by GET /cgi-bin/health.
pub const HealthResponse = struct {
    status: []const u8,
    backend: []const u8,
    tailscale: TailscaleStatus,
    sandboxes: u32,
    modules: u32,
    base_ready: bool,
};

pub const TailscaleStatus = struct {
    status: []const u8,
    ip: []const u8,
};

/// Snapshot creation result.
pub const SnapshotResult = struct {
    snapshot: []const u8,
    size: u64,
};

/// Standard error response.
pub const ErrorResponse = struct {
    @"error": []const u8,
};

// ── Serialization Helpers ───────────────────────────────────────────────

/// Serialize a value to JSON bytes. Caller owns the returned slice.
pub fn stringify(allocator: std.mem.Allocator, v: anytype) ![]u8 {
    return std.json.Stringify.valueAlloc(allocator, v, .{});
}

/// Serialize an error message as {"error":"<msg>"}.
pub fn errorJson(allocator: std.mem.Allocator, msg: []const u8) ![]u8 {
    return stringify(allocator, ErrorResponse{ .@"error" = msg });
}

/// Parse a JSON request body into a typed struct.
/// Returns a parsed value whose lifetime is tied to the returned Parsed.
pub fn parse(comptime T: type, allocator: std.mem.Allocator, body: []const u8) !std.json.Parsed(T) {
    return std.json.parseFromSlice(T, allocator, body, .{
        .ignore_unknown_fields = true,
        .allocate = .alloc_always,
    });
}

// ── Layers Parsing ──────────────────────────────────────────────────────

/// Split a comma-separated string into a slice of trimmed, non-empty strings.
/// Caller owns returned slice and its elements.
fn splitCsv(allocator: std.mem.Allocator, input: []const u8) ![][]const u8 {
    var list: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (list.items) |s| allocator.free(s);
        list.deinit(allocator);
    }

    var iter = std.mem.splitScalar(u8, input, ',');
    while (iter.next()) |segment| {
        const trimmed = std.mem.trim(u8, segment, " ");
        if (trimmed.len > 0) {
            try list.append(allocator, try allocator.dupe(u8, trimmed));
        }
    }

    return list.toOwnedSlice(allocator);
}

/// Free a layers slice returned by parseLayers or splitCsv.
pub fn freeLayers(allocator: std.mem.Allocator, layers: [][]const u8) void {
    for (layers) |s| allocator.free(s);
    allocator.free(layers);
}

// ── ISO 8601 Timestamp ─────────────────────────────────────────────────

/// Format a Unix timestamp as ISO 8601 with +00:00 timezone.
/// Writes into the provided buffer and returns the formatted slice.
pub fn formatTimestamp(buf: []u8, unix_ts: i64) ![]const u8 {
    const epoch_secs: std.time.epoch.EpochSeconds = .{ .secs = @intCast(unix_ts) };
    const day = epoch_secs.getDaySeconds();
    const year_day = epoch_secs.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    return std.fmt.bufPrint(buf, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}+00:00", .{
        year_day.year,
        month_day.month.numeric(),
        month_day.day_index + 1,
        day.getHoursIntoDay(),
        day.getMinutesIntoHour(),
        day.getSecondsIntoMinute(),
    });
}

// ── Tests ───────────────────────────────────────────────────────────────

test "parse CreateRequest with string layers" {
    const body =
        \\{"id":"dev","layers":"000-base-alpine,100-python312","owner":"alice"}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("dev", parsed.value.id);
    try std.testing.expectEqualStrings("alice", parsed.value.owner.?);

    const layers = try parsed.value.parseLayers(std.testing.allocator);
    defer freeLayers(std.testing.allocator, layers);

    try std.testing.expectEqual(@as(usize, 2), layers.len);
    try std.testing.expectEqualStrings("000-base-alpine", layers[0]);
    try std.testing.expectEqualStrings("100-python312", layers[1]);
}

test "parse CreateRequest with array layers" {
    const body =
        \\{"id":"dev","layers":["000-base-alpine","100-python312"]}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const layers = try parsed.value.parseLayers(std.testing.allocator);
    defer freeLayers(std.testing.allocator, layers);

    try std.testing.expectEqual(@as(usize, 2), layers.len);
    try std.testing.expectEqualStrings("000-base-alpine", layers[0]);
    try std.testing.expectEqualStrings("100-python312", layers[1]);
}

test "parse CreateRequest with null layers returns empty" {
    const body =
        \\{"id":"dev"}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const layers = try parsed.value.parseLayers(std.testing.allocator);
    // Empty slice, nothing to free
    try std.testing.expectEqual(@as(usize, 0), layers.len);
}

test "parse CreateRequest with empty string layers returns empty" {
    const body =
        \\{"id":"dev","layers":""}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const layers = try parsed.value.parseLayers(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), layers.len);
}

test "parse CreateRequest defaults" {
    const body =
        \\{"id":"test"}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const req = parsed.value;
    try std.testing.expectEqualStrings("anon", req.effectiveOwner());
    try std.testing.expectEqual(@as(f64, 2.0), req.effectiveCpu());
    try std.testing.expectEqual(@as(u64, 1024), req.effectiveMemoryMb());
    try std.testing.expectEqual(@as(u64, 0), req.effectiveMaxLifetimeS());
    try std.testing.expect(req.task == null);
    try std.testing.expect(req.allow_net == null);
}

test "parse CreateRequest with all fields" {
    const body =
        \\{"id":"sb1","owner":"bob","layers":"base","task":"testing","cpu":4.0,"memory_mb":2048,"max_lifetime_s":3600,"allow_net":["example.com","api.test.io"]}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const req = parsed.value;
    try std.testing.expectEqualStrings("sb1", req.id);
    try std.testing.expectEqualStrings("bob", req.effectiveOwner());
    try std.testing.expectEqualStrings("testing", req.task.?);
    try std.testing.expectEqual(@as(f64, 4.0), req.effectiveCpu());
    try std.testing.expectEqual(@as(u64, 2048), req.effectiveMemoryMb());
    try std.testing.expectEqual(@as(u64, 3600), req.effectiveMaxLifetimeS());
    try std.testing.expectEqual(@as(usize, 2), req.allow_net.?.len);
    try std.testing.expectEqualStrings("example.com", req.allow_net.?[0]);
    try std.testing.expectEqualStrings("api.test.io", req.allow_net.?[1]);
}

test "layers with integer element returns error" {
    const body =
        \\{"id":"dev","layers":[1,2,3]}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const result = parsed.value.parseLayers(std.testing.allocator);
    try std.testing.expectError(error.InvalidLayerFormat, result);
}

test "layers with non-string non-array returns error" {
    const body =
        \\{"id":"dev","layers":42}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const result = parsed.value.parseLayers(std.testing.allocator);
    try std.testing.expectError(error.InvalidLayerFormat, result);
}

test "layers comma-separated with spaces are trimmed" {
    const body =
        \\{"id":"dev","layers":"base , python , node"}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const layers = try parsed.value.parseLayers(std.testing.allocator);
    defer freeLayers(std.testing.allocator, layers);

    try std.testing.expectEqual(@as(usize, 3), layers.len);
    try std.testing.expectEqualStrings("base", layers[0]);
    try std.testing.expectEqualStrings("python", layers[1]);
    try std.testing.expectEqualStrings("node", layers[2]);
}

test "layers single element string" {
    const body =
        \\{"id":"dev","layers":"000-base-alpine"}
    ;
    const parsed = try parse(CreateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    const layers = try parsed.value.parseLayers(std.testing.allocator);
    defer freeLayers(std.testing.allocator, layers);

    try std.testing.expectEqual(@as(usize, 1), layers.len);
    try std.testing.expectEqualStrings("000-base-alpine", layers[0]);
}

test "parse ExecRequestJson" {
    const body =
        \\{"cmd":"echo hello","workdir":"/tmp","timeout":60}
    ;
    const parsed = try parse(ExecRequestJson, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("echo hello", parsed.value.cmd);
    try std.testing.expectEqualStrings("/tmp", parsed.value.effectiveWorkdir());
    try std.testing.expectEqual(@as(u64, 60), parsed.value.effectiveTimeout());
}

test "parse ExecRequestJson defaults" {
    const body =
        \\{"cmd":"ls -la"}
    ;
    const parsed = try parse(ExecRequestJson, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("/", parsed.value.effectiveWorkdir());
    try std.testing.expectEqual(@as(u64, 300), parsed.value.effectiveTimeout());
}

test "parse SnapshotRequest" {
    const body =
        \\{"label":"checkpoint-1"}
    ;
    const parsed = try parse(SnapshotRequest, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("checkpoint-1", parsed.value.label);
}

test "parse RestoreRequest" {
    const body =
        \\{"label":"my-save"}
    ;
    const parsed = try parse(RestoreRequest, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("my-save", parsed.value.label);
}

test "parse ActivateRequest" {
    const body =
        \\{"module":"100-nodejs22"}
    ;
    const parsed = try parse(ActivateRequest, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("100-nodejs22", parsed.value.module);
}

test "parse ignores unknown fields" {
    const body =
        \\{"cmd":"test","unknown_field":"ignored","another":123}
    ;
    const parsed = try parse(ExecRequestJson, std.testing.allocator, body);
    defer parsed.deinit();

    try std.testing.expectEqualStrings("test", parsed.value.cmd);
}

test "stringify SandboxInfo" {
    const info = SandboxInfo{
        .id = "dev",
        .owner = "alice",
        .task = "testing",
        .layers = &[_][]const u8{ "000-base-alpine", "100-python312" },
        .created = "2025-02-14T12:30:00+00:00",
        .last_active = "2025-02-14T12:30:00+00:00",
        .mounted = true,
        .exec_count = 5,
        .upper_bytes = 1024000,
        .snapshots = &[_]SnapshotInfo{},
        .active_snapshot = null,
        .cpu = 2.0,
        .memory_mb = 1024,
        .max_lifetime_s = 1800,
        .allow_net = &[_][]const u8{"api.example.com"},
    };

    const json_str = try stringify(std.testing.allocator, info);
    defer std.testing.allocator.free(json_str);

    // Parse it back to verify round-trip
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("dev", obj.get("id").?.string);
    try std.testing.expectEqualStrings("alice", obj.get("owner").?.string);
    try std.testing.expectEqualStrings("testing", obj.get("task").?.string);
    try std.testing.expect(obj.get("mounted").?.bool);
    try std.testing.expectEqual(@as(usize, 2), obj.get("layers").?.array.items.len);
    try std.testing.expectEqual(@as(usize, 1), obj.get("allow_net").?.array.items.len);
    try std.testing.expect(obj.get("active_snapshot").? == .null);
}

test "stringify ExecResult" {
    const result = ExecResult{
        .seq = 1,
        .cmd = "echo hello",
        .workdir = "/",
        .exit_code = 0,
        .started = "2025-02-14T12:31:00+00:00",
        .finished = "2025-02-14T12:31:05+00:00",
        .stdout = "hello\n",
        .stderr = "",
    };

    const json_str = try stringify(std.testing.allocator, result);
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqual(@as(i64, 1), obj.get("seq").?.integer);
    try std.testing.expectEqualStrings("echo hello", obj.get("cmd").?.string);
    try std.testing.expectEqual(@as(i64, 0), obj.get("exit_code").?.integer);
    try std.testing.expectEqualStrings("hello\n", obj.get("stdout").?.string);
}

test "stringify HealthResponse" {
    const health = HealthResponse{
        .status = "ok",
        .backend = "chroot",
        .tailscale = .{ .status = "off", .ip = "" },
        .sandboxes = 3,
        .modules = 5,
        .base_ready = true,
    };

    const json_str = try stringify(std.testing.allocator, health);
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("ok", obj.get("status").?.string);
    try std.testing.expect(obj.get("base_ready").?.bool);
    try std.testing.expectEqual(@as(i64, 3), obj.get("sandboxes").?.integer);
    try std.testing.expectEqual(@as(i64, 5), obj.get("modules").?.integer);

    const ts = obj.get("tailscale").?.object;
    try std.testing.expectEqualStrings("off", ts.get("status").?.string);
}

test "stringify ModuleInfo" {
    const mod = ModuleInfo{
        .name = "000-base-alpine",
        .size = 8388608,
        .location = "local",
    };

    const json_str = try stringify(std.testing.allocator, mod);
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("000-base-alpine", obj.get("name").?.string);
    try std.testing.expectEqual(@as(i64, 8388608), obj.get("size").?.integer);
    try std.testing.expectEqualStrings("local", obj.get("location").?.string);
}

test "stringify SnapshotResult" {
    const snap = SnapshotResult{
        .snapshot = "my-checkpoint",
        .size = 5242880,
    };

    const json_str = try stringify(std.testing.allocator, snap);
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("my-checkpoint", obj.get("snapshot").?.string);
    try std.testing.expectEqual(@as(i64, 5242880), obj.get("size").?.integer);
}

test "errorJson produces correct format" {
    const json_str = try errorJson(std.testing.allocator, "not found: dev");
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    try std.testing.expectEqualStrings("not found: dev", parsed.value.object.get("error").?.string);
}

test "formatTimestamp produces ISO 8601" {
    var buf: [32]u8 = undefined;
    // 2025-01-15T00:00:00+00:00 => Unix 1736899200
    const ts = try formatTimestamp(&buf, 1736899200);
    try std.testing.expectEqualStrings("2025-01-15T00:00:00+00:00", ts);
}

test "formatTimestamp epoch zero" {
    var buf: [32]u8 = undefined;
    const ts = try formatTimestamp(&buf, 0);
    try std.testing.expectEqualStrings("1970-01-01T00:00:00+00:00", ts);
}

test "splitCsv basic" {
    const result = try splitCsv(std.testing.allocator, "a,b,c");
    defer freeLayers(std.testing.allocator, result);

    try std.testing.expectEqual(@as(usize, 3), result.len);
    try std.testing.expectEqualStrings("a", result[0]);
    try std.testing.expectEqualStrings("b", result[1]);
    try std.testing.expectEqualStrings("c", result[2]);
}

test "splitCsv skips empty segments" {
    const result = try splitCsv(std.testing.allocator, "a,,b,,c");
    defer freeLayers(std.testing.allocator, result);

    try std.testing.expectEqual(@as(usize, 3), result.len);
    try std.testing.expectEqualStrings("a", result[0]);
    try std.testing.expectEqualStrings("b", result[1]);
    try std.testing.expectEqualStrings("c", result[2]);
}

test "splitCsv trims whitespace" {
    const result = try splitCsv(std.testing.allocator, " a , b , c ");
    defer freeLayers(std.testing.allocator, result);

    try std.testing.expectEqual(@as(usize, 3), result.len);
    try std.testing.expectEqualStrings("a", result[0]);
    try std.testing.expectEqualStrings("b", result[1]);
    try std.testing.expectEqualStrings("c", result[2]);
}

test "stringify ErrorResponse has correct key name" {
    const err = ErrorResponse{ .@"error" = "test message" };
    const json_str = try stringify(std.testing.allocator, err);
    defer std.testing.allocator.free(json_str);

    // Verify the key is "error" not "@\"error\""
    try std.testing.expect(std.mem.indexOf(u8, json_str, "\"error\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json_str, "test message") != null);
}

test "stringify SandboxInfo with snapshots" {
    const snaps = [_]SnapshotInfo{
        .{
            .label = "cp-1",
            .created = "2025-02-14T12:45:00+00:00",
            .size = 5242880,
        },
        .{
            .label = "cp-2",
            .created = "2025-02-14T13:00:00+00:00",
            .size = 6291456,
        },
    };

    const info = SandboxInfo{
        .id = "sb1",
        .owner = "bob",
        .task = null,
        .layers = &[_][]const u8{"000-base-alpine"},
        .created = "2025-02-14T12:00:00+00:00",
        .last_active = "2025-02-14T13:00:00+00:00",
        .mounted = true,
        .exec_count = 0,
        .upper_bytes = 0,
        .snapshots = &snaps,
        .active_snapshot = "cp-1",
        .cpu = 1.5,
        .memory_mb = 512,
        .max_lifetime_s = 0,
        .allow_net = &[_][]const u8{},
    };

    const json_str = try stringify(std.testing.allocator, info);
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expect(obj.get("task").? == .null);
    try std.testing.expectEqualStrings("cp-1", obj.get("active_snapshot").?.string);
    try std.testing.expectEqual(@as(usize, 2), obj.get("snapshots").?.array.items.len);

    const snap0 = obj.get("snapshots").?.array.items[0].object;
    try std.testing.expectEqualStrings("cp-1", snap0.get("label").?.string);
    try std.testing.expectEqual(@as(i64, 5242880), snap0.get("size").?.integer);
}

test "stringify array of SandboxInfo (list endpoint)" {
    const infos = [_]SandboxInfo{
        .{
            .id = "a",
            .owner = "alice",
            .layers = &[_][]const u8{},
            .created = "2025-01-01T00:00:00+00:00",
            .last_active = "2025-01-01T00:00:00+00:00",
            .mounted = true,
            .exec_count = 0,
            .upper_bytes = 0,
            .snapshots = &[_]SnapshotInfo{},
            .cpu = 2.0,
            .memory_mb = 1024,
            .max_lifetime_s = 0,
            .allow_net = &[_][]const u8{},
        },
        .{
            .id = "b",
            .owner = "bob",
            .layers = &[_][]const u8{},
            .created = "2025-01-01T00:00:00+00:00",
            .last_active = "2025-01-01T00:00:00+00:00",
            .mounted = false,
            .exec_count = 3,
            .upper_bytes = 4096,
            .snapshots = &[_]SnapshotInfo{},
            .cpu = 1.0,
            .memory_mb = 256,
            .max_lifetime_s = 600,
            .allow_net = &[_][]const u8{},
        },
    };

    const json_str = try stringify(std.testing.allocator, @as([]const SandboxInfo, &infos));
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    const arr = parsed.value.array;
    try std.testing.expectEqual(@as(usize, 2), arr.items.len);
    try std.testing.expectEqualStrings("a", arr.items[0].object.get("id").?.string);
    try std.testing.expectEqualStrings("b", arr.items[1].object.get("id").?.string);
}

test "stringify array of ModuleInfo (modules endpoint)" {
    const mods = [_]ModuleInfo{
        .{ .name = "000-base-alpine", .size = 8388608, .location = "local" },
        .{ .name = "100-python312", .size = 52428800, .location = "local" },
        .{ .name = "200-node-service", .size = 0, .location = "remote" },
    };

    const json_str = try stringify(std.testing.allocator, @as([]const ModuleInfo, &mods));
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    try std.testing.expectEqual(@as(usize, 3), parsed.value.array.items.len);
}

test "stringify array of ExecResult (logs endpoint)" {
    const logs = [_]ExecResult{
        .{
            .seq = 1,
            .cmd = "echo hello",
            .workdir = "/",
            .exit_code = 0,
            .started = "2025-02-14T12:31:00+00:00",
            .finished = "2025-02-14T12:31:05+00:00",
            .stdout = "hello\n",
            .stderr = "",
        },
    };

    const json_str = try stringify(std.testing.allocator, @as([]const ExecResult, &logs));
    defer std.testing.allocator.free(json_str);

    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json_str, .{});
    defer parsed.deinit();

    try std.testing.expectEqual(@as(usize, 1), parsed.value.array.items.len);
}

test "ExecResult float cpu serializes as number" {
    const info = SandboxInfo{
        .id = "x",
        .owner = "y",
        .layers = &[_][]const u8{},
        .created = "",
        .last_active = "",
        .mounted = true,
        .exec_count = 0,
        .upper_bytes = 0,
        .snapshots = &[_]SnapshotInfo{},
        .cpu = 0.5,
        .memory_mb = 128,
        .max_lifetime_s = 0,
        .allow_net = &[_][]const u8{},
    };

    const json_str = try stringify(std.testing.allocator, info);
    defer std.testing.allocator.free(json_str);

    // cpu should be a number in the output
    try std.testing.expect(std.mem.indexOf(u8, json_str, "\"cpu\":5.0e-1") != null or
        std.mem.indexOf(u8, json_str, "\"cpu\":0.5") != null or
        std.mem.indexOf(u8, json_str, "\"cpu\":5e-1") != null);
}
