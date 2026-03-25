/// Request-response client for the sq-store sidecar.
///
/// Communicates with the Irmin-backed content-addressed snapshot store
/// over a Unix domain socket. Sends a JSON request line, reads a JSON
/// response line back.
const std = @import("std");

pub const StoreError = error{
    ConnectionFailed,
    SendFailed,
    ReceiveFailed,
    ParseFailed,
    StoreError,
};

pub const StoreResponse = struct {
    ok: bool = false,
    err_msg: ?[]const u8 = null,
    label: ?[]const u8 = null,
    size: u64 = 0,
    commit: ?[]const u8 = null,
    files_written: u64 = 0,
    files_deleted: u64 = 0,
};

/// Check if the irmin snapshot backend is enabled.
pub fn isIrminEnabled() bool {
    const val = std.posix.getenv("SQUASH_SNAPSHOT_BACKEND") orelse return false;
    return std.mem.eql(u8, val, "irmin");
}

/// Get the sq-store socket path.
pub fn getStoreSockPath(data_dir: []const u8, buf: []u8) []const u8 {
    if (std.posix.getenv("SQUASH_STORE_SOCK")) |sock| {
        return sock;
    }
    const path = std.fmt.bufPrint(buf, "{s}/.sq-store.sock", .{data_dir}) catch return "/data/.sq-store.sock";
    return path;
}

/// Send a request to sq-store and return the raw response.
/// Caller owns the returned memory.
pub fn request(allocator: std.mem.Allocator, sock_path: []const u8, json_msg: []const u8) ![]const u8 {
    // Connect to Unix socket
    const addr = std.net.Address.initUnix(sock_path) catch return StoreError.ConnectionFailed;
    const stream = std.net.Stream{ .handle = std.posix.socket(
        @intFromEnum(std.posix.AF.UNIX),
        @intFromEnum(std.posix.SOCK.STREAM),
        0,
    ) catch return StoreError.ConnectionFailed };
    defer stream.close();

    std.posix.connect(stream.handle, &addr.any, addr.getOsSockLen()) catch return StoreError.ConnectionFailed;

    // Send request line
    stream.writeAll(json_msg) catch return StoreError.SendFailed;
    stream.writeAll("\n") catch return StoreError.SendFailed;
    std.posix.shutdown(stream.handle, .send) catch {};

    // Read response
    var response_buf = std.ArrayList(u8).init(allocator);
    errdefer response_buf.deinit();

    var read_buf: [4096]u8 = undefined;
    while (true) {
        const n = stream.read(&read_buf) catch return StoreError.ReceiveFailed;
        if (n == 0) break;
        response_buf.appendSlice(read_buf[0..n]) catch return StoreError.ReceiveFailed;
    }

    return response_buf.toOwnedSlice() catch return StoreError.ReceiveFailed;
}

/// Build a snapshot request JSON message.
pub fn buildSnapshotRequest(
    allocator: std.mem.Allocator,
    sandbox_id: []const u8,
    label: []const u8,
    upper_data: []const u8,
) ![]const u8 {
    return std.fmt.allocPrint(allocator,
        \\{{"op":"snapshot","sandbox_id":"{s}","label":"{s}","upper_data":"{s}"}}
    , .{ sandbox_id, label, upper_data });
}

/// Build a restore request JSON message.
pub fn buildRestoreRequest(
    allocator: std.mem.Allocator,
    sandbox_id: []const u8,
    label: []const u8,
    upper_data: []const u8,
) ![]const u8 {
    return std.fmt.allocPrint(allocator,
        \\{{"op":"restore","sandbox_id":"{s}","label":"{s}","upper_data":"{s}"}}
    , .{ sandbox_id, label, upper_data });
}

/// Build a fork request JSON message.
pub fn buildForkRequest(
    allocator: std.mem.Allocator,
    source_id: []const u8,
    source_label: []const u8,
    target_id: []const u8,
) ![]const u8 {
    return std.fmt.allocPrint(allocator,
        \\{{"op":"fork","source_id":"{s}","source_label":"{s}","target_id":"{s}"}}
    , .{ source_id, source_label, target_id });
}

/// Build a diff request JSON message.
pub fn buildDiffRequest(
    allocator: std.mem.Allocator,
    sandbox_id: []const u8,
    from: []const u8,
    to: []const u8,
) ![]const u8 {
    return std.fmt.allocPrint(allocator,
        \\{{"op":"diff","sandbox_id":"{s}","from":"{s}","to":"{s}"}}
    , .{ sandbox_id, from, to });
}

/// Build a history request JSON message.
pub fn buildHistoryRequest(
    allocator: std.mem.Allocator,
    sandbox_id: []const u8,
) ![]const u8 {
    return std.fmt.allocPrint(allocator,
        \\{{"op":"history","sandbox_id":"{s}"}}
    , .{sandbox_id});
}

/// Check if a response indicates success by looking for "ok":true.
pub fn isOk(response: []const u8) bool {
    return std.mem.indexOf(u8, response, "\"ok\":true") != null;
}

/// Extract the error message from a response (simple string search).
pub fn getError(response: []const u8) ?[]const u8 {
    // Find "error":"..." pattern
    const prefix = "\"error\":\"";
    const start = std.mem.indexOf(u8, response, prefix) orelse return null;
    const val_start = start + prefix.len;
    const end = std.mem.indexOfPos(u8, response, val_start, "\"") orelse return null;
    return response[val_start..end];
}
