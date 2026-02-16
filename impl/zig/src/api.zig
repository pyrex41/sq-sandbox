// api.zig — HTTP server, routing, and request handling.
//
// Concurrency model: Thread pool (not async/await). Zig's std library does not
// provide an async runtime like Tokio — std.event exists but async HTTP server
// support is not production-ready in Zig 0.15. The thread pool provides
// equivalent concurrency: accept() on main thread, dispatch to pool workers.
// Each worker handles one connection at a time; pool_size workers run in parallel.
//
// Uses std.http.Server (Zig 0.15 I/O-decoupled API) with std.net for
// the listening socket. Dispatches requests via a fixed thread pool.
// Provides auth token checking, JSON error responses, Content-Type
// enforcement on POST endpoints, and handlers for all 10 API endpoints.

const std = @import("std");
const builtin = @import("builtin");
const http = std.http;
const net = std.net;
const config_mod = @import("config.zig");
const json_mod = @import("json.zig");
const validate = @import("validate.zig");
const exec_mod = @import("exec.zig");
const mounts_mod = @import("mounts.zig");
const snapshot_mod = @import("snapshot.zig");
const cgroup_mod = @import("cgroup.zig");
const netns_mod = @import("netns.zig");
const fc_mod = @import("firecracker.zig");
const s3_mod = @import("s3.zig");

const Config = config_mod.Config;
const log = std.log.scoped(.api);
const is_linux = builtin.os.tag == .linux;

/// Atomic shutdown flag, set by signal handler.
pub var shutdown_requested: std.atomic.Value(bool) = std.atomic.Value(bool).init(false);

/// Number of worker threads in the connection pool.
/// Override via SQUASH_HTTP_POOL_SIZE env var (default 8).
fn poolSize() usize {
    if (std.posix.getenv("SQUASH_HTTP_POOL_SIZE")) |s| {
        if (std.fmt.parseInt(usize, s, 10)) |n| {
            if (n >= 1 and n <= 64) return n;
        } else |_| {}
    }
    return 8;
}

// ── Route Definitions ──────────────────────────────────────────────────

/// Parsed route from the request target.
pub const Route = union(enum) {
    health,
    list_sandboxes,
    create_sandbox,
    get_sandbox: []const u8,
    delete_sandbox: []const u8,
    exec_sandbox: []const u8,
    exec_logs: []const u8,
    snapshot_sandbox: []const u8,
    restore_sandbox: []const u8,
    activate_sandbox: []const u8,
    list_modules,
    not_found,
};

/// Match a request target to a Route.
/// Expects paths like /cgi-bin/health, /cgi-bin/api/sandboxes, etc.
pub fn matchRoute(method: http.Method, target: []const u8) Route {
    // Strip query string if present
    const path = if (std.mem.indexOfScalar(u8, target, '?')) |qi| target[0..qi] else target;

    if (eql(path, "/cgi-bin/health")) {
        return .health;
    }

    if (eql(path, "/cgi-bin/api/modules")) {
        return .list_modules;
    }

    if (eql(path, "/cgi-bin/api/sandboxes")) {
        return switch (method) {
            .GET => .list_sandboxes,
            .POST => .create_sandbox,
            else => .not_found,
        };
    }

    // /cgi-bin/api/sandboxes/:id/...
    const prefix = "/cgi-bin/api/sandboxes/";
    if (std.mem.startsWith(u8, path, prefix)) {
        const rest = path[prefix.len..];
        // rest is "id" or "id/action"
        if (std.mem.indexOfScalar(u8, rest, '/')) |slash| {
            const id = rest[0..slash];
            const action = rest[slash + 1 ..];

            if (!validate.validId(id)) return .not_found;

            if (eql(action, "exec")) return .{ .exec_sandbox = id };
            if (eql(action, "logs")) return .{ .exec_logs = id };
            if (eql(action, "snapshot")) return .{ .snapshot_sandbox = id };
            if (eql(action, "restore")) return .{ .restore_sandbox = id };
            if (eql(action, "activate")) return .{ .activate_sandbox = id };

            return .not_found;
        } else {
            // No action — GET or DELETE on sandbox by id
            const id = rest;
            if (!validate.validId(id)) return .not_found;
            return switch (method) {
                .GET => .{ .get_sandbox = id },
                .DELETE => .{ .delete_sandbox = id },
                else => .not_found,
            };
        }
    }

    return .not_found;
}

fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

// ── Server ─────────────────────────────────────────────────────────────

/// Start the HTTP server with a thread pool.
/// Returns when shutdown_requested is set (via signal handler).
pub fn startServer(cfg: *const Config, allocator: std.mem.Allocator) !void {
    const address = net.Address.initIp4(.{ 0, 0, 0, 0 }, cfg.port);
    var listener = try address.listen(.{
        .reuse_address = true,
    });
    defer listener.deinit();

    const pool_size = poolSize();
    log.info("listening on 0.0.0.0:{d} (pool={d})", .{ cfg.port, pool_size });

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{
        .allocator = allocator,
        .n_jobs = pool_size,
    });
    defer pool.deinit();

    while (!shutdown_requested.load(.acquire)) {
        const conn = listener.accept() catch |err| {
            // accept may fail with EINTR when signal fires — check flag
            if (shutdown_requested.load(.acquire)) break;
            log.warn("accept error: {}", .{err});
            continue;
        };

        pool.spawn(handleConnection, .{ conn, cfg, allocator }) catch |err| {
            log.warn("pool dispatch error: {}", .{err});
            conn.stream.close();
        };
    }

    log.info("server shutting down", .{});
}

/// Handle a single TCP connection (may serve multiple requests via keep-alive).
fn handleConnection(conn: net.Server.Connection, cfg: *const Config, allocator: std.mem.Allocator) void {
    defer conn.stream.close();

    // Set 30-second read timeout to prevent slowloris-style attacks.
    const timeout = std.posix.timeval{ .sec = 30, .usec = 0 };
    std.posix.setsockopt(conn.stream.handle, std.posix.SOL.SOCKET, std.posix.SO.RCVTIMEO, std.mem.asBytes(&timeout)) catch {
        log.debug("failed to set SO_RCVTIMEO, proceeding without timeout", .{});
    };

    var recv_buf: [8192]u8 = undefined;
    var send_buf: [8192]u8 = undefined;

    var conn_reader = conn.stream.reader(&recv_buf);
    var conn_writer = conn.stream.writer(&send_buf);

    var server = http.Server.init(conn_reader.interface(), &conn_writer.interface);

    // Support keep-alive: serve multiple requests per connection
    while (true) {
        var request = server.receiveHead() catch |err| {
            if (err == error.EndOfStream) return; // client closed
            log.debug("receiveHead error: {}", .{err});
            return;
        };
        handleRequest(&request, cfg, allocator) catch |err| {
            log.debug("handleRequest error: {}", .{err});
            return;
        };
    }
}

/// Process a single HTTP request: auth check, route, dispatch.
fn handleRequest(request: *http.Server.Request, cfg: *const Config, allocator: std.mem.Allocator) !void {
    const route = matchRoute(request.head.method, request.head.target);

    // Auth check — skip for health endpoint
    const needs_auth = switch (route) {
        .health => false,
        else => true,
    };

    if (needs_auth) {
        if (cfg.auth_token) |token| {
            if (!checkAuth(request, token)) {
                try sendJsonError(request, .unauthorized, "unauthorized", allocator);
                return;
            }
        }
    }

    switch (route) {
        .health => try handleHealth(request, cfg, allocator),
        .list_sandboxes => try handleListSandboxes(request, cfg, allocator),
        .create_sandbox => try handleCreateSandbox(request, cfg, allocator),
        .get_sandbox => |id| try handleGetSandbox(request, cfg, id, allocator),
        .delete_sandbox => |id| try handleDeleteSandbox(request, cfg, id, allocator),
        .exec_sandbox => |id| try handleExec(request, cfg, id, allocator),
        .exec_logs => |id| try handleExecLogs(request, cfg, id, allocator),
        .snapshot_sandbox => |id| try handleSnapshot(request, cfg, id, allocator),
        .restore_sandbox => |id| try handleRestore(request, cfg, id, allocator),
        .activate_sandbox => |id| try handleActivate(request, cfg, id, allocator),
        .list_modules => try handleListModules(request, cfg, allocator),
        .not_found => try sendJsonError(request, .not_found, "not found", allocator),
    }
}

// ── Auth ───────────────────────────────────────────────────────────────

/// Check Authorization header against expected token.
/// Accepts "Bearer <token>" format.
fn checkAuth(request: *const http.Server.Request, expected_token: []const u8) bool {
    var iter = request.iterateHeaders();
    while (iter.next()) |header| {
        if (std.ascii.eqlIgnoreCase(header.name, "authorization")) {
            const bearer_prefix = "Bearer ";
            if (std.mem.startsWith(u8, header.value, bearer_prefix)) {
                const provided = header.value[bearer_prefix.len..];
                return std.mem.eql(u8, provided, expected_token);
            }
            return false;
        }
    }
    return false;
}

// ── Endpoint Handlers ──────────────────────────────────────────────────

fn handleHealth(request: *http.Server.Request, cfg: *const Config, allocator: std.mem.Allocator) !void {

    // Count sandboxes and modules from filesystem
    const sandbox_count = countDirEntries(cfg, "sandboxes");
    const module_count = countDirEntries(cfg, "modules");
    const base_ready = checkBaseReady(cfg);

    const health = json_mod.HealthResponse{
        .status = "ok",
        .backend = @tagName(cfg.backend),
        .tailscale = .{
            .status = if (cfg.tailscale_authkey != null) "configured" else "off",
            .ip = "",
        },
        .sandboxes = sandbox_count,
        .modules = module_count,
        .base_ready = base_ready,
    };

    try sendJson(request, .ok, allocator, health);
}

fn handleListSandboxes(request: *http.Server.Request, cfg: *const Config, allocator: std.mem.Allocator) !void {

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    var dir = std.fs.openDirAbsolute(sb_dir_path, .{ .iterate = true }) catch {
        // No sandboxes directory — return empty array
        try sendJsonLiteral(request, .ok, "[]");
        return;
    };
    defer dir.close();

    var list: std.ArrayList(json_mod.SandboxInfo) = .empty;
    defer {
        for (list.items) |info| freeSandboxInfo(allocator, info);
        list.deinit(allocator);
    }

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;
        if (!validate.validId(entry.name)) continue;

        if (readSandboxInfo(allocator, sb_dir_path, entry.name)) |info| {
            list.append(allocator, info) catch continue;
        } else |_| continue;
    }

    const body = json_mod.stringify(allocator, list.items) catch {
        try sendJsonError(request, .internal_server_error, "serialization error", allocator);
        return;
    };
    defer allocator.free(body);

    try sendJsonBody(request, .ok, body);
}

fn handleCreateSandbox(request: *http.Server.Request, cfg: *const Config, allocator: std.mem.Allocator) !void {

    if (!requireJsonContentType(request, allocator)) return;

    const parsed = readJsonBody(json_mod.CreateRequest, request, allocator) catch {
        try sendJsonError(request, .bad_request, "invalid JSON body", allocator);
        return;
    };
    defer parsed.deinit();

    const req = parsed.value;

    // Validate sandbox id
    if (req.id.len == 0) {
        try sendJsonError(request, .bad_request, "id required", allocator);
        return;
    }
    if (!validate.validId(req.id)) {
        try sendJsonError(request, .bad_request, "id: alphanumeric/dash/underscore only", allocator);
        return;
    }

    // Validate layers if provided
    const layers = req.parseLayers(allocator) catch {
        try sendJsonError(request, .bad_request, "invalid layers format", allocator);
        return;
    };
    defer json_mod.freeLayers(allocator, layers);

    // Validate each layer exists as a module
    var mod_dir_buf: [256]u8 = undefined;
    const mod_dir_path = cfg.modulesDir(&mod_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    for (layers) |layer| {
        if (!validate.validLabel(layer)) {
            var err_buf: [256]u8 = undefined;
            const err_msg = std.fmt.bufPrint(&err_buf, "invalid layer name: {s}", .{layer}) catch "invalid layer name";
            try sendJsonError(request, .bad_request, err_msg, allocator);
            return;
        }
        // Check module exists on disk; if not, try S3 pull (matches Rust)
        var check_buf: [std.fs.max_path_bytes]u8 = undefined;
        const mod_path = std.fmt.bufPrint(&check_buf, "{s}/{s}.squashfs", .{ mod_dir_path, layer }) catch continue;
        std.fs.accessAbsolute(mod_path, .{}) catch {
            if (cfg.s3Enabled()) {
                if (s3_mod.S3Client.fromEnv()) |s3_client| {
                    var s3_key_buf: [256]u8 = undefined;
                    const s3_key = std.fmt.bufPrint(&s3_key_buf, "modules/{s}.squashfs", .{layer}) catch {
                        var err_buf: [256]u8 = undefined;
                        const err_msg = std.fmt.bufPrint(&err_buf, "module not found: {s}", .{layer}) catch "module not found";
                        try sendJsonError(request, .bad_request, err_msg, allocator);
                        return;
                    };
                    s3_client.pull(allocator, s3_key, mod_path) catch {
                        var err_buf: [256]u8 = undefined;
                        const err_msg = std.fmt.bufPrint(&err_buf, "module not found: {s}", .{layer}) catch "module not found";
                        try sendJsonError(request, .bad_request, err_msg, allocator);
                        return;
                    };
                    log.info("create: pulled missing module {s} from S3", .{layer});
                } else {
                    var err_buf: [256]u8 = undefined;
                    const err_msg = std.fmt.bufPrint(&err_buf, "module not found: {s}", .{layer}) catch "module not found";
                    try sendJsonError(request, .bad_request, err_msg, allocator);
                    return;
                }
            } else {
                var err_buf: [256]u8 = undefined;
                const err_msg = std.fmt.bufPrint(&err_buf, "module not found: {s}", .{layer}) catch "module not found";
                try sendJsonError(request, .bad_request, err_msg, allocator);
                return;
            }
        };
    }

    // Build sandbox directory path
    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check sandbox limit
    const current_count = countDirEntries(cfg, "sandboxes");
    if (current_count >= cfg.max_sandboxes) {
        var err_buf: [128]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "sandbox limit reached ({d})", .{cfg.max_sandboxes}) catch "sandbox limit reached";
        try sendJsonError(request, .bad_request, err_msg, allocator);
        return;
    }

    // Create sandbox directory structure
    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, req.id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check if already exists
    std.fs.accessAbsolute(sandbox_path, .{}) catch |err| {
        if (err != error.FileNotFound) {
            try sendJsonError(request, .internal_server_error, "filesystem error", allocator);
            return;
        }
        // FileNotFound is good — sandbox doesn't exist yet
        // Create sandbox directory and metadata
        createSandboxOnDisk(allocator, sandbox_path, &req, layers) catch {
            try sendJsonError(request, .internal_server_error, "failed to create sandbox", allocator);
            return;
        };

        if (shouldProvisionKernelResources()) {
            provisionSandboxResources(
                allocator,
                cfg,
                sandbox_path,
                req.id,
                layers,
                req.effectiveCpu(),
                req.effectiveMemoryMb(),
                req.allow_net,
            ) catch {
                teardownSandboxResources(allocator, sandbox_path, req.id) catch {};
                std.fs.deleteTreeAbsolute(sandbox_path) catch {};
                try sendJsonError(request, .internal_server_error, "failed to create sandbox", allocator);
                return;
            };
        }

        // Read back and return the sandbox info
        const info = readSandboxInfo(allocator, sb_dir_path, req.id) catch {
            try sendJsonError(request, .internal_server_error, "failed to read sandbox", allocator);
            return;
        };
        defer freeSandboxInfo(allocator, info);

        try sendJson(request, .created, allocator, info);
        return;
    };

    // If we get here, sandbox already exists
    var err_buf: [256]u8 = undefined;
    const err_msg = std.fmt.bufPrint(&err_buf, "already exists: {s}", .{req.id}) catch "already exists";
    try sendJsonError(request, .bad_request, err_msg, allocator);
}

fn handleGetSandbox(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    const info = readSandboxInfo(allocator, sb_dir_path, id) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };
    defer freeSandboxInfo(allocator, info);

    try sendJson(request, .ok, allocator, info);
}

fn handleDeleteSandbox(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {
    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check exists
    std.fs.accessAbsolute(sandbox_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };

    // Best-effort teardown before deleting on-disk state.
    teardownSandboxResources(allocator, sandbox_path, id) catch {};

    std.fs.deleteTreeAbsolute(sandbox_path) catch {
        try sendJsonError(request, .internal_server_error, "failed to delete sandbox", allocator);
        return;
    };

    // 204 No Content
    try request.respond("", .{
        .status = .no_content,
    });
}

fn handleExec(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {

    if (!requireJsonContentType(request, allocator)) return;

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check sandbox exists
    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(sandbox_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };

    // Parse request body
    const parsed = readJsonBody(json_mod.ExecRequestJson, request, allocator) catch {
        try sendJsonError(request, .bad_request, "invalid JSON body", allocator);
        return;
    };
    defer parsed.deinit();

    const exec_req = parsed.value;

    if (exec_req.cmd.len == 0) {
        try sendJsonError(request, .bad_request, "cmd required", allocator);
        return;
    }

    // Check if sandbox is mounted
    if (!isSandboxMounted(allocator, sandbox_path)) {
        try sendJsonError(request, .bad_request, "not mounted", allocator);
        return;
    }

    // Read current seq from log directory
    var log_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const log_dir = std.fmt.bufPrint(&log_dir_buf, "{s}/.meta/log", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Get next sequence number from existing log files
    const seq = getNextSeq(log_dir);

    // Get timestamps
    const started = std.time.timestamp();

    // Firecracker backend: exec via vsock
    if (cfg.backend == .firecracker) {
        const cid = fc_mod.readCid(sandbox_path) catch {
            try sendJsonError(request, .internal_server_error, "failed to read CID", allocator);
            return;
        };

        var vsock_result = fc_mod.execVsock(
            cid,
            exec_req.cmd,
            exec_req.effectiveWorkdir(),
            exec_req.effectiveTimeout(),
            allocator,
        ) catch {
            try sendJsonError(request, .internal_server_error, "vsock exec failed", allocator);
            return;
        };
        defer vsock_result.deinit(allocator);

        const finished = std.time.timestamp();

        var started_buf2: [32]u8 = undefined;
        var finished_buf2: [32]u8 = undefined;
        const started_str2 = json_mod.formatTimestamp(&started_buf2, started) catch "?";
        const finished_str2 = json_mod.formatTimestamp(&finished_buf2, finished) catch "?";

        // Write exec log to file (best-effort)
        writeFirecrackerExecLog(
            log_dir,
            seq,
            exec_req.cmd,
            exec_req.effectiveWorkdir(),
            vsock_result.exit_code,
            started_str2,
            finished_str2,
            vsock_result.stdout,
            vsock_result.stderr,
        );

        updateLastActive(sandbox_path, started);

        const fc_exec_result = json_mod.ExecResult{
            .seq = seq,
            .cmd = exec_req.cmd,
            .workdir = exec_req.effectiveWorkdir(),
            .exit_code = vsock_result.exit_code,
            .started = started_str2,
            .finished = finished_str2,
            .stdout = vsock_result.stdout,
            .stderr = vsock_result.stderr,
        };

        try sendJson(request, .ok, allocator, fc_exec_result);
        return;
    }

    // Chroot backend: exec via fork+chroot
    var mounted_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_path = std.fmt.bufPrint(&mounted_path_buf, "{s}/merged", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Build merged path as null-terminated for exec
    var merged_z_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_z = std.fmt.bufPrintZ(&merged_z_buf, "{s}", .{merged_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    const netns_name = readMetaFile(allocator, sandbox_path, "netns_name") catch null;
    defer if (netns_name) |s| allocator.free(s);
    const cgroup_path = readMetaFile(allocator, sandbox_path, "cgroup_path") catch null;
    defer if (cgroup_path) |s| allocator.free(s);

    // Execute in sandbox — use the real exec module
    var exec_ctx = exec_mod.SandboxContext{
        .merged_path = merged_z,
        .netns_name = netns_name,
        .cgroup_path = cgroup_path,
        .log_dir = log_dir,
    };

    // Create a sequence counter initialized to current seq
    var counter = exec_mod.SeqCounter{};
    // Set to seq-1 so next() returns seq
    if (seq > 1) {
        _ = counter.value.fetchAdd(seq - 1, .monotonic);
    }
    exec_ctx.seq_counter = &counter;

    const exec_request = exec_mod.ExecRequest{
        .cmd = exec_req.cmd,
        .workdir = exec_req.effectiveWorkdir(),
        .timeout_s = exec_req.effectiveTimeout(),
    };

    const result = exec_mod.execInSandbox(allocator, exec_ctx, exec_request) catch {
        try sendJsonError(request, .internal_server_error, "exec failed", allocator);
        return;
    };
    defer {
        var res = result;
        res.deinit(allocator);
    }

    // Format timestamps
    var started_buf: [32]u8 = undefined;
    var finished_buf: [32]u8 = undefined;
    const started_str = json_mod.formatTimestamp(&started_buf, result.started) catch "?";
    const finished_str = json_mod.formatTimestamp(&finished_buf, result.finished) catch "?";

    // Update last_active
    updateLastActive(sandbox_path, started);

    const exec_result = json_mod.ExecResult{
        .seq = result.seq,
        .cmd = exec_req.cmd,
        .workdir = exec_req.effectiveWorkdir(),
        .exit_code = result.exit_code,
        .started = started_str,
        .finished = finished_str,
        .stdout = result.stdout,
        .stderr = result.stderr,
    };

    try sendJson(request, .ok, allocator, exec_result);
}

fn handleExecLogs(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check sandbox exists
    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(sandbox_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };

    var log_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const log_dir = std.fmt.bufPrint(&log_dir_buf, "{s}/.meta/log", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    const logs = exec_mod.readExecLogs(allocator, log_dir) catch {
        try sendJsonLiteral(request, .ok, "[]");
        return;
    };
    defer exec_mod.freeExecLogs(allocator, logs);

    const body = json_mod.stringify(allocator, logs) catch {
        try sendJsonError(request, .internal_server_error, "serialization error", allocator);
        return;
    };
    defer allocator.free(body);

    try sendJsonBody(request, .ok, body);
}

fn handleSnapshot(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {

    if (!requireJsonContentType(request, allocator)) return;

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check sandbox exists
    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(sandbox_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };

    // Parse request body
    const parsed = readJsonBody(json_mod.SnapshotRequest, request, allocator) catch {
        // No body or invalid JSON — generate default label from timestamp
        const now = std.time.timestamp();
        var label_buf: [32]u8 = undefined;
        const label = generateSnapshotLabel(&label_buf, now);

        if (cfg.backend == .firecracker) {
            return doFirecrackerSnapshot(request, cfg, allocator, sandbox_path, id, label);
        }
        return doSnapshot(request, cfg, allocator, sandbox_path, id, label);
    };
    defer parsed.deinit();

    const label = parsed.value.label;

    if (label.len == 0) {
        try sendJsonError(request, .bad_request, "label required", allocator);
        return;
    }
    if (!validate.validLabel(label)) {
        try sendJsonError(request, .bad_request, "label: alphanumeric/dash/underscore/dot only", allocator);
        return;
    }

    if (cfg.backend == .firecracker) {
        return doFirecrackerSnapshot(request, cfg, allocator, sandbox_path, id, label);
    }
    return doSnapshot(request, cfg, allocator, sandbox_path, id, label);
}

fn handleRestore(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {

    if (!requireJsonContentType(request, allocator)) return;

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check sandbox exists
    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(sandbox_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };

    // Parse request body
    const parsed = readJsonBody(json_mod.RestoreRequest, request, allocator) catch {
        try sendJsonError(request, .bad_request, "invalid JSON body", allocator);
        return;
    };
    defer parsed.deinit();

    const label = parsed.value.label;

    if (label.len == 0) {
        try sendJsonError(request, .bad_request, "label required", allocator);
        return;
    }
    if (!validate.validLabel(label)) {
        try sendJsonError(request, .bad_request, "label: alphanumeric/dash/underscore/dot only", allocator);
        return;
    }

    // Check snapshot exists
    var snap_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path = std.fmt.bufPrint(&snap_path_buf, "{s}/.meta/snapshots/{s}.squashfs", .{ sandbox_path, label }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(snap_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{label}) catch "snapshot not found";
        try sendJsonError(request, .bad_request, err_msg, allocator);
        return;
    };

    // Firecracker backend: stop VM, restart with snapshot layer
    if (cfg.backend == .firecracker) {
        const cid_val = fc_mod.readCid(sandbox_path) catch {
            try sendJsonError(request, .internal_server_error, "failed to read CID", allocator);
            return;
        };
        _ = cid_val;

        var meta_buf3: [std.fs.max_path_bytes]u8 = undefined;
        const meta_dir = std.fmt.bufPrint(&meta_buf3, "{s}/.meta", .{sandbox_path}) catch {
            try sendJsonError(request, .internal_server_error, "path error", allocator);
            return;
        };

        // Stop the VM
        fc_mod.stopVm(id, meta_dir, allocator);

        // Allocate new CID for restarted VM
        // Use the parent data dir (two levels up from sandbox/.meta)
        const restore_layers = readLayers(allocator, sandbox_path) catch &[_][]const u8{};
        defer {
            for (restore_layers) |layer| allocator.free(layer);
            if (restore_layers.len > 0) allocator.free(restore_layers);
        }

        // Build squashfs paths including the snapshot layer
        var modules_dir_buf2: [256]u8 = undefined;
        const modules_dir = cfg.modulesDir(&modules_dir_buf2) catch {
            try sendJsonError(request, .internal_server_error, "path error", allocator);
            return;
        };

        var sqfs_paths: std.ArrayList([]const u8) = .empty;
        defer {
            for (sqfs_paths.items) |p| allocator.free(p);
            sqfs_paths.deinit(allocator);
        }

        // Add snapshot layer first (highest priority)
        const snap_path_dup = try allocator.dupe(u8, snap_path);
        try sqfs_paths.append(allocator, snap_path_dup);

        // Then base layers
        for (restore_layers) |layer| {
            const path = try std.fmt.allocPrint(allocator, "{s}/{s}.squashfs", .{ modules_dir, layer });
            try sqfs_paths.append(allocator, path);
        }

        // Get CPU/memory from metadata
        const cpu_meta = readMetaFile(allocator, sandbox_path, "cpu") catch null;
        defer if (cpu_meta) |s| allocator.free(s);
        const cpu_val = if (cpu_meta) |s| std.fmt.parseFloat(f64, s) catch 2.0 else 2.0;

        const mem_meta = readMetaFile(allocator, sandbox_path, "memory_mb") catch null;
        defer if (mem_meta) |s| allocator.free(s);
        const mem_val = if (mem_meta) |s| std.fmt.parseInt(u64, s, 10) catch 1024 else 1024;

        // Allocate new CID for restarted VM
        const new_cid = fc_mod.allocateCid(cfg.data_dir, allocator) catch {
            try sendJsonError(request, .internal_server_error, "CID allocation failed", allocator);
            return;
        };
        fc_mod.writeCid(sandbox_path, new_cid);

        // Restart VM with snapshot layer
        fc_mod.startVm(id, cpu_val, mem_val, sqfs_paths.items, new_cid, meta_dir, allocator) catch {
            try sendJsonError(request, .internal_server_error, "VM restart failed", allocator);
            return;
        };

        writeMetaFile(sandbox_path, "active_snapshot", label);
        updateLastActive(sandbox_path, std.time.timestamp());

        const info = readSandboxInfo(allocator, sb_dir_path, id) catch {
            try sendJsonError(request, .internal_server_error, "failed to read sandbox", allocator);
            return;
        };
        defer freeSandboxInfo(allocator, info);

        try sendJson(request, .ok, allocator, info);
        return;
    }

    // Chroot backend: overlay-based restore
    var snapshot_mp_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snapshot_mp_path = std.fmt.bufPrint(&snapshot_mp_path_buf, "{s}/images/_snapshot", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.makeDirAbsolute(snapshot_mp_path) catch {};

    var merged_buf: [std.fs.max_path_bytes]u8 = undefined;
    var upper_data_buf: [std.fs.max_path_bytes]u8 = undefined;
    var upper_work_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_path = std.fmt.bufPrintZ(&merged_buf, "{s}/merged", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    const upper_data = std.fmt.bufPrintZ(&upper_data_buf, "{s}/upper/data", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    const upper_work = std.fmt.bufPrintZ(&upper_work_buf, "{s}/upper/work", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    const snapshot_mp = std.fmt.bufPrintZ(&snapshot_mp_path_buf, "{s}", .{snapshot_mp_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    const layers = readLayers(allocator, sandbox_path) catch &[_][]const u8{};
    defer {
        for (layers) |layer| allocator.free(layer);
        if (layers.len > 0) allocator.free(layers);
    }

    var lower_layers: std.ArrayList([]const u8) = .empty;
    defer {
        for (lower_layers.items) |p| allocator.free(p);
        lower_layers.deinit(allocator);
    }
    var idx: usize = layers.len;
    while (idx > 0) {
        idx -= 1;
        const layer = layers[idx];
        const mount_path = std.fmt.allocPrint(allocator, "{s}/images/{s}.squashfs", .{ sandbox_path, layer }) catch {
            try sendJsonError(request, .internal_server_error, "path error", allocator);
            return;
        };
        lower_layers.append(allocator, mount_path) catch {
            allocator.free(mount_path);
            try sendJsonError(request, .internal_server_error, "path error", allocator);
            return;
        };
    }

    var overlay = mounts_mod.OverlayMount{
        .merged_path = merged_path,
        .active = true,
    };
    const active_snapshot = readMetaFile(allocator, sandbox_path, "active_snapshot") catch null;
    defer if (active_snapshot) |s| allocator.free(s);

    var prev_snapshot_mount: ?mounts_mod.SquashfsMount = null;
    if (active_snapshot) |_| {
        prev_snapshot_mount = mounts_mod.SquashfsMount{
            .mount_point = snapshot_mp,
            .active = true,
        };
    }
    const prev_snapshot_ptr: ?*mounts_mod.SquashfsMount =
        if (prev_snapshot_mount) |*m| m else null;

    _ = snapshot_mod.restoreSnapshot(
        allocator,
        .{
            .sandbox_dir = sandbox_path,
            .snapshot_path = snap_path,
            .snapshot_mount_point = snapshot_mp,
            .merged_path = merged_path,
            .upper_data = upper_data,
            .upper_work = upper_work,
            .lower_layers = lower_layers.items,
        },
        prev_snapshot_ptr,
        &overlay,
    ) catch {
        try sendJsonError(request, .internal_server_error, "restore failed", allocator);
        return;
    };

    writeMetaFile(sandbox_path, "active_snapshot", label);
    updateLastActive(sandbox_path, std.time.timestamp());

    const info = readSandboxInfo(allocator, sb_dir_path, id) catch {
        try sendJsonError(request, .internal_server_error, "failed to read sandbox", allocator);
        return;
    };
    defer freeSandboxInfo(allocator, info);

    try sendJson(request, .ok, allocator, info);
}

fn handleActivate(request: *http.Server.Request, cfg: *const Config, id: []const u8, allocator: std.mem.Allocator) !void {

    if (!requireJsonContentType(request, allocator)) return;

    var sb_dir_buf: [256]u8 = undefined;
    const sb_dir_path = cfg.sandboxesDir(&sb_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    // Check sandbox exists
    var sandbox_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = std.fmt.bufPrint(&sandbox_path_buf, "{s}/{s}", .{ sb_dir_path, id }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(sandbox_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "not found: {s}", .{id}) catch "not found";
        try sendJsonError(request, .not_found, err_msg, allocator);
        return;
    };

    // Parse request body
    const parsed = readJsonBody(json_mod.ActivateRequest, request, allocator) catch {
        try sendJsonError(request, .bad_request, "invalid JSON body", allocator);
        return;
    };
    defer parsed.deinit();

    const module = parsed.value.module;

    if (module.len == 0) {
        try sendJsonError(request, .bad_request, "module required", allocator);
        return;
    }
    if (!validate.validModule(module)) {
        try sendJsonError(request, .bad_request, "module: alphanumeric/dash/underscore/dot only", allocator);
        return;
    }

    // Check module exists
    var mod_dir_buf: [256]u8 = undefined;
    const mod_dir_path = cfg.modulesDir(&mod_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    var mod_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const mod_path = std.fmt.bufPrint(&mod_path_buf, "{s}/{s}.squashfs", .{ mod_dir_path, module }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.accessAbsolute(mod_path, .{}) catch {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "module not found: {s}", .{module}) catch "module not found";
        try sendJsonError(request, .bad_request, err_msg, allocator);
        return;
    };

    // Check if module is already in the sandbox's layers
    const current_layers_content = readMetaFile(allocator, sandbox_path, "layers") catch null;
    if (current_layers_content) |content| {
        defer allocator.free(content);
        // Check if module already present (each line is a layer)
        var line_iter = std.mem.splitScalar(u8, content, '\n');
        while (line_iter.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \r");
            if (trimmed.len > 0 and eql(trimmed, module)) {
                var err_buf: [256]u8 = undefined;
                const err_msg = std.fmt.bufPrint(&err_buf, "already active: {s}", .{module}) catch "already active";
                try sendJsonError(request, .bad_request, err_msg, allocator);
                return;
            }
        }

        // Append the new module to layers
        var new_layers_buf: [4096]u8 = undefined;
        const new_layers = std.fmt.bufPrint(&new_layers_buf, "{s}\n{s}", .{ std.mem.trim(u8, content, "\n \r"), module }) catch {
            try sendJsonError(request, .internal_server_error, "path error", allocator);
            return;
        };
        writeMetaFile(sandbox_path, "layers", new_layers);
    } else {
        writeMetaFile(sandbox_path, "layers", module);
    }

    // Update last_active
    updateLastActive(sandbox_path, std.time.timestamp());

    // Firecracker backend: hot-add drive + vsock remount
    if (cfg.backend == .firecracker) {
        const cid = fc_mod.readCid(sandbox_path) catch {
            try sendJsonError(request, .internal_server_error, "failed to read CID", allocator);
            return;
        };
        var meta_buf2: [std.fs.max_path_bytes]u8 = undefined;
        const meta_dir = std.fmt.bufPrint(&meta_buf2, "{s}/.meta", .{sandbox_path}) catch {
            try sendJsonError(request, .internal_server_error, "path error", allocator);
            return;
        };

        fc_mod.addDrive(id, module, mod_path, cid, meta_dir, allocator) catch {
            try sendJsonError(request, .internal_server_error, "failed to add drive", allocator);
            return;
        };
    } else {
        // Chroot backend: mount squashfs + remount overlay
        mountModuleLayer(sandbox_path, mod_path, module) catch {
            try sendJsonError(request, .internal_server_error, "failed to mount module", allocator);
            return;
        };
        remountOverlayForSandbox(allocator, sandbox_path) catch {
            try sendJsonError(request, .internal_server_error, "failed to remount overlay", allocator);
            return;
        };
    }

    const info = readSandboxInfo(allocator, sb_dir_path, id) catch {
        try sendJsonError(request, .internal_server_error, "failed to read sandbox", allocator);
        return;
    };
    defer freeSandboxInfo(allocator, info);

    try sendJson(request, .ok, allocator, info);
}

fn handleListModules(request: *http.Server.Request, cfg: *const Config, allocator: std.mem.Allocator) !void {

    var mod_dir_buf: [256]u8 = undefined;
    const mod_dir_path = cfg.modulesDir(&mod_dir_buf) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    var dir = std.fs.openDirAbsolute(mod_dir_path, .{ .iterate = true }) catch {
        // No modules directory — return empty array
        try sendJsonLiteral(request, .ok, "[]");
        return;
    };
    defer dir.close();

    var list: std.ArrayList(json_mod.ModuleInfo) = .empty;
    defer {
        for (list.items) |info| {
            allocator.free(info.name);
        }
        list.deinit(allocator);
    }

    var local_set = std.StringHashMap(void).init(allocator);
    defer local_set.deinit();

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".squashfs")) continue;

        // Strip .squashfs extension for the module name
        const name = entry.name[0 .. entry.name.len - ".squashfs".len];
        if (!validate.validLabel(name)) continue;

        // Get file size
        const stat = dir.statFile(entry.name) catch continue;

        local_set.put(name, {}) catch {};
        const duped_name = allocator.dupe(u8, name) catch continue;
        list.append(allocator, json_mod.ModuleInfo{
            .name = duped_name,
            .size = stat.size,
            .location = "local",
        }) catch {
            allocator.free(duped_name);
            continue;
        };
    }

    // Add S3 remote-only modules (matches Rust)
    if (cfg.s3Enabled()) {
        if (s3_mod.S3Client.fromEnv()) |s3_client| {
            if (s3_client.list(allocator, "modules/")) |remote_keys| {
                defer s3_mod.S3Client.freeList(allocator, remote_keys);
                for (remote_keys) |key| {
                const name = if (std.mem.startsWith(u8, key, "modules/"))
                    key["modules/".len..]
                else
                    key;
                if (!std.mem.endsWith(u8, name, ".squashfs")) continue;
                const stem = name[0 .. name.len - ".squashfs".len];
                if (stem.len == 0 or local_set.contains(stem)) continue;
                const duped_name = allocator.dupe(u8, stem) catch continue;
                list.append(allocator, json_mod.ModuleInfo{
                    .name = duped_name,
                    .size = 0,
                    .location = "s3",
                }) catch {
                    allocator.free(duped_name);
                    continue;
                };
            }
            } else |_| {}
        }
    }

    const body = json_mod.stringify(allocator, list.items) catch {
        try sendJsonError(request, .internal_server_error, "serialization error", allocator);
        return;
    };
    defer allocator.free(body);

    try sendJsonBody(request, .ok, body);
}

// ── Firecracker Snapshot Helper ────────────────────────────────────────

fn doFirecrackerSnapshot(
    request: *http.Server.Request,
    cfg: *const Config,
    allocator: std.mem.Allocator,
    sandbox_path: []const u8,
    id: []const u8,
    label: []const u8,
) !void {
    // Check for existing snapshot with same label
    var snap_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path = std.fmt.bufPrint(&snap_path_buf, "{s}/.meta/snapshots/{s}.squashfs", .{ sandbox_path, label }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    if (std.fs.accessAbsolute(snap_path, .{})) |_| {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "exists: {s}", .{label}) catch "snapshot exists";
        try sendJsonError(request, .bad_request, err_msg, allocator);
        return;
    } else |_| {}

    // Ensure snapshots directory exists
    var snap_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_dir = std.fmt.bufPrint(&snap_dir_buf, "{s}/.meta/snapshots", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.makeDirAbsolute(snap_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot dir", allocator);
            return;
        },
    };

    // Tell the guest to snapshot via vsock
    const cid = fc_mod.readCid(sandbox_path) catch {
        try sendJsonError(request, .internal_server_error, "failed to read CID", allocator);
        return;
    };

    // The guest agent runs __squash_snapshot which creates a squashfs of the upper layer
    var cmd_buf: [256]u8 = undefined;
    const snapshot_cmd = std.fmt.bufPrint(&cmd_buf, "__squash_snapshot {s}", .{label}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    var vsock_result = fc_mod.execVsock(cid, snapshot_cmd, "/", 120, allocator) catch {
        try sendJsonError(request, .internal_server_error, "vsock snapshot failed", allocator);
        return;
    };
    defer vsock_result.deinit(allocator);

    if (vsock_result.exit_code != 0) {
        try sendJsonError(request, .internal_server_error, "snapshot command failed", allocator);
        return;
    }

    // Get file size (the guest should have created the snapshot file)
    const stat = std.fs.cwd().statFile(snap_path) catch blk: {
        break :blk std.fs.File.Stat{
            .inode = 0, .size = 0, .mode = 0, .kind = .file,
            .atime = 0, .mtime = 0, .ctime = 0,
        };
    };

    const result = json_mod.SnapshotResult{
        .snapshot = label,
        .size = stat.size,
    };

    // Push to S3 in background (matches Rust)
    if (cfg.s3Enabled()) {
        if (s3_mod.S3Client.fromEnv()) |s3_client| {
            var s3_key_buf: [512]u8 = undefined;
            if (std.fmt.bufPrint(&s3_key_buf, "sandboxes/{s}/snapshots/{s}.squashfs", .{ id, label })) |s3_key| {
                s3_client.pushBg(allocator, snap_path, s3_key);
            } else |_| {}
        }
    }

    try sendJson(request, .ok, allocator, result);
}

// ── Snapshot Helper ────────────────────────────────────────────────────

fn doSnapshot(
    request: *http.Server.Request,
    cfg: *const Config,
    allocator: std.mem.Allocator,
    sandbox_path: []const u8,
    id: []const u8,
    label: []const u8,
) !void {
    // Check for existing snapshot with same label
    var snap_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path = std.fmt.bufPrint(&snap_path_buf, "{s}/.meta/snapshots/{s}.squashfs", .{ sandbox_path, label }) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    if (std.fs.accessAbsolute(snap_path, .{})) |_| {
        var err_buf: [256]u8 = undefined;
        const err_msg = std.fmt.bufPrint(&err_buf, "exists: {s}", .{label}) catch "snapshot exists";
        try sendJsonError(request, .bad_request, err_msg, allocator);
        return;
    } else |_| {}

    var snap_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_dir = std.fmt.bufPrint(&snap_dir_buf, "{s}/.meta/snapshots", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };
    std.fs.makeDirAbsolute(snap_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot dir", allocator);
            return;
        },
    };

    var upper_data_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_data = std.fmt.bufPrint(&upper_data_buf, "{s}/upper/data", .{sandbox_path}) catch {
        try sendJsonError(request, .internal_server_error, "path error", allocator);
        return;
    };

    const use_zstd = snapshot_mod.detectZstdSupport(allocator);
    var argv_list: std.ArrayList([]const u8) = .empty;
    defer argv_list.deinit(allocator);

    argv_list.append(allocator, "mksquashfs") catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };
    argv_list.append(allocator, upper_data) catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };
    argv_list.append(allocator, snap_path) catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };
    argv_list.append(allocator, "-comp") catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };
    if (use_zstd) {
        argv_list.append(allocator, "zstd") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
        argv_list.append(allocator, "-Xcompression-level") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
        argv_list.append(allocator, "3") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
        argv_list.append(allocator, "-b") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
        argv_list.append(allocator, "128K") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
    } else {
        argv_list.append(allocator, "gzip") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
        argv_list.append(allocator, "-b") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
        argv_list.append(allocator, "256K") catch {
            try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
            return;
        };
    }
    argv_list.append(allocator, "-noappend") catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };
    argv_list.append(allocator, "-quiet") catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };

    const cmd = std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv_list.items,
    }) catch {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    };
    defer {
        allocator.free(cmd.stdout);
        allocator.free(cmd.stderr);
    }
    if (cmd.term.Exited != 0) {
        try sendJsonError(request, .internal_server_error, "failed to create snapshot", allocator);
        return;
    }

    // Get file size
    const stat = std.fs.cwd().statFile(snap_path) catch blk: {
        break :blk std.fs.File.Stat{
            .inode = 0,
            .size = 0,
            .mode = 0,
            .kind = .file,
            .atime = 0,
            .mtime = 0,
            .ctime = 0,
        };
    };

    const result = json_mod.SnapshotResult{
        .snapshot = label,
        .size = stat.size,
    };

    // Push to S3 in background (matches Rust)
    if (cfg.s3Enabled()) {
        if (s3_mod.S3Client.fromEnv()) |s3_client| {
            var s3_key_buf: [512]u8 = undefined;
            if (std.fmt.bufPrint(&s3_key_buf, "sandboxes/{s}/snapshots/{s}.squashfs", .{ id, label })) |s3_key| {
                s3_client.pushBg(allocator, snap_path, s3_key);
            } else |_| {}
        }
    }

    try sendJson(request, .ok, allocator, result);
}

// ── Content-Type Enforcement ───────────────────────────────────────────

/// Check Content-Type and send 415 error if missing. Returns true if valid.
fn requireJsonContentType(request: *http.Server.Request, allocator: std.mem.Allocator) bool {
    if (request.head.method == .POST) {
        if (!hasJsonContentType(request)) {
            sendJsonError(request, .unsupported_media_type, "Content-Type must be application/json", allocator) catch {};
            return false;
        }
    }
    return true;
}

// ── Response Helpers ───────────────────────────────────────────────────

/// Send a JSON error response: {"error":"<message>"}
fn sendJsonError(request: *http.Server.Request, status: http.Status, message: []const u8, allocator: std.mem.Allocator) !void {
    const body = json_mod.errorJson(allocator, message) catch {
        // Last-resort fallback
        try request.respond("{\"error\":\"internal error\"}", .{
            .status = .internal_server_error,
            .extra_headers = &.{
                .{ .name = "content-type", .value = "application/json" },
            },
        });
        return;
    };
    defer allocator.free(body);

    try request.respond(body, .{
        .status = status,
        .extra_headers = &.{
            .{ .name = "content-type", .value = "application/json" },
        },
    });
}

/// Send a serializable value as JSON with the given status.
fn sendJson(request: *http.Server.Request, status: http.Status, allocator: std.mem.Allocator, value: anytype) !void {
    const body = json_mod.stringify(allocator, value) catch {
        try sendJsonError(request, .internal_server_error, "serialization error", allocator);
        return;
    };
    defer allocator.free(body);

    try sendJsonBody(request, status, body);
}

/// Send a pre-serialized JSON body with the given status.
fn sendJsonBody(request: *http.Server.Request, status: http.Status, body: []const u8) !void {
    try request.respond(body, .{
        .status = status,
        .extra_headers = &.{
            .{ .name = "content-type", .value = "application/json" },
        },
    });
}

/// Send a literal JSON string (for empty arrays etc).
fn sendJsonLiteral(request: *http.Server.Request, status: http.Status, literal: []const u8) !void {
    try request.respond(literal, .{
        .status = status,
        .extra_headers = &.{
            .{ .name = "content-type", .value = "application/json" },
        },
    });
}

/// Read the request body as a JSON-parsed struct.
/// Returns a Parsed(T) whose lifetime is tied to the allocator.
pub fn readJsonBody(comptime T: type, request: *http.Server.Request, allocator: std.mem.Allocator) !std.json.Parsed(T) {
    var body_buf: [65536]u8 = undefined;
    const body_reader = if (request.head.expect != null)
        try request.readerExpectContinue(&body_buf)
    else
        request.readerExpectNone(&body_buf);

    const body = body_reader.readAlloc(allocator, 1024 * 1024) catch {
        return error.BodyReadFailed;
    };
    defer allocator.free(body);

    return json_mod.parse(T, allocator, body);
}

/// Check if request has application/json content type.
fn hasJsonContentType(request: *const http.Server.Request) bool {
    const ct = request.head.content_type orelse return false;
    if (std.mem.startsWith(u8, ct, "application/json")) return true;
    return false;
}

// ── Filesystem Helpers ─────────────────────────────────────────────────

/// Read a sandbox's metadata from disk and return a SandboxInfo.
fn readSandboxInfo(allocator: std.mem.Allocator, sandboxes_dir: []const u8, id: []const u8) !json_mod.SandboxInfo {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const sandbox_path = try std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ sandboxes_dir, id });

    // Check sandbox directory exists
    std.fs.accessAbsolute(sandbox_path, .{}) catch return error.FileNotFound;

    // Read metadata files
    const owner = readMetaFile(allocator, sandbox_path, "owner") catch try allocator.dupe(u8, "anon");
    errdefer allocator.free(owner);

    const task = readMetaFile(allocator, sandbox_path, "task") catch null;
    errdefer if (task) |t| allocator.free(t);

    const created = readMetaFile(allocator, sandbox_path, "created") catch try allocator.dupe(u8, "");
    errdefer allocator.free(created);

    const last_active = readMetaFile(allocator, sandbox_path, "last_active") catch try allocator.dupe(u8, "");
    errdefer allocator.free(last_active);

    const active_snapshot = readMetaFile(allocator, sandbox_path, "active_snapshot") catch null;
    errdefer if (active_snapshot) |s| allocator.free(s);

    // Parse numeric metadata
    const cpu_str = readMetaFile(allocator, sandbox_path, "cpu") catch null;
    defer if (cpu_str) |s| allocator.free(s);
    const cpu = if (cpu_str) |s| std.fmt.parseFloat(f64, s) catch 2.0 else 2.0;

    const memory_str = readMetaFile(allocator, sandbox_path, "memory_mb") catch null;
    defer if (memory_str) |s| allocator.free(s);
    const memory_mb = if (memory_str) |s| std.fmt.parseInt(u64, s, 10) catch 1024 else 1024;

    const lifetime_str = readMetaFile(allocator, sandbox_path, "max_lifetime_s") catch null;
    defer if (lifetime_str) |s| allocator.free(s);
    const max_lifetime_s = if (lifetime_str) |s| std.fmt.parseInt(u64, s, 10) catch 0 else 0;

    // Parse layers from newline-separated file
    const layers = readLayers(allocator, sandbox_path) catch &[_][]const u8{};

    // Parse allow_net
    const allow_net = readAllowNet(allocator, sandbox_path) catch &[_][]const u8{};

    // Read exec count from log directory
    var log_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const log_dir = std.fmt.bufPrint(&log_dir_buf, "{s}/.meta/log", .{sandbox_path}) catch "";
    const exec_count = countLogEntries(log_dir);

    // Mounted state is explicit metadata when available.
    const mounted = isSandboxMounted(allocator, sandbox_path);

    // Get upper bytes
    var upper_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_path = std.fmt.bufPrint(&upper_buf, "{s}/upper/data", .{sandbox_path}) catch "";
    const upper_bytes = getDirSize(upper_path);

    // Read snapshots
    const snapshots = readSnapshots(allocator, sandbox_path) catch &[_]json_mod.SnapshotInfo{};

    const id_duped = try allocator.dupe(u8, id);
    errdefer allocator.free(id_duped);

    return json_mod.SandboxInfo{
        .id = id_duped,
        .owner = owner,
        .task = task,
        .layers = layers,
        .created = created,
        .last_active = last_active,
        .mounted = mounted,
        .exec_count = exec_count,
        .upper_bytes = upper_bytes,
        .snapshots = snapshots,
        .active_snapshot = active_snapshot,
        .cpu = cpu,
        .memory_mb = memory_mb,
        .max_lifetime_s = max_lifetime_s,
        .allow_net = allow_net,
    };
}

/// Free a SandboxInfo allocated by readSandboxInfo.
fn freeSandboxInfo(allocator: std.mem.Allocator, info: json_mod.SandboxInfo) void {
    allocator.free(info.id);
    allocator.free(info.owner);
    if (info.task) |t| allocator.free(t);
    allocator.free(info.created);
    allocator.free(info.last_active);
    if (info.active_snapshot) |s| allocator.free(s);
    for (info.layers) |l| allocator.free(l);
    if (info.layers.len > 0) allocator.free(info.layers);
    for (info.allow_net) |a| allocator.free(a);
    if (info.allow_net.len > 0) allocator.free(info.allow_net);
    for (info.snapshots) |s| {
        allocator.free(s.label);
        allocator.free(s.created);
    }
    if (info.snapshots.len > 0) allocator.free(info.snapshots);
}

/// Read a single metadata file from .meta/<name>.
fn readMetaFile(allocator: std.mem.Allocator, sandbox_path: []const u8, name: []const u8) ![]u8 {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const file_path = try std.fmt.bufPrint(&path_buf, "{s}/.meta/{s}", .{ sandbox_path, name });

    const content = try std.fs.cwd().readFileAlloc(allocator, file_path, 4096);
    // Trim trailing newlines/whitespace
    const trimmed = std.mem.trim(u8, content, "\n\r \t");
    if (trimmed.len == content.len) return content;

    const result = try allocator.dupe(u8, trimmed);
    allocator.free(content);
    return result;
}

/// Write a metadata file to .meta/<name>.
fn writeMetaFile(sandbox_path: []const u8, name: []const u8, content: []const u8) void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const file_path = std.fmt.bufPrint(&path_buf, "{s}/.meta/{s}", .{ sandbox_path, name }) catch return;

    const file = std.fs.createFileAbsolute(file_path, .{}) catch return;
    defer file.close();
    file.writeAll(content) catch {};
}

fn shouldProvisionKernelResources() bool {
    if (!is_linux) return false;
    return std.posix.geteuid() == 0;
}

fn isSandboxMounted(allocator: std.mem.Allocator, sandbox_path: []const u8) bool {
    const mounted_meta = readMetaFile(allocator, sandbox_path, "mounted") catch null;
    defer if (mounted_meta) |m| allocator.free(m);
    if (mounted_meta) |m| {
        if (std.mem.eql(u8, m, "1") or std.mem.eql(u8, m, "true")) return true;
        if (std.mem.eql(u8, m, "0") or std.mem.eql(u8, m, "false")) return false;
    }

    var merged_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_path = std.fmt.bufPrint(&merged_buf, "{s}/merged", .{sandbox_path}) catch return false;
    if (std.fs.accessAbsolute(merged_path, .{})) |_| {
        return true;
    } else |_| {
        return false;
    }
}

fn provisionSandboxResources(
    allocator: std.mem.Allocator,
    cfg: *const Config,
    sandbox_path: []const u8,
    sandbox_id: []const u8,
    layers: []const []const u8,
    cpu: f64,
    memory_mb: u64,
    allow_net: ?[]const []const u8,
) !void {
    if (cfg.backend == .firecracker) {
        return provisionFirecrackerSandbox(allocator, cfg, sandbox_path, sandbox_id, layers, cpu, memory_mb, allow_net);
    }

    var modules_dir_buf: [256]u8 = undefined;
    const modules_dir = try cfg.modulesDir(&modules_dir_buf);

    for (layers) |layer| {
        var mod_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const module_path = try std.fmt.bufPrint(&mod_path_buf, "{s}/{s}.squashfs", .{ modules_dir, layer });
        try mountModuleLayer(sandbox_path, module_path, layer);
    }

    var upper_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_z = try std.fmt.bufPrintZ(&upper_buf, "{s}/upper", .{sandbox_path});
    _ = try mounts_mod.TmpfsMount.mount(upper_z, cfg.upper_limit_mb);

    try remountOverlayForSandbox(allocator, sandbox_path);

    if (cgroup_mod.CgroupHandle.create(sandbox_id, cpu, memory_mb)) |cg| {
        writeMetaFile(sandbox_path, "cgroup_path", cg.path());
    } else |_| {}

    if (netns_mod.NetnsHandle.setup(allocator, cfg.data_dir, sandbox_id, allow_net)) |ns| {
        writeMetaFile(sandbox_path, "netns_name", ns.name);
        writeMetaFile(sandbox_path, "veth_host", ns.veth_host);

        var idx_buf: [8]u8 = undefined;
        const idx_str = std.fmt.bufPrint(&idx_buf, "{d}", .{ns.index}) catch "0";
        writeMetaFile(sandbox_path, "netns_index", idx_str);

        netns_mod.seedResolvConf(cfg.data_dir, sandbox_id, ns.index) catch {};
    } else |_| {}

    writeMetaFile(sandbox_path, "mounted", "1");
}

/// Provision a Firecracker sandbox: allocate CID, set up tap, start VM.
fn provisionFirecrackerSandbox(
    allocator: std.mem.Allocator,
    cfg: *const Config,
    sandbox_path: []const u8,
    sandbox_id: []const u8,
    layers: []const []const u8,
    cpu: f64,
    memory_mb: u64,
    allow_net: ?[]const []const u8,
) !void {
    // 1. Allocate CID
    const cid = try fc_mod.allocateCid(cfg.data_dir, allocator);
    fc_mod.writeCid(sandbox_path, cid);

    // 2. Allocate a network index (reuse the netns index allocator)
    const net_index: u8 = @intCast(cid % 254 + 1);
    fc_mod.writeNetIndex(sandbox_path, net_index);

    // 3. Set up tap networking
    const allow_net_str: ?[]const u8 = if (allow_net) |nets| blk: {
        if (nets.len > 0) {
            break :blk nets[0];
        }
        break :blk null;
    } else null;
    fc_mod.setupNetwork(sandbox_id, net_index, allow_net_str, allocator) catch |err| {
        log.warn("firecracker network setup failed: {}", .{err});
    };

    // 4. Build squashfs paths for VM start
    var modules_dir_buf: [256]u8 = undefined;
    const modules_dir = try cfg.modulesDir(&modules_dir_buf);

    var sqfs_paths: std.ArrayList([]const u8) = .empty;
    defer {
        for (sqfs_paths.items) |p| allocator.free(p);
        sqfs_paths.deinit(allocator);
    }
    for (layers) |layer| {
        const path = try std.fmt.allocPrint(allocator, "{s}/{s}.squashfs", .{ modules_dir, layer });
        try sqfs_paths.append(allocator, path);
    }

    // 5. Start VM
    var meta_buf: [std.fs.max_path_bytes]u8 = undefined;
    const meta_dir = try std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{sandbox_path});

    try fc_mod.startVm(sandbox_id, cpu, memory_mb, sqfs_paths.items, cid, meta_dir, allocator);

    writeMetaFile(sandbox_path, "mounted", "1");
}

fn runCommandSilent(allocator: std.mem.Allocator, argv: []const []const u8) void {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
    }) catch return;
    allocator.free(result.stdout);
    allocator.free(result.stderr);
}

fn cgroupHandleFromPath(path: []const u8) ?cgroup_mod.CgroupHandle {
    var handle = cgroup_mod.CgroupHandle{
        .path_buf = undefined,
        .path_len = 0,
    };
    if (path.len > handle.path_buf.len) return null;
    @memcpy(handle.path_buf[0..path.len], path);
    handle.path_len = path.len;
    return handle;
}

fn mountModuleLayer(sandbox_path: []const u8, module_path: []const u8, module_name: []const u8) !void {
    var mount_buf: [std.fs.max_path_bytes]u8 = undefined;
    const mount_path = try std.fmt.bufPrint(&mount_buf, "{s}/images/{s}.squashfs", .{ sandbox_path, module_name });
    std.fs.makeDirAbsolute(mount_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var source_z_buf: [std.fs.max_path_bytes]u8 = undefined;
    var target_z_buf: [std.fs.max_path_bytes]u8 = undefined;
    const source_z = try std.fmt.bufPrintZ(&source_z_buf, "{s}", .{module_path});
    const target_z = try std.fmt.bufPrintZ(&target_z_buf, "{s}", .{mount_path});
    _ = try mounts_mod.SquashfsMount.mount(source_z, target_z);
}

fn remountOverlayForSandbox(allocator: std.mem.Allocator, sandbox_path: []const u8) !void {
    const layers = readLayers(allocator, sandbox_path) catch &[_][]const u8{};
    defer {
        for (layers) |layer| allocator.free(layer);
        if (layers.len > 0) allocator.free(layers);
    }

    var lower_components: std.ArrayList([]const u8) = .empty;
    defer {
        for (lower_components.items) |item| allocator.free(item);
        lower_components.deinit(allocator);
    }

    const active_snapshot = readMetaFile(allocator, sandbox_path, "active_snapshot") catch null;
    defer if (active_snapshot) |s| allocator.free(s);

    if (active_snapshot) |label| {
        if (label.len > 0) {
            var snap_file_buf: [std.fs.max_path_bytes]u8 = undefined;
            const snap_file = try std.fmt.bufPrint(&snap_file_buf, "{s}/.meta/snapshots/{s}.squashfs", .{ sandbox_path, label });
            if (std.fs.accessAbsolute(snap_file, .{})) |_| {
                var snap_mp_buf: [std.fs.max_path_bytes]u8 = undefined;
                const snap_mp = try std.fmt.bufPrint(&snap_mp_buf, "{s}/images/_snapshot", .{sandbox_path});
                std.fs.makeDirAbsolute(snap_mp) catch {};

                var snap_file_z_buf: [std.fs.max_path_bytes]u8 = undefined;
                var snap_mp_z_buf: [std.fs.max_path_bytes]u8 = undefined;
                const snap_file_z = try std.fmt.bufPrintZ(&snap_file_z_buf, "{s}", .{snap_file});
                const snap_mp_z = try std.fmt.bufPrintZ(&snap_mp_z_buf, "{s}", .{snap_mp});
                _ = mounts_mod.SquashfsMount.mount(snap_file_z, snap_mp_z) catch {};
                try lower_components.append(allocator, try allocator.dupe(u8, snap_mp));
            } else |_| {}
        }
    }

    var idx: usize = layers.len;
    while (idx > 0) {
        idx -= 1;
        const layer = layers[idx];
        const layer_mp = try std.fmt.allocPrint(allocator, "{s}/images/{s}.squashfs", .{ sandbox_path, layer });
        errdefer allocator.free(layer_mp);
        try lower_components.append(allocator, layer_mp);
    }

    if (lower_components.items.len == 0) return error.FileNotFound;

    var merged_buf: [std.fs.max_path_bytes]u8 = undefined;
    var upper_data_buf: [std.fs.max_path_bytes]u8 = undefined;
    var upper_work_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_path = try std.fmt.bufPrintZ(&merged_buf, "{s}/merged", .{sandbox_path});
    const upper_data = try std.fmt.bufPrintZ(&upper_data_buf, "{s}/upper/data", .{sandbox_path});
    const upper_work = try std.fmt.bufPrintZ(&upper_work_buf, "{s}/upper/work", .{sandbox_path});

    var old_overlay = mounts_mod.OverlayMount{
        .merged_path = merged_path,
        .active = true,
    };
    old_overlay.deinit();
    _ = try mounts_mod.OverlayMount.mount(
        lower_components.items,
        upper_data,
        upper_work,
        merged_path,
    );
    writeMetaFile(sandbox_path, "mounted", "1");
}

pub fn teardownSandboxResources(allocator: std.mem.Allocator, sandbox_path: []const u8, id: []const u8) !void {
    writeMetaFile(sandbox_path, "mounted", "0");

    // Check if this is a firecracker sandbox (has fc.cid metadata)
    if (fc_mod.readCid(sandbox_path)) |cid| {
        _ = cid;
        teardownFirecrackerSandbox(allocator, sandbox_path, id);
        return;
    } else |_| {}

    // Chroot backend teardown
    var merged_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fmt.bufPrintZ(&merged_buf, "{s}/merged", .{sandbox_path})) |merged_z| {
        var overlay = mounts_mod.OverlayMount{
            .merged_path = merged_z,
            .active = true,
        };
        overlay.deinit();
    } else |_| {}

    var snap_mp_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fmt.bufPrintZ(&snap_mp_buf, "{s}/images/_snapshot", .{sandbox_path})) |snap_mp_z| {
        var snap_mount = mounts_mod.SquashfsMount{
            .mount_point = snap_mp_z,
            .active = true,
        };
        snap_mount.deinit();
    } else |_| {}

    const layers = readLayers(allocator, sandbox_path) catch &[_][]const u8{};
    defer {
        for (layers) |layer| allocator.free(layer);
        if (layers.len > 0) allocator.free(layers);
    }
    for (layers) |layer| {
        var layer_mp_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fmt.bufPrintZ(&layer_mp_buf, "{s}/images/{s}.squashfs", .{ sandbox_path, layer })) |layer_mp_z| {
            var layer_mount = mounts_mod.SquashfsMount{
                .mount_point = layer_mp_z,
                .active = true,
            };
            layer_mount.deinit();
        } else |_| {}
    }

    var upper_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fmt.bufPrintZ(&upper_buf, "{s}/upper", .{sandbox_path})) |upper_z| {
        var tmpfs = mounts_mod.TmpfsMount{
            .mount_point = upper_z,
            .active = true,
        };
        tmpfs.deinit();
    } else |_| {}

    const cgroup_path = readMetaFile(allocator, sandbox_path, "cgroup_path") catch null;
    defer if (cgroup_path) |p| allocator.free(p);
    if (cgroup_path) |path| {
        if (cgroupHandleFromPath(path)) |cg_value| {
            var cg = cg_value;
            cg.deinit();
        }
    } else {
        var default_cg_buf: [256]u8 = undefined;
        if (std.fmt.bufPrint(&default_cg_buf, "/sys/fs/cgroup/squash-{s}", .{id})) |p| {
            if (cgroupHandleFromPath(p)) |cg_value| {
                var cg = cg_value;
                cg.deinit();
            }
        } else |_| {}
    }

    const netns_name = readMetaFile(allocator, sandbox_path, "netns_name") catch null;
    defer if (netns_name) |n| allocator.free(n);
    if (netns_name) |name| {
        const veth_host = std.fmt.allocPrint(allocator, "sq-{s}-h", .{id}) catch "";
        defer if (veth_host.len > 0) allocator.free(veth_host);
        if (veth_host.len > 0) {
            runCommandSilent(allocator, &.{ "ip", "link", "delete", veth_host });
        }
        runCommandSilent(allocator, &.{ "ip", "netns", "delete", name });
    }
}

/// Write a Firecracker exec log entry to the log directory.
/// Best-effort — failures are logged but don't fail the exec.
fn writeFirecrackerExecLog(
    log_dir: []const u8,
    seq: u32,
    cmd: []const u8,
    workdir: []const u8,
    exit_code: i32,
    started_str: []const u8,
    finished_str: []const u8,
    stdout_data: []const u8,
    stderr_data: []const u8,
) void {
    // Ensure log directory exists
    std.fs.makeDirAbsolute(log_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return,
    };

    // Build log file path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const file_path = std.fmt.bufPrint(&path_buf, "{s}/{d:0>4}.json", .{ log_dir, seq }) catch return;

    // Build JSON content using a fixed buffer
    var buf: [exec_mod.max_output_bytes * 6 * 2 + 4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    exec_mod.writeLogJson(
        writer,
        seq,
        exec_mod.ExecRequest{ .cmd = cmd, .workdir = workdir },
        exit_code,
        started_str,
        finished_str,
        stdout_data,
        stderr_data,
        false,
    ) catch return;

    const json_bytes = stream.getWritten();

    const file = std.fs.createFileAbsolute(file_path, .{}) catch return;
    defer file.close();
    file.writeAll(json_bytes) catch {};
}

/// Tear down a Firecracker sandbox: stop VM, tear down tap networking.
fn teardownFirecrackerSandbox(allocator: std.mem.Allocator, sandbox_path: []const u8, id: []const u8) void {
    var meta_buf: [std.fs.max_path_bytes]u8 = undefined;
    const meta_dir = std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{sandbox_path}) catch return;

    // Stop the VM
    fc_mod.stopVm(id, meta_dir, allocator);

    // Tear down tap networking
    const net_index = fc_mod.readNetIndex(sandbox_path) catch 0;
    if (net_index > 0) {
        fc_mod.teardownNetwork(id, net_index, allocator);
    }
}

/// Read layers from .meta/layers (newline-separated).
fn readLayers(allocator: std.mem.Allocator, sandbox_path: []const u8) ![]const []const u8 {
    const content = readMetaFile(allocator, sandbox_path, "layers") catch return error.FileNotFound;
    defer allocator.free(content);

    if (content.len == 0) return &[_][]const u8{};

    var list: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (list.items) |s| allocator.free(s);
        list.deinit(allocator);
    }

    var iter = std.mem.splitScalar(u8, content, '\n');
    while (iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \r");
        if (trimmed.len > 0) {
            try list.append(allocator, try allocator.dupe(u8, trimmed));
        }
    }

    return list.toOwnedSlice(allocator);
}

/// Read allow_net from .meta/allow_net (JSON array).
fn readAllowNet(allocator: std.mem.Allocator, sandbox_path: []const u8) ![]const []const u8 {
    const content = readMetaFile(allocator, sandbox_path, "allow_net") catch return error.FileNotFound;
    defer allocator.free(content);

    if (content.len == 0) return &[_][]const u8{};

    // Parse as JSON array
    const parsed = std.json.parseFromSlice([]const []const u8, allocator, content, .{
        .allocate = .alloc_always,
    }) catch return &[_][]const u8{};
    defer parsed.deinit();

    // Dupe for caller ownership
    var result = try allocator.alloc([]const u8, parsed.value.len);
    for (parsed.value, 0..) |s, i| {
        result[i] = try allocator.dupe(u8, s);
    }

    return result;
}

/// Read snapshot metadata from .meta/snapshots/ directory.
fn readSnapshots(allocator: std.mem.Allocator, sandbox_path: []const u8) ![]const json_mod.SnapshotInfo {
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_dir_path = try std.fmt.bufPrint(&dir_buf, "{s}/.meta/snapshots", .{sandbox_path});

    var dir = std.fs.openDirAbsolute(snap_dir_path, .{ .iterate = true }) catch return &[_]json_mod.SnapshotInfo{};
    defer dir.close();

    var list: std.ArrayList(json_mod.SnapshotInfo) = .empty;
    errdefer {
        for (list.items) |s| {
            allocator.free(s.label);
            allocator.free(s.created);
        }
        list.deinit(allocator);
    }

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".squashfs")) continue;

        const label_raw = entry.name[0 .. entry.name.len - ".squashfs".len];
        if (!validate.validLabel(label_raw)) continue;

        const stat = dir.statFile(entry.name) catch continue;

        // Format mtime as ISO timestamp
        var ts_buf: [32]u8 = undefined;
        const mtime_secs: i64 = @intCast(@divFloor(stat.mtime, std.time.ns_per_s));
        const created_str = json_mod.formatTimestamp(&ts_buf, mtime_secs) catch "?";

        const label = try allocator.dupe(u8, label_raw);
        errdefer allocator.free(label);
        const created = try allocator.dupe(u8, created_str);

        try list.append(allocator, json_mod.SnapshotInfo{
            .label = label,
            .created = created,
            .size = stat.size,
        });
    }

    return list.toOwnedSlice(allocator);
}

/// Create a sandbox directory structure on disk.
fn createSandboxOnDisk(
    allocator: std.mem.Allocator,
    sandbox_path: []const u8,
    req: *const json_mod.CreateRequest,
    layers: []const []const u8,
) !void {
    // Create directory tree
    try std.fs.makeDirAbsolute(sandbox_path);
    errdefer std.fs.deleteTreeAbsolute(sandbox_path) catch {};

    var meta_buf: [std.fs.max_path_bytes]u8 = undefined;
    const meta_path = try std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{sandbox_path});
    try std.fs.makeDirAbsolute(meta_path);

    var log_buf: [std.fs.max_path_bytes]u8 = undefined;
    const log_path = try std.fmt.bufPrint(&log_buf, "{s}/.meta/log", .{sandbox_path});
    std.fs.makeDirAbsolute(log_path) catch {};

    var images_buf: [std.fs.max_path_bytes]u8 = undefined;
    const images_path = try std.fmt.bufPrint(&images_buf, "{s}/images", .{sandbox_path});
    std.fs.makeDirAbsolute(images_path) catch {};

    var merged_buf: [std.fs.max_path_bytes]u8 = undefined;
    const merged_path = try std.fmt.bufPrint(&merged_buf, "{s}/merged", .{sandbox_path});
    std.fs.makeDirAbsolute(merged_path) catch {};

    var upper_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_path = try std.fmt.bufPrint(&upper_buf, "{s}/upper", .{sandbox_path});
    std.fs.makeDirAbsolute(upper_path) catch {};

    var upper_data_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_data_path = try std.fmt.bufPrint(&upper_data_buf, "{s}/upper/data", .{sandbox_path});
    std.fs.makeDirAbsolute(upper_data_path) catch {};

    var upper_work_buf: [std.fs.max_path_bytes]u8 = undefined;
    const upper_work_path = try std.fmt.bufPrint(&upper_work_buf, "{s}/upper/work", .{sandbox_path});
    std.fs.makeDirAbsolute(upper_work_path) catch {};

    // Write metadata files
    writeMetaFile(sandbox_path, "owner", req.effectiveOwner());

    if (req.task) |task| {
        writeMetaFile(sandbox_path, "task", task);
    }

    // Write layers (newline-separated)
    if (layers.len > 0) {
        var layers_buf: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&layers_buf);
        const writer = stream.writer();
        for (layers, 0..) |layer, i| {
            if (i > 0) writer.writeByte('\n') catch break;
            writer.writeAll(layer) catch break;
        }
        writeMetaFile(sandbox_path, "layers", stream.getWritten());
    }

    // Write timestamps
    var ts_buf: [32]u8 = undefined;
    const now = std.time.timestamp();
    const ts_str = json_mod.formatTimestamp(&ts_buf, now) catch "?";
    writeMetaFile(sandbox_path, "created", ts_str);
    writeMetaFile(sandbox_path, "last_active", ts_str);
    var created_ts_buf: [32]u8 = undefined;
    const created_ts = std.fmt.bufPrint(&created_ts_buf, "{d}", .{now}) catch "0";
    writeMetaFile(sandbox_path, "created_ts", created_ts);
    writeMetaFile(sandbox_path, "mounted", "0");

    // Write numeric config
    var cpu_buf: [32]u8 = undefined;
    const cpu_str = std.fmt.bufPrint(&cpu_buf, "{d}", .{req.effectiveCpu()}) catch "2";
    writeMetaFile(sandbox_path, "cpu", cpu_str);

    var mem_buf: [32]u8 = undefined;
    const mem_str = std.fmt.bufPrint(&mem_buf, "{d}", .{req.effectiveMemoryMb()}) catch "1024";
    writeMetaFile(sandbox_path, "memory_mb", mem_str);

    var life_buf: [32]u8 = undefined;
    const life_str = std.fmt.bufPrint(&life_buf, "{d}", .{req.effectiveMaxLifetimeS()}) catch "0";
    writeMetaFile(sandbox_path, "max_lifetime_s", life_str);

    // Write allow_net as JSON array
    if (req.allow_net) |nets| {
        const allow_net_json = json_mod.stringify(allocator, nets) catch null;
        if (allow_net_json) |s| {
            defer allocator.free(s);
            writeMetaFile(sandbox_path, "allow_net", s);
        }
    } else {
        writeMetaFile(sandbox_path, "allow_net", "[]");
    }
}

/// Count directory entries (for sandbox/module counts).
fn countDirEntries(cfg: *const Config, subdir: []const u8) u32 {
    var buf: [256]u8 = undefined;
    const dir_path = std.fmt.bufPrint(&buf, "{s}/{s}", .{ cfg.data_dir, subdir }) catch return 0;

    var dir = std.fs.openDirAbsolute(dir_path, .{ .iterate = true }) catch return 0;
    defer dir.close();

    var count: u32 = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (std.mem.eql(u8, subdir, "modules")) {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".squashfs")) count += 1;
        } else {
            if (entry.kind == .directory and validate.validId(entry.name)) count += 1;
        }
    }
    return count;
}

/// Check if a base module (000-*) exists in the modules directory.
fn checkBaseReady(cfg: *const Config) bool {
    var buf: [256]u8 = undefined;
    const dir_path = cfg.modulesDir(&buf) catch return false;

    var dir = std.fs.openDirAbsolute(dir_path, .{ .iterate = true }) catch return false;
    defer dir.close();

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind == .file and std.mem.startsWith(u8, entry.name, "000-") and std.mem.endsWith(u8, entry.name, ".squashfs")) {
            return true;
        }
    }
    return false;
}

/// Count exec log entries in a directory.
fn countLogEntries(log_dir: []const u8) u32 {
    if (log_dir.len == 0) return 0;
    var dir = std.fs.openDirAbsolute(log_dir, .{ .iterate = true }) catch return 0;
    defer dir.close();

    var count: u32 = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".json")) count += 1;
    }
    return count;
}

/// Get the next sequence number from existing log files.
fn getNextSeq(log_dir: []const u8) u32 {
    var dir = std.fs.openDirAbsolute(log_dir, .{ .iterate = true }) catch return 1;
    defer dir.close();

    var max_seq: u32 = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".json")) continue;
        // Parse sequence number from filename like "0001.json"
        const stem = entry.name[0 .. entry.name.len - ".json".len];
        const seq = std.fmt.parseInt(u32, stem, 10) catch continue;
        if (seq > max_seq) max_seq = seq;
    }
    return max_seq + 1;
}

/// Get approximate size of a directory's contents.
fn getDirSize(dir_path: []const u8) u64 {
    if (dir_path.len == 0) return 0;
    var dir = std.fs.openDirAbsolute(dir_path, .{ .iterate = true }) catch return 0;
    defer dir.close();

    var total: u64 = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind == .file) {
            const stat = dir.statFile(entry.name) catch continue;
            total += stat.size;
        }
    }
    return total;
}

/// Update the last_active timestamp for a sandbox.
fn updateLastActive(sandbox_path: []const u8, timestamp: i64) void {
    var ts_buf: [32]u8 = undefined;
    const ts_str = json_mod.formatTimestamp(&ts_buf, timestamp) catch return;
    writeMetaFile(sandbox_path, "last_active", ts_str);
}

/// Generate a snapshot label from a timestamp: YYYYMMDD-HHMMSS
fn generateSnapshotLabel(buf: []u8, unix_ts: i64) []const u8 {
    const epoch_secs: std.time.epoch.EpochSeconds = .{ .secs = @intCast(unix_ts) };
    const day = epoch_secs.getDaySeconds();
    const year_day = epoch_secs.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    return std.fmt.bufPrint(buf, "{d:0>4}{d:0>2}{d:0>2}-{d:0>2}{d:0>2}{d:0>2}", .{
        year_day.year,
        month_day.month.numeric(),
        month_day.day_index + 1,
        day.getHoursIntoDay(),
        day.getMinutesIntoHour(),
        day.getSecondsIntoMinute(),
    }) catch "snapshot";
}

// ── Tests ──────────────────────────────────────────────────────────────

test "matchRoute health" {
    const r = matchRoute(.GET, "/cgi-bin/health");
    try std.testing.expect(r == .health);
}

test "matchRoute list sandboxes" {
    const r = matchRoute(.GET, "/cgi-bin/api/sandboxes");
    try std.testing.expect(r == .list_sandboxes);
}

test "matchRoute create sandbox" {
    const r = matchRoute(.POST, "/cgi-bin/api/sandboxes");
    try std.testing.expect(r == .create_sandbox);
}

test "matchRoute get sandbox by id" {
    const r = matchRoute(.GET, "/cgi-bin/api/sandboxes/my-box");
    switch (r) {
        .get_sandbox => |id| try std.testing.expectEqualStrings("my-box", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute delete sandbox" {
    const r = matchRoute(.DELETE, "/cgi-bin/api/sandboxes/test-1");
    switch (r) {
        .delete_sandbox => |id| try std.testing.expectEqualStrings("test-1", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute exec" {
    const r = matchRoute(.POST, "/cgi-bin/api/sandboxes/dev/exec");
    switch (r) {
        .exec_sandbox => |id| try std.testing.expectEqualStrings("dev", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute logs" {
    const r = matchRoute(.GET, "/cgi-bin/api/sandboxes/dev/logs");
    switch (r) {
        .exec_logs => |id| try std.testing.expectEqualStrings("dev", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute snapshot" {
    const r = matchRoute(.POST, "/cgi-bin/api/sandboxes/sb1/snapshot");
    switch (r) {
        .snapshot_sandbox => |id| try std.testing.expectEqualStrings("sb1", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute restore" {
    const r = matchRoute(.POST, "/cgi-bin/api/sandboxes/sb1/restore");
    switch (r) {
        .restore_sandbox => |id| try std.testing.expectEqualStrings("sb1", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute activate" {
    const r = matchRoute(.POST, "/cgi-bin/api/sandboxes/sb1/activate");
    switch (r) {
        .activate_sandbox => |id| try std.testing.expectEqualStrings("sb1", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute list modules" {
    const r = matchRoute(.GET, "/cgi-bin/api/modules");
    try std.testing.expect(r == .list_modules);
}

test "matchRoute not found" {
    const r = matchRoute(.GET, "/nonexistent");
    try std.testing.expect(r == .not_found);
}

test "matchRoute unknown action" {
    const r = matchRoute(.POST, "/cgi-bin/api/sandboxes/sb1/unknown");
    try std.testing.expect(r == .not_found);
}

test "matchRoute invalid sandbox id" {
    const r = matchRoute(.GET, "/cgi-bin/api/sandboxes/../../etc");
    try std.testing.expect(r == .not_found);
}

test "matchRoute strips query string" {
    const r = matchRoute(.GET, "/cgi-bin/health?foo=bar");
    try std.testing.expect(r == .health);
}

test "matchRoute sandbox id with query string" {
    const r = matchRoute(.GET, "/cgi-bin/api/sandboxes/dev?details=true");
    switch (r) {
        .get_sandbox => |id| try std.testing.expectEqualStrings("dev", id),
        else => return error.UnexpectedRoute,
    }
}

test "matchRoute PUT on sandboxes returns not_found" {
    const r = matchRoute(.PUT, "/cgi-bin/api/sandboxes");
    try std.testing.expect(r == .not_found);
}

test "generateSnapshotLabel formats correctly" {
    var buf: [32]u8 = undefined;
    // 2025-01-15T00:00:00 = 1736899200
    const label = generateSnapshotLabel(&buf, 1736899200);
    try std.testing.expectEqualStrings("20250115-000000", label);
}

test "generateSnapshotLabel with time components" {
    var buf: [32]u8 = undefined;
    // 2025-02-14T15:30:45 = 1739544645
    const label = generateSnapshotLabel(&buf, 1739544645);
    // Just verify it has the right format (XXXXXXXX-XXXXXX)
    try std.testing.expectEqual(@as(usize, 15), label.len);
    try std.testing.expectEqual(@as(u8, '-'), label[8]);
}

test "countDirEntries returns 0 for nonexistent dir" {
    const cfg = Config{ .data_dir = "/nonexistent/test/dir" };
    try std.testing.expectEqual(@as(u32, 0), countDirEntries(&cfg, "sandboxes"));
    try std.testing.expectEqual(@as(u32, 0), countDirEntries(&cfg, "modules"));
}

test "checkBaseReady returns false for nonexistent dir" {
    const cfg = Config{ .data_dir = "/nonexistent/test/dir" };
    try std.testing.expect(!checkBaseReady(&cfg));
}

test "getNextSeq returns 1 for nonexistent dir" {
    try std.testing.expectEqual(@as(u32, 1), getNextSeq("/nonexistent/dir"));
}

test "getDirSize returns 0 for empty path" {
    try std.testing.expectEqual(@as(u64, 0), getDirSize(""));
}

test "getDirSize returns 0 for nonexistent dir" {
    try std.testing.expectEqual(@as(u64, 0), getDirSize("/nonexistent/dir"));
}

test "createSandboxOnDisk and readSandboxInfo round-trip" {
    const allocator = std.testing.allocator;

    // Use a temp directory
    const test_dir = "/tmp/squash-test-api-sandbox";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    // Create sandbox
    const body =
        \\{"id":"test-sb","owner":"alice","layers":"base,python","task":"testing","cpu":4.0,"memory_mb":2048,"max_lifetime_s":3600}
    ;
    const parsed = try json_mod.parse(json_mod.CreateRequest, allocator, body);
    defer parsed.deinit();

    const layers = [_][]const u8{ "base", "python" };

    try createSandboxOnDisk(allocator, test_dir, &parsed.value, &layers);

    // Read it back — use the parent directory as sandboxes_dir
    // We need to set up the directory structure properly
    const parent_dir = "/tmp/squash-test-api-sandboxes";
    std.fs.deleteTreeAbsolute(parent_dir) catch {};
    defer std.fs.deleteTreeAbsolute(parent_dir) catch {};

    try std.fs.makeDirAbsolute(parent_dir);

    // Create sandbox in the proper location
    const sb_path_buf = try std.fmt.allocPrint(allocator, "{s}/my-sb", .{parent_dir});
    defer allocator.free(sb_path_buf);

    try createSandboxOnDisk(allocator, sb_path_buf, &parsed.value, &layers);

    const info = try readSandboxInfo(allocator, parent_dir, "my-sb");
    defer freeSandboxInfo(allocator, info);

    try std.testing.expectEqualStrings("my-sb", info.id);
    try std.testing.expectEqualStrings("alice", info.owner);
    try std.testing.expectEqualStrings("testing", info.task.?);
    try std.testing.expectEqual(@as(usize, 2), info.layers.len);
    try std.testing.expectEqualStrings("base", info.layers[0]);
    try std.testing.expectEqualStrings("python", info.layers[1]);
    try std.testing.expectEqual(@as(f64, 4.0), info.cpu);
    try std.testing.expectEqual(@as(u64, 2048), info.memory_mb);
    try std.testing.expectEqual(@as(u64, 3600), info.max_lifetime_s);
    try std.testing.expect(info.created.len > 0);
}

test "readSandboxInfo returns error for nonexistent sandbox" {
    const allocator = std.testing.allocator;
    const result = readSandboxInfo(allocator, "/nonexistent", "nope");
    try std.testing.expectError(error.FileNotFound, result);
}

test "writeMetaFile and readMetaFile round-trip" {
    const allocator = std.testing.allocator;

    const test_dir = "/tmp/squash-test-meta-rw";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    try std.fs.makeDirAbsolute(test_dir);

    var meta_buf: [std.fs.max_path_bytes]u8 = undefined;
    const meta_dir = try std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{test_dir});
    try std.fs.makeDirAbsolute(meta_dir);

    writeMetaFile(test_dir, "test_key", "test_value");

    const value = try readMetaFile(allocator, test_dir, "test_key");
    defer allocator.free(value);

    try std.testing.expectEqualStrings("test_value", value);
}

test "readMetaFile returns error for missing file" {
    const allocator = std.testing.allocator;
    const result = readMetaFile(allocator, "/nonexistent", "missing");
    try std.testing.expectError(error.FileNotFound, result);
}

test "countLogEntries returns 0 for empty or missing dir" {
    try std.testing.expectEqual(@as(u32, 0), countLogEntries(""));
    try std.testing.expectEqual(@as(u32, 0), countLogEntries("/nonexistent"));
}

test "readLayers parses newline-separated content" {
    const allocator = std.testing.allocator;

    const test_dir = "/tmp/squash-test-layers";
    std.fs.deleteTreeAbsolute(test_dir) catch {};
    defer std.fs.deleteTreeAbsolute(test_dir) catch {};

    try std.fs.makeDirAbsolute(test_dir);
    var meta_buf: [std.fs.max_path_bytes]u8 = undefined;
    const meta_dir = try std.fmt.bufPrint(&meta_buf, "{s}/.meta", .{test_dir});
    try std.fs.makeDirAbsolute(meta_dir);

    writeMetaFile(test_dir, "layers", "000-base-alpine\n100-python312\n200-node");

    const layers = try readLayers(allocator, test_dir);
    defer {
        for (layers) |l| allocator.free(l);
        allocator.free(layers);
    }

    try std.testing.expectEqual(@as(usize, 3), layers.len);
    try std.testing.expectEqualStrings("000-base-alpine", layers[0]);
    try std.testing.expectEqualStrings("100-python312", layers[1]);
    try std.testing.expectEqualStrings("200-node", layers[2]);
}
