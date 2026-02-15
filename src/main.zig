const std = @import("std");
pub const config = @import("config.zig");
pub const validate = @import("validate.zig");
pub const exec = @import("exec.zig");
pub const snapshot = @import("snapshot.zig");
pub const init_mod = @import("init.zig");
pub const reaper = @import("reaper.zig");
pub const json = @import("json.zig");
pub const sigv4 = @import("sigv4.zig");
pub const s3 = @import("s3.zig");
pub const api = @import("api.zig");
pub const mounts = @import("mounts.zig");
pub const cgroup = @import("cgroup.zig");
pub const netns = @import("netns.zig");
pub const sandbox_test = @import("sandbox_test.zig");
pub const manager = @import("manager.zig");
pub const firecracker = @import("firecracker.zig");

const log = std.log.scoped(.squashd);

/// Signal handler: sets the atomic shutdown flag so the accept loop exits.
fn handleSignal(_: c_int) callconv(.c) void {
    api.shutdown_requested.store(true, .release);
}

/// Install SIGTERM and SIGINT handlers for graceful shutdown.
fn installSignalHandlers() void {
    const handler: std.posix.Sigaction = .{
        .handler = .{ .handler = handleSignal },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.posix.sigaction(std.posix.SIG.TERM, &handler, null);
    std.posix.sigaction(std.posix.SIG.INT, &handler, null);
}

pub fn main() !void {
    // Use GeneralPurposeAllocator for the daemon lifetime. It catches
    // double-free and use-after-free in debug builds, while being
    // production-ready in release mode.
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const cfg = config.Config.fromEnv();

    log.info("squash v4 (zig) — port={d} backend={s} data={s}", .{
        cfg.port,
        @tagName(cfg.backend),
        cfg.data_dir,
    });

    // 1. Init / recovery — create dirs, ensure base image, clean stale resources
    init_mod.run(&cfg);

    // 2. Initialize sandbox manager
    var mgr = manager.SandboxManager.init(allocator, cfg.max_sandboxes);
    defer {
        manager.destroyAll(&mgr);
        mgr.deinit();
    }
    mgr.setGlobal();
    mgr.setDataDir(cfg.data_dir);
    mgr.recoverFromDisk(cfg.data_dir);

    // 3. Start HTTPS proxy (if configured)
    if (cfg.proxy_https) {
        startProxy(&cfg);
    }

    // 4. Start tailscale (if configured)
    if (cfg.tailscale_authkey != null) {
        startTailscale(&cfg);
    }

    // 5. Start reaper thread — scans for expired sandboxes
    const reaper_cfg = mgr.reaperConfig();
    const reaper_thread = std.Thread.spawn(.{}, reaper.run, .{reaper_cfg}) catch |err| {
        log.err("failed to spawn reaper thread: {}", .{err});
        return err;
    };
    reaper_thread.detach();
    log.info("reaper thread started", .{});

    // 6. Install signal handlers for graceful shutdown
    installSignalHandlers();

    // 7. Start HTTP server (returns on SIGTERM/SIGINT)
    log.info("starting HTTP server on port {d}", .{cfg.port});
    try api.startServer(&cfg, allocator);

    // 8. Cleanup runs via defers (destroyAll, mgr.deinit, gpa.deinit)
    log.info("graceful shutdown complete", .{});
}

// ── Proxy and Tailscale startup ──────────────────────────────────────

/// Start the HTTPS forward proxy in a background thread.
/// Shells out to the bundled sq-proxy helper or spawns an inline listener.
fn startProxy(cfg: *const config.Config) void {
    var proxy_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const proxy_path = std.fmt.bufPrint(&proxy_path_buf, "{s}/proxy/sq-proxy", .{cfg.data_dir}) catch "sq-proxy";

    var child = std.process.Child.init(
        &.{ proxy_path, "--listen", "0.0.0.0:8888", "--data-dir", cfg.data_dir },
        std.heap.page_allocator,
    );
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    child.spawn() catch {
        // Fallback to PATH lookup if bundled helper does not exist.
        var fallback = std.process.Child.init(
            &.{ "sq-proxy", "--listen", "0.0.0.0:8888", "--data-dir", cfg.data_dir },
            std.heap.page_allocator,
        );
        fallback.stdin_behavior = .Ignore;
        fallback.stdout_behavior = .Ignore;
        fallback.stderr_behavior = .Ignore;
        fallback.spawn() catch |err| {
            log.warn("HTTPS proxy: failed to start: {}", .{err});
            return;
        };
    };
    log.info("HTTPS proxy started on :8888", .{});
}

/// Start tailscale in a background thread.
/// Runs `tailscale up --authkey=... --hostname=...` and monitors status.
fn startTailscale(cfg: *const config.Config) void {
    const authkey = cfg.tailscale_authkey orelse return;

    var authkey_buf: [256]u8 = undefined;
    var hostname_buf: [256]u8 = undefined;
    const auth_arg = std.fmt.bufPrint(&authkey_buf, "--authkey={s}", .{authkey}) catch return;
    const hostname_arg = std.fmt.bufPrint(&hostname_buf, "--hostname={s}", .{cfg.tailscale_hostname}) catch return;

    var child = std.process.Child.init(
        &.{ "tailscale", "up", auth_arg, hostname_arg },
        std.heap.page_allocator,
    );
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;
    child.spawn() catch |err| {
        log.warn("tailscale: failed to start: {}", .{err});
        return;
    };
    log.info("tailscale startup command issued for {s}", .{cfg.tailscale_hostname});
}

// ── Pull in tests from all modules ──────────────────────────────────

test {
    _ = config;
    _ = validate;
    _ = exec;
    _ = snapshot;
    _ = init_mod;
    _ = reaper;
    _ = json;
    _ = sigv4;
    _ = s3;
    _ = api;
    _ = mounts;
    _ = cgroup;
    _ = netns;
    _ = sandbox_test;
    _ = manager;
    _ = firecracker;
}
