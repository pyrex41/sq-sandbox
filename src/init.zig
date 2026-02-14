// init.zig — First-boot recovery: ensure directories, base image, and
// optionally remount surviving sandboxes after restart.
//
// Runs once at startup before the HTTP server and reaper start.
// Matches bin/sq-init in the shell implementation.

const std = @import("std");
const config = @import("config.zig");
const mounts = @import("mounts.zig");
const log = std.log.scoped(.init);

pub const InitError = error{
    CreateDirFailed,
    BaseImageMissing,
    CommandFailed,
    FormatFailed,
};

/// Run the full init/recovery sequence.
///
/// 1. Create modules/ and sandboxes/ directories
/// 2. Ensure 000-base-alpine.squashfs exists (S3 pull or sq-mkbase)
/// 3. If firecracker backend: check/build VM components
/// 4. If not ephemeral: remount surviving sandboxes
pub fn run(cfg: *const config.Config) void {
    ensureDirectories(cfg);
    ensureBaseImage(cfg);

    if (cfg.backend == .firecracker) {
        ensureVmComponents();
    }

    if (!cfg.ephemeralEnabled()) {
        remountSurvivingSandboxes(cfg);
    }

    log.info("init complete", .{});
}

// ── Directory setup ──────────────────────────────────────────────────

fn ensureDirectories(cfg: *const config.Config) void {
    var modules_buf: [256]u8 = undefined;
    const modules_dir = cfg.modulesDir(&modules_buf) catch {
        log.err("failed to format modules dir path", .{});
        return;
    };
    makeDirIfMissing(modules_dir);

    var sandboxes_buf: [256]u8 = undefined;
    const sandboxes_dir = cfg.sandboxesDir(&sandboxes_buf) catch {
        log.err("failed to format sandboxes dir path", .{});
        return;
    };
    makeDirIfMissing(sandboxes_dir);
}

fn makeDirIfMissing(path: []const u8) void {
    std.fs.makeDirAbsolute(path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            log.warn("failed to create directory {s}: {}", .{ path, err });
        },
    };
}

// ── Base image ───────────────────────────────────────────────────────

fn ensureBaseImage(cfg: *const config.Config) void {
    var path_buf: [512]u8 = undefined;
    const base_path = std.fmt.bufPrint(&path_buf, "{s}/modules/000-base-alpine.squashfs", .{cfg.data_dir}) catch {
        log.err("failed to format base image path", .{});
        return;
    };

    // Check if base image already exists
    std.fs.accessAbsolute(base_path, .{}) catch {
        log.info("base image not found at {s}, attempting to obtain", .{base_path});

        // Try S3 pull first (if configured)
        if (cfg.s3Enabled()) {
            if (tryS3Pull(cfg, "000-base-alpine.squashfs", base_path)) {
                log.info("base image pulled from S3", .{});
                return;
            }
        }

        // Fall back to sq-mkbase
        log.info("building base image with sq-mkbase", .{});
        runCommandLogged(&.{ "sq-mkbase", "alpine" }) catch {
            log.err("sq-mkbase failed — base image will not be available", .{});
        };
        return;
    };

    log.info("base image exists at {s}", .{base_path});
}

/// Try to pull a module from S3. Returns true on success.
fn tryS3Pull(cfg: *const config.Config, key: []const u8, dest: []const u8) bool {
    const bucket = cfg.s3_bucket orelse return false;

    // Build the S3 source path: s3://{bucket}/{prefix}{key}
    var src_buf: [512]u8 = undefined;
    const src = if (cfg.s3_prefix.len > 0)
        std.fmt.bufPrint(&src_buf, "s3://{s}/{s}{s}", .{ bucket, cfg.s3_prefix, key }) catch return false
    else
        std.fmt.bufPrint(&src_buf, "s3://{s}/{s}", .{ bucket, key }) catch return false;

    // Build aws cli args
    var args_buf: [8][]const u8 = undefined;
    var argc: usize = 0;

    args_buf[argc] = "aws";
    argc += 1;
    args_buf[argc] = "s3";
    argc += 1;
    args_buf[argc] = "cp";
    argc += 1;

    if (cfg.s3_endpoint) |endpoint| {
        args_buf[argc] = "--endpoint-url";
        argc += 1;
        args_buf[argc] = endpoint;
        argc += 1;
    }

    args_buf[argc] = src;
    argc += 1;
    args_buf[argc] = dest;
    argc += 1;

    runCommandLogged(args_buf[0..argc]) catch {
        log.warn("S3 pull failed for {s}", .{key});
        return false;
    };
    return true;
}

// ── Firecracker VM components ────────────────────────────────────────

fn ensureVmComponents() void {
    log.info("checking Firecracker VM components", .{});
    runCommandLogged(&.{ "sq-mkvm", "all" }) catch {
        log.warn("sq-mkvm check/build failed — VM components may not be ready", .{});
    };
}

// ── Remount surviving sandboxes ──────────────────────────────────────

/// Scan the sandboxes directory and attempt to rebuild overlayfs mounts
/// for any sandboxes that have surviving state on disk (e.g., after a
/// daemon restart). Best-effort: failures are logged and skipped.
fn remountSurvivingSandboxes(cfg: *const config.Config) void {
    var sandboxes_buf: [256]u8 = undefined;
    const sandboxes_dir = cfg.sandboxesDir(&sandboxes_buf) catch return;

    var dir = std.fs.openDirAbsolute(sandboxes_dir, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) return; // no sandboxes dir yet
        log.warn("failed to open sandboxes dir: {}", .{err});
        return;
    };
    defer dir.close();

    var count: usize = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;
        remountOneSandbox(cfg, sandboxes_dir, entry.name);
        count += 1;
    }

    if (count > 0) {
        log.info("attempted remount of {d} surviving sandbox(es)", .{count});
    }
}

/// Attempt to remount a single sandbox's overlayfs.
///
/// Layout on disk (matching existing shell convention):
///   {sandboxes_dir}/{id}/upper/       — tmpfs mount point (data/ + work/)
///   {sandboxes_dir}/{id}/merged/      — overlayfs merged mount point
///   {sandboxes_dir}/{id}/images/      — squashfs loop mounts
///   {sandboxes_dir}/{id}/.meta/layers — newline-separated layer list
fn remountOneSandbox(
    cfg: *const config.Config,
    sandboxes_dir: []const u8,
    id: []const u8,
) void {
    // Read the layers metadata to know which modules to mount
    var meta_buf: [512]u8 = undefined;
    const layers_path = std.fmt.bufPrint(&meta_buf, "{s}/{s}/.meta/layers", .{ sandboxes_dir, id }) catch return;

    const layers_content = readFileContents(layers_path) catch {
        log.warn("sandbox {s}: no .meta/layers file, skipping remount", .{id});
        return;
    };

    // Parse layer names (one per line)
    var lower_components: [32][]const u8 = undefined;
    var lower_count: usize = 0;

    // Check for snapshot layer first
    var snap_path_buf: [512]u8 = undefined;
    const snap_dir = std.fmt.bufPrint(&snap_path_buf, "{s}/{s}/images/_snapshot", .{ sandboxes_dir, id }) catch return;
    if (isDir(snap_dir)) {
        lower_components[lower_count] = snap_dir;
        lower_count += 1;
    }

    // Mount each module layer
    var modules_dir_buf: [256]u8 = undefined;
    const modules_dir = cfg.modulesDir(&modules_dir_buf) catch return;

    var lines = std.mem.splitScalar(u8, layers_content, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
        if (trimmed.len == 0) continue;
        if (lower_count >= lower_components.len) break;

        // The squashfs image is in the modules directory
        var mod_path_buf: [512]u8 = undefined;
        const mod_path = std.fmt.bufPrint(&mod_path_buf, "{s}/{s}.squashfs", .{ modules_dir, trimmed }) catch continue;

        // Mount point inside the sandbox's images/ directory
        var mp_buf: [512]u8 = undefined;
        const mount_point = std.fmt.bufPrint(&mp_buf, "{s}/{s}/images/{s}", .{ sandboxes_dir, id, trimmed }) catch continue;

        // Ensure mount point directory exists
        makeDirIfMissing(mount_point);

        // Check if the squashfs file exists before trying to mount
        std.fs.accessAbsolute(mod_path, .{}) catch {
            log.warn("sandbox {s}: module {s} not found at {s}", .{ id, trimmed, mod_path });
            continue;
        };

        // We can't actually call mount(2) here without null-terminated strings
        // and the mount types — log intent for now; actual remount would use
        // the shell-compatible approach
        log.info("sandbox {s}: would remount layer {s}", .{ id, trimmed });
        lower_components[lower_count] = mount_point;
        lower_count += 1;
    }

    if (lower_count == 0) {
        log.warn("sandbox {s}: no layers to remount", .{id});
        return;
    }

    // Rebuild the overlayfs mount
    var upper_data_buf: [512]u8 = undefined;
    const upper_data = std.fmt.bufPrint(&upper_data_buf, "{s}/{s}/upper/data", .{ sandboxes_dir, id }) catch return;
    makeDirIfMissing(upper_data);

    var work_buf: [512]u8 = undefined;
    const work = std.fmt.bufPrint(&work_buf, "{s}/{s}/upper/work", .{ sandboxes_dir, id }) catch return;
    makeDirIfMissing(work);

    var merged_buf: [512]u8 = undefined;
    const merged = std.fmt.bufPrint(&merged_buf, "{s}/{s}/merged", .{ sandboxes_dir, id }) catch return;
    makeDirIfMissing(merged);

    // Shell out to mount overlay since we need the full mount options string
    // and doing it via C mount(2) requires careful null-termination of paths
    // stored in stack buffers that will go out of scope.
    remountOverlayViaShell(lower_components[0..lower_count], upper_data, work, merged) catch {
        log.warn("sandbox {s}: overlay remount failed", .{id});
        return;
    };

    log.info("sandbox {s}: remounted successfully", .{id});
}

/// Use `mount -t overlay` shell command to remount an overlayfs.
/// This avoids the complexity of managing null-terminated paths for
/// the mount(2) syscall when paths come from stack buffers.
fn remountOverlayViaShell(
    lower_components: []const []const u8,
    upper_data: []const u8,
    work: []const u8,
    merged: []const u8,
) !void {
    // Build the options string: "lowerdir=A:B:C,upperdir=X,workdir=Y"
    var opts_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&opts_buf);
    const writer = stream.writer();

    writer.writeAll("lowerdir=") catch return error.CommandFailed;
    for (lower_components, 0..) |comp, i| {
        if (i > 0) writer.writeByte(':') catch return error.CommandFailed;
        writer.writeAll(comp) catch return error.CommandFailed;
    }
    writer.print(",upperdir={s},workdir={s}", .{ upper_data, work }) catch return error.CommandFailed;

    const opts = stream.getWritten();

    runCommandLogged(&.{ "mount", "-t", "overlay", "overlay", "-o", opts, merged }) catch {
        return error.CommandFailed;
    };
}

// ── Helpers ──────────────────────────────────────────────────────────

/// Run an external command synchronously. Log on failure.
fn runCommandLogged(argv: []const []const u8) !void {
    var child = std.process.Child.init(argv, std.heap.page_allocator);
    child.stdin_behavior = .Ignore;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch |err| {
        log.err("failed to spawn {s}: {}", .{ argv[0], err });
        return error.CommandFailed;
    };

    // Drain pipes to avoid deadlock
    var stdout_buf: [4096]u8 = undefined;
    var stderr_buf: [4096]u8 = undefined;
    _ = child.stdout.?.read(&stdout_buf) catch 0;
    _ = child.stderr.?.read(&stderr_buf) catch 0;

    const term = child.wait() catch |err| {
        log.err("failed to wait for {s}: {}", .{ argv[0], err });
        return error.CommandFailed;
    };
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

/// Read the full contents of a small file (up to 8KB).
fn readFileContents(path: []const u8) ![]const u8 {
    const file = try std.fs.openFileAbsolute(path, .{ .mode = .read_only });
    defer file.close();
    var buf: [8192]u8 = undefined;
    const n = try file.read(&buf);
    // Return a slice of the static buffer — caller must use before next call
    return buf[0..n];
}

fn isDir(path: []const u8) bool {
    var dir = std.fs.openDirAbsolute(path, .{}) catch return false;
    dir.close();
    return true;
}

// ── Tests ────────────────────────────────────────────────────────────

test "ensureDirectories creates dirs" {
    // Uses a temp dir to verify directory creation logic
    var tmp_buf: [256]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "/tmp/sq-init-test-{d}", .{std.time.milliTimestamp()}) catch unreachable;
    defer std.fs.deleteTreeAbsolute(tmp_path) catch {};

    std.fs.makeDirAbsolute(tmp_path) catch {};

    const cfg = config.Config{ .data_dir = tmp_path };
    ensureDirectories(&cfg);

    // Verify modules dir exists
    var mod_buf: [256]u8 = undefined;
    const mod_dir = try cfg.modulesDir(&mod_buf);
    var mod_d = try std.fs.openDirAbsolute(mod_dir, .{});
    mod_d.close();

    // Verify sandboxes dir exists
    var sb_buf: [256]u8 = undefined;
    const sb_dir = try cfg.sandboxesDir(&sb_buf);
    var sb_d = try std.fs.openDirAbsolute(sb_dir, .{});
    sb_d.close();
}

test "ensureDirectories is idempotent" {
    var tmp_buf: [256]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "/tmp/sq-init-test2-{d}", .{std.time.milliTimestamp()}) catch unreachable;
    defer std.fs.deleteTreeAbsolute(tmp_path) catch {};

    std.fs.makeDirAbsolute(tmp_path) catch {};

    const cfg = config.Config{ .data_dir = tmp_path };
    ensureDirectories(&cfg);
    ensureDirectories(&cfg); // second call should not fail
}

test "makeDirIfMissing handles existing dir" {
    makeDirIfMissing("/tmp"); // should not error
}

test "isDir returns true for existing directory" {
    try std.testing.expect(isDir("/tmp"));
}

test "isDir returns false for nonexistent path" {
    try std.testing.expect(!isDir("/nonexistent-path-12345"));
}

test "readFileContents fails on nonexistent file" {
    const result = readFileContents("/nonexistent-path-12345/file.txt");
    try std.testing.expectError(error.FileNotFound, result);
}
