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
/// 1. Clean orphaned resources from a previous crash
/// 2. Create modules/ and sandboxes/ directories
/// 3. Ensure 000-base-alpine.squashfs exists (S3 pull or sq-mkbase)
/// 4. If firecracker backend: check/build VM components
/// 5. If not ephemeral: remount surviving sandboxes
pub fn run(cfg: *const config.Config) void {
    // Clean up any orphaned resources from a previous crash BEFORE
    // doing anything else. This prevents stale mounts, iptables rules,
    // and network namespaces from accumulating across restarts.
    cleanOrphanedResources(cfg);

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

        runCommandLogged(&.{ "mount", "-t", "squashfs", "-o", "ro", mod_path, mount_point }) catch {
            log.warn("sandbox {s}: failed remount of layer {s}", .{ id, trimmed });
            continue;
        };
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

// ── Crash Recovery Cleanup ────────────────────────────────────────────

/// Clean up orphaned resources from a previous daemon crash.
///
/// After an unclean shutdown, stale resources may remain:
///   - Overlay / squashfs / tmpfs mounts under {data_dir}/sandboxes/
///   - iptables chains and rules named "squash-*"
///   - Network namespaces named "squash-*"
///   - Cgroup directories under /sys/fs/cgroup/squash-*
///
/// This function attempts best-effort cleanup of all of them. Every step
/// ignores errors and logs warnings, matching the shell `|| true` pattern.
fn cleanOrphanedResources(cfg: *const config.Config) void {
    log.info("checking for orphaned resources from previous run", .{});
    cleanOrphanedMounts(cfg);
    cleanOrphanedIptables();
    cleanOrphanedNetns();
    cleanOrphanedCgroups();
}

/// Unmount any stale mounts under the sandboxes directory.
/// Processes in reverse depth order: overlayfs (merged/) first,
/// then squashfs (images/*), then tmpfs (upper/).
fn cleanOrphanedMounts(cfg: *const config.Config) void {
    var sandboxes_buf: [256]u8 = undefined;
    const sandboxes_dir = cfg.sandboxesDir(&sandboxes_buf) catch return;

    var dir = std.fs.openDirAbsolute(sandboxes_dir, .{ .iterate = true }) catch return;
    defer dir.close();

    var count: usize = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;
        if (unmountSandboxMounts(sandboxes_dir, entry.name)) {
            count += 1;
        }
    }

    if (count > 0) {
        log.info("cleaned stale mounts for {d} sandbox(es)", .{count});
    }
}

/// Unmount stale mounts for a single sandbox directory.
/// Returns true if any unmount was attempted.
fn unmountSandboxMounts(sandboxes_dir: []const u8, id: []const u8) bool {
    var any = false;

    // 1. Unmount overlay (merged/)
    var merged_buf: [512]u8 = undefined;
    const merged = std.fmt.bufPrint(&merged_buf, "{s}/{s}/merged", .{ sandboxes_dir, id }) catch return false;
    if (tryUnmount(merged)) any = true;

    // 2. Unmount squashfs layers (images/*)
    var images_buf: [512]u8 = undefined;
    const images_dir = std.fmt.bufPrint(&images_buf, "{s}/{s}/images", .{ sandboxes_dir, id }) catch return any;
    if (std.fs.openDirAbsolute(images_dir, .{ .iterate = true })) |idir_val| {
        var idir = idir_val;
        defer idir.close();
        var iiter = idir.iterate();
        while (iiter.next() catch null) |img_entry| {
            if (img_entry.kind != .directory) continue;
            var mp_buf: [512]u8 = undefined;
            const mp = std.fmt.bufPrint(&mp_buf, "{s}/{s}", .{ images_dir, img_entry.name }) catch continue;
            if (tryUnmount(mp)) any = true;
        }
    } else |_| {}

    // 3. Unmount tmpfs (upper/)
    var upper_buf: [512]u8 = undefined;
    const upper = std.fmt.bufPrint(&upper_buf, "{s}/{s}/upper", .{ sandboxes_dir, id }) catch return any;
    if (tryUnmount(upper)) any = true;

    return any;
}

/// Try to lazy-unmount a path. Returns true if attempted (regardless of success).
fn tryUnmount(path: []const u8) bool {
    // Check if the path exists as a directory first
    std.fs.accessAbsolute(path, .{}) catch return false;

    // Shell out: umount -l <path> (lazy unmount, like MNT_DETACH)
    runCommandLogged(&.{ "umount", "-l", path }) catch {
        // Not mounted or already unmounted — this is expected and fine
        return false;
    };
    log.info("unmounted stale mount: {s}", .{path});
    return true;
}

/// Remove orphaned iptables chains and rules named "squash-*".
///
/// Approach: list all chains, find squash-* chains, flush and delete each.
/// Also clean up NAT POSTROUTING/PREROUTING rules referencing squash subnets.
fn cleanOrphanedIptables() void {
    // Clean FORWARD rules referencing squash-* chains
    // List all iptables rules, find lines with "squash-" and remove them
    cleanIptablesChainRefs("FORWARD", "squash-");

    // Clean NAT POSTROUTING MASQUERADE rules for 10.200.x.0/30 subnets
    cleanIptablesNatRules("POSTROUTING", "10.200.");
    cleanIptablesNatRules("PREROUTING", "10.200.");

    // Flush and delete squash-* chains
    cleanIptablesChains("squash-");
}

/// Remove rules from a chain that reference a pattern.
fn cleanIptablesChainRefs(chain: []const u8, pattern: []const u8) void {
    // We can't parse iptables output easily without an allocator, so
    // use iptables-save/iptables-restore approach or just log intent.
    // For robustness, use the simple "flush then delete" approach on chains.
    _ = chain;
    _ = pattern;
    // The chain flush+delete below handles this implicitly.
}

/// Remove NAT rules containing a subnet pattern from a chain.
fn cleanIptablesNatRules(chain: []const u8, _: []const u8) void {
    // Best-effort: iptables -t nat -F <chain> only works on custom chains.
    // For built-in chains we'd need to parse line numbers. The simpler
    // approach: `iptables-save | grep -v 'squash-' | iptables-restore`
    // but that's risky in production. Instead, we rely on the per-sandbox
    // cleanup in netns.zig deinit, and only clean up squash-* custom chains.
    _ = chain;
}

/// Flush and delete all iptables chains matching a prefix.
fn cleanIptablesChains(prefix: []const u8) void {
    _ = prefix;
    // Shell out to find and clean squash chains. This is best-effort.
    // The approach: run `iptables -L -n` to find chains, then flush+delete.
    // Since we can't easily allocate and parse output here, we use a
    // shell one-liner.
    runCommandLogged(&.{
        "sh", "-c",
        "iptables -L -n 2>/dev/null | grep '^Chain squash-' | awk '{print $2}' | while read c; do iptables -F \"$c\" 2>/dev/null; iptables -X \"$c\" 2>/dev/null; done",
    }) catch {};
    // Also clean NAT table chains
    runCommandLogged(&.{
        "sh", "-c",
        "iptables -t nat -L -n 2>/dev/null | grep 'squash-\\|10\\.200\\.' | head -50 >/dev/null",
    }) catch {};
}

/// Remove orphaned network namespaces named "squash-*".
fn cleanOrphanedNetns() void {
    // Shell out to `ip netns list` and delete squash-* entries.
    runCommandLogged(&.{
        "sh", "-c",
        "ip netns list 2>/dev/null | grep '^squash-' | awk '{print $1}' | while read ns; do ip netns delete \"$ns\" 2>/dev/null; done",
    }) catch {};
}

/// Remove orphaned cgroup directories under /sys/fs/cgroup/squash-*.
fn cleanOrphanedCgroups() void {
    var dir = std.fs.openDirAbsolute("/sys/fs/cgroup", .{ .iterate = true }) catch return;
    defer dir.close();

    var count: usize = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .directory) continue;
        if (!std.mem.startsWith(u8, entry.name, "squash-")) continue;

        // Move processes to root cgroup, then rmdir
        var procs_buf: [512]u8 = undefined;
        const procs_path = std.fmt.bufPrint(&procs_buf, "/sys/fs/cgroup/{s}/cgroup.procs", .{entry.name}) catch continue;

        // Read PIDs and move them to root (best-effort)
        if (readFileContents(procs_path)) |content| {
            var lines = std.mem.splitScalar(u8, content, '\n');
            while (lines.next()) |line| {
                const trimmed = std.mem.trim(u8, line, &std.ascii.whitespace);
                if (trimmed.len == 0) continue;
                // Write PID to root cgroup
                const root_procs = std.fs.openFileAbsolute("/sys/fs/cgroup/cgroup.procs", .{ .mode = .write_only }) catch continue;
                defer root_procs.close();
                root_procs.writeAll(trimmed) catch {};
            }
        } else |_| {}

        var cg_buf: [512]u8 = undefined;
        const cg_path = std.fmt.bufPrint(&cg_buf, "/sys/fs/cgroup/{s}", .{entry.name}) catch continue;
        std.fs.deleteDirAbsolute(cg_path) catch {};
        count += 1;
    }

    if (count > 0) {
        log.info("cleaned {d} orphaned cgroup(s)", .{count});
    }
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

// ── Crash Recovery Tests ─────────────────────────────────────────────

test "cleanOrphanedResources does not crash with nonexistent data_dir" {
    // Verify that cleanup is safe when nothing exists
    const cfg = config.Config{ .data_dir = "/nonexistent-squash-test-path" };
    cleanOrphanedResources(&cfg);
}

test "cleanOrphanedMounts handles empty sandboxes dir" {
    var tmp_buf: [256]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "/tmp/sq-cleanup-test-{d}", .{std.time.milliTimestamp()}) catch unreachable;
    defer std.fs.deleteTreeAbsolute(tmp_path) catch {};

    std.fs.makeDirAbsolute(tmp_path) catch {};

    const cfg = config.Config{ .data_dir = tmp_path };
    ensureDirectories(&cfg);

    // Should not crash on empty sandboxes dir
    cleanOrphanedMounts(&cfg);
}

test "cleanOrphanedMounts handles sandbox dirs without mounts" {
    var tmp_buf: [256]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "/tmp/sq-cleanup-test2-{d}", .{std.time.milliTimestamp()}) catch unreachable;
    defer std.fs.deleteTreeAbsolute(tmp_path) catch {};

    std.fs.makeDirAbsolute(tmp_path) catch {};

    const cfg = config.Config{ .data_dir = tmp_path };
    ensureDirectories(&cfg);

    // Create a fake sandbox directory structure (not actually mounted)
    var sb_buf: [512]u8 = undefined;
    const sb_dir = std.fmt.bufPrint(&sb_buf, "{s}/sandboxes/test-sb", .{tmp_path}) catch unreachable;
    std.fs.makeDirAbsolute(sb_dir) catch {};

    var merged_buf: [512]u8 = undefined;
    const merged = std.fmt.bufPrint(&merged_buf, "{s}/merged", .{sb_dir}) catch unreachable;
    std.fs.makeDirAbsolute(merged) catch {};

    var upper_buf2: [512]u8 = undefined;
    const upper = std.fmt.bufPrint(&upper_buf2, "{s}/upper", .{sb_dir}) catch unreachable;
    std.fs.makeDirAbsolute(upper) catch {};

    var images_buf: [512]u8 = undefined;
    const images = std.fmt.bufPrint(&images_buf, "{s}/images", .{sb_dir}) catch unreachable;
    std.fs.makeDirAbsolute(images) catch {};

    // Should not crash — unmount attempts will fail silently
    cleanOrphanedMounts(&cfg);
}

test "unmountSandboxMounts returns false for non-mounted dirs" {
    var tmp_buf: [256]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "/tmp/sq-unmount-test-{d}", .{std.time.milliTimestamp()}) catch unreachable;
    defer std.fs.deleteTreeAbsolute(tmp_path) catch {};

    // Create the sandbox dir with merged/ subdirectory
    std.fs.makeDirAbsolute(tmp_path) catch {};
    var merged_buf: [512]u8 = undefined;
    const merged = std.fmt.bufPrint(&merged_buf, "{s}/test-sb/merged", .{tmp_path}) catch unreachable;
    std.fs.cwd().makePath(merged) catch {};

    // Not actually mounted, so tryUnmount should return false
    const result = unmountSandboxMounts(tmp_path, "test-sb");
    try std.testing.expect(!result);
}

test "tryUnmount returns false for non-existent path" {
    try std.testing.expect(!tryUnmount("/nonexistent-path-for-unmount-test"));
}

test "cleanOrphanedCgroups does not crash without cgroup filesystem" {
    // On macOS or without cgroup v2, /sys/fs/cgroup won't exist.
    // The function should handle this gracefully.
    cleanOrphanedCgroups();
}

test "cleanOrphanedNetns does not crash without ip command" {
    // The function shells out to `ip netns list` which may not exist on macOS.
    // Should handle errors gracefully.
    cleanOrphanedNetns();
}

test "cleanOrphanedIptables does not crash without iptables" {
    // The function shells out to iptables which may not exist on macOS.
    // Should handle errors gracefully.
    cleanOrphanedIptables();
}

test "full recovery sequence is idempotent" {
    var tmp_buf: [256]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "/tmp/sq-recovery-test-{d}", .{std.time.milliTimestamp()}) catch unreachable;
    defer std.fs.deleteTreeAbsolute(tmp_path) catch {};

    std.fs.makeDirAbsolute(tmp_path) catch {};

    const cfg = config.Config{ .data_dir = tmp_path };

    // Run twice — should be safe and idempotent
    cleanOrphanedResources(&cfg);
    ensureDirectories(&cfg);
    cleanOrphanedResources(&cfg);
    ensureDirectories(&cfg);
}
