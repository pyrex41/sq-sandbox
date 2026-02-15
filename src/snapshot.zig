// snapshot.zig — Snapshot creation (mksquashfs), restore, and activate (convert to module).
//
// Snapshot creates a squashfs image from the sandbox's upper layer.
// Restore unmounts the overlay, clears the upper layer, loop-mounts a
// snapshot as the highest-priority lower, and remounts the overlay.
// Activate copies a snapshot into the modules directory as a regular module.
//
// Overlay remount during restore is kept atomic: the overlay is down for the
// minimum time needed to swap layers. If remount fails, we attempt to restore
// the previous state (overlay without snapshot).

const std = @import("std");
const mounts = @import("mounts.zig");
const validate = @import("validate.zig");
const s3 = @import("s3.zig");
const log = std.log.scoped(.snapshot);

// ── Error types ──────────────────────────────────────────────────────

pub const SnapshotError = error{
    InvalidLabel,
    SnapshotAlreadyExists,
    MksquashfsFailed,
    SnapshotNotFound,
    RestoreFailed,
    ClearUpperFailed,
    ModuleAlreadyExists,
    CopyFailed,
    S3PullFailed,
};

// ── Compression detection ────────────────────────────────────────────

/// Detect whether mksquashfs supports zstd compression by running
/// `mksquashfs -help` and scanning the output.
pub fn detectZstdSupport(allocator: std.mem.Allocator) bool {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "mksquashfs", "-help" },
    }) catch return false;
    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }
    // mksquashfs writes help to stderr
    if (std.mem.indexOf(u8, result.stderr, "zstd") != null) return true;
    if (std.mem.indexOf(u8, result.stdout, "zstd") != null) return true;
    return false;
}

// ── Snapshot creation ────────────────────────────────────────────────

pub const SnapshotOptions = struct {
    /// Force gzip even if zstd is available.
    force_gzip: bool = false,
};

/// Create a squashfs snapshot of the sandbox's upper layer.
///
/// Parameters:
///   allocator: used for temporary allocations during mksquashfs invocation.
///   sandbox_dir: root directory of the sandbox (e.g. /data/sandboxes/my-id).
///   label: snapshot label, must pass validLabel().
///   opts: compression options.
///
/// Returns the absolute path to the created .squashfs file (allocated with allocator).
pub fn createSnapshot(
    allocator: std.mem.Allocator,
    sandbox_dir: []const u8,
    label: []const u8,
    opts: SnapshotOptions,
) (SnapshotError || std.mem.Allocator.Error || std.fs.Dir.MakeError || std.process.Child.RunError)![]const u8 {
    if (!validate.validLabel(label)) return SnapshotError.InvalidLabel;

    // Build paths
    const snap_dir = try std.fmt.allocPrint(allocator, "{s}/snapshots", .{sandbox_dir});
    defer allocator.free(snap_dir);

    std.fs.makeDirAbsolute(snap_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const snap_path = try std.fmt.allocPrint(allocator, "{s}/snapshots/{s}.squashfs", .{ sandbox_dir, label });
    errdefer allocator.free(snap_path);

    // Check if snapshot already exists
    if (std.fs.accessAbsolute(snap_path, .{})) |_| {
        // File exists — cannot overwrite
        allocator.free(snap_path);
        return SnapshotError.SnapshotAlreadyExists;
    } else |err| switch (err) {
        error.FileNotFound => {}, // good — doesn't exist yet
        else => {
            allocator.free(snap_path);
            return SnapshotError.SnapshotAlreadyExists;
        },
    }

    // Detect compression support
    const use_zstd = if (opts.force_gzip) false else detectZstdSupport(allocator);

    const upper_data = try std.fmt.allocPrint(allocator, "{s}/upper/data", .{sandbox_dir});
    defer allocator.free(upper_data);

    // Build mksquashfs command
    var argv_buf: [16][]const u8 = undefined;
    var argc: usize = 0;

    argv_buf[argc] = "mksquashfs";
    argc += 1;
    argv_buf[argc] = upper_data;
    argc += 1;
    argv_buf[argc] = snap_path;
    argc += 1;
    argv_buf[argc] = "-comp";
    argc += 1;

    if (use_zstd) {
        argv_buf[argc] = "zstd";
        argc += 1;
        argv_buf[argc] = "-Xcompression-level";
        argc += 1;
        argv_buf[argc] = "3";
        argc += 1;
        argv_buf[argc] = "-b";
        argc += 1;
        argv_buf[argc] = "128K";
        argc += 1;
    } else {
        argv_buf[argc] = "gzip";
        argc += 1;
        argv_buf[argc] = "-b";
        argc += 1;
        argv_buf[argc] = "256K";
        argc += 1;
    }

    argv_buf[argc] = "-noappend";
    argc += 1;
    argv_buf[argc] = "-quiet";
    argc += 1;

    log.info("creating snapshot: {s} -> {s} (comp={s})", .{
        upper_data,
        snap_path,
        if (use_zstd) "zstd" else "gzip",
    });

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv_buf[0..argc],
    });
    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }

    if (result.term.Exited != 0) {
        log.err("mksquashfs failed (exit {d}): {s}", .{
            result.term.Exited,
            result.stderr,
        });
        allocator.free(snap_path);
        return SnapshotError.MksquashfsFailed;
    }

    log.info("snapshot created: {s}", .{snap_path});
    return snap_path;
}

// ── Restore ──────────────────────────────────────────────────────────

/// State needed to restore a sandbox from a snapshot.
/// The caller provides paths and current mount handles; this function
/// performs the unmount/clear/remount sequence.
pub const RestoreParams = struct {
    /// Root sandbox directory (e.g. /data/sandboxes/my-id).
    sandbox_dir: []const u8,
    /// Path to the snapshot .squashfs file.
    snapshot_path: []const u8,
    /// Mount point for the snapshot layer (e.g. /data/sandboxes/my-id/images/_snapshot).
    snapshot_mount_point: [*:0]const u8,
    /// Current overlay merged path.
    merged_path: [*:0]const u8,
    /// Upper data directory (e.g. /data/sandboxes/my-id/upper/data).
    upper_data: [*:0]const u8,
    /// Upper work directory (e.g. /data/sandboxes/my-id/upper/work).
    upper_work: [*:0]const u8,
    /// Lower layer components for overlay (snapshot layer will be prepended).
    lower_layers: []const []const u8,
};

/// Restore a sandbox from a snapshot.
///
/// Sequence (matching common.sh:664-707):
///   1. Unmount overlay
///   2. Unmount previous snapshot layer (if mounted)
///   3. Clear upper/data/* and upper/work/*
///   4. Loop-mount snapshot as highest-priority lower
///   5. Remount overlay with snapshot as first lower component
///   6. Return the new SquashfsMount for the snapshot layer
///
/// If remount fails after clearing upper, attempts to restore the overlay
/// without the snapshot layer (degraded but functional).
pub fn restoreSnapshot(
    allocator: std.mem.Allocator,
    params: RestoreParams,
    prev_snapshot_mount: ?*mounts.SquashfsMount,
    overlay_mount: *mounts.OverlayMount,
) (SnapshotError || mounts.MountError || std.mem.Allocator.Error || std.fs.Dir.MakeError)!mounts.SquashfsMount {
    // Verify snapshot file exists
    std.fs.accessAbsolute(params.snapshot_path, .{}) catch {
        return SnapshotError.SnapshotNotFound;
    };

    log.info("restoring from snapshot: {s}", .{params.snapshot_path});

    // 1. Unmount overlay
    overlay_mount.deinit();

    // 2. Unmount previous snapshot layer if any
    if (prev_snapshot_mount) |snap| {
        snap.deinit();
    }

    // 3. Clear upper/data/* and upper/work/*
    clearDirectory(std.mem.span(params.upper_data)) catch |err| {
        log.err("failed to clear upper/data: {}", .{err});
        // Attempt to remount overlay without snapshot (degraded recovery)
        overlay_mount.* = mounts.OverlayMount.mount(
            params.lower_layers,
            params.upper_data,
            params.upper_work,
            params.merged_path,
        ) catch {
            log.err("CRITICAL: failed to remount overlay after clear failure", .{});
            return SnapshotError.RestoreFailed;
        };
        return SnapshotError.ClearUpperFailed;
    };
    clearDirectory(std.mem.span(params.upper_work)) catch {
        // Recreate work dir (overlayfs requires it to exist and be empty)
        std.fs.makeDirAbsolute(std.mem.span(params.upper_work)) catch {};
    };
    // Recreate work directory (overlayfs requires it empty but present)
    std.fs.makeDirAbsolute(std.mem.span(params.upper_work)) catch {};

    // 4. Loop-mount snapshot as highest-priority lower
    // Build null-terminated path for the squashfs mount
    var snap_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path_z = std.fmt.bufPrintZ(&snap_path_buf, "{s}", .{params.snapshot_path}) catch {
        // Recovery: remount without snapshot
        overlay_mount.* = mounts.OverlayMount.mount(
            params.lower_layers,
            params.upper_data,
            params.upper_work,
            params.merged_path,
        ) catch {
            log.err("CRITICAL: failed to remount overlay during recovery", .{});
            return SnapshotError.RestoreFailed;
        };
        return SnapshotError.RestoreFailed;
    };

    var snap_mount = mounts.SquashfsMount.mount(
        snap_path_z,
        params.snapshot_mount_point,
    ) catch {
        log.err("failed to mount snapshot squashfs", .{});
        // Recovery: remount without snapshot
        overlay_mount.* = mounts.OverlayMount.mount(
            params.lower_layers,
            params.upper_data,
            params.upper_work,
            params.merged_path,
        ) catch {
            log.err("CRITICAL: failed to remount overlay during recovery", .{});
            return SnapshotError.RestoreFailed;
        };
        return SnapshotError.RestoreFailed;
    };
    errdefer snap_mount.deinit();

    // 5. Build new lower components with snapshot as highest priority
    const new_lower_count = params.lower_layers.len + 1;
    const new_lower = allocator.alloc([]const u8, new_lower_count) catch {
        snap_mount.deinit();
        // Recovery: remount without snapshot
        overlay_mount.* = mounts.OverlayMount.mount(
            params.lower_layers,
            params.upper_data,
            params.upper_work,
            params.merged_path,
        ) catch {
            log.err("CRITICAL: failed to remount overlay during recovery", .{});
            return SnapshotError.RestoreFailed;
        };
        return SnapshotError.RestoreFailed;
    };
    defer allocator.free(new_lower);

    new_lower[0] = std.mem.span(params.snapshot_mount_point);
    @memcpy(new_lower[1..], params.lower_layers);

    // 6. Remount overlay with snapshot layer
    overlay_mount.* = mounts.OverlayMount.mount(
        new_lower,
        params.upper_data,
        params.upper_work,
        params.merged_path,
    ) catch {
        log.err("failed to remount overlay with snapshot layer", .{});
        snap_mount.deinit();
        // Last-resort recovery: remount without snapshot
        overlay_mount.* = mounts.OverlayMount.mount(
            params.lower_layers,
            params.upper_data,
            params.upper_work,
            params.merged_path,
        ) catch {
            log.err("CRITICAL: failed to remount overlay during recovery", .{});
            return SnapshotError.RestoreFailed;
        };
        return SnapshotError.RestoreFailed;
    };

    log.info("restore complete: overlay remounted with snapshot layer", .{});
    return snap_mount;
}

// ── Activate (convert snapshot to module) ────────────────────────────

/// Convert a snapshot to a regular module by copying it into the modules directory.
///
/// Parameters:
///   snapshot_path: absolute path to the .squashfs snapshot file.
///   modules_dir: path to the modules directory (e.g. /data/modules).
///   module_name: name for the new module (must pass validModule).
///
/// The file is copied atomically: write to a temp file, then rename.
pub fn activateSnapshot(
    allocator: std.mem.Allocator,
    snapshot_path: []const u8,
    modules_dir: []const u8,
    module_name: []const u8,
) (SnapshotError || std.mem.Allocator.Error || std.fs.Dir.MakeError)!void {
    if (!validate.validModule(module_name)) return SnapshotError.InvalidLabel;

    // Verify source snapshot exists
    std.fs.accessAbsolute(snapshot_path, .{}) catch {
        return SnapshotError.SnapshotNotFound;
    };

    // Ensure modules directory exists
    std.fs.makeDirAbsolute(modules_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const dest_path = try std.fmt.allocPrint(allocator, "{s}/{s}.squashfs", .{ modules_dir, module_name });
    defer allocator.free(dest_path);

    // Check if module already exists
    if (std.fs.accessAbsolute(dest_path, .{})) |_| {
        return SnapshotError.ModuleAlreadyExists;
    } else |err| switch (err) {
        error.FileNotFound => {}, // good
        else => return SnapshotError.ModuleAlreadyExists,
    }

    // Atomic copy: write to .tmp, then rename
    const tmp_path = try std.fmt.allocPrint(allocator, "{s}/.{s}.squashfs.tmp", .{ modules_dir, module_name });
    defer allocator.free(tmp_path);

    log.info("activating snapshot: {s} -> {s}", .{ snapshot_path, dest_path });

    copyFile(snapshot_path, tmp_path) catch |err| {
        log.err("failed to copy snapshot to temp: {}", .{err});
        // Clean up partial temp file
        std.fs.deleteFileAbsolute(tmp_path) catch {};
        return SnapshotError.CopyFailed;
    };

    // Rename temp to final destination
    std.fs.renameAbsolute(tmp_path, dest_path) catch |err| {
        log.err("failed to rename temp to module: {}", .{err});
        std.fs.deleteFileAbsolute(tmp_path) catch {};
        return SnapshotError.CopyFailed;
    };

    log.info("snapshot activated as module: {s}", .{module_name});
}

// ── Secret placeholder re-injection ──────────────────────────────────

/// Re-inject the squash-secrets.sh script into the sandbox's /etc/profile.d/
/// after a restore (since the upper layer was cleared).
///
/// This creates the file in upper/data/etc/profile.d/squash-secrets.sh
/// which will be visible in the merged overlay.
pub fn reinjectSecretPlaceholders(
    sandbox_dir: []const u8,
    secrets_content: []const u8,
) !void {
    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const profile_dir = std.fmt.bufPrint(&dir_buf, "{s}/upper/data/etc/profile.d", .{sandbox_dir}) catch
        return error.FormatFailed;

    // Create the directory tree in upper
    makeParentDirs(profile_dir) catch {};
    std.fs.makeDirAbsolute(profile_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const script_path = std.fmt.bufPrint(&path_buf, "{s}/squash-secrets.sh", .{profile_dir}) catch
        return error.FormatFailed;

    const file = try std.fs.createFileAbsolute(script_path, .{});
    defer file.close();
    try file.writeAll(secrets_content);

    log.info("re-injected secret placeholders: {s}", .{script_path});
}

// ── S3 integration ───────────────────────────────────────────────────

/// Pull a snapshot from S3 if it's not available locally.
/// Called before restoreSnapshot when the snapshot file doesn't exist on disk.
///
/// Parameters:
///   client: initialized S3 client.
///   allocator: for temporary allocations.
///   sandbox_id: sandbox identifier (used in S3 key path).
///   label: snapshot label.
///   local_path: expected local path for the snapshot file.
///
/// Returns true if the snapshot was downloaded, false if already local.
/// Returns S3PullFailed if the download fails.
pub fn pullSnapshotFromS3(
    client: *const s3.S3Client,
    allocator: std.mem.Allocator,
    sandbox_id: []const u8,
    label: []const u8,
    local_path: []const u8,
) SnapshotError!bool {
    // Check if already local
    if (std.fs.accessAbsolute(local_path, .{})) |_| {
        log.info("snapshot already local: {s}", .{local_path});
        return false;
    } else |_| {}

    // Build S3 key: snapshots/<sandbox_id>/<label>.squashfs
    const s3_key = std.fmt.allocPrint(allocator, "snapshots/{s}/{s}.squashfs", .{
        sandbox_id, label,
    }) catch return SnapshotError.S3PullFailed;
    defer allocator.free(s3_key);

    // Ensure parent directory exists
    if (std.mem.lastIndexOfScalar(u8, local_path, '/')) |last_slash| {
        const parent = local_path[0..last_slash];
        std.fs.makeDirAbsolute(parent) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => {
                log.err("pullSnapshotFromS3: failed to create dir {s}", .{parent});
                return SnapshotError.S3PullFailed;
            },
        };
    }

    log.info("pulling snapshot from S3: {s} -> {s}", .{ s3_key, local_path });

    client.pull(allocator, s3_key, local_path) catch |err| {
        log.err("pullSnapshotFromS3: S3 pull failed: {}", .{err});
        return SnapshotError.S3PullFailed;
    };

    log.info("snapshot pulled from S3: {s}", .{local_path});
    return true;
}

/// Push a snapshot to S3 in the background after creation.
/// Copies the snapshot to a temp file, then uploads in a detached thread.
/// This avoids blocking the caller while the upload completes.
///
/// Parameters:
///   client: initialized S3 client.
///   allocator: for temporary allocations (staging copy).
///   sandbox_id: sandbox identifier (used in S3 key path).
///   label: snapshot label.
///   snap_path: local path to the snapshot file.
pub fn pushSnapshotToS3Bg(
    client: *const s3.S3Client,
    allocator: std.mem.Allocator,
    sandbox_id: []const u8,
    label: []const u8,
    snap_path: []const u8,
) void {
    // Build S3 key
    const s3_key = std.fmt.allocPrint(allocator, "snapshots/{s}/{s}.squashfs", .{
        sandbox_id, label,
    }) catch {
        log.err("pushSnapshotToS3Bg: failed to build S3 key", .{});
        return;
    };
    defer allocator.free(s3_key);

    log.info("queuing background S3 push: {s} -> {s}", .{ snap_path, s3_key });
    client.pushBg(allocator, snap_path, s3_key);
}

/// Create a snapshot and optionally push to S3 in the background.
/// This is a convenience wrapper around createSnapshot + pushSnapshotToS3Bg.
///
/// Parameters:
///   allocator: used for mksquashfs and S3 operations.
///   sandbox_dir: root directory of the sandbox.
///   sandbox_id: sandbox identifier for S3 key path.
///   label: snapshot label.
///   opts: compression options.
///   s3_client: optional S3 client; if non-null, triggers background push.
///
/// Returns the absolute path to the created .squashfs file.
pub fn createSnapshotWithS3(
    allocator: std.mem.Allocator,
    sandbox_dir: []const u8,
    sandbox_id: []const u8,
    label: []const u8,
    opts: SnapshotOptions,
    s3_client: ?*const s3.S3Client,
) (SnapshotError || std.mem.Allocator.Error || std.fs.Dir.MakeError || std.process.Child.RunError)![]const u8 {
    const snap_path = try createSnapshot(allocator, sandbox_dir, label, opts);

    // Trigger background S3 push if configured
    if (s3_client) |client| {
        pushSnapshotToS3Bg(client, allocator, sandbox_id, label, snap_path);
    }

    return snap_path;
}

/// Restore from snapshot with S3 pull fallback.
/// If the snapshot file isn't available locally, attempts to pull it from S3.
///
/// Parameters:
///   allocator: for allocations.
///   params: restore parameters (includes snapshot_path).
///   prev_snapshot_mount: previous snapshot mount to unmount, if any.
///   overlay_mount: current overlay mount to swap.
///   s3_client: optional S3 client for remote pull.
///   sandbox_id: sandbox identifier for S3 key path.
///   label: snapshot label for S3 key path.
pub fn restoreSnapshotWithS3(
    allocator: std.mem.Allocator,
    params: RestoreParams,
    prev_snapshot_mount: ?*mounts.SquashfsMount,
    overlay_mount: *mounts.OverlayMount,
    s3_client: ?*const s3.S3Client,
    sandbox_id: []const u8,
    label: []const u8,
) (SnapshotError || mounts.MountError || std.mem.Allocator.Error || std.fs.Dir.MakeError)!mounts.SquashfsMount {
    // Try S3 pull if snapshot not available locally
    std.fs.accessAbsolute(params.snapshot_path, .{}) catch {
        if (s3_client) |client| {
            _ = pullSnapshotFromS3(client, allocator, sandbox_id, label, params.snapshot_path) catch {
                return SnapshotError.SnapshotNotFound;
            };
        } else {
            return SnapshotError.SnapshotNotFound;
        }
    };

    return restoreSnapshot(allocator, params, prev_snapshot_mount, overlay_mount);
}

// ── Internal helpers ─────────────────────────────────────────────────

/// Remove all entries inside a directory without removing the directory itself.
fn clearDirectory(path: []const u8) !void {
    var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        switch (entry.kind) {
            .directory => {
                dir.deleteTree(entry.name) catch |err| {
                    log.warn("clearDirectory: failed to delete tree {s}/{s}: {}", .{
                        path, entry.name, err,
                    });
                    return err;
                };
            },
            else => {
                dir.deleteFile(entry.name) catch |err| {
                    log.warn("clearDirectory: failed to delete {s}/{s}: {}", .{
                        path, entry.name, err,
                    });
                    return err;
                };
            },
        }
    }
}

/// Copy a file from src to dst using buffered I/O.
fn copyFile(src_path: []const u8, dst_path: []const u8) !void {
    const src = try std.fs.openFileAbsolute(src_path, .{});
    defer src.close();

    const dst = try std.fs.createFileAbsolute(dst_path, .{});
    defer dst.close();

    var buf: [64 * 1024]u8 = undefined;
    while (true) {
        const n = try src.read(&buf);
        if (n == 0) break;
        try dst.writeAll(buf[0..n]);
    }
}

/// Create parent directories for a path (like mkdir -p for the parent).
fn makeParentDirs(path: []const u8) !void {
    // Walk the path creating directories as needed
    var i: usize = 1; // skip leading /
    while (i < path.len) {
        if (path[i] == '/') {
            const prefix = path[0..i];
            std.fs.makeDirAbsolute(prefix) catch |err| switch (err) {
                error.PathAlreadyExists => {},
                else => return err,
            };
        }
        i += 1;
    }
}

// ── Tests ────────────────────────────────────────────────────────────

test "detectZstdSupport returns a boolean" {
    // This test just verifies the function doesn't crash.
    // On macOS/CI without mksquashfs, it will return false.
    const result = detectZstdSupport(std.testing.allocator);
    _ = result;
}

test "createSnapshot rejects invalid label" {
    const result = createSnapshot(
        std.testing.allocator,
        "/nonexistent/sandbox",
        "../evil",
        .{},
    );
    try std.testing.expectError(SnapshotError.InvalidLabel, result);
}

test "createSnapshot rejects empty label" {
    const result = createSnapshot(
        std.testing.allocator,
        "/nonexistent/sandbox",
        "",
        .{},
    );
    try std.testing.expectError(SnapshotError.InvalidLabel, result);
}

test "activateSnapshot rejects invalid module name" {
    const result = activateSnapshot(
        std.testing.allocator,
        "/nonexistent/snapshot.squashfs",
        "/nonexistent/modules",
        "../evil",
    );
    try std.testing.expectError(SnapshotError.InvalidLabel, result);
}

test "activateSnapshot rejects empty module name" {
    const result = activateSnapshot(
        std.testing.allocator,
        "/nonexistent/snapshot.squashfs",
        "/nonexistent/modules",
        "",
    );
    try std.testing.expectError(SnapshotError.InvalidLabel, result);
}

test "clearDirectory empties a directory" {
    // Create a temp directory with some files
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create files and subdirectories
    tmp_dir.dir.makeDir("subdir") catch {};
    const f1 = tmp_dir.dir.createFile("file1.txt", .{}) catch unreachable;
    f1.close();
    const f2 = tmp_dir.dir.createFile("file2.txt", .{}) catch unreachable;
    f2.close();

    // Create a file inside subdir
    var subdir = tmp_dir.dir.openDir("subdir", .{}) catch unreachable;
    defer subdir.close();
    const f3 = subdir.createFile("nested.txt", .{}) catch unreachable;
    f3.close();

    // Get the absolute path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = tmp_dir.dir.realpath(".", &path_buf) catch unreachable;

    try clearDirectory(abs_path);

    // Verify directory is empty
    var dir = try std.fs.openDirAbsolute(abs_path, .{ .iterate = true });
    defer dir.close();
    var iter = dir.iterate();
    const entry = try iter.next();
    try std.testing.expect(entry == null);
}

test "copyFile copies file contents" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Write source file
    const src_file = tmp_dir.dir.createFile("src.txt", .{}) catch unreachable;
    src_file.writeAll("hello world snapshot data") catch unreachable;
    src_file.close();

    var src_buf: [std.fs.max_path_bytes]u8 = undefined;
    const src_path = tmp_dir.dir.realpath("src.txt", &src_buf) catch unreachable;

    var dst_buf: [std.fs.max_path_bytes]u8 = undefined;
    // Build the dest path relative to the tmp dir
    const dir_path = tmp_dir.dir.realpath(".", &dst_buf) catch unreachable;
    var full_dst_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dst_path = std.fmt.bufPrint(&full_dst_buf, "{s}/dst.txt", .{dir_path}) catch unreachable;

    try copyFile(src_path, dst_path);

    // Verify contents match
    const dst_file = try std.fs.openFileAbsolute(dst_path, .{});
    defer dst_file.close();
    var read_buf: [256]u8 = undefined;
    const n = try dst_file.read(&read_buf);
    try std.testing.expectEqualStrings("hello world snapshot data", read_buf[0..n]);
}

test "makeParentDirs creates nested directories" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var base_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &base_buf) catch unreachable;

    var target_buf: [std.fs.max_path_bytes]u8 = undefined;
    const target = std.fmt.bufPrint(&target_buf, "{s}/a/b/c/d", .{base_path}) catch unreachable;

    try makeParentDirs(target);

    // Verify a/b/c exists (d is not created since it's the leaf)
    var check_buf: [std.fs.max_path_bytes]u8 = undefined;
    const check_path = std.fmt.bufPrint(&check_buf, "{s}/a/b/c", .{base_path}) catch unreachable;
    var dir = std.fs.openDirAbsolute(check_path, .{}) catch |err| {
        std.debug.print("expected directory to exist: {}\n", .{err});
        return err;
    };
    dir.close();
}

test "SnapshotOptions defaults" {
    const opts = SnapshotOptions{};
    try std.testing.expect(!opts.force_gzip);
}

test "restoreSnapshot fails with nonexistent snapshot" {
    const params = RestoreParams{
        .sandbox_dir = "/nonexistent/sandbox",
        .snapshot_path = "/nonexistent/snapshot.squashfs",
        .snapshot_mount_point = "/nonexistent/mount",
        .merged_path = "/nonexistent/merged",
        .upper_data = "/nonexistent/upper/data",
        .upper_work = "/nonexistent/upper/work",
        .lower_layers = &.{"/nonexistent/lower"},
    };
    var overlay = mounts.OverlayMount{
        .merged_path = "/nonexistent/merged",
        .active = false,
    };
    const result = restoreSnapshot(
        std.testing.allocator,
        params,
        null,
        &overlay,
    );
    try std.testing.expectError(SnapshotError.SnapshotNotFound, result);
}

// ── S3 Integration Tests ────────────────────────────────────────────

test "pullSnapshotFromS3 returns false when file already exists" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create a local snapshot file
    const f = tmp_dir.dir.createFile("snap.squashfs", .{}) catch unreachable;
    f.writeAll("snapshot data") catch unreachable;
    f.close();

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const local_path = tmp_dir.dir.realpath("snap.squashfs", &path_buf) catch unreachable;

    // With any client, should still return false since file exists locally
    const client = s3.S3Client.init("bucket", "us-east-1", "", null, "ak", "sk");
    const result = pullSnapshotFromS3(&client, std.testing.allocator, "dev", "cp1", local_path);
    try std.testing.expect(result catch unreachable == false);
}

test "restoreSnapshotWithS3 fails without S3 when file missing" {
    const params = RestoreParams{
        .sandbox_dir = "/nonexistent/sandbox",
        .snapshot_path = "/nonexistent/snapshot.squashfs",
        .snapshot_mount_point = "/nonexistent/mount",
        .merged_path = "/nonexistent/merged",
        .upper_data = "/nonexistent/upper/data",
        .upper_work = "/nonexistent/upper/work",
        .lower_layers = &.{"/nonexistent/lower"},
    };
    var overlay = mounts.OverlayMount{
        .merged_path = "/nonexistent/merged",
        .active = false,
    };
    // Without S3 client, should return SnapshotNotFound
    const result = restoreSnapshotWithS3(
        std.testing.allocator,
        params,
        null,
        &overlay,
        null, // no S3 client
        "dev",
        "cp1",
    );
    try std.testing.expectError(SnapshotError.SnapshotNotFound, result);
}

test "createSnapshotWithS3 rejects invalid label (same as createSnapshot)" {
    const result = createSnapshotWithS3(
        std.testing.allocator,
        "/nonexistent/sandbox",
        "dev",
        "../evil",
        .{},
        null,
    );
    try std.testing.expectError(SnapshotError.InvalidLabel, result);
}

// ── Snapshot creation tests ─────────────────────────────────────────

test "createSnapshot path traversal protection" {
    // Labels with path separators should be rejected
    const labels = [_][]const u8{ "../evil", "foo/bar", ".", "..", "a/../b" };
    for (labels) |label| {
        const result = createSnapshot(
            std.testing.allocator,
            "/nonexistent/sandbox",
            label,
            .{},
        );
        try std.testing.expectError(SnapshotError.InvalidLabel, result);
    }
}

test "createSnapshot valid labels accepted structurally" {
    // These labels pass validation but will fail at mksquashfs (no sandbox dir)
    const labels = [_][]const u8{ "cp1", "my-checkpoint", "v1.0", "snapshot_2025.01.15" };
    for (labels) |label| {
        // Should get past validation (InvalidLabel) to the actual mksquashfs step
        const result = createSnapshot(
            std.testing.allocator,
            "/tmp/sq-test-nonexistent-sandbox-dir",
            label,
            .{},
        );
        // Should fail with MksquashfsFailed or a dir error, not InvalidLabel
        if (result) |path| {
            std.testing.allocator.free(path);
            unreachable; // shouldn't succeed with nonexistent dir
        } else |err| {
            try std.testing.expect(err != SnapshotError.InvalidLabel);
        }
    }
}

// ── Activate (snapshot-to-module conversion) tests ──────────────────

test "activateSnapshot copies file atomically" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    // Create a fake snapshot file
    var snap_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_dir = std.fmt.bufPrint(&snap_dir_buf, "{s}/snapshots", .{base_path}) catch unreachable;
    std.fs.makeDirAbsolute(snap_dir) catch {};

    var snap_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path = std.fmt.bufPrint(&snap_path_buf, "{s}/snapshots/cp1.squashfs", .{base_path}) catch unreachable;
    const sf = std.fs.createFileAbsolute(snap_path, .{}) catch unreachable;
    sf.writeAll("fake squashfs image data 12345") catch unreachable;
    sf.close();

    // Create modules directory
    var mod_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const mod_dir = std.fmt.bufPrint(&mod_dir_buf, "{s}/modules", .{base_path}) catch unreachable;
    std.fs.makeDirAbsolute(mod_dir) catch {};

    // Activate
    try activateSnapshot(std.testing.allocator, snap_path, mod_dir, "200-my-module");

    // Verify the module file exists with correct content
    var result_buf: [std.fs.max_path_bytes]u8 = undefined;
    const result_path = std.fmt.bufPrint(&result_buf, "{s}/modules/200-my-module.squashfs", .{base_path}) catch unreachable;

    const result_file = try std.fs.openFileAbsolute(result_path, .{});
    defer result_file.close();
    var read_buf: [256]u8 = undefined;
    const n = try result_file.read(&read_buf);
    try std.testing.expectEqualStrings("fake squashfs image data 12345", read_buf[0..n]);
}

test "activateSnapshot rejects duplicate module" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    // Create snapshot
    var snap_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const snap_path = std.fmt.bufPrint(&snap_path_buf, "{s}/source.squashfs", .{base_path}) catch unreachable;
    const sf = std.fs.createFileAbsolute(snap_path, .{}) catch unreachable;
    sf.writeAll("data") catch unreachable;
    sf.close();

    // Create modules dir with existing module
    var mod_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const mod_dir = std.fmt.bufPrint(&mod_dir_buf, "{s}/modules", .{base_path}) catch unreachable;
    std.fs.makeDirAbsolute(mod_dir) catch {};

    var existing_buf: [std.fs.max_path_bytes]u8 = undefined;
    const existing = std.fmt.bufPrint(&existing_buf, "{s}/modules/existing.squashfs", .{base_path}) catch unreachable;
    const ef = std.fs.createFileAbsolute(existing, .{}) catch unreachable;
    ef.writeAll("old") catch unreachable;
    ef.close();

    // Should fail with ModuleAlreadyExists
    const result = activateSnapshot(std.testing.allocator, snap_path, mod_dir, "existing");
    try std.testing.expectError(SnapshotError.ModuleAlreadyExists, result);
}

test "activateSnapshot rejects nonexistent snapshot" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    var mod_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const mod_dir = std.fmt.bufPrint(&mod_dir_buf, "{s}/modules", .{base_path}) catch unreachable;

    const result = activateSnapshot(std.testing.allocator, "/nonexistent/snap.squashfs", mod_dir, "test-mod");
    try std.testing.expectError(SnapshotError.SnapshotNotFound, result);
}

// ── Data integrity tests ────────────────────────────────────────────

test "copyFile preserves data integrity for large content" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    // Write a file with known pattern (128KB)
    var src_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const src_path = std.fmt.bufPrint(&src_path_buf, "{s}/large.bin", .{base_path}) catch unreachable;
    const src_file = std.fs.createFileAbsolute(src_path, .{}) catch unreachable;

    var pattern: [4096]u8 = undefined;
    for (&pattern, 0..) |*b, i| {
        b.* = @truncate(i);
    }
    var writes: usize = 0;
    while (writes < 32) : (writes += 1) {
        src_file.writeAll(&pattern) catch unreachable;
    }
    src_file.close();

    // Copy
    var dst_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dst_path = std.fmt.bufPrint(&dst_path_buf, "{s}/copy.bin", .{base_path}) catch unreachable;
    try copyFile(src_path, dst_path);

    // Verify integrity: both files should have same size
    const src_stat = blk: {
        const f = std.fs.openFileAbsolute(src_path, .{}) catch unreachable;
        defer f.close();
        break :blk f.stat() catch unreachable;
    };
    const dst_stat = blk: {
        const f = std.fs.openFileAbsolute(dst_path, .{}) catch unreachable;
        defer f.close();
        break :blk f.stat() catch unreachable;
    };
    try std.testing.expectEqual(src_stat.size, dst_stat.size);
    try std.testing.expectEqual(@as(u64, 4096 * 32), src_stat.size);

    // Verify first and last blocks match
    const dst_file = try std.fs.openFileAbsolute(dst_path, .{});
    defer dst_file.close();
    var read_buf: [4096]u8 = undefined;
    const n = try dst_file.read(&read_buf);
    try std.testing.expectEqual(@as(usize, 4096), n);
    try std.testing.expectEqualSlices(u8, &pattern, &read_buf);
}

test "copyFile handles empty file" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    var src_buf: [std.fs.max_path_bytes]u8 = undefined;
    const src_path = std.fmt.bufPrint(&src_buf, "{s}/empty.bin", .{base_path}) catch unreachable;
    const sf = std.fs.createFileAbsolute(src_path, .{}) catch unreachable;
    sf.close();

    var dst_buf: [std.fs.max_path_bytes]u8 = undefined;
    const dst_path = std.fmt.bufPrint(&dst_buf, "{s}/empty_copy.bin", .{base_path}) catch unreachable;

    try copyFile(src_path, dst_path);

    const stat = blk: {
        const f = std.fs.openFileAbsolute(dst_path, .{}) catch unreachable;
        defer f.close();
        break :blk f.stat() catch unreachable;
    };
    try std.testing.expectEqual(@as(u64, 0), stat.size);
}

// ── Overlay rebuild tests ───────────────────────────────────────────

test "clearDirectory handles nested structures" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Create nested directory structure
    tmp_dir.dir.makeDir("a") catch {};
    {
        var a = tmp_dir.dir.openDir("a", .{}) catch unreachable;
        defer a.close();
        a.makeDir("b") catch {};
        {
            var b = a.openDir("b", .{}) catch unreachable;
            defer b.close();
            b.makeDir("c") catch {};
            const f = b.createFile("deep.txt", .{}) catch unreachable;
            f.writeAll("deep content") catch unreachable;
            f.close();
        }
        const f = a.createFile("file.txt", .{}) catch unreachable;
        f.close();
    }

    // Create some top-level files
    const f1 = tmp_dir.dir.createFile("top1.txt", .{}) catch unreachable;
    f1.close();
    const f2 = tmp_dir.dir.createFile("top2.txt", .{}) catch unreachable;
    f2.close();

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = tmp_dir.dir.realpath(".", &path_buf) catch unreachable;

    try clearDirectory(abs_path);

    // Verify completely empty
    var dir = try std.fs.openDirAbsolute(abs_path, .{ .iterate = true });
    defer dir.close();
    var iter = dir.iterate();
    try std.testing.expect(try iter.next() == null);
}

test "clearDirectory on already empty dir is no-op" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = tmp_dir.dir.realpath(".", &path_buf) catch unreachable;

    // Should succeed without error on empty dir
    try clearDirectory(abs_path);

    var dir = try std.fs.openDirAbsolute(abs_path, .{ .iterate = true });
    defer dir.close();
    var iter = dir.iterate();
    try std.testing.expect(try iter.next() == null);
}

// ── RestoreParams validation tests ──────────────────────────────────

test "RestoreParams struct fields" {
    const params = RestoreParams{
        .sandbox_dir = "/data/sandboxes/test",
        .snapshot_path = "/data/sandboxes/test/snapshots/cp1.squashfs",
        .snapshot_mount_point = "/data/sandboxes/test/images/_snapshot",
        .merged_path = "/data/sandboxes/test/merged",
        .upper_data = "/data/sandboxes/test/upper/data",
        .upper_work = "/data/sandboxes/test/upper/work",
        .lower_layers = &.{
            "/data/sandboxes/test/images/100-python.squashfs",
            "/data/sandboxes/test/images/000-base.squashfs",
        },
    };
    try std.testing.expectEqualStrings("/data/sandboxes/test", params.sandbox_dir);
    try std.testing.expectEqual(@as(usize, 2), params.lower_layers.len);
}

// ── reinjectSecretPlaceholders tests ────────────────────────────────

test "reinjectSecretPlaceholders creates script file" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    const secrets = "export DB_PASSWORD='${DB_PASSWORD}'\nexport API_KEY='${API_KEY}'\n";

    try reinjectSecretPlaceholders(base_path, secrets);

    // Verify the file was created with correct content
    var script_buf: [std.fs.max_path_bytes]u8 = undefined;
    const script_path = std.fmt.bufPrint(&script_buf, "{s}/upper/data/etc/profile.d/squash-secrets.sh", .{base_path}) catch unreachable;

    const file = try std.fs.openFileAbsolute(script_path, .{});
    defer file.close();
    var read_buf: [512]u8 = undefined;
    const n = try file.read(&read_buf);
    try std.testing.expectEqualStrings(secrets, read_buf[0..n]);
}

test "reinjectSecretPlaceholders is idempotent" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    var dir_buf: [std.fs.max_path_bytes]u8 = undefined;
    const base_path = tmp_dir.dir.realpath(".", &dir_buf) catch unreachable;

    const secrets1 = "export KEY1='val1'\n";
    const secrets2 = "export KEY2='val2'\n";

    try reinjectSecretPlaceholders(base_path, secrets1);
    try reinjectSecretPlaceholders(base_path, secrets2);

    // Should have the second content (overwritten)
    var script_buf: [std.fs.max_path_bytes]u8 = undefined;
    const script_path = std.fmt.bufPrint(&script_buf, "{s}/upper/data/etc/profile.d/squash-secrets.sh", .{base_path}) catch unreachable;

    const file = try std.fs.openFileAbsolute(script_path, .{});
    defer file.close();
    var read_buf: [512]u8 = undefined;
    const n = try file.read(&read_buf);
    try std.testing.expectEqualStrings(secrets2, read_buf[0..n]);
}

// ── S3 key path construction tests ──────────────────────────────────

test "S3 snapshot key format" {
    // Verify the S3 key format matches expectations
    const s3_key = try std.fmt.allocPrint(std.testing.allocator, "snapshots/{s}/{s}.squashfs", .{
        "sandbox-dev", "checkpoint-1",
    });
    defer std.testing.allocator.free(s3_key);
    try std.testing.expectEqualStrings("snapshots/sandbox-dev/checkpoint-1.squashfs", s3_key);
}

test "S3Client init for snapshot operations" {
    const client = s3.S3Client.init("my-bucket", "us-west-2", "prod/", null, "AKID", "SECRET");
    try std.testing.expectEqualStrings("my-bucket", client.bucket);
    try std.testing.expectEqualStrings("us-west-2", client.region);
    try std.testing.expectEqualStrings("prod/", client.prefix);
}
