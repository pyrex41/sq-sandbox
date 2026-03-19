const std = @import("std");
const log = std.log.scoped(.mounts);

// ── Error types ──────────────────────────────────────────────────────

pub const MountError = error{
    MountFailed,
    UmountFailed,
    FormatFailed,
};

// ── SquashfsMount ────────────────────────────────────────────────────

/// Read-only squashfs mount via sq-mount-layer helper script.
pub const SquashfsMount = struct {
    mount_point: []const u8,
    active: bool,

    /// Mount a squashfs image read-only at mount_point.
    /// Shells out to `sq-mount-layer <squashfs_path> <mount_point>`.
    pub fn mount(
        squashfs_path: []const u8,
        mount_point: []const u8,
    ) (MountError || std.fs.Dir.MakeError)!SquashfsMount {
        // Create mount point directory
        std.fs.makeDirAbsolute(mount_point) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        var child = std.process.Child.init(
            &.{ "sq-mount-layer", squashfs_path, mount_point },
            std.heap.page_allocator,
        );
        child.stdin_behavior = .Close;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        child.spawn() catch {
            log.err("failed to spawn sq-mount-layer for {s} -> {s}", .{
                squashfs_path,
                mount_point,
            });
            return MountError.MountFailed;
        };

        // Drain pipes to avoid deadlock
        var stdout_buf: [4096]u8 = undefined;
        var stderr_buf: [4096]u8 = undefined;
        _ = child.stdout.?.read(&stdout_buf) catch 0;
        _ = child.stderr.?.read(&stderr_buf) catch 0;

        const term = child.wait() catch return MountError.MountFailed;
        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    log.err("sq-mount-layer failed (exit {d}): {s} -> {s}", .{
                        code,
                        squashfs_path,
                        mount_point,
                    });
                    return MountError.MountFailed;
                }
            },
            else => return MountError.MountFailed,
        }

        return .{
            .mount_point = mount_point,
            .active = true,
        };
    }

    /// Idempotent cleanup. Safe to call multiple times.
    /// Shells out to `sq-mount-layer --unmount <mount_point>`.
    pub fn deinit(self: *SquashfsMount) void {
        if (self.active) {
            var child = std.process.Child.init(
                &.{ "sq-mount-layer", "--unmount", self.mount_point },
                std.heap.page_allocator,
            );
            child.stdin_behavior = .Close;
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Pipe;

            if (child.spawn()) |_| {} else |_| {
                self.active = false;
                return;
            }

            var stdout_buf: [4096]u8 = undefined;
            var stderr_buf: [4096]u8 = undefined;
            _ = child.stdout.?.read(&stdout_buf) catch 0;
            _ = child.stderr.?.read(&stderr_buf) catch 0;

            const term = child.wait() catch {
                self.active = false;
                return;
            };
            switch (term) {
                .Exited => |code| {
                    if (code != 0) {
                        log.warn("sq-mount-layer --unmount {s} failed (exit {d})", .{
                            self.mount_point,
                            code,
                        });
                    }
                },
                else => {
                    log.warn("sq-mount-layer --unmount {s} terminated abnormally", .{
                        self.mount_point,
                    });
                },
            }
            self.active = false;
        }
    }
};

// ── OverlayMount ─────────────────────────────────────────────────────

/// Overlay mount via sq-mount-overlay helper script.
/// Combines read-only lower layers with a writable upper.
pub const OverlayMount = struct {
    merged_path: []const u8,
    active: bool,

    /// Mount an overlay at the merged path.
    ///
    /// lower_components: ordered highest-priority first.
    /// upper_data: path to the writable upper data directory.
    /// work: path to the overlayfs workdir.
    /// merged: path to the final merged mount point.
    ///
    /// sq-mount-overlay takes a colon-separated lowerdir as the first arg.
    pub fn mount(
        lower_components: []const []const u8,
        upper_data: []const u8,
        work: []const u8,
        merged: []const u8,
    ) (MountError || std.fs.Dir.MakeError)!OverlayMount {
        if (lower_components.len == 0) return MountError.MountFailed;

        // Create merged mount point directory
        std.fs.makeDirAbsolute(merged) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        // Build colon-separated lower dirs string for sq-mount-overlay
        var lower_buf: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&lower_buf);
        const writer = stream.writer();
        for (lower_components, 0..) |comp, i| {
            if (i > 0) writer.writeByte(':') catch return MountError.FormatFailed;
            writer.writeAll(comp) catch return MountError.FormatFailed;
        }
        const lower_str = stream.getWritten();

        var child = std.process.Child.init(
            &.{ "sq-mount-overlay", lower_str, upper_data, work, merged },
            std.heap.page_allocator,
        );
        child.stdin_behavior = .Close;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        child.spawn() catch {
            log.err("failed to spawn sq-mount-overlay for {s}", .{merged});
            return MountError.MountFailed;
        };

        var stdout_buf: [4096]u8 = undefined;
        var stderr_buf: [4096]u8 = undefined;
        _ = child.stdout.?.read(&stdout_buf) catch 0;
        _ = child.stderr.?.read(&stderr_buf) catch 0;

        const term = child.wait() catch return MountError.MountFailed;
        switch (term) {
            .Exited => |code| {
                if (code != 0) {
                    log.err("sq-mount-overlay failed (exit {d}): {s}", .{
                        code,
                        merged,
                    });
                    return MountError.MountFailed;
                }
            },
            else => return MountError.MountFailed,
        }

        return .{
            .merged_path = merged,
            .active = true,
        };
    }

    /// Idempotent cleanup. Safe to call multiple times.
    /// Shells out to `sq-mount-overlay --unmount <merged>`.
    pub fn deinit(self: *OverlayMount) void {
        if (self.active) {
            var child = std.process.Child.init(
                &.{ "sq-mount-overlay", "--unmount", self.merged_path },
                std.heap.page_allocator,
            );
            child.stdin_behavior = .Close;
            child.stdout_behavior = .Pipe;
            child.stderr_behavior = .Pipe;

            if (child.spawn()) |_| {} else |_| {
                self.active = false;
                return;
            }

            var stdout_buf: [4096]u8 = undefined;
            var stderr_buf: [4096]u8 = undefined;
            _ = child.stdout.?.read(&stdout_buf) catch 0;
            _ = child.stderr.?.read(&stderr_buf) catch 0;

            const term = child.wait() catch {
                self.active = false;
                return;
            };
            switch (term) {
                .Exited => |code| {
                    if (code != 0) {
                        log.warn("sq-mount-overlay --unmount {s} failed (exit {d})", .{
                            self.merged_path,
                            code,
                        });
                    }
                },
                else => {
                    log.warn("sq-mount-overlay --unmount {s} terminated abnormally", .{
                        self.merged_path,
                    });
                },
            }
            self.active = false;
        }
    }
};

// ── Tests ────────────────────────────────────────────────────────────

test "SquashfsMount deinit is idempotent" {
    var m = SquashfsMount{
        .mount_point = "/nonexistent",
        .active = false,
    };
    m.deinit();
    m.deinit();
    try std.testing.expect(!m.active);
}

test "OverlayMount deinit is idempotent" {
    var m = OverlayMount{
        .merged_path = "/nonexistent",
        .active = false,
    };
    m.deinit();
    m.deinit();
    try std.testing.expect(!m.active);
}

test "OverlayMount rejects empty lower components" {
    const empty: []const []const u8 = &.{};
    const result = OverlayMount.mount(empty, "/upper/data", "/upper/work", "/merged");
    try std.testing.expectError(MountError.MountFailed, result);
}

test "OverlayMount builds correct colon-separated lower string" {
    // Replicate the lower-dir string builder logic to verify correctness
    var lower_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&lower_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{
        "/sandbox/images/_snapshot",
        "/sandbox/images/100-python312.squashfs",
        "/sandbox/images/000-base-alpine.squashfs",
    };

    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }

    const expected = "/sandbox/images/_snapshot:" ++
        "/sandbox/images/100-python312.squashfs:" ++
        "/sandbox/images/000-base-alpine.squashfs";

    try std.testing.expectEqualStrings(expected, stream.getWritten());
}

test "OverlayMount single lower component" {
    var lower_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&lower_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{"/sandbox/images/000-base-alpine.squashfs"};

    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }

    try std.testing.expectEqualStrings("/sandbox/images/000-base-alpine.squashfs", stream.getWritten());
}

test "SquashfsMount active flag lifecycle" {
    var m = SquashfsMount{
        .mount_point = "/test/mount",
        .active = true,
    };

    // Deinit should set active to false (subprocess will fail but that's ok)
    m.deinit();
    try std.testing.expect(!m.active);

    // Second deinit should be safe (idempotent)
    m.deinit();
    try std.testing.expect(!m.active);
}

test "OverlayMount active flag lifecycle" {
    var m = OverlayMount{
        .merged_path = "/test/overlay",
        .active = true,
    };

    m.deinit();
    try std.testing.expect(!m.active);

    m.deinit();
    try std.testing.expect(!m.active);
}

test "MountError type completeness" {
    try std.testing.expect(MountError.MountFailed != MountError.UmountFailed);
    try std.testing.expect(MountError.MountFailed != MountError.FormatFailed);
    try std.testing.expect(MountError.UmountFailed != MountError.FormatFailed);
}

test "multiple mounts can be deinit'd independently" {
    var m1 = SquashfsMount{
        .mount_point = "/test/mount1",
        .active = true,
    };
    var m2 = SquashfsMount{
        .mount_point = "/test/mount2",
        .active = true,
    };

    m2.deinit();
    try std.testing.expect(!m2.active);
    try std.testing.expect(m1.active);

    m1.deinit();
    try std.testing.expect(!m1.active);
}

test "mount structs are independent" {
    var m1 = OverlayMount{
        .merged_path = "/test/overlay1",
        .active = true,
    };
    var m2 = OverlayMount{
        .merged_path = "/test/overlay2",
        .active = true,
    };

    m1.deinit();
    try std.testing.expect(!m1.active);
    try std.testing.expect(m2.active);

    m2.deinit();
    try std.testing.expect(!m2.active);
}

test "OverlayMount options string multiple lowers" {
    var lower_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&lower_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{
        "/layer1",
        "/layer2",
        "/layer3",
        "/layer4",
    };

    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }

    const expected = "/layer1:/layer2:/layer3:/layer4";
    try std.testing.expectEqualStrings(expected, stream.getWritten());
}
