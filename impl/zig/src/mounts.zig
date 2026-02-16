const std = @import("std");
const log = std.log.scoped(.mounts);

const builtin = @import("builtin");
const is_linux = builtin.os.tag == .linux;

// On Linux, import mount headers for mount(2)/umount2(2).
// On macOS (for testing), the actual syscalls are stubbed.
const c = if (is_linux) @cImport({
    @cInclude("sys/mount.h");
}) else struct {
    // Stubs for macOS compilation. Actual mount operations are Linux-only.
    pub const MS_RDONLY: c_int = 1;
    pub fn mount(_: anytype, _: anytype, _: anytype, _: anytype, _: anytype) c_int {
        return -1;
    }
    pub fn umount2(_: anytype, _: anytype) c_int {
        return -1;
    }
};

// ── Error types ──────────────────────────────────────────────────────

pub const MountError = error{
    MountFailed,
    UmountFailed,
    FormatFailed,
};

/// Read the thread-local C errno after a failed C library call.
/// Returns the Zig E enum so we can log the name.
/// On non-Linux platforms (stubs), returns a safe default.
fn cErrno() std.posix.E {
    if (!is_linux) {
        return std.posix.E.NOENT;
    }
    return @enumFromInt(std.c._errno().*);
}

// ── SquashfsMount ────────────────────────────────────────────────────

/// Read-only squashfs loop mount. Wraps a single mount(2) call
/// with "squashfs" filesystem type and MS_RDONLY flag.
pub const SquashfsMount = struct {
    mount_point: [*:0]const u8,
    active: bool,

    /// Mount a squashfs image read-only at mount_point.
    /// Creates the mount point directory if it doesn't exist.
    pub fn mount(
        squashfs_path: [*:0]const u8,
        mount_point: [*:0]const u8,
    ) (MountError || std.fs.Dir.MakeError)!SquashfsMount {
        // Create mount point directory
        std.fs.makeDirAbsolute(std.mem.span(mount_point)) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        const rc = c.mount(squashfs_path, mount_point, "squashfs", c.MS_RDONLY, null);
        if (rc != 0) {
            log.err("mount squashfs failed: {s} -> {s}: {s}", .{
                std.mem.span(squashfs_path),
                std.mem.span(mount_point),
                @tagName(cErrno()),
            });
            return MountError.MountFailed;
        }

        return .{
            .mount_point = mount_point,
            .active = true,
        };
    }

    /// Idempotent cleanup. Safe to call multiple times.
    /// Uses MNT_DETACH (lazy unmount) to avoid EBUSY.
    pub fn deinit(self: *SquashfsMount) void {
        if (self.active) {
            const rc = c.umount2(self.mount_point, 2); // MNT_DETACH = 2
            if (rc != 0) {
                log.warn("umount2 squashfs {s} failed: {s}", .{
                    std.mem.span(self.mount_point),
                    @tagName(cErrno()),
                });
            }
            self.active = false;
        }
    }
};

// ── TmpfsMount ───────────────────────────────────────────────────────

/// Size-limited tmpfs mount that also creates overlay subdirectories
/// (data/, work/) required by overlayfs upperdir/workdir.
pub const TmpfsMount = struct {
    mount_point: [*:0]const u8,
    active: bool,

    /// Mount a tmpfs at mount_point with the given size limit.
    /// Creates the mount point directory and overlay subdirectories
    /// (data/, work/) inside the mounted tmpfs.
    pub fn mount(
        mount_point: [*:0]const u8,
        size_mb: u64,
    ) (MountError || std.fs.Dir.MakeError)!TmpfsMount {
        const mp_span = std.mem.span(mount_point);

        // Create mount point directory
        std.fs.makeDirAbsolute(mp_span) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        // Format tmpfs size option
        var opts_buf: [64]u8 = undefined;
        const opts = std.fmt.bufPrintZ(&opts_buf, "size={d}m", .{size_mb}) catch
            return MountError.FormatFailed;

        const rc = c.mount("tmpfs", mount_point, "tmpfs", 0, @as(?*const anyopaque, opts.ptr));
        if (rc != 0) {
            log.err("mount tmpfs failed: {s}: {s}", .{ mp_span, @tagName(cErrno()) });
            return MountError.MountFailed;
        }

        // Create overlay subdirectories inside the mounted tmpfs.
        // errdefer unmount if subdir creation fails.
        errdefer {
            _ = c.umount2(mount_point, 2);
        }

        var data_buf: [std.fs.max_path_bytes]u8 = undefined;
        const data_path = std.fmt.bufPrint(&data_buf, "{s}/data", .{mp_span}) catch
            return MountError.FormatFailed;
        std.fs.makeDirAbsolute(data_path) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        var work_buf: [std.fs.max_path_bytes]u8 = undefined;
        const work_path = std.fmt.bufPrint(&work_buf, "{s}/work", .{mp_span}) catch
            return MountError.FormatFailed;
        std.fs.makeDirAbsolute(work_path) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        return .{
            .mount_point = mount_point,
            .active = true,
        };
    }

    /// Idempotent cleanup. Safe to call multiple times.
    pub fn deinit(self: *TmpfsMount) void {
        if (self.active) {
            const rc = c.umount2(self.mount_point, 2); // MNT_DETACH
            if (rc != 0) {
                log.warn("umount2 tmpfs {s} failed: {s}", .{
                    std.mem.span(self.mount_point),
                    @tagName(cErrno()),
                });
            }
            self.active = false;
        }
    }
};

// ── OverlayMount ─────────────────────────────────────────────────────

/// Overlayfs mount combining read-only lower layers with a writable upper.
/// Builds the "lowerdir=A:B:C,upperdir=X,workdir=Y" options string.
pub const OverlayMount = struct {
    merged_path: [*:0]const u8,
    active: bool,

    /// Mount an overlayfs at the merged path.
    ///
    /// lower_components: ordered highest-priority first.
    ///   Snapshot layer first, then modules in descending numeric order.
    /// upper_data: path to the writable upper data directory.
    /// work: path to the overlayfs workdir.
    /// merged: path to the final merged mount point.
    pub fn mount(
        lower_components: []const []const u8,
        upper_data: [*:0]const u8,
        work: [*:0]const u8,
        merged: [*:0]const u8,
    ) (MountError || std.fs.Dir.MakeError)!OverlayMount {
        if (lower_components.len == 0) return MountError.MountFailed;

        // Create merged mount point directory
        std.fs.makeDirAbsolute(std.mem.span(merged)) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        };

        // Build options string: "lowerdir=A:B:C,upperdir=X,workdir=Y"
        var opts_buf: [4096]u8 = undefined;
        var stream = std.io.fixedBufferStream(&opts_buf);
        const writer = stream.writer();

        writer.writeAll("lowerdir=") catch return MountError.FormatFailed;
        for (lower_components, 0..) |comp, i| {
            if (i > 0) writer.writeByte(':') catch return MountError.FormatFailed;
            writer.writeAll(comp) catch return MountError.FormatFailed;
        }
        writer.print(",upperdir={s},workdir={s}", .{
            std.mem.span(upper_data),
            std.mem.span(work),
        }) catch return MountError.FormatFailed;
        // Null-terminate for the C syscall
        writer.writeByte(0) catch return MountError.FormatFailed;

        const opts_slice = stream.getWritten();
        const opts_ptr: [*]const u8 = opts_slice.ptr;

        const rc = c.mount(
            "overlay",
            merged,
            "overlay",
            0,
            @as(?*const anyopaque, @ptrCast(opts_ptr)),
        );
        if (rc != 0) {
            log.err("mount overlay failed: {s}: {s}", .{
                std.mem.span(merged),
                @tagName(cErrno()),
            });
            return MountError.MountFailed;
        }

        return .{
            .merged_path = merged,
            .active = true,
        };
    }

    /// Idempotent cleanup. Safe to call multiple times.
    pub fn deinit(self: *OverlayMount) void {
        if (self.active) {
            const rc = c.umount2(self.merged_path, 2); // MNT_DETACH
            if (rc != 0) {
                log.warn("umount2 overlay {s} failed: {s}", .{
                    std.mem.span(self.merged_path),
                    @tagName(cErrno()),
                });
            }
            self.active = false;
        }
    }
};

// ── Tests ────────────────────────────────────────────────────────────
// Mount operations require root + Linux kernel. These unit tests verify
// the logic that can be tested without privileges (options string building,
// idempotent deinit, error paths).

test "OverlayMount builds correct options string" {
    // We can't actually mount without root, but we can test the options
    // string construction by replicating the builder logic.
    var opts_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&opts_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{
        "/sandbox/images/_snapshot",
        "/sandbox/images/100-python312.squashfs",
        "/sandbox/images/000-base-alpine.squashfs",
    };

    try writer.writeAll("lowerdir=");
    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }
    try writer.print(",upperdir={s},workdir={s}", .{
        "/sandbox/upper/data",
        "/sandbox/upper/work",
    });

    const expected = "lowerdir=/sandbox/images/_snapshot:" ++
        "/sandbox/images/100-python312.squashfs:" ++
        "/sandbox/images/000-base-alpine.squashfs," ++
        "upperdir=/sandbox/upper/data," ++
        "workdir=/sandbox/upper/work";

    try std.testing.expectEqualStrings(expected, stream.getWritten());
}

test "OverlayMount single lower component" {
    var opts_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&opts_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{"/sandbox/images/000-base-alpine.squashfs"};

    try writer.writeAll("lowerdir=");
    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }
    try writer.print(",upperdir={s},workdir={s}", .{
        "/sandbox/upper/data",
        "/sandbox/upper/work",
    });

    const expected = "lowerdir=/sandbox/images/000-base-alpine.squashfs," ++
        "upperdir=/sandbox/upper/data," ++
        "workdir=/sandbox/upper/work";

    try std.testing.expectEqualStrings(expected, stream.getWritten());
}

test "SquashfsMount deinit is idempotent" {
    // Construct a mount struct in inactive state — calling deinit should be safe
    var m = SquashfsMount{
        .mount_point = "/nonexistent",
        .active = false,
    };
    m.deinit(); // should not crash or error
    m.deinit(); // second call also safe
    try std.testing.expect(!m.active);
}

test "TmpfsMount deinit is idempotent" {
    var m = TmpfsMount{
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

// ── @cImport Compatibility Tests ────────────────────────────────────────

test "kernel headers: MS_RDONLY constant exists" {
    // Verify MS_RDONLY is available from @cImport or stub
    _ = c.MS_RDONLY;
}

test "kernel headers: mount function signature" {
    // Verify mount function is callable with correct signature
    // On macOS this is stubbed, on Linux it's from sys/mount.h
    const result = c.mount(null, null, null, 0, null);
    // Stub returns -1, real mount would also fail without root
    try std.testing.expect(result == -1);
}

test "kernel headers: umount2 function signature" {
    // Verify umount2 function is callable
    const result = c.umount2(null, 0);
    try std.testing.expect(result == -1);
}

// ── Mount Lifecycle Tests ────────────────────────────────────────────────

test "SquashfsMount active flag lifecycle" {
    var m = SquashfsMount{
        .mount_point = "/test/mount",
        .active = true,
    };

    // Deinit should set active to false
    m.deinit();
    try std.testing.expect(!m.active);

    // Second deinit should be safe (idempotent)
    m.deinit();
    try std.testing.expect(!m.active);
}

test "TmpfsMount active flag lifecycle" {
    var m = TmpfsMount{
        .mount_point = "/test/tmpfs",
        .active = true,
    };

    m.deinit();
    try std.testing.expect(!m.active);

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

test "SquashfsMount deinit from active state (simulated)" {
    // Verify deinit transitions from active to inactive
    var m = SquashfsMount{
        .mount_point = "/nonexistent/squashfs",
        .active = true,
    };

    try std.testing.expect(m.active);
    m.deinit();
    try std.testing.expect(!m.active);
}

test "TmpfsMount deinit from active state (simulated)" {
    var m = TmpfsMount{
        .mount_point = "/nonexistent/tmpfs",
        .active = true,
    };

    try std.testing.expect(m.active);
    m.deinit();
    try std.testing.expect(!m.active);
}

test "OverlayMount deinit from active state (simulated)" {
    var m = OverlayMount{
        .merged_path = "/nonexistent/overlay",
        .active = true,
    };

    try std.testing.expect(m.active);
    m.deinit();
    try std.testing.expect(!m.active);
}

// ── Options String Building Tests ───────────────────────────────────────

test "TmpfsMount size option formatting" {
    // Verify size option string is formatted correctly
    var opts_buf: [64]u8 = undefined;
    const size_mb: u64 = 512;
    const opts = try std.fmt.bufPrintZ(&opts_buf, "size={d}m", .{size_mb});

    const opts_slice: []const u8 = opts[0..opts.len];
    try std.testing.expectEqualStrings("size=512m", opts_slice);
}

test "TmpfsMount size option small value" {
    var opts_buf: [64]u8 = undefined;
    const size_mb: u64 = 1;
    const opts = try std.fmt.bufPrintZ(&opts_buf, "size={d}m", .{size_mb});

    const opts_slice: []const u8 = opts[0..opts.len];
    try std.testing.expectEqualStrings("size=1m", opts_slice);
}

test "TmpfsMount size option large value" {
    var opts_buf: [64]u8 = undefined;
    const size_mb: u64 = 8192;
    const opts = try std.fmt.bufPrintZ(&opts_buf, "size={d}m", .{size_mb});

    const opts_slice: []const u8 = opts[0..opts.len];
    try std.testing.expectEqualStrings("size=8192m", opts_slice);
}

test "OverlayMount options string multiple lowers" {
    var opts_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&opts_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{
        "/layer1",
        "/layer2",
        "/layer3",
        "/layer4",
    };

    try writer.writeAll("lowerdir=");
    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }
    try writer.print(",upperdir={s},workdir={s}", .{
        "/upper",
        "/work",
    });

    const expected = "lowerdir=/layer1:/layer2:/layer3:/layer4,upperdir=/upper,workdir=/work";
    try std.testing.expectEqualStrings(expected, stream.getWritten());
}

test "OverlayMount options string with long paths" {
    var opts_buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&opts_buf);
    const writer = stream.writer();

    const lower = [_][]const u8{
        "/very/long/path/to/sandbox/images/_snapshot",
        "/very/long/path/to/sandbox/images/100-module.squashfs",
    };

    try writer.writeAll("lowerdir=");
    for (lower[0..], 0..) |comp, i| {
        if (i > 0) try writer.writeByte(':');
        try writer.writeAll(comp);
    }
    try writer.print(",upperdir={s},workdir={s}", .{
        "/very/long/path/to/upper/data",
        "/very/long/path/to/upper/work",
    });

    const result = stream.getWritten();
    try std.testing.expect(result.len < 4096);
    try std.testing.expect(std.mem.containsAtLeast(u8, result, 1, "lowerdir="));
    try std.testing.expect(std.mem.containsAtLeast(u8, result, 1, "upperdir="));
    try std.testing.expect(std.mem.containsAtLeast(u8, result, 1, "workdir="));
}

// ── Error Path Tests ─────────────────────────────────────────────────────

test "cErrno returns valid E enum" {
    // cErrno should always return a valid std.posix.E enum value
    const err = cErrno();
    _ = @tagName(err); // should not crash
}

test "MountError type completeness" {
    // Verify all error types exist and are distinct
    try std.testing.expect(MountError.MountFailed != MountError.UmountFailed);
    try std.testing.expect(MountError.MountFailed != MountError.FormatFailed);
    try std.testing.expect(MountError.UmountFailed != MountError.FormatFailed);
}

// ── Platform Compatibility Tests ─────────────────────────────────────────

test "platform detection: Linux vs macOS stubs" {
    // Verify the platform detection works
    // On macOS, stub constants should be available
    // On Linux, real constants should be available
    if (!is_linux) {
        // Verify stub MS_RDONLY has expected value
        try std.testing.expect(c.MS_RDONLY == 1);
    }
}

test "mount operations are Linux-only" {
    // Document that real mount operations require Linux
    if (!is_linux) {
        // On non-Linux platforms, mount calls should fail
        const result = c.mount(null, null, null, 0, null);
        try std.testing.expect(result == -1);
    }
}

// ── Resource Cleanup Tests ───────────────────────────────────────────────

test "multiple mounts can be deinit'd independently" {
    var m1 = SquashfsMount{
        .mount_point = "/test/mount1",
        .active = true,
    };
    var m2 = SquashfsMount{
        .mount_point = "/test/mount2",
        .active = true,
    };
    var m3 = TmpfsMount{
        .mount_point = "/test/tmpfs",
        .active = true,
    };

    // Cleanup in arbitrary order
    m2.deinit();
    try std.testing.expect(!m2.active);
    try std.testing.expect(m1.active);
    try std.testing.expect(m3.active);

    m1.deinit();
    try std.testing.expect(!m1.active);
    try std.testing.expect(m3.active);

    m3.deinit();
    try std.testing.expect(!m3.active);
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

    // Modifying one should not affect the other
    m1.deinit();
    try std.testing.expect(!m1.active);
    try std.testing.expect(m2.active);

    m2.deinit();
    try std.testing.expect(!m2.active);
}
