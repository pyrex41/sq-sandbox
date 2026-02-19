// Network namespace — REMOVED in unprivileged mode.
// Sandboxes now use bubblewrap for isolation (no veth pairs, no iptables).
// This stub exists for API compatibility.

const std = @import("std");

pub const NetnsHandle = struct {
    name: []const u8,
};

pub fn setup_netns(_: std.mem.Allocator, _: []const u8, _: []const u8, _: ?[]const []const u8) ?NetnsHandle {
    return null;
}

pub fn teardown_netns(_: ?NetnsHandle) void {}
