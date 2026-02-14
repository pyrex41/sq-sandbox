const std = @import("std");

/// Sandbox IDs: non-empty, [a-zA-Z0-9_-]+
pub fn validId(s: []const u8) bool {
    if (s.len == 0) return false;
    for (s) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '-') return false;
    }
    return true;
}

/// Snapshot labels and module names: non-empty, [a-zA-Z0-9_.-]+
pub fn validLabel(s: []const u8) bool {
    if (s.len == 0) return false;
    for (s) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '_' and ch != '.' and ch != '-') return false;
    }
    return true;
}

/// Module names follow the same rules as labels.
pub const validModule = validLabel;

// ── Tests ────────────────────────────────────────────────────────────

test "validId accepts alphanumeric with dashes and underscores" {
    try std.testing.expect(validId("my-sandbox-123"));
    try std.testing.expect(validId("a"));
    try std.testing.expect(validId("test_sandbox"));
    try std.testing.expect(validId("ABC-123_xyz"));
}

test "validId rejects empty string" {
    try std.testing.expect(!validId(""));
}

test "validId rejects path traversal" {
    try std.testing.expect(!validId("../../etc"));
    try std.testing.expect(!validId("../passwd"));
}

test "validId rejects spaces" {
    try std.testing.expect(!validId("has space"));
    try std.testing.expect(!validId(" leading"));
    try std.testing.expect(!validId("trailing "));
}

test "validId rejects special characters" {
    try std.testing.expect(!validId("foo/bar"));
    try std.testing.expect(!validId("foo\\bar"));
    try std.testing.expect(!validId("foo@bar"));
    try std.testing.expect(!validId("foo:bar"));
    try std.testing.expect(!validId("foo;bar"));
    try std.testing.expect(!validId("foo$bar"));
}

test "validId rejects dots (unlike validLabel)" {
    try std.testing.expect(!validId("has.dot"));
    try std.testing.expect(!validId("v1.0"));
}

test "validLabel accepts alphanumeric with dashes, underscores, and dots" {
    try std.testing.expect(validLabel("my-checkpoint"));
    try std.testing.expect(validLabel("v1.0"));
    try std.testing.expect(validLabel("000-base-alpine"));
    try std.testing.expect(validLabel("100-python312"));
    try std.testing.expect(validLabel("snapshot_2025.01.15"));
    try std.testing.expect(validLabel("a"));
}

test "validLabel rejects empty string" {
    try std.testing.expect(!validLabel(""));
}

test "validLabel rejects path traversal" {
    try std.testing.expect(!validLabel("../../etc"));
    try std.testing.expect(!validLabel("../hack"));
}

test "validLabel rejects spaces and special characters" {
    try std.testing.expect(!validLabel("has space"));
    try std.testing.expect(!validLabel("foo/bar"));
    try std.testing.expect(!validLabel("foo@bar"));
    try std.testing.expect(!validLabel("foo:bar"));
}

test "validModule is the same as validLabel" {
    try std.testing.expect(validModule("000-base-alpine"));
    try std.testing.expect(validModule("100-python312"));
    try std.testing.expect(validModule("my.module.v2"));
    try std.testing.expect(!validModule(""));
    try std.testing.expect(!validModule("bad/module"));
}
