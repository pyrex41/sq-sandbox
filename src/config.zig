const std = @import("std");

pub const Backend = enum {
    chroot,
    firecracker,
};

pub const Config = struct {
    backend: Backend = .chroot,
    data_dir: []const u8 = "/data",
    port: u16 = 8080,
    auth_token: ?[]const u8 = null,
    s3_bucket: ?[]const u8 = null,
    s3_endpoint: ?[]const u8 = null,
    s3_region: []const u8 = "us-east-1",
    s3_prefix: []const u8 = "",
    ephemeral: bool = false,
    upper_limit_mb: u64 = 512,
    max_sandboxes: usize = 100,
    proxy_https: bool = false,
    tailscale_authkey: ?[]const u8 = null,
    tailscale_hostname: []const u8 = "squash",

    /// Directory path for squashfs module files.
    pub fn modulesDir(self: *const Config, buf: []u8) ![]const u8 {
        return std.fmt.bufPrint(buf, "{s}/modules", .{self.data_dir});
    }

    /// Directory path for sandbox state.
    pub fn sandboxesDir(self: *const Config, buf: []u8) ![]const u8 {
        return std.fmt.bufPrint(buf, "{s}/sandboxes", .{self.data_dir});
    }

    /// Load config from environment variables with defaults.
    pub fn fromEnv() Config {
        var cfg = Config{};

        if (getEnvNonEmpty("SQUASH_BACKEND")) |v| {
            cfg.backend = if (std.mem.eql(u8, v, "firecracker")) .firecracker else .chroot;
        }
        if (getEnvNonEmpty("SQUASH_DATA")) |v| cfg.data_dir = v;
        if (getEnvNonEmpty("SQUASH_PORT")) |v| {
            cfg.port = std.fmt.parseInt(u16, v, 10) catch 8080;
        }
        cfg.auth_token = getEnvNonEmpty("SQUASH_AUTH_TOKEN");
        cfg.s3_bucket = getEnvNonEmpty("SQUASH_S3_BUCKET");
        cfg.s3_endpoint = getEnvNonEmpty("SQUASH_S3_ENDPOINT");
        if (getEnvNonEmpty("SQUASH_S3_REGION")) |v| cfg.s3_region = v;
        if (getEnvNonEmpty("SQUASH_S3_PREFIX")) |v| cfg.s3_prefix = v;
        if (getEnvNonEmpty("SQUASH_EPHEMERAL")) |v| cfg.ephemeral = std.mem.eql(u8, v, "1");
        if (getEnvNonEmpty("SQUASH_UPPER_LIMIT_MB")) |v| {
            cfg.upper_limit_mb = std.fmt.parseInt(u64, v, 10) catch 512;
        }
        if (getEnvNonEmpty("SQUASH_MAX_SANDBOXES")) |v| {
            cfg.max_sandboxes = std.fmt.parseInt(usize, v, 10) catch 100;
        }
        if (getEnvNonEmpty("SQUASH_PROXY_HTTPS")) |v| cfg.proxy_https = std.mem.eql(u8, v, "1");
        cfg.tailscale_authkey = getEnvNonEmpty("TAILSCALE_AUTHKEY");
        if (getEnvNonEmpty("TAILSCALE_HOSTNAME")) |v| cfg.tailscale_hostname = v;

        return cfg;
    }

    pub fn s3Enabled(self: *const Config) bool {
        return self.s3_bucket != null;
    }

    pub fn ephemeralEnabled(self: *const Config) bool {
        return self.ephemeral and self.s3Enabled();
    }
};

/// Return env var value if set and non-empty, otherwise null.
fn getEnvNonEmpty(key: []const u8) ?[]const u8 {
    const val = std.posix.getenv(key) orelse return null;
    return if (val.len > 0) val else null;
}

// ── Tests ────────────────────────────────────────────────────────────

test "default config has expected values" {
    const cfg = Config{};
    try std.testing.expectEqual(Backend.chroot, cfg.backend);
    try std.testing.expectEqualStrings("/data", cfg.data_dir);
    try std.testing.expectEqual(@as(u16, 8080), cfg.port);
    try std.testing.expect(cfg.auth_token == null);
    try std.testing.expect(cfg.s3_bucket == null);
    try std.testing.expect(cfg.s3_endpoint == null);
    try std.testing.expectEqualStrings("us-east-1", cfg.s3_region);
    try std.testing.expectEqualStrings("", cfg.s3_prefix);
    try std.testing.expect(!cfg.ephemeral);
    try std.testing.expectEqual(@as(u64, 512), cfg.upper_limit_mb);
    try std.testing.expectEqual(@as(usize, 100), cfg.max_sandboxes);
    try std.testing.expect(!cfg.proxy_https);
    try std.testing.expect(cfg.tailscale_authkey == null);
    try std.testing.expectEqualStrings("squash", cfg.tailscale_hostname);
}

test "modulesDir builds correct path" {
    const cfg = Config{};
    var buf: [256]u8 = undefined;
    const path = try cfg.modulesDir(&buf);
    try std.testing.expectEqualStrings("/data/modules", path);
}

test "sandboxesDir builds correct path" {
    const cfg = Config{};
    var buf: [256]u8 = undefined;
    const path = try cfg.sandboxesDir(&buf);
    try std.testing.expectEqualStrings("/data/sandboxes", path);
}

test "modulesDir with custom data_dir" {
    const cfg = Config{ .data_dir = "/mnt/storage" };
    var buf: [256]u8 = undefined;
    const path = try cfg.modulesDir(&buf);
    try std.testing.expectEqualStrings("/mnt/storage/modules", path);
}

test "s3Enabled returns false when bucket is null" {
    const cfg = Config{};
    try std.testing.expect(!cfg.s3Enabled());
}

test "s3Enabled returns true when bucket is set" {
    const cfg = Config{ .s3_bucket = "my-bucket" };
    try std.testing.expect(cfg.s3Enabled());
}

test "ephemeralEnabled requires both flags" {
    const none = Config{};
    try std.testing.expect(!none.ephemeralEnabled());

    const only_ephemeral = Config{ .ephemeral = true };
    try std.testing.expect(!only_ephemeral.ephemeralEnabled());

    const only_s3 = Config{ .s3_bucket = "bucket" };
    try std.testing.expect(!only_s3.ephemeralEnabled());

    const both = Config{ .ephemeral = true, .s3_bucket = "bucket" };
    try std.testing.expect(both.ephemeralEnabled());
}

test "fromEnv returns defaults when no env vars set" {
    // This test relies on the test environment not having SQUASH_* vars set.
    // If it fails in CI, the env vars need to be cleared before running tests.
    const cfg = Config.fromEnv();
    try std.testing.expectEqual(Backend.chroot, cfg.backend);
    try std.testing.expectEqual(@as(u16, 8080), cfg.port);
}
