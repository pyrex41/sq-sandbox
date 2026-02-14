const std = @import("std");
pub const config = @import("config.zig");
pub const validate = @import("validate.zig");
pub const exec = @import("exec.zig");
pub const snapshot = @import("snapshot.zig");
pub const init = @import("init.zig");
pub const reaper = @import("reaper.zig");
pub const json = @import("json.zig");
pub const sigv4 = @import("sigv4.zig");
pub const mounts = @import("mounts.zig");
pub const cgroup = @import("cgroup.zig");
pub const netns = @import("netns.zig");
pub const sandbox_test = @import("sandbox_test.zig");

const log = std.log.scoped(.squashd);

pub fn main() !void {
    const cfg = config.Config.fromEnv();

    log.info("squash v4 (zig) — port={d} backend={s} data={s}", .{
        cfg.port,
        @tagName(cfg.backend),
        cfg.data_dir,
    });

    // 1. Init / recovery
    init.run(&cfg);

    // TODO: 2. Start sandbox manager, proxy, tailscale
    // TODO: 3. Start reaper thread (requires sandbox manager callbacks)
    // const reaper_thread = try std.Thread.spawn(.{}, reaper.run, .{reaper_cfg});
    // reaper_thread.detach();
    // TODO: 4. Start HTTP server
}

// Pull in tests from all modules
test {
    _ = config;
    _ = validate;
    _ = exec;
    _ = snapshot;
    _ = init;
    _ = reaper;
    _ = json;
    _ = sigv4;
    _ = mounts;
    _ = cgroup;
    _ = netns;
    _ = sandbox_test;
}
