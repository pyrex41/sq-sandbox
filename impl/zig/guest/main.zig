const std = @import("std");

const log = std.log.scoped(.guest);

pub fn main() !void {
    log.info("sq-guest-agent starting", .{});
    // TODO: mount essential filesystems, parse cmdline, mount layers, listen on vsock
}
