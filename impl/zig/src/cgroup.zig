// Cgroup v2 — REMOVED in unprivileged mode.
// Sandboxes run under bubblewrap without cgroup isolation.

pub const CgroupHandle = void;

pub fn create_cgroup(_: []const u8, _: f64, _: u64) ?CgroupHandle {
    return null;
}

pub fn destroy_cgroup(_: ?CgroupHandle) void {}
