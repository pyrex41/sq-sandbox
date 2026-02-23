package squashd

// Cgroup v2 — REMOVED in unprivileged mode.
// Sandboxes use bubblewrap for isolation; resource limits are not enforced.

Cgroup_Handle :: struct {
	path: string,
	id:   string,
}

create_cgroup :: proc(id: string, cpu_cores: f64, memory_mb: u64) -> Maybe(Cgroup_Handle) {
	return nil
}

destroy_cgroup :: proc(handle: ^Cgroup_Handle) {}

cgroup_add_process :: proc(handle: ^Cgroup_Handle, pid: i32) {}
