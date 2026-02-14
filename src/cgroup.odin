package squashd

import "core:fmt"
import "core:os"

// ---------------------------------------------------------------------------
// Cgroup handle
// ---------------------------------------------------------------------------

Cgroup_Handle :: struct {
	path: string, // "/sys/fs/cgroup/squash-{id}"
}

// Destroy a cgroup by removing its directory.
cgroup_destroy :: proc(h: ^Cgroup_Handle) {
	if len(h.path) > 0 {
		os.remove(h.path)
	}
}

// Add a process to this cgroup by writing its PID to cgroup.procs.
cgroup_add_process :: proc(h: ^Cgroup_Handle, pid: i32) {
	procs_path := fmt.tprintf("%s/cgroup.procs", h.path)
	pid_str := fmt.tprintf("%d", pid)
	os.write_entire_file(procs_path, transmute([]byte)pid_str)
}
