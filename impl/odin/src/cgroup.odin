package squashd

import "core:fmt"
import "core:os"
import "core:strings"

// ---------------------------------------------------------------------------
// Cgroup handle and errors
// ---------------------------------------------------------------------------

Cgroup_Handle :: struct {
	path: string, // "/sys/fs/cgroup/squash-{id}"
}

Cgroup_Error :: enum {
	None,
	Mkdir_Failed,
	Write_Failed,
}

// Create a cgroup v2 for the given sandbox with CPU and memory limits.
// cpu: fraction of a CPU core (e.g. 2.0 = 2 full cores).
// memory_mb: memory limit in megabytes.
cgroup_create :: proc(
	id: string,
	cpu: f64,
	memory_mb: u64,
	allocator := context.allocator,
) -> (Cgroup_Handle, Cgroup_Error) {
	path := fmt.aprintf("/sys/fs/cgroup/squash-%s", id, allocator = allocator)

	if os.make_directory(path) != nil {
		// May already exist from a previous run — check it's a directory
		if !os.exists(path) {
			return {}, .Mkdir_Failed
		}
	}

	// CPU: quota = cpu * 100000, period = 100000
	quota := u64(cpu * 100_000.0)
	cpu_val := fmt.tprintf("%d 100000", quota)
	if !os.write_entire_file(fmt.tprintf("%s/cpu.max", path), transmute([]byte)cpu_val) {
		return {}, .Write_Failed
	}

	// Memory limit in bytes
	mem_bytes := memory_mb * 1024 * 1024
	mem_val := fmt.tprintf("%d", mem_bytes)
	if !os.write_entire_file(fmt.tprintf("%s/memory.max", path), transmute([]byte)mem_val) {
		return {}, .Write_Failed
	}

	return Cgroup_Handle{path = path}, .None
}

// Destroy a cgroup. Idempotent — safe to call multiple times.
// Moves any remaining processes to the root cgroup first.
cgroup_destroy :: proc(h: ^Cgroup_Handle) {
	if len(h.path) == 0 {
		return
	}
	// Move remaining processes to root cgroup before removal
	procs_path := fmt.tprintf("%s/cgroup.procs", h.path)
	if procs_data, ok := os.read_entire_file(procs_path, context.temp_allocator); ok {
		content := strings.trim_space(string(procs_data))
		if len(content) > 0 {
			os.write_entire_file("/sys/fs/cgroup/cgroup.procs", procs_data)
		}
	}
	os.remove(h.path)
	h.path = ""
}

// Add a process to this cgroup by writing its PID to cgroup.procs.
cgroup_add_process :: proc(h: ^Cgroup_Handle, pid: i32) {
	procs_path := fmt.tprintf("%s/cgroup.procs", h.path)
	pid_str := fmt.tprintf("%d", pid)
	os.write_entire_file(procs_path, transmute([]byte)pid_str)
}
