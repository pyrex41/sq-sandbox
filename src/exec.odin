package squashd

import "core:c"
import "core:fmt"
import "core:os"
import "core:strings"
import "core:time"

// ---------------------------------------------------------------------------
// Process control syscalls — libc foreign import needed per-file in Odin.
// c_open, c_close are declared in linux_extra.odin; exec-specific ones here.
// ---------------------------------------------------------------------------

foreign import libc "system:c"

// nfds_t matches the platform-specific type used by the POSIX poll() signature.
when ODIN_OS == .Darwin {
	Nfds_t :: c.uint
} else {
	Nfds_t :: c.ulong
}

foreign libc {
	@(link_name = "fork")
	c_fork :: proc() -> c.int ---
	@(link_name = "execve")
	c_execve :: proc(path: cstring, argv: [^]cstring, envp: [^]cstring) -> c.int ---
	@(link_name = "chroot")
	c_chroot :: proc(path: cstring) -> c.int ---
	@(link_name = "chdir")
	c_chdir :: proc(path: cstring) -> c.int ---
	@(link_name = "unshare")
	c_unshare :: proc(flags: c.int) -> c.int ---
	@(link_name = "setns")
	c_setns :: proc(fd: c.int, nstype: c.int) -> c.int ---
	@(link_name = "dup2")
	c_dup2 :: proc(oldfd: c.int, newfd: c.int) -> c.int ---
	@(link_name = "pipe")
	c_pipe :: proc(fds: [^]c.int) -> c.int ---
	@(link_name = "read")
	c_read :: proc(fd: c.int, buf: [^]byte, count: c.size_t) -> c.ssize_t ---
	@(link_name = "poll")
	c_poll :: proc(fds: [^]Poll_Fd, nfds: Nfds_t, timeout: c.int) -> c.int ---
	@(link_name = "waitpid")
	c_waitpid :: proc(pid: c.int, status: ^c.int, options: c.int) -> c.int ---
	@(link_name = "kill")
	c_kill :: proc(pid: c.int, sig: c.int) -> c.int ---
	@(link_name = "getpid")
	c_getpid :: proc() -> c.int ---
	@(link_name = "_exit")
	c_exit :: proc(status: c.int) ---
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

SIGKILL       :: 9
STDOUT_FILENO :: 1
STDERR_FILENO :: 2

CLONE_NEWNS  :: 0x00020000
CLONE_NEWPID :: 0x20000000
CLONE_NEWIPC :: 0x08000000
CLONE_NEWUTS :: 0x04000000
CLONE_NEWNET :: 0x40000000

O_RDONLY :: 0

MAX_OUTPUT :: 65536 // Max bytes captured per stream (matches shell impl)

Poll_Fd :: struct {
	fd:      c.int,
	events:  c.short,
	revents: c.short,
}

POLLIN  :: c.short(0x0001)
POLLHUP :: c.short(0x0010)

// ---------------------------------------------------------------------------
// Exec request/result types
// ---------------------------------------------------------------------------

Exec_Request :: struct {
	cmd:     string,
	workdir: string,
	timeout: u64, // seconds
}

Exec_Result :: struct {
	exit_code: i32,
	stdout:    string,
	stderr:    string,
	started:   time.Time,
	finished:  time.Time,
	seq:       u32,
}

Exec_Error :: enum {
	None,
	Not_Ready,
	Pipe_Failed,
	Fork_Failed,
	Netns_Open_Failed,
}

// ---------------------------------------------------------------------------
// exec_in_sandbox — fork/unshare/chroot/execve with I/O capture
//
// Replaces common.sh:546-607.
// Child: enters cgroup, network namespace, creates new mount/PID/IPC/UTS
//        namespaces, chroots into merged overlay, execves /bin/sh -c "cmd".
// Parent: captures stdout/stderr via pipes with poll-based timeout, SIGKILL
//         on timeout, waitpid for exit status.
// ---------------------------------------------------------------------------

exec_in_sandbox :: proc(
	sandbox: ^Sandbox,
	req: Exec_Request,
	allocator := context.allocator,
) -> (Exec_Result, Exec_Error) {
	// Extract Ready state — sandbox must be in Ready state to exec
	ready, is_ready := &sandbox.state.(Ready)
	if !is_ready {
		return {}, .Not_Ready
	}

	started := time.now()

	// Create pipes for stdout and stderr capture
	stdout_fds: [2]c.int
	stderr_fds: [2]c.int
	if c_pipe(&stdout_fds[0]) != 0 {
		return {}, .Pipe_Failed
	}
	if c_pipe(&stderr_fds[0]) != 0 {
		c_close(stdout_fds[0])
		c_close(stdout_fds[1])
		return {}, .Pipe_Failed
	}

	// Open netns fd before fork (if sandbox has a network namespace)
	netns_fd: c.int = -1
	if netns, ok := ready.netns.?; ok {
		ns_path := fmt.ctprintf("/var/run/netns/%s", netns.name)
		netns_fd = c_open(ns_path, O_RDONLY, 0)
		if netns_fd < 0 {
			c_close(stdout_fds[0])
			c_close(stdout_fds[1])
			c_close(stderr_fds[0])
			c_close(stderr_fds[1])
			return {}, .Netns_Open_Failed
		}
	}

	pid := c_fork()
	if pid < 0 {
		if netns_fd >= 0 { c_close(netns_fd) }
		c_close(stdout_fds[0])
		c_close(stdout_fds[1])
		c_close(stderr_fds[0])
		c_close(stderr_fds[1])
		return {}, .Fork_Failed
	}

	if pid == 0 {
		// ═══ CHILD PROCESS ═══
		_child_exec(ready, req, stdout_fds, stderr_fds, netns_fd)
		// _child_exec never returns — it calls c_exit(127) on execve failure
	}

	// ═══ PARENT PROCESS ═══

	// Close write ends of pipes (child owns them now)
	c_close(stdout_fds[1])
	c_close(stderr_fds[1])
	if netns_fd >= 0 { c_close(netns_fd) }

	// Transition to Executing state — save Ready data and restore after waitpid
	saved_ready := ready^
	sandbox.state = Executing{pid = i32(pid), started = started}

	// Read stdout/stderr with poll-based timeout
	stdout_buf: [MAX_OUTPUT]byte
	stderr_buf: [MAX_OUTPUT]byte
	stdout_len: int = 0
	stderr_len: int = 0

	timeout_ms := i64(req.timeout) * 1000
	deadline := time.tick_now()
	timed_out := false

	stdout_eof := false
	stderr_eof := false

	for !stdout_eof || !stderr_eof {
		elapsed := time.tick_diff(deadline, time.tick_now())
		remaining := timeout_ms - i64(time.duration_milliseconds(elapsed))
		if remaining <= 0 {
			c_kill(pid, SIGKILL)
			timed_out = true
			break
		}

		poll_fds := [2]Poll_Fd{
			{fd = stdout_fds[0], events = POLLIN, revents = 0},
			{fd = stderr_fds[0], events = POLLIN, revents = 0},
		}

		// Clamp remaining to max c.int value for poll timeout
		clamped := remaining if remaining < i64(max(c.int)) else i64(max(c.int))
		poll_timeout := c.int(clamped)
		c_poll(&poll_fds[0], 2, poll_timeout)

		// Read stdout
		if poll_fds[0].revents & POLLIN != 0 && stdout_len < MAX_OUTPUT {
			n := c_read(
				stdout_fds[0],
				raw_data(stdout_buf[stdout_len:]),
				c.size_t(MAX_OUTPUT - stdout_len),
			)
			if n <= 0 {
				stdout_eof = true
			} else {
				stdout_len += int(n)
			}
		}
		if poll_fds[0].revents & POLLHUP != 0 {
			stdout_eof = true
		}

		// Read stderr
		if poll_fds[1].revents & POLLIN != 0 && stderr_len < MAX_OUTPUT {
			n := c_read(
				stderr_fds[0],
				raw_data(stderr_buf[stderr_len:]),
				c.size_t(MAX_OUTPUT - stderr_len),
			)
			if n <= 0 {
				stderr_eof = true
			} else {
				stderr_len += int(n)
			}
		}
		if poll_fds[1].revents & POLLHUP != 0 {
			stderr_eof = true
		}
	}

	// Wait for child to exit
	status: c.int
	c_waitpid(pid, &status, 0)

	// Transition back to Ready state
	sandbox.state = saved_ready

	// Close read ends
	c_close(stdout_fds[0])
	c_close(stderr_fds[0])

	// Extract exit code using waitpid status macros
	exit_code: i32
	if timed_out {
		exit_code = 124 // Same as GNU timeout
	} else if status & 0x7f == 0 {
		// WIFEXITED: normal exit
		exit_code = i32((status >> 8) & 0xff) // WEXITSTATUS
	} else {
		// Killed by signal: 128 + signal number
		exit_code = 128 + i32(status & 0x7f)
	}

	finished := time.now()

	// Write execution log and get sequence number
	seq := sandbox_next_log_seq(sandbox)
	write_exec_log(
		sandbox,
		seq,
		req,
		exit_code,
		started,
		finished,
		stdout_buf[:stdout_len],
		stderr_buf[:stderr_len],
	)

	return Exec_Result{
		exit_code = exit_code,
		stdout    = strings.clone_from_bytes(stdout_buf[:stdout_len], allocator),
		stderr    = strings.clone_from_bytes(stderr_buf[:stderr_len], allocator),
		started   = started,
		finished  = finished,
		seq       = seq,
	}, .None
}

// ---------------------------------------------------------------------------
// _child_exec — child-side logic after fork. Never returns.
//
// Sequence: redirect I/O -> enter cgroup -> enter netns -> unshare
//           mount/PID/IPC/UTS -> chroot -> chdir -> execve /bin/sh
// ---------------------------------------------------------------------------

_child_exec :: proc(
	ready: ^Ready,
	req: Exec_Request,
	stdout_fds: [2]c.int,
	stderr_fds: [2]c.int,
	netns_fd: c.int,
) {
	// Close read ends of pipes (parent owns them)
	c_close(stdout_fds[0])
	c_close(stderr_fds[0])

	// Redirect stdout/stderr to pipe write ends
	c_dup2(stdout_fds[1], STDOUT_FILENO)
	c_dup2(stderr_fds[1], STDERR_FILENO)
	c_close(stdout_fds[1])
	c_close(stderr_fds[1])

	// Enter cgroup (optional — write our PID to cgroup.procs)
	if cg, ok := &ready.cgroup.?; ok {
		cgroup_add_process(cg, i32(c_getpid()))
	}

	// Enter network namespace (if present)
	if netns_fd >= 0 {
		c_setns(netns_fd, CLONE_NEWNET)
		c_close(netns_fd)
	}

	// Create new mount/PID/IPC/UTS namespaces
	if c_unshare(CLONE_NEWNS | CLONE_NEWPID | CLONE_NEWIPC | CLONE_NEWUTS) != 0 {
		c_exit(126)
	}

	// After unshare(CLONE_NEWPID), this process remains in the old PID namespace.
	// Fork again so the inner child runs as PID 1 in the new namespace.
	inner_pid := c_fork()
	if inner_pid < 0 {
		c_exit(126)
	}
	if inner_pid > 0 {
		status: c.int
		c_waitpid(inner_pid, &status, 0)
		if status & 0x7f == 0 {
			c_exit((status >> 8) & 0xff)
		} else {
			c_exit(128 + (status & 0x7f))
		}
	}

	// chroot into the merged overlay filesystem
	merged_c := _to_cstr(ready.mounts.overlay.merged_path)
	c_chroot(merged_c)

	// Change to requested working directory
	workdir := req.workdir if len(req.workdir) > 0 else "/"
	workdir_c := _to_cstr(workdir)
	c_chdir(workdir_c)

	// execve /bin/sh -c "{cmd}"
	cmd_c := _to_cstr(req.cmd)
	argv := [4]cstring{"/bin/sh", "-c", cmd_c, nil}
	envp := [1]cstring{nil}
	c_execve("/bin/sh", &argv[0], &envp[0])

	// If execve returns, it failed
	c_exit(127)
}

// ---------------------------------------------------------------------------
// Execution logging
// ---------------------------------------------------------------------------

// Get the next log sequence number for a sandbox by counting existing log files.
sandbox_next_log_seq :: proc(sandbox: ^Sandbox) -> u32 {
	log_dir := fmt.tprintf("%s/.meta/log", sandbox.dir)

	// Ensure log directory exists
	if !os.exists(log_dir) {
		os.make_directory(log_dir)
	}

	dh, err := os.open(log_dir)
	if err != nil {
		return 1
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return 1
	}
	defer delete(entries)

	return u32(len(entries)) + 1
}

// Write a JSON log entry for an execution.
// Format matches the shell implementation:
//   {seq, cmd, workdir, exit_code, started, finished, stdout, stderr}
write_exec_log :: proc(
	sandbox: ^Sandbox,
	seq: u32,
	req: Exec_Request,
	exit_code: i32,
	started: time.Time,
	finished: time.Time,
	stdout_data: []byte,
	stderr_data: []byte,
) {
	log_dir := fmt.tprintf("%s/.meta/log", sandbox.dir)
	if !os.exists(log_dir) {
		os.make_directory(log_dir)
	}

	log_path := fmt.tprintf("%s/%04d.json", log_dir, seq)

	// Format timestamps as ISO 8601
	started_str := _format_iso8601(started)
	finished_str := _format_iso8601(finished)

	// Build JSON manually to avoid allocation overhead from json.marshal
	// and to handle byte slices directly as strings
	buf: [MAX_OUTPUT * 2 + 4096]byte
	b := strings.builder_from_bytes(buf[:])

	strings.write_string(&b, `{"seq":`)
	fmt.sbprintf(&b, "%d", seq)
	strings.write_string(&b, `,"cmd":`)
	_write_json_string(&b, req.cmd)
	strings.write_string(&b, `,"workdir":`)
	_write_json_string(&b, req.workdir if len(req.workdir) > 0 else "/")
	strings.write_string(&b, `,"exit_code":`)
	fmt.sbprintf(&b, "%d", exit_code)
	strings.write_string(&b, `,"started":`)
	_write_json_string(&b, started_str)
	strings.write_string(&b, `,"finished":`)
	_write_json_string(&b, finished_str)
	strings.write_string(&b, `,"stdout":`)
	_write_json_string(&b, string(stdout_data))
	strings.write_string(&b, `,"stderr":`)
	_write_json_string(&b, string(stderr_data))
	strings.write_string(&b, "}")

	content := strings.to_string(b)
	os.write_entire_file(log_path, transmute([]byte)content)
}

// ---------------------------------------------------------------------------
// JSON string escaping helper
// ---------------------------------------------------------------------------

_write_json_string :: proc(b: ^strings.Builder, s: string) {
	strings.write_byte(b, '"')
	for ch in s {
		switch ch {
		case '"':
			strings.write_string(b, `\"`)
		case '\\':
			strings.write_string(b, `\\`)
		case '\n':
			strings.write_string(b, `\n`)
		case '\r':
			strings.write_string(b, `\r`)
		case '\t':
			strings.write_string(b, `\t`)
		case '\b':
			strings.write_string(b, `\b`)
		case '\x0C': // form feed
			strings.write_string(b, `\f`)
		case:
			if ch < 0x20 {
				fmt.sbprintf(b, "\\u%04x", u32(ch))
			} else {
				strings.write_rune(b, ch)
			}
		}
	}
	strings.write_byte(b, '"')
}

// ---------------------------------------------------------------------------
// ISO 8601 timestamp formatting
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// exec_in_sandbox_firecracker — exec via vsock for Firecracker backend
//
// Reads CID from .meta/fc.cid, sends command through socat to the guest
// agent, parses the JSON response. Writes execution log identical to the
// chroot path for API compatibility.
// ---------------------------------------------------------------------------

exec_in_sandbox_firecracker :: proc(
	sandbox: ^Sandbox,
	req: Exec_Request,
	allocator := context.allocator,
) -> (Exec_Result, Exec_Error) {
	_, is_ready := &sandbox.state.(Ready)
	if !is_ready {
		return {}, .Not_Ready
	}

	// Read CID from metadata
	cid, cid_ok := read_fc_cid(sandbox.dir)
	if !cid_ok {
		fmt.printfln("[exec-fc] %s: failed to read CID", sandbox.id)
		return {}, .Pipe_Failed
	}

	timeout := int(req.timeout) if req.timeout > 0 else 300
	workdir := req.workdir if len(req.workdir) > 0 else "/"

	result, fc_err := firecracker_exec(cid, req.cmd, workdir, timeout, allocator)
	if fc_err != .None {
		fmt.printfln("[exec-fc] %s: vsock exec failed", sandbox.id)
		return {}, .Pipe_Failed
	}

	// Write execution log for API compatibility
	seq := sandbox_next_log_seq(sandbox)
	result.seq = seq
	write_exec_log(
		sandbox,
		seq,
		req,
		result.exit_code,
		result.started,
		result.finished,
		transmute([]byte)result.stdout,
		transmute([]byte)result.stderr,
	)

	return result, .None
}

// ---------------------------------------------------------------------------
// ISO 8601 timestamp formatting
// ---------------------------------------------------------------------------

_format_iso8601 :: proc(t: time.Time) -> string {
	y, mon, d := time.date(t)
	h, m, s := time.clock(t)
	return fmt.tprintf(
		"%04d-%02d-%02dT%02d:%02d:%02d+00:00",
		y,
		int(mon),
		d,
		h,
		m,
		s,
	)
}
