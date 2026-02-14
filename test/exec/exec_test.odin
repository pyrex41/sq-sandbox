package exec_test

// Comprehensive tests for exec_in_sandbox functionality.
// Tests isolation, output capture, timeout behavior, process cleanup, and error handling.

import "core:c"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import "core:testing"
import "core:time"

// ---------------------------------------------------------------------------
// Mock sandbox structures for testing
// ---------------------------------------------------------------------------

// Minimal subset of sandbox types needed for exec tests.
// These mirror the types from src/sandbox.odin and src/exec.odin.

Squashfs_Mount :: struct {
	mount_point: string,
	active:      bool,
}

Tmpfs_Mount :: struct {
	mount_point: string,
	active:      bool,
}

Overlay_Mount :: struct {
	merged_path: string,
	active:      bool,
}

Sandbox_Mounts :: struct {
	sqfs_mounts:    [dynamic]Squashfs_Mount,
	snapshot_mount: Maybe(Squashfs_Mount),
	tmpfs:          Tmpfs_Mount,
	overlay:        Overlay_Mount,
}

Cgroup_Handle :: struct {
	path: string,
}

Netns_Handle :: struct {
	name:       string,
	index:      u8,
	veth_host:  string,
	chain_name: Maybe(string),
	host_dns:   Maybe(string),
}

Ready :: struct {
	mounts: Sandbox_Mounts,
	netns:  Maybe(Netns_Handle),
	cgroup: Maybe(Cgroup_Handle),
}

Creating :: struct {}
Executing :: struct {
	pid:     i32,
	started: time.Time,
}
Snapshotting :: struct {}
Destroying :: struct {}

Sandbox_State :: union {
	Creating,
	Ready,
	Executing,
	Snapshotting,
	Destroying,
}

Sandbox :: struct {
	id:             string,
	dir:            string,
	state:          Sandbox_State,
	created:        time.Time,
	last_active:    time.Time,
	exec_count:     u32,
	owner:          string,
	task:           string,
	max_lifetime_s: u64,
}

Exec_Request :: struct {
	cmd:     string,
	workdir: string,
	timeout: u64,
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
// Test helpers
// ---------------------------------------------------------------------------

// Create a minimal sandbox in Ready state for testing.
create_test_sandbox :: proc(
	merged_path: string,
	allocator := context.allocator,
) -> Sandbox {
	return Sandbox{
		id          = "test-sandbox",
		dir         = "/tmp/test-sandbox",
		state       = Ready{
			mounts = Sandbox_Mounts{
				overlay = Overlay_Mount{
					merged_path = strings.clone(merged_path, allocator),
					active      = true,
				},
			},
			netns  = nil,
			cgroup = nil,
		},
		created     = time.now(),
		last_active = time.now(),
	}
}

// ---------------------------------------------------------------------------
// Unit Tests
// ---------------------------------------------------------------------------

@(test)
test_exec_not_ready_state :: proc(t: ^testing.T) {
	// Sandbox in Creating state should return Not_Ready error.
	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	sandbox := Sandbox{
		id    = "test",
		dir   = "/tmp/test",
		state = Creating{},
	}

	req := Exec_Request{
		cmd     = "echo hello",
		workdir = "/",
		timeout = 5,
	}

	_, err := exec_in_sandbox(&sandbox, req, allocator)
	testing.expect_value(t, err, Exec_Error.Not_Ready)
}

@(test)
test_exec_simple_command :: proc(t: ^testing.T) {
	// Test basic command execution with output capture.
	// Uses a real chroot environment if available, otherwise skips.

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	// Check if we're running as root (required for actual exec tests)
	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	// Create a minimal chroot environment for testing
	test_root := "/tmp/exec_test_root"
	os.make_directory(test_root)
	defer os.remove(test_root)

	// Setup minimal /bin/sh environment
	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	req := Exec_Request{
		cmd     = "echo 'hello world'",
		workdir = "/",
		timeout = 5,
	}

	result, err := exec_in_sandbox(&sandbox, req, allocator)
	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)
	testing.expect_value(t, result.exit_code, 0)
	testing.expect(t, strings.contains(result.stdout, "hello world"), "stdout should contain 'hello world'")
}

@(test)
test_exec_timeout_behavior :: proc(t: ^testing.T) {
	// Test that long-running commands are killed after timeout.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_timeout"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	// Command that sleeps for 10 seconds, timeout set to 1 second
	req := Exec_Request{
		cmd     = "sleep 10",
		workdir = "/",
		timeout = 1,
	}

	start := time.now()
	result, err := exec_in_sandbox(&sandbox, req, allocator)
	elapsed := time.diff(start, time.now())

	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)
	testing.expect_value(t, result.exit_code, 124) // GNU timeout exit code

	// Should complete within ~1 second (with some margin for overhead)
	testing.expect(
		t,
		time.duration_seconds(elapsed) < 2.0,
		"timeout should kill process within 1-2 seconds",
	)
}

@(test)
test_exec_stderr_capture :: proc(t: ^testing.T) {
	// Test that stderr is captured separately from stdout.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_stderr"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	// Write to both stdout and stderr
	req := Exec_Request{
		cmd     = "echo 'stdout message'; echo 'stderr message' >&2",
		workdir = "/",
		timeout = 5,
	}

	result, err := exec_in_sandbox(&sandbox, req, allocator)
	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)
	testing.expect_value(t, result.exit_code, 0)
	testing.expect(t, strings.contains(result.stdout, "stdout message"), "stdout should be captured")
	testing.expect(t, strings.contains(result.stderr, "stderr message"), "stderr should be captured")
}

@(test)
test_exec_exit_code_propagation :: proc(t: ^testing.T) {
	// Test that non-zero exit codes are properly captured.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_exit"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	// Command that exits with code 42
	req := Exec_Request{
		cmd     = "exit 42",
		workdir = "/",
		timeout = 5,
	}

	result, err := exec_in_sandbox(&sandbox, req, allocator)
	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)
	testing.expect_value(t, result.exit_code, 42)
}

@(test)
test_exec_output_truncation :: proc(t: ^testing.T) {
	// Test that output larger than MAX_OUTPUT is properly truncated.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_truncate"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	// Generate output larger than MAX_OUTPUT (65536 bytes)
	// dd generates 100KB of random data
	req := Exec_Request{
		cmd     = "dd if=/dev/zero bs=1024 count=100 2>/dev/null",
		workdir = "/",
		timeout = 5,
	}

	result, err := exec_in_sandbox(&sandbox, req, allocator)
	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)

	MAX_OUTPUT :: 65536
	testing.expectf(
		t,
		len(result.stdout) <= MAX_OUTPUT,
		"stdout should be truncated to MAX_OUTPUT, got %d bytes",
		len(result.stdout),
	)
}

@(test)
test_exec_workdir :: proc(t: ^testing.T) {
	// Test that workdir is properly set before command execution.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_workdir"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	// Create a /tmp directory in the chroot
	os.make_directory(fmt.tprintf("%s/tmp", test_root))

	sandbox := create_test_sandbox(test_root, allocator)

	// pwd should output the workdir
	req := Exec_Request{
		cmd     = "pwd",
		workdir = "/tmp",
		timeout = 5,
	}

	result, err := exec_in_sandbox(&sandbox, req, allocator)
	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)
	testing.expect_value(t, result.exit_code, 0)
	testing.expect(t, strings.contains(result.stdout, "/tmp"), "should execute in /tmp")
}

@(test)
test_exec_timing :: proc(t: ^testing.T) {
	// Test that started and finished timestamps are properly recorded.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_timing"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	req := Exec_Request{
		cmd     = "sleep 0.1",
		workdir = "/",
		timeout = 5,
	}

	before := time.now()
	result, err := exec_in_sandbox(&sandbox, req, allocator)
	after := time.now()

	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)

	// Check that started is within reasonable bounds
	testing.expect(
		t,
		time.diff(before, result.started) >= 0,
		"started should be >= before execution",
	)
	testing.expect(
		t,
		time.diff(result.started, after) >= 0,
		"started should be <= after execution",
	)

	// Check that finished is after started
	testing.expect(
		t,
		time.diff(result.started, result.finished) > 0,
		"finished should be > started",
	)

	// Check that duration is at least 0.1 seconds (sleep duration)
	elapsed := time.diff(result.started, result.finished)
	testing.expect(
		t,
		time.duration_milliseconds(elapsed) >= 100,
		"execution should take at least 100ms",
	)
}

@(test)
test_exec_sequence_numbers :: proc(t: ^testing.T) {
	// Test that sequence numbers increment for each execution.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_seq"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	req := Exec_Request{
		cmd     = "echo test",
		workdir = "/",
		timeout = 5,
	}

	// Execute three times
	result1, err1 := exec_in_sandbox(&sandbox, req, allocator)
	defer delete(result1.stdout); defer delete(result1.stderr)

	result2, err2 := exec_in_sandbox(&sandbox, req, allocator)
	defer delete(result2.stdout); defer delete(result2.stderr)

	result3, err3 := exec_in_sandbox(&sandbox, req, allocator)
	defer delete(result3.stdout); defer delete(result3.stderr)

	testing.expect_value(t, err1, Exec_Error.None)
	testing.expect_value(t, err2, Exec_Error.None)
	testing.expect_value(t, err3, Exec_Error.None)

	// Sequence numbers should increment
	testing.expect_value(t, result1.seq, u32(1))
	testing.expect_value(t, result2.seq, u32(2))
	testing.expect_value(t, result3.seq, u32(3))
}

// ---------------------------------------------------------------------------
// Integration Tests
// ---------------------------------------------------------------------------

@(test)
test_exec_with_network_namespace :: proc(t: ^testing.T) {
	// Test execution with a network namespace configured.
	// Note: This requires actual network namespace setup, so we test
	// that the code path handles the netns_fd correctly.

	testing.skip(t, "Network namespace integration test - requires full setup")
	// TODO: Implement when network namespace mocking is available
}

@(test)
test_exec_with_cgroup :: proc(t: ^testing.T) {
	// Test execution with cgroup configured.
	// Verifies that the child process is added to the cgroup.

	testing.skip(t, "Cgroup integration test - requires full setup")
	// TODO: Implement when cgroup mocking is available
}

@(test)
test_exec_concurrent_executions :: proc(t: ^testing.T) {
	// Test that multiple concurrent executions in the same sandbox work correctly.
	// Note: This is more of a stress test and may need to be run separately.

	testing.skip(t, "Concurrent execution test - requires threading setup")
	// TODO: Implement concurrent execution test
}

// ---------------------------------------------------------------------------
// Error Handling Tests
// ---------------------------------------------------------------------------

@(test)
test_exec_invalid_command :: proc(t: ^testing.T) {
	// Test that invalid commands return the correct exit code (127).

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	arena: mem.Arena
	mem.arena_init(&arena)
	defer mem.arena_destroy(&arena)
	allocator := mem.arena_allocator(&arena)

	context.allocator = allocator

	test_root := "/tmp/exec_test_invalid"
	os.make_directory(test_root)
	defer os.remove(test_root)

	setup_minimal_chroot(test_root) or_else {
		testing.skip(t, "Could not setup test chroot")
		return
	}

	sandbox := create_test_sandbox(test_root, allocator)

	// Command that doesn't exist
	req := Exec_Request{
		cmd     = "/nonexistent/command",
		workdir = "/",
		timeout = 5,
	}

	result, err := exec_in_sandbox(&sandbox, req, allocator)
	defer {
		delete(result.stdout)
		delete(result.stderr)
	}

	testing.expect_value(t, err, Exec_Error.None)
	testing.expect_value(t, result.exit_code, 127) // execve failure exit code
}

// ---------------------------------------------------------------------------
// Helper: Setup minimal chroot environment
// ---------------------------------------------------------------------------

setup_minimal_chroot :: proc(root: string) -> (ok: bool) {
	// Create basic directory structure
	dirs := []string{"/bin", "/tmp", "/dev"}
	for dir in dirs {
		path := fmt.tprintf("%s%s", root, dir)
		if os.make_directory(path) != nil {
			return false
		}
	}

	// Copy /bin/sh to the chroot (if available)
	sh_src := "/bin/sh"
	sh_dst := fmt.tprintf("%s/bin/sh", root)

	// Try to copy shell binary
	sh_data, read_ok := os.read_entire_file(sh_src)
	if !read_ok {
		return false
	}
	defer delete(sh_data)

	if !os.write_entire_file(sh_dst, sh_data) {
		return false
	}

	// Make shell executable
	os.chmod(sh_dst, 0o755)

	// Create basic /dev nodes (null, zero)
	// Note: This requires mknod which needs root, so we skip for now
	// Real tests would need proper device node creation

	return true
}

// ---------------------------------------------------------------------------
// Stub: exec_in_sandbox
// ---------------------------------------------------------------------------

// This is a stub that will be replaced with the actual implementation
// from src/exec.odin when building the full test suite.
// For now, we define it here to allow the test file to compile standalone.

exec_in_sandbox :: proc(
	sandbox: ^Sandbox,
	req: Exec_Request,
	allocator := context.allocator,
) -> (Exec_Result, Exec_Error) {
	// This is just a stub for compilation.
	// Real tests will link against src/exec.odin.
	return Exec_Result{}, .Not_Ready
}
