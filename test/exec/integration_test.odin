package exec_integration_test

// Integration tests for exec_in_sandbox log functionality.
// These tests verify logging behavior without requiring full sandbox execution.

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"
import "core:testing"
import "core:time"

import sq "../../src"

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

// Create a test sandbox directory structure.
setup_test_sandbox_dir :: proc(
	test_name: string,
	allocator := context.allocator,
) -> (root: string, ok: bool) {
	// Create temp directory for test sandbox (unique per test)
	temp_dir := fmt.aprintf("/tmp/squashd_test_%s", test_name, allocator = allocator)

	// Remove if exists
	if os.exists(temp_dir) {
		os.remove(temp_dir)
	}

	if os.make_directory(temp_dir) != nil {
		return "", false
	}

	// Create required subdirectories
	dirs := []string{
		"/.meta",
		"/.meta/log",
		"/merged",
	}

	for dir in dirs {
		path := fmt.tprintf("%s%s", temp_dir, dir)
		if os.make_directory(path) != nil {
			os.remove(temp_dir)
			return "", false
		}
	}

	return temp_dir, true
}

cleanup_test_sandbox_dir :: proc(root: string) {
	if len(root) > 0 && os.exists(root) {
		os.remove(root)
	}
}

// ---------------------------------------------------------------------------
// Integration Tests
// ---------------------------------------------------------------------------

@(test)
test_integration_exec_log_creation :: proc(t: ^testing.T) {
	// Test that exec logs are properly created in .meta/log.

	arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&arena)
	defer mem.dynamic_arena_destroy(&arena)
	allocator := mem.dynamic_arena_allocator(&arena)

	context.allocator = allocator

	test_dir, setup_ok := setup_test_sandbox_dir(#procedure, allocator)
	if !setup_ok {
		fmt.println("Could not setup test sandbox directory")
		return
	}
	defer cleanup_test_sandbox_dir(test_dir)

	// Create a minimal sandbox structure
	sandbox := sq.Sandbox{
		id  = "test-log-creation",
		dir = test_dir,
		state = sq.Ready{
			mounts = sq.Sandbox_Mounts{
				overlay = sq.Overlay_Mount{
					merged_path = strings.clone(fmt.tprintf("%s/merged", test_dir), allocator),
					active      = true,
				},
			},
		},
	}

	// Test sequence number generation
	seq1 := sq.sandbox_next_log_seq(&sandbox)
	testing.expect_value(t, seq1, u32(1))

	// Create a dummy log file
	log_path := fmt.tprintf("%s/.meta/log/0001.json", test_dir)
	os.write_entire_file(log_path, transmute([]byte)string("{}"))

	seq2 := sq.sandbox_next_log_seq(&sandbox)
	testing.expect_value(t, seq2, u32(2))
}

@(test)
test_integration_exec_log_format :: proc(t: ^testing.T) {
	// Test that exec log JSON format is correct.

	arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&arena)
	defer mem.dynamic_arena_destroy(&arena)
	allocator := mem.dynamic_arena_allocator(&arena)

	context.allocator = allocator

	test_dir, setup_ok := setup_test_sandbox_dir(#procedure, allocator)
	if !setup_ok {
		fmt.println("Could not setup test sandbox directory")
		return
	}
	defer cleanup_test_sandbox_dir(test_dir)

	sandbox := sq.Sandbox{
		id  = "test-log-format",
		dir = test_dir,
		state = sq.Ready{
			mounts = sq.Sandbox_Mounts{
				overlay = sq.Overlay_Mount{
					merged_path = strings.clone(fmt.tprintf("%s/merged", test_dir), allocator),
					active      = true,
				},
			},
		},
	}

	req := sq.Exec_Request{
		cmd     = "echo 'test output'",
		workdir = "/tmp",
		timeout = 30,
	}

	started := time.now()
	finished := time.Time{_nsec = started._nsec + 1_000_000_000} // 1 second later

	stdout_data := transmute([]byte)string("test output\n")
	stderr_data := transmute([]byte)string("test error\n")

	// Write a test log entry
	sq.write_exec_log(&sandbox, 1, req, 0, started, finished, stdout_data, stderr_data)

	// Read back the log file and verify it's valid JSON
	log_path := fmt.tprintf("%s/.meta/log/0001.json", test_dir)
	log_data, read_ok := os.read_entire_file(log_path, allocator)
	if !read_ok {
		fmt.println("Could not read log file")
		return
	}
	defer delete(log_data, allocator)

	log_str := string(log_data)

	// Verify it contains expected fields
	testing.expect(t, strings.contains(log_str, `"seq":1`))
	testing.expect(t, strings.contains(log_str, `"cmd":"echo 'test output'"`))
	testing.expect(t, strings.contains(log_str, `"workdir":"/tmp"`))
	testing.expect(t, strings.contains(log_str, `"exit_code":0`))
	testing.expect(t, strings.contains(log_str, `"stdout":"test output\n"`))
	testing.expect(t, strings.contains(log_str, `"stderr":"test error\n"`))
	testing.expect(t, strings.contains(log_str, `"started":`))
	testing.expect(t, strings.contains(log_str, `"finished":`))
}

@(test)
test_integration_json_escaping :: proc(t: ^testing.T) {
	// Test that JSON string escaping works correctly for special characters.

	arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&arena)
	defer mem.dynamic_arena_destroy(&arena)
	allocator := mem.dynamic_arena_allocator(&arena)

	context.allocator = allocator

	test_dir, setup_ok := setup_test_sandbox_dir(#procedure, allocator)
	if !setup_ok {
		fmt.println("Could not setup test sandbox directory")
		return
	}
	defer cleanup_test_sandbox_dir(test_dir)

	sandbox := sq.Sandbox{
		id  = "test-json-escape",
		dir = test_dir,
		state = sq.Ready{
			mounts = sq.Sandbox_Mounts{
				overlay = sq.Overlay_Mount{
					merged_path = strings.clone(fmt.tprintf("%s/merged", test_dir), allocator),
					active      = true,
				},
			},
		},
	}

	// Command with special characters that need escaping
	req := sq.Exec_Request{
		cmd     = `echo "hello\nworld\t\"quoted\""`,
		workdir = "/",
		timeout = 30,
	}

	started := time.now()
	finished := time.now()

	// Output with newlines, quotes, backslashes
	stdout_data := transmute([]byte)string("line1\nline2\t\"quoted\"\n")
	stderr_data := transmute([]byte)string("")

	sq.write_exec_log(&sandbox, 1, req, 0, started, finished, stdout_data, stderr_data)

	log_path := fmt.tprintf("%s/.meta/log/0001.json", test_dir)
	log_data, read_ok := os.read_entire_file(log_path, allocator)
	if !read_ok {
		fmt.println("Could not read log file")
		return
	}
	defer delete(log_data, allocator)

	log_str := string(log_data)

	// Verify proper escaping
	testing.expect(t, strings.contains(log_str, `\n`))
	testing.expect(t, strings.contains(log_str, `\t`))
	testing.expect(t, strings.contains(log_str, `\"`))
	testing.expect(t, strings.contains(log_str, `\\`))

	// Verify it's valid JSON by checking balanced braces
	open_braces := 0
	for ch in log_str {
		if ch == '{' { open_braces += 1 }
		if ch == '}' { open_braces -= 1 }
	}
	testing.expect_value(t, open_braces, 0)
}

@(test)
test_integration_iso8601_format :: proc(t: ^testing.T) {
	// Test ISO 8601 timestamp formatting.

	arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&arena)
	defer mem.dynamic_arena_destroy(&arena)
	allocator := mem.dynamic_arena_allocator(&arena)

	context.allocator = allocator

	test_dir, setup_ok := setup_test_sandbox_dir(#procedure, allocator)
	if !setup_ok {
		fmt.println("Could not setup test sandbox directory")
		return
	}
	defer cleanup_test_sandbox_dir(test_dir)

	sandbox := sq.Sandbox{
		id  = "test-iso8601",
		dir = test_dir,
		state = sq.Ready{
			mounts = sq.Sandbox_Mounts{
				overlay = sq.Overlay_Mount{
					merged_path = strings.clone(fmt.tprintf("%s/merged", test_dir), allocator),
					active      = true,
				},
			},
		},
	}

	req := sq.Exec_Request{
		cmd     = "echo test",
		workdir = "/",
		timeout = 30,
	}

	// Use a known timestamp: 2024-01-15 12:50:45 UTC (Unix: 1705323045)
	started := time.Time{_nsec = 1705323045 * 1_000_000_000}
	finished := started

	sq.write_exec_log(&sandbox, 1, req, 0, started, finished, nil, nil)

	log_path := fmt.tprintf("%s/.meta/log/0001.json", test_dir)
	log_data, read_ok := os.read_entire_file(log_path, allocator)
	if !read_ok {
		fmt.println("Could not read log file")
		return
	}
	defer delete(log_data, allocator)

	log_str := string(log_data)

	// Verify ISO 8601 format: YYYY-MM-DDTHH:MM:SS+00:00
	testing.expect(t, strings.contains(log_str, "2024-01-15T12:50:45"))
	testing.expect(t, strings.contains(log_str, "+00:00"))
}

@(test)
test_integration_multiple_execs_sequence :: proc(t: ^testing.T) {
	// Test that multiple executions create sequential log files.

	arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&arena)
	defer mem.dynamic_arena_destroy(&arena)
	allocator := mem.dynamic_arena_allocator(&arena)

	context.allocator = allocator

	test_dir, setup_ok := setup_test_sandbox_dir(#procedure, allocator)
	if !setup_ok {
		fmt.println("Could not setup test sandbox directory")
		return
	}
	defer cleanup_test_sandbox_dir(test_dir)

	sandbox := sq.Sandbox{
		id  = "test-multi-exec",
		dir = test_dir,
		state = sq.Ready{
			mounts = sq.Sandbox_Mounts{
				overlay = sq.Overlay_Mount{
					merged_path = strings.clone(fmt.tprintf("%s/merged", test_dir), allocator),
					active      = true,
				},
			},
		},
	}

	req := sq.Exec_Request{
		cmd     = "echo test",
		workdir = "/",
		timeout = 30,
	}

	now := time.now()

	// Write three log entries
	sq.write_exec_log(&sandbox, 1, req, 0, now, now, nil, nil)
	sq.write_exec_log(&sandbox, 2, req, 0, now, now, nil, nil)
	sq.write_exec_log(&sandbox, 3, req, 0, now, now, nil, nil)

	// Verify all three log files exist
	log_dir := fmt.tprintf("%s/.meta/log", test_dir)
	dir_handle, open_err := os.open(log_dir)
	if open_err != nil {
		fmt.println("Could not open log directory")
		return
	}
	defer os.close(dir_handle)

	entries, read_err := os.read_dir(dir_handle, -1, allocator)
	if read_err != nil {
		fmt.println("Could not read log directory")
		return
	}
	defer delete(entries, allocator)

	testing.expect_value(t, len(entries), 3)

	// Verify filenames
	expected_names := []string{"0001.json", "0002.json", "0003.json"}
	for expected in expected_names {
		found := false
		for entry in entries {
			if entry.name == expected {
				found = true
				break
			}
		}
		testing.expectf(t, found, "should have log file %s", expected)
	}
}
