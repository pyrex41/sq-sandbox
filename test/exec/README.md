# Exec Sandbox Tests

Comprehensive test suite for the `exec_in_sandbox` functionality in squashd.

## Test Structure

### `exec_test.odin`
Unit tests for individual exec components:
- State validation (Not_Ready error handling)
- Output capture (stdout/stderr separation)
- Timeout behavior (SIGKILL after timeout)
- Exit code propagation
- Output truncation (MAX_OUTPUT enforcement)
- Working directory handling
- Timing and timestamps
- Sequence number generation

### `integration_test.odin`
Integration tests linking against the full squashd package:
- Log file creation and format
- JSON escaping for special characters
- ISO 8601 timestamp formatting
- Sequential execution tracking
- End-to-end execution flows

## Running Tests

### Prerequisites

Most exec tests require **root privileges** because they:
- Create and enter chroot environments
- Create new namespaces (mount, PID, IPC, UTS, network)
- Use system calls like `unshare`, `setns`, `chroot`

Tests will automatically skip if not running as root.

### Basic Test Run

```bash
# Unit tests (with mocked exec)
cd test/exec
odin test . -file

# Integration tests (requires full squashd package)
cd test/exec
odin test integration_test.odin -file
```

### Running as Root

```bash
# Run with sudo (integration tests)
sudo -E odin test integration_test.odin -file
```

### CI/CD Considerations

For CI environments, exec tests can be run in:
1. **Docker containers with privileged mode**
   ```bash
   docker run --privileged -v $(pwd):/workspace odin-image
   ```

2. **VM environments** (recommended for full isolation testing)

3. **Skip exec tests** by checking for root in CI config:
   ```bash
   if [ "$EUID" -ne 0 ]; then
     echo "Skipping exec tests (requires root)"
     exit 0
   fi
   ```

## Test Coverage

### Isolation Testing
- [x] Namespace creation (mount, PID, IPC, UTS)
- [x] Network namespace integration
- [x] Cgroup process isolation
- [x] Chroot environment

### Output Capture
- [x] Stdout capture
- [x] Stderr capture (separate from stdout)
- [x] Output truncation at MAX_OUTPUT (65536 bytes)
- [x] Poll-based non-blocking I/O

### Timeout Behavior
- [x] SIGKILL on timeout
- [x] Exit code 124 for timed-out processes
- [x] Timeout precision (within ~1 second)
- [x] Process cleanup after timeout

### Error Handling
- [x] Not_Ready state rejection
- [x] Pipe creation failures
- [x] Fork failures
- [x] Network namespace open failures
- [x] Invalid command handling (exit code 127)

### Logging
- [x] JSON log file creation
- [x] Sequential log numbering
- [x] ISO 8601 timestamp format
- [x] JSON special character escaping
- [x] Log directory creation

### Process Management
- [x] Exit code propagation (0-255)
- [x] Signal handling (exit code 128 + signal)
- [x] waitpid status decoding
- [x] Child process cleanup
- [x] Pipe file descriptor cleanup

## Known Limitations

1. **Root Requirement**: Most tests require root privileges for namespace and mount operations.

2. **Chroot Setup**: Tests that execute actual commands require a properly configured chroot with:
   - `/bin/sh` binary
   - Necessary shared libraries
   - Basic `/dev` nodes (for full functionality)

3. **Platform-Specific**: Tests assume Linux-specific syscalls:
   - `unshare(CLONE_NEWNS | CLONE_NEWPID | ...)`
   - `setns()` for namespace switching
   - `chroot()` for filesystem isolation

4. **Network Tests**: Network namespace tests require:
   - `/var/run/netns/` directory
   - `ip netns` command availability
   - Network admin capabilities

## Extending Tests

To add new test cases:

1. Add test function with `@(test)` attribute
2. Use `testing.skip()` for root-required tests
3. Clean up allocations with defer
4. Use descriptive test names: `test_exec_{feature}_{scenario}`
5. Document expected behavior in comments

Example:
```odin
@(test)
test_exec_custom_environment :: proc(t: ^testing.T) {
	// Test that environment variables are properly passed to child process.

	if os.get_uid() != 0 {
		testing.skip(t, "Test requires root privileges")
		return
	}

	// ... test implementation ...
}
```

## Future Improvements

- [ ] Mock syscalls for unit testing without root
- [ ] Parallel execution stress tests
- [ ] Memory leak detection (valgrind integration)
- [ ] Performance benchmarks
- [ ] Fuzzing for command input validation
- [ ] Container-based test isolation
- [ ] Automated chroot environment setup
