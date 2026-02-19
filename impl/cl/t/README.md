# SCUD Task 6.2 - Sandbox Execution Tests

This directory contains tests for sandbox command execution and output capture functionality.

## Overview

The test suite validates the `exec-in-sandbox` functionality from `src/exec.lisp`, which:
- Executes commands inside sandboxes via fork/execve
- Captures stdout/stderr separately
- Enforces timeout limits (killing with SIGKILL → exit code 124)
- Enforces namespace isolation (netns, cgroup, chroot)
- Logs all executions to `.meta/log/exec-<seq>.json`

## Running Tests

### Quick test (basic functionality only):
```bash
./quick-test.lisp
```

### Full test suite with FiveAM:
```bash
./run-exec-tests.lisp
```

### Manual REPL testing:
```lisp
(ql:quickload "squashd-test")
(in-package :squashd-test)
(run-tests)
```

## Test Structure

### Unit Tests (Currently Implemented)
These tests run immediately without requiring a full sandbox:

1. **get-unix-time-test** - Validates Unix timestamp generation
2. **get-monotonic-ms-test** - Validates monotonic clock for timeouts
3. **exec-result-struct-test** - Tests exec-result structure and accessors
4. **make-pipe-test** - Tests pipe creation via CFFI
5. **close-fd-safe-test** - Tests safe file descriptor closing
6. **pipe-write-read-test** - Tests actual pipe I/O
7. **constants-test** - Verifies exec constants are defined correctly

### Integration Tests (Spec Only - Awaiting Task 5)
These tests are documented as specifications and will be fully implemented once task 5 (sandbox creation) is complete:

1. **exec-simple-command-spec** - Basic command execution
2. **exec-exit-codes-spec** - Exit code propagation (0, 1, 42, etc.)
3. **exec-stdout-stderr-separation-spec** - Output stream separation
4. **exec-timeout-kills-process-spec** - Timeout enforcement (exit code 124)
5. **exec-timeout-boundary-spec** - Edge case: commands completing just before timeout
6. **exec-signal-exit-codes-spec** - Signal handling (128 + signal number)
7. **exec-large-output-spec** - Output truncation at 65536 bytes
8. **exec-namespace-isolation-spec** - Network namespace isolation
9. **exec-cgroup-enforcement-spec** - Cgroup process placement
10. **exec-chroot-isolation-spec** - Filesystem isolation via chroot
11. **exec-workdir-spec** - Working directory enforcement
12. **exec-concurrent-spec** - Concurrent execution in same sandbox
13. **exec-log-sequence-spec** - Sequence number incrementing
14. **exec-after-sandbox-destroy-spec** - Graceful failure on destroyed sandbox
15. **exec-binary-output-spec** - Binary output handling
16. **exec-empty-command-spec** - Edge case: empty commands
17. **exec-shell-metacharacters-spec** - Shell metacharacter support (\"|, &&, $VAR\")
18. **write-exec-log-spec** - JSON log file writing

## Test Results

All currently implemented tests pass:

```
Running test suite EXEC-TESTS
 Running test GET-UNIX-TIME-TEST ...
 Running test GET-MONOTONIC-MS-TEST ..
 Running test EXEC-RESULT-STRUCT-TEST ......
 Running test MAKE-PIPE-TEST .....
 Running test CLOSE-FD-SAFE-TEST ...
 Running test PIPE-WRITE-READ-TEST ...
 Running test CONSTANTS-TEST ....

✓ All tests passed!
```

Integration tests are marked as "skipped" (awaiting sandbox struct from task 5).

## Test Coverage

### What is Tested
- ✅ Helper functions (get-unix-time, get-monotonic-ms)
- ✅ Data structures (exec-result)
- ✅ Low-level operations (pipe creation, fd management)
- ✅ Constants and basic validation
- ✅ Comprehensive spec documentation for all integration scenarios

### What Requires Full Integration Testing
(These specs will be implemented when task 5 is complete):
- 🔲 Actual command execution in sandbox
- 🔲 Output capture (stdout/stderr)
- 🔲 Exit code propagation
- 🔲 Timeout enforcement
- 🔲 Namespace isolation
- 🔲 Exec log writing

## Running in Privileged Container

Full integration tests require:
1. Docker container with `--privileged` flag
2. Sandbox struct implementation (task 5)
3. Real filesystem mounts (squashfs + overlay)

Example:
```bash
docker run --rm -it --privileged \
  -v $(pwd):/app \
  -w /app \
  alpine:3.21 sh

# Inside container:
apk add sbcl
./run-exec-tests.lisp
```

## Next Steps

Once task 5 (sandbox creation) is complete:
1. Implement the integration test helpers to create real sandboxes
2. Convert all `-spec` tests to full implementations
3. Run tests in privileged container
4. Verify namespace isolation, timeout enforcement, and log writing

## Files

- `packages.lisp` - Test package definition
- `test-exec.lisp` - All exec tests (unit + integration specs)
- `../run-exec-tests.lisp` - Test runner script
- `../quick-test.lisp` - Quick smoke test
- `../squashd-test.asd` - Test system definition

## Dependencies

- **fiveam** - Unit testing framework
- **jonathan** - JSON parsing (for log validation)
- **alexandria** - Utilities (file reading)
- All squashd dependencies (cffi, etc.)
