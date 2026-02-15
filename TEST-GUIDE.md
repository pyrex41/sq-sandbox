# Quick Test Guide for SCUD Task 6.2

## Run Tests

```bash
# Fast smoke test (< 1 second)
./quick-test.lisp

# Full test suite with FiveAM
./run-exec-tests.lisp
```

## Test Files

- `t/test-exec.lisp` - Main test suite (625 lines)
  - 7 unit tests (all passing)
  - 18 integration test specs (awaiting task 5)

- `t/packages.lisp` - Test package setup
- `squashd-test.asd` - Test system definition
- `run-exec-tests.lisp` - Test runner
- `quick-test.lisp` - Smoke test

## What's Tested

### ✅ Unit Tests (Passing Now)
- Time functions (unix time, monotonic clock)
- exec-result struct
- Pipe creation and I/O
- File descriptor management
- Constants validation

### 📋 Integration Specs (Task 5 Required)
- Command execution in sandbox
- stdout/stderr capture
- Exit code propagation
- Timeout enforcement (exit code 124)
- Namespace isolation
- Exec log writing

## Test Output

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

## Next Steps

After task 5 (sandbox creation):
1. Implement integration test helpers
2. Convert specs to full tests
3. Run in privileged container
4. Verify all features work

See `t/README.md` for full documentation.
