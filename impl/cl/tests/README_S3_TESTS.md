# S3 Tests - Quick Start

## Running the Tests

### Option 1: Quick Run (Summary Only)
```bash
./run-s3-tests.lisp
```

Output:
```
Loading squashd system...
Loading squashd-tests system...

═══════════════════════════════════════════════════════════
  Running S3 Tests
═══════════════════════════════════════════════════════════

Running test suite SQUASHD-S3-TESTS

═══════════════════════════════════════════════════════════
  Test Summary
═══════════════════════════════════════════════════════════

✓ All S3 tests passed!
```

### Option 2: Verbose Run (Detailed Output)
```bash
./run-s3-tests-verbose.lisp
```

Output shows each test as it runs:
```
 Running test FORMAT-DATETIME-SIGV4-FORMAT ....
 Running test SHA256-STRING-KNOWN-VALUE .
 Running test SHA256-OCTETS-KNOWN-VALUE .
 ...
 Did 56 checks.
    Pass: 56 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

### Option 3: From REPL
```lisp
(ql:quickload :squashd-tests)
(fiveam:run! 'squashd-s3-tests::squashd-s3-tests)
```

### Option 4: Via ASDF Test Operation
```lisp
(asdf:test-system :squashd-tests)
```

## Test Coverage

**Total**: 28 test cases, 56 checks

### Categories
- Helper Functions (7 tests): SHA256, HMAC, datetime formatting
- URL Builders (6 tests): AWS and custom endpoint URL construction
- SigV4 Signing (4 tests): AWS authentication signature generation
- Atomic Operations (2 tests): Temp file → rename pattern
- Module Integration (2 tests): Module path construction, error handling
- Background Threading (1 test): Thread creation verification
- Client Structure (2 tests): Struct field validation
- Edge Cases (4 tests): Unicode, empty strings, prefix variations

## What's Tested

✅ **Cryptographic correctness** - SHA256 and HMAC-SHA256 against known test vectors
✅ **SigV4 authentication** - AWS signature format, determinism, method sensitivity
✅ **URL construction** - AWS virtual-hosted style vs custom endpoint path-style
✅ **Atomic file operations** - Safe .s3tmp → rename pattern
✅ **Module directory integration** - Path formatting and error signaling
✅ **Error handling** - Condition types and signaling
✅ **Edge cases** - Unicode, empty inputs, prefix variations

## What's NOT Tested (Requires Integration Tests)

❌ Actual HTTP requests to S3
❌ Network error handling and retries
❌ Background thread execution and error propagation
❌ Real .squashfs file upload/download
❌ S3-compatible service compatibility (MinIO, R2, B2)

These require:
- HTTP mocking library
- Test S3 server (MinIO instance)
- Thread synchronization test harness

## Files

- `s3-test-package.lisp` - Package definition and test suite setup
- `s3-test.lisp` - All 28 test case implementations
- `S3_TEST_SUMMARY.md` - Detailed test documentation
- `README_S3_TESTS.md` - This file

## Prerequisites

The test scripts will auto-install via Quicklisp:
- fiveam (testing framework)
- ironclad (cryptography)
- babel (string/octet conversion)
- cl-ppcre (regex)
- alexandria (utilities)
- bordeaux-threads (threading)

## Troubleshooting

**"Component squashd not found"**
- Make sure you're running from the project root directory
- Check that `squashd.asd` exists in the current directory

**"Error loading dependencies"**
- Install Quicklisp: https://www.quicklisp.org/
- Or manually install dependencies via your CL package manager

**Tests don't run**
- Check that `(in-suite squashd-s3-tests)` appears in `s3-test.lisp` after package declaration
- Verify test suite exists: `(fiveam:get-test 'squashd-s3-tests::squashd-s3-tests)`

## Next Steps

For integration testing:
1. Set up MinIO test server
2. Add HTTP mocking with `cl-mock` or similar
3. Create thread synchronization test fixtures
4. Add actual .squashfs file creation in tests
5. Test with real AWS credentials (dev account)
