# Squashd Test Suite

This directory contains the test suite for the squashd container runtime, focusing on testing CFFI syscall wrappers.

## Requirements

### System Requirements
- **CAP_SYS_ADMIN capability**: Many tests require privileged operations (mount, unmount, etc.)
- **Linux kernel**: Tests use Linux-specific syscalls
- **FiveAM testing framework**: Install via Quicklisp

### Dependencies
```lisp
(ql:quickload :fiveam)
(ql:quickload :squashd-tests)
```

## Running Tests

### Option 1: Inside Privileged Container (Recommended)

The safest way to run these tests is inside a privileged Docker container:

```bash
# Build the squashd container
docker-compose build

# Run tests in privileged mode
docker run --rm --privileged \
  -v $(pwd):/app \
  -w /app \
  squashd-sandbox:latest \
  sbcl --eval '(ql:quickload :squashd-tests)' \
       --eval '(fiveam:run! :squashd-tests)' \
       --quit
```

### Option 2: Using docker-compose

Add a test service to `docker-compose.yml`:

```yaml
test:
  build: .
  privileged: true
  volumes:
    - .:/app
  working_dir: /app
  command: sbcl --eval '(ql:quickload :squashd-tests)' \
               --eval '(fiveam:run! :squashd-tests)' \
               --quit
```

Then run:
```bash
docker-compose run --rm test
```

### Option 3: Local Development (Requires Root)

**WARNING**: Running these tests locally requires root privileges and can affect your system's mount table.

```bash
# Run with sudo to get CAP_SYS_ADMIN
sudo sbcl --eval '(ql:quickload :squashd-tests)' \
          --eval '(fiveam:run! :squashd-tests)' \
          --quit
```

### Option 4: REPL Interactive Testing

```lisp
;; Load the system
(ql:quickload :squashd-tests)

;; Run all tests
(fiveam:run! :squashd-tests)

;; Run specific test
(fiveam:run! 'squashd-tests::mount-tmpfs-success)

;; Run with verbose output
(setf fiveam:*verbose* t)
(fiveam:run! :squashd-tests)
```

## Test Coverage

### Mount/Umount Tests
- ✓ `mount-tmpfs-success`: Mount tmpfs filesystem
- ✓ `mount-with-flags`: Mount with nodev, nosuid, noexec flags
- ✓ `umount2-success`: Unmount filesystem
- ✓ `umount2-detach`: Lazy unmount with MNT_DETACH
- ✓ `mount-invalid-path-errno`: Error handling for invalid paths
- ✓ `umount2-nonexistent-errno`: Error handling for nonexistent mounts

### Fork Tests
- ✓ `fork-returns-valid-pid`: Fork returns positive PID in parent
- ✓ `fork-child-has-different-pid`: Child has different PID
- ✓ `fork-child-exit-status`: Waitpid captures child exit status

### Pipe/Read/Write Tests
- ✓ `pipe-creation-success`: Pipe creates valid file descriptors
- ✓ `pipe-write-read-data-transfer`: Data transfer through pipe
- ✓ `pipe-read-blocks-until-data`: Read behavior on empty pipe
- ✓ `pipe-multiple-writes-reads`: Multiple operations on same pipe

### Errno Tests
- ✓ `errno-read-invalid-fd`: Read with invalid fd sets errno
- ✓ `errno-write-invalid-fd`: Write with invalid fd sets errno
- ✓ `errno-close-invalid-fd`: Close with invalid fd sets errno
- ✓ `errno-pipe-invalid-pointer`: Pipe with NULL pointer sets errno

### Integration Tests
- ✓ `fork-pipe-ipc`: Fork with pipe for inter-process communication

## Troubleshooting

### "Operation not permitted" errors
- Tests require CAP_SYS_ADMIN capability
- Run inside privileged container or with sudo

### "Address already in use" errors
- Ensure previous test runs cleaned up properly
- Check for stray mount points: `mount | grep squashd-test`
- Manual cleanup: `sudo umount /tmp/squashd-test-*`

### Fork tests hanging
- Child processes may not be exiting properly
- Check for zombie processes: `ps aux | grep sbcl`
- Kill if needed: `sudo pkill -9 sbcl`

### Cleanup Issues
- Tests use unique temporary directories with timestamps
- Automatic cleanup in `unwind-protect` blocks
- Manual cleanup helper: `cleanup-mount` function

## Test Design Principles

1. **Isolation**: Each test uses unique temporary directories
2. **Cleanup**: All tests use `unwind-protect` for resource cleanup
3. **Explicit verification**: Tests verify results via /proc/mounts and return values
4. **Error coverage**: Both success and failure paths are tested
5. **Integration**: Complex scenarios combine multiple syscalls

## Contributing

When adding new syscall tests:
1. Add test to appropriate section in `syscalls-test.lisp`
2. Use unique resources (directories, fds) to avoid conflicts
3. Always include cleanup in `unwind-protect`
4. Test both success and error cases
5. Verify errno is set correctly on failures
6. Update this README with test description
