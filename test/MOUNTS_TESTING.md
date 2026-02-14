# Mount Cycle Integration Tests

This document describes the comprehensive mount/unmount cycle tests for the sandbox mount infrastructure.

## Overview

The mount tests verify correct behavior of:
- **SquashfsMount**: Read-only squashfs filesystem mounts
- **TmpfsMount**: Temporary filesystem mounts with size limits
- **OverlayMount**: Overlay filesystem combining multiple layers
- **SandboxMounts**: Complete sandbox mount stack with automatic cleanup

## Test Coverage

### Unit Tests (Platform-Agnostic)
Located in `src/sandbox/mounts.rs`:
- Input validation (empty layers, mismatched arrays)
- Path handling
- Configuration validation

These run on any platform during `cargo test`.

### Integration Tests (Linux-Only)
Located in `src/sandbox/mounts_integration_tests.rs`:

#### SquashfsMount Tests
- ✓ Mount/unmount cycle
- ✓ Explicit unmount
- ✓ Content verification
- ✓ Drop-based cleanup
- ✓ Double unmount safety

#### TmpfsMount Tests
- ✓ Mount/unmount cycle
- ✓ Explicit unmount
- ✓ Write verification
- ✓ Size limit configuration
- ✓ Drop-based cleanup
- ✓ Double unmount safety

#### OverlayMount Tests
- ✓ Mount/unmount cycle
- ✓ Explicit unmount
- ✓ Lower layer visibility
- ✓ Upper layer writes
- ✓ Multiple lower layers
- ✓ Drop-based cleanup
- ✓ Double unmount safety

#### SandboxMounts Tests
- ✓ Full mount stack creation
- ✓ All layers mounted correctly
- ✓ Snapshot support
- ✓ Reverse-order unmounting (overlay → tmpfs → snapshot → layers)
- ✓ No orphaned mounts after Drop
- ✓ Partial failure cleanup (rollback on error)
- ✓ Mount count verification

## Running the Tests

### Prerequisites
- Linux environment (native or Docker)
- CAP_SYS_ADMIN capability for mount operations
- `mksquashfs` utility (from squashfs-tools)
- Rust toolchain

### Option 1: Using Docker (Recommended)

Run all mount integration tests in a privileged container:

```bash
./test/mounts_integration_test.sh
```

This script:
1. Builds a test container with all dependencies
2. Runs tests with required capabilities (SYS_ADMIN, MKNOD)
3. Automatically cleans up

### Option 2: Manual Testing (Linux Host)

On a Linux system with privileges:

```bash
# Install dependencies
sudo apt-get install squashfs-tools  # Debian/Ubuntu
# or
sudo yum install squashfs-tools      # RHEL/CentOS

# Run tests (requires root or CAP_SYS_ADMIN)
sudo cargo test --bin squashd mounts -- --test-threads=1 --nocapture
```

**Note**: Tests must run with `--test-threads=1` to avoid mount conflicts.

### Option 3: CI Environment

For CI pipelines, use a privileged container:

```yaml
# GitHub Actions example
jobs:
  test-mounts:
    runs-on: ubuntu-latest
    container:
      image: rust:1.75-alpine
      options: --privileged
    steps:
      - uses: actions/checkout@v3
      - run: apk add --no-cache squashfs-tools musl-dev
      - run: cargo test --bin squashd mounts -- --test-threads=1
```

## Test Requirements

### Capabilities
Tests require the following Linux capabilities:
- **CAP_SYS_ADMIN**: For mount/umount syscalls
- **CAP_MKNOD**: For creating device nodes (if needed)

### Filesystem
- Tests use `/proc/mounts` to verify mount state
- Require writable temp directories for test fixtures
- Need loop device support for squashfs mounting

### Dependencies
- `mksquashfs`: Create test squashfs images
- `tempfile` crate: Temporary test directories
- Linux kernel with overlayfs support (3.18+)

## Test Design

### No Orphaned Mounts
Each test verifies that all mounts are cleaned up after Drop:
```rust
let before_count = count_mounts(prefix);
{
    let _mounts = SandboxMounts::mount(...);
    // Use mounts
} // Drop happens here
let after_count = count_mounts(prefix);
assert_eq!(after_count, before_count);
```

### Reverse-Order Unmounting
SandboxMounts explicitly unmounts in reverse creation order:
1. Overlay (depends on all lower layers)
2. Tmpfs (contains overlay upper/work)
3. Snapshot (topmost lower layer)
4. Module layers (reverse: top to bottom)

This prevents "device busy" errors from dependent mounts.

### Partial Failure Cleanup
If any mount fails during SandboxMounts::mount(), all previously
successful mounts are cleaned up via Drop:
```rust
let result = SandboxMounts::mount(
    vec![valid_layer, invalid_layer],  // Second mount will fail
    ...
);
assert!(result.is_err());
assert!(!is_mounted(valid_layer));  // First mount was cleaned up
```

### Double Unmount Safety
All mount types track `mounted` state and make unmount idempotent:
```rust
mount.unmount()?;  // First unmount
mount.unmount()?;  // No-op, returns Ok
drop(mount);       // Also safe
```

## Debugging Test Failures

### Check Available Mounts
```bash
cat /proc/mounts | grep squashd
```

### Verify Capabilities
```bash
capsh --print | grep sys_admin
```

### Manual Mount Testing
```bash
# Create test squashfs
mkdir test-content
echo "test" > test-content/file.txt
mksquashfs test-content test.squashfs -comp gzip

# Mount it
sudo mkdir /mnt/test
sudo mount -t squashfs -o loop,ro test.squashfs /mnt/test

# Verify
cat /mnt/test/file.txt

# Unmount
sudo umount /mnt/test
```

### Common Issues
1. **Permission denied**: Need CAP_SYS_ADMIN or root
2. **Device busy**: Another process using the mount (lsof, fuser)
3. **No squashfs support**: Kernel missing CONFIG_SQUASHFS
4. **No overlay support**: Kernel missing CONFIG_OVERLAY_FS

## Architecture Notes

### Drop Order
Rust drops struct fields in declaration order. SandboxMounts declares
fields in reverse unmount order to ensure proper cleanup:
```rust
pub struct SandboxMounts {
    pub overlay: OverlayMount,    // Dropped first
    pub tmpfs: TmpfsMount,         // Dropped second
    pub snapshot: Option<...>,     // Dropped third
    pub layers: Vec<...>,          // Dropped last (vec drops in order)
}
```

However, Drop impl explicitly unmounts in reverse to be defensive.

### MNT_DETACH Flag
All unmounts use MNT_DETACH (lazy unmount) to handle:
- Active file handles
- Processes with cwd inside mount
- Race conditions during cleanup

This matches the shell implementation and prevents cleanup failures.

### Mount Verification
Tests use `/proc/mounts` instead of checking directories because:
- Directories exist even when unmounted
- `/proc/mounts` shows kernel state
- Reliable across all mount types

## Future Improvements

- [ ] Add bind mount support tests
- [ ] Test concurrent mount/unmount (stress testing)
- [ ] Add performance benchmarks
- [ ] Test recovery from SIGKILL during mount
- [ ] Test cleanup with active file handles
- [ ] Add tests for mount options validation
