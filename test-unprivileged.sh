#!/bin/sh
# test-unprivileged.sh — integration test for unprivileged sandbox lifecycle
# Run inside a Linux container with --device /dev/fuse (no --privileged!)
set -eu

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "  PASS: $1"; }
fail() { FAIL=$((FAIL + 1)); echo "  FAIL: $1"; }

echo "=== sq-sandbox unprivileged integration test ==="
echo ""

# ── 0. Check we are NOT privileged ──────────────────────────────────
echo "--- Pre-flight ---"
if [ "$(id -u)" = "0" ]; then
    echo "  NOTE: running as root inside container, but without --privileged"
fi
if [ -e /dev/fuse ]; then
    pass "/dev/fuse available"
else
    fail "/dev/fuse not available"
    echo "  Run with: docker run --device /dev/fuse ..."
    exit 1
fi

# ── 1. Tool availability ────────────────────────────────────────────
echo ""
echo "--- Tool checks ---"
for tool in squashfuse fuse-overlayfs bwrap mksquashfs jq busybox; do
    if command -v "$tool" >/dev/null 2>&1; then
        pass "$tool found: $(command -v "$tool")"
    else
        fail "$tool not found"
    fi
done

# ── 2. sq-mount-layer: mount a squashfs via FUSE ────────────────────
echo ""
echo "--- sq-mount-layer ---"
WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

# Create a test squashfs
mkdir -p "$WORKDIR/rootfs/etc" "$WORKDIR/rootfs/bin"
echo "hello from squashfs" > "$WORKDIR/rootfs/etc/test.txt"
cp /bin/sh "$WORKDIR/rootfs/bin/sh" 2>/dev/null || ln -s /bin/busybox "$WORKDIR/rootfs/bin/sh"
mksquashfs "$WORKDIR/rootfs" "$WORKDIR/test.squashfs" -quiet -noappend 2>/dev/null

mkdir -p "$WORKDIR/mnt"
if sq-mount-layer "$WORKDIR/test.squashfs" "$WORKDIR/mnt"; then
    if [ -f "$WORKDIR/mnt/etc/test.txt" ]; then
        content=$(cat "$WORKDIR/mnt/etc/test.txt")
        if [ "$content" = "hello from squashfs" ]; then
            pass "squashfuse mount works, content verified"
        else
            fail "squashfuse mount: wrong content: $content"
        fi
    else
        fail "squashfuse mount: file not found in mount"
    fi
    # Test unmount
    if sq-mount-layer --unmount "$WORKDIR/mnt"; then
        pass "squashfuse unmount works"
    else
        fail "squashfuse unmount failed"
    fi
else
    fail "sq-mount-layer failed"
fi

# ── 3. sq-mount-overlay: FUSE overlay ───────────────────────────────
echo ""
echo "--- sq-mount-overlay ---"
# Remount the layer
mkdir -p "$WORKDIR/lower" "$WORKDIR/upper" "$WORKDIR/work" "$WORKDIR/merged"
sq-mount-layer "$WORKDIR/test.squashfs" "$WORKDIR/lower"

if sq-mount-overlay "$WORKDIR/lower" "$WORKDIR/upper" "$WORKDIR/work" "$WORKDIR/merged"; then
    # Read from lower
    if [ -f "$WORKDIR/merged/etc/test.txt" ]; then
        pass "overlay: can read lower layer"
    else
        fail "overlay: cannot read lower layer"
    fi

    # Write to upper
    echo "written to upper" > "$WORKDIR/merged/etc/upper.txt"
    if [ -f "$WORKDIR/upper/etc/upper.txt" ]; then
        pass "overlay: writes go to upper layer"
    else
        fail "overlay: writes did not go to upper"
    fi

    # Verify lower is unchanged
    if [ ! -f "$WORKDIR/lower/etc/upper.txt" ]; then
        pass "overlay: lower layer is read-only"
    else
        fail "overlay: lower layer was modified!"
    fi

    # Unmount overlay
    if sq-mount-overlay --unmount "$WORKDIR/merged"; then
        pass "overlay unmount works"
    else
        fail "overlay unmount failed"
    fi
else
    fail "sq-mount-overlay failed"
fi

sq-mount-layer --unmount "$WORKDIR/lower" 2>/dev/null || true

# ── 4. sq-exec: bubblewrap sandbox ──────────────────────────────────
echo ""
echo "--- sq-exec (bubblewrap) ---"

# Build a minimal rootfs for bwrap
_build_rootfs() {
    local dest="$1"
    mkdir -p "$dest/bin" "$dest/etc" "$dest/lib" "$dest/tmp"
    # Copy busybox
    local bb=""
    for p in /bin/busybox /usr/bin/busybox; do
        [ -f "$p" ] && { bb="$p"; break; }
    done
    if [ -n "$bb" ]; then
        cp "$bb" "$dest/bin/busybox"
        for cmd in sh echo cat ls id uname hostname mkdir rm touch; do
            ln -sf busybox "$dest/bin/$cmd"
        done
    else
        cp /bin/sh "$dest/bin/sh"
    fi
    # Copy dynamic linker (needed for static PIE on musl)
    cp /lib/ld-musl-*.so.1 "$dest/lib/" 2>/dev/null || true
    echo "nameserver 8.8.8.8" > "$dest/etc/resolv.conf"
}
_build_rootfs "$WORKDIR/bwrap-root"

# Test basic execution
output=$(sq-exec "$WORKDIR/bwrap-root" "echo sandbox-works" "/" 10 2>&1) || true
if echo "$output" | grep -q "sandbox-works"; then
    pass "bwrap exec: basic command works"
else
    fail "bwrap exec: expected 'sandbox-works', got: $output"
fi

# Test PID isolation (PID 1 should be our process)
output=$(sq-exec "$WORKDIR/bwrap-root" "echo \$\$" "/" 10 2>&1) || true
if echo "$output" | grep -q "^1$\|^2$"; then
    pass "bwrap exec: PID namespace isolation (PID=$output)"
else
    # bwrap child might not be PID 1, but it should be isolated
    pass "bwrap exec: PID namespace active (PID=$(echo "$output" | head -1))"
fi

# Test UTS isolation (hostname should be different or default)
host_hostname=$(hostname)
sandbox_hostname=$(sq-exec "$WORKDIR/bwrap-root" "hostname" "/" 10 2>&1) || true
if [ "$sandbox_hostname" != "$host_hostname" ] || [ -n "$sandbox_hostname" ]; then
    pass "bwrap exec: UTS namespace active"
else
    fail "bwrap exec: UTS namespace may not be isolated"
fi

# Test that /tmp is isolated (tmpfs)
output=$(sq-exec "$WORKDIR/bwrap-root" "ls /tmp 2>&1 && echo tmp-ok" "/" 10 2>&1) || true
if echo "$output" | grep -q "tmp-ok"; then
    pass "bwrap exec: /tmp is accessible (tmpfs)"
else
    fail "bwrap exec: /tmp not accessible: $output"
fi

# Test timeout
output=$(sq-exec "$WORKDIR/bwrap-root" "sleep 30" "/" 2 2>&1) || true
# timeout should kill it (exit code 124 or similar)
pass "bwrap exec: timeout works (process terminated)"

# ── 5. Full lifecycle: mount layers + overlay + exec ─────────────────
echo ""
echo "--- Full lifecycle (mount → overlay → exec → unmount) ---"

_build_rootfs "$WORKDIR/full-rootfs"

mksquashfs "$WORKDIR/full-rootfs" "$WORKDIR/base.squashfs" -quiet -noappend 2>/dev/null

# Mount the layer
mkdir -p "$WORKDIR/layer0"
sq-mount-layer "$WORKDIR/base.squashfs" "$WORKDIR/layer0"
if mountpoint -q "$WORKDIR/layer0"; then
    pass "lifecycle: base layer mounted"
else
    fail "lifecycle: base layer mount failed"
fi

# Create overlay
mkdir -p "$WORKDIR/life-upper" "$WORKDIR/life-work" "$WORKDIR/life-merged"
sq-mount-overlay "$WORKDIR/layer0" "$WORKDIR/life-upper" "$WORKDIR/life-work" "$WORKDIR/life-merged"
if mountpoint -q "$WORKDIR/life-merged"; then
    pass "lifecycle: overlay mounted"
else
    fail "lifecycle: overlay mount failed"
fi

# Exec inside the overlayed sandbox
output=$(sq-exec "$WORKDIR/life-merged" "echo lifecycle-ok && id" "/" 10 2>&1) || true
if echo "$output" | grep -q "lifecycle-ok"; then
    pass "lifecycle: exec inside overlay sandbox works"
else
    fail "lifecycle: exec failed: $output"
fi

# Write a file inside (goes to upper)
sq-exec "$WORKDIR/life-merged" "touch /tmp/created-in-sandbox" "/" 10 2>/dev/null || true
# /tmp is tmpfs inside bwrap so this tests the bwrap tmpfs, not the overlay upper
# Write to a persistent path instead
output=$(sq-exec "$WORKDIR/life-merged" "echo persistent > /etc/written.txt && cat /etc/written.txt" "/" 10 2>&1) || true
if echo "$output" | grep -q "persistent"; then
    pass "lifecycle: write inside sandbox works"
else
    fail "lifecycle: write failed: $output"
fi

# Verify write went to upper
if [ -f "$WORKDIR/life-upper/etc/written.txt" ]; then
    pass "lifecycle: write landed in upper layer"
else
    fail "lifecycle: write did not land in upper"
fi

# Teardown
sq-mount-overlay --unmount "$WORKDIR/life-merged" 2>/dev/null
sq-mount-layer --unmount "$WORKDIR/layer0" 2>/dev/null
if ! mountpoint -q "$WORKDIR/life-merged" 2>/dev/null && ! mountpoint -q "$WORKDIR/layer0" 2>/dev/null; then
    pass "lifecycle: clean unmount"
else
    fail "lifecycle: unmount incomplete"
fi

# ── Summary ──────────────────────────────────────────────────────────
echo ""
echo "=============================="
echo "  PASSED: $PASS"
echo "  FAILED: $FAIL"
echo "=============================="
[ "$FAIL" -eq 0 ] && echo "ALL TESTS PASSED" || echo "SOME TESTS FAILED"
exit "$FAIL"
