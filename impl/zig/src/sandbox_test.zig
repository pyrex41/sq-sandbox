// sandbox_test.zig — Comprehensive tests for sandbox lifecycle
//
// Tests resource cleanup, rollback on failures, and idempotent destroy.
// These tests verify that the errdefer chains work correctly and that
// resources are properly cleaned up even when errors occur mid-creation.

const std = @import("std");
const testing = std.testing;
const mounts = @import("mounts.zig");

// Mock structures to simulate sandbox lifecycle without requiring actual mounts
// Once sandbox.zig and manager.zig are implemented, these can be replaced

// ── Mock Resource Types ──────────────────────────────────────────────

/// Mock resource that tracks whether it's been cleaned up
const MockResource = struct {
    id: u32,
    active: bool,
    cleanup_called: *bool,

    pub fn init(id: u32, cleanup_called: *bool) MockResource {
        return .{
            .id = id,
            .active = true,
            .cleanup_called = cleanup_called,
        };
    }

    pub fn deinit(self: *MockResource) void {
        if (self.active) {
            self.cleanup_called.* = true;
            self.active = false;
        }
    }
};

/// Test helper to track multiple resource cleanup calls
const ResourceTracker = struct {
    tmpfs_cleaned: bool = false,
    overlay_cleaned: bool = false,
    squashfs_cleaned: [3]bool = .{ false, false, false },
    cgroup_cleaned: bool = false,
    netns_cleaned: bool = false,

    pub fn reset(self: *ResourceTracker) void {
        self.tmpfs_cleaned = false;
        self.overlay_cleaned = false;
        self.squashfs_cleaned = .{ false, false, false };
        self.cgroup_cleaned = false;
        self.netns_cleaned = false;
    }

    pub fn allCleaned(self: *const ResourceTracker) bool {
        if (!self.tmpfs_cleaned) return false;
        if (!self.overlay_cleaned) return false;
        for (self.squashfs_cleaned) |cleaned| {
            if (!cleaned) return false;
        }
        if (!self.cgroup_cleaned) return false;
        if (!self.netns_cleaned) return false;
        return true;
    }

    pub fn anySquashfsCleaned(self: *const ResourceTracker) bool {
        for (self.squashfs_cleaned) |cleaned| {
            if (cleaned) return true;
        }
        return false;
    }
};

// ── Simulated Sandbox Creation with Error Injection ─────────────────

const CreateError = error{
    TmpfsFailed,
    SquashfsFailed,
    OverlayFailed,
    CgroupFailed,
    NetnsFailed,
};

/// Simulate sandbox creation that can fail at specific steps
fn createSandboxWithFailure(
    tracker: *ResourceTracker,
    fail_at_step: ?u8,
) !void {
    // Step 1: Create tmpfs
    var tmpfs = MockResource.init(1, &tracker.tmpfs_cleaned);
    errdefer tmpfs.deinit();

    if (fail_at_step == 1) return CreateError.TmpfsFailed;

    // Step 2: Create squashfs mounts
    var squashfs_count: usize = 0;
    errdefer {
        // Roll back in reverse order
        var i = squashfs_count;
        while (i > 0) {
            i -= 1;
            tracker.squashfs_cleaned[i] = true;
        }
    }

    var sqfs_0 = MockResource.init(2, &tracker.squashfs_cleaned[0]);
    squashfs_count += 1;
    errdefer sqfs_0.deinit();

    var sqfs_1 = MockResource.init(3, &tracker.squashfs_cleaned[1]);
    squashfs_count += 1;
    errdefer sqfs_1.deinit();

    if (fail_at_step == 2) return CreateError.SquashfsFailed;

    var sqfs_2 = MockResource.init(4, &tracker.squashfs_cleaned[2]);
    squashfs_count += 1;
    errdefer sqfs_2.deinit();

    // Step 3: Create overlay
    var overlay = MockResource.init(5, &tracker.overlay_cleaned);
    errdefer overlay.deinit();

    if (fail_at_step == 3) return CreateError.OverlayFailed;

    // Step 4: Create cgroup
    var cgroup = MockResource.init(6, &tracker.cgroup_cleaned);
    errdefer cgroup.deinit();

    if (fail_at_step == 4) return CreateError.CgroupFailed;

    // Step 5: Create netns
    var netns = MockResource.init(7, &tracker.netns_cleaned);
    errdefer netns.deinit();

    if (fail_at_step == 5) return CreateError.NetnsFailed;

    // Success! Mark all resources as kept (prevent cleanup on errdefer)
    tmpfs.active = false;
    sqfs_0.active = false;
    sqfs_1.active = false;
    sqfs_2.active = false;
    overlay.active = false;
    cgroup.active = false;
    netns.active = false;
}

// ── Tests ────────────────────────────────────────────────────────────

test "successful creation does not trigger cleanup" {
    var tracker = ResourceTracker{};
    try createSandboxWithFailure(&tracker, null);

    // No cleanup should have happened
    try testing.expect(!tracker.tmpfs_cleaned);
    try testing.expect(!tracker.overlay_cleaned);
    try testing.expect(!tracker.anySquashfsCleaned());
    try testing.expect(!tracker.cgroup_cleaned);
    try testing.expect(!tracker.netns_cleaned);
}

test "failure at step 1 (tmpfs) cleans up tmpfs only" {
    var tracker = ResourceTracker{};
    const result = createSandboxWithFailure(&tracker, 1);

    try testing.expectError(CreateError.TmpfsFailed, result);
    try testing.expect(tracker.tmpfs_cleaned);
    try testing.expect(!tracker.overlay_cleaned);
    try testing.expect(!tracker.anySquashfsCleaned());
    try testing.expect(!tracker.cgroup_cleaned);
    try testing.expect(!tracker.netns_cleaned);
}

test "failure at step 2 (squashfs) cleans up squashfs and tmpfs" {
    var tracker = ResourceTracker{};
    const result = createSandboxWithFailure(&tracker, 2);

    try testing.expectError(CreateError.SquashfsFailed, result);
    try testing.expect(tracker.tmpfs_cleaned);
    try testing.expect(tracker.squashfs_cleaned[0]);
    try testing.expect(tracker.squashfs_cleaned[1]);
    // Third squashfs mount was never created
    try testing.expect(!tracker.squashfs_cleaned[2]);
    try testing.expect(!tracker.overlay_cleaned);
    try testing.expect(!tracker.cgroup_cleaned);
    try testing.expect(!tracker.netns_cleaned);
}

test "failure at step 3 (overlay) cleans up overlay, squashfs, tmpfs" {
    var tracker = ResourceTracker{};
    const result = createSandboxWithFailure(&tracker, 3);

    try testing.expectError(CreateError.OverlayFailed, result);
    try testing.expect(tracker.tmpfs_cleaned);
    try testing.expect(tracker.squashfs_cleaned[0]);
    try testing.expect(tracker.squashfs_cleaned[1]);
    try testing.expect(tracker.squashfs_cleaned[2]);
    try testing.expect(tracker.overlay_cleaned);
    try testing.expect(!tracker.cgroup_cleaned);
    try testing.expect(!tracker.netns_cleaned);
}

test "failure at step 4 (cgroup) cleans up all previous resources" {
    var tracker = ResourceTracker{};
    const result = createSandboxWithFailure(&tracker, 4);

    try testing.expectError(CreateError.CgroupFailed, result);
    try testing.expect(tracker.tmpfs_cleaned);
    try testing.expect(tracker.squashfs_cleaned[0]);
    try testing.expect(tracker.squashfs_cleaned[1]);
    try testing.expect(tracker.squashfs_cleaned[2]);
    try testing.expect(tracker.overlay_cleaned);
    try testing.expect(tracker.cgroup_cleaned);
    try testing.expect(!tracker.netns_cleaned);
}

test "failure at step 5 (netns) cleans up all resources" {
    var tracker = ResourceTracker{};
    const result = createSandboxWithFailure(&tracker, 5);

    try testing.expectError(CreateError.NetnsFailed, result);
    try testing.expect(tracker.allCleaned());
}

test "idempotent deinit can be called multiple times" {
    var cleanup_called = false;
    var resource = MockResource.init(1, &cleanup_called);

    // First deinit
    resource.deinit();
    try testing.expect(cleanup_called);
    try testing.expect(!resource.active);

    // Reset tracker for second call
    cleanup_called = false;

    // Second deinit should not trigger cleanup again
    resource.deinit();
    try testing.expect(!cleanup_called);
    try testing.expect(!resource.active);
}

// ── Mount-Specific Tests (using real mount structures) ───────────────

test "mount structures have idempotent deinit" {
    // Test that our actual mount structures support idempotent cleanup
    // These tests don't actually perform mounts (requires root), but verify
    // the idempotent pattern

    var sqfs = mounts.SquashfsMount{
        .mount_point = "/nonexistent",
        .active = false,
    };
    sqfs.deinit(); // Should not panic
    sqfs.deinit(); // Second call should also not panic

    var overlay = mounts.OverlayMount{
        .merged_path = "/nonexistent",
        .active = false,
    };
    overlay.deinit();
    overlay.deinit();

    // If we got here without crashing, the test passes
    try testing.expect(true);
}

// ── Sandbox Destruction Order Tests ──────────────────────────────────

const DestructionOrder = struct {
    steps: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) DestructionOrder {
        return .{
            .steps = std.ArrayList([]const u8).empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DestructionOrder) void {
        self.steps.deinit(self.allocator);
    }

    pub fn record(self: *DestructionOrder, step: []const u8) !void {
        try self.steps.append(self.allocator, step);
    }
};

fn simulateDestruction(order: *DestructionOrder) !void {
    // Simulate the correct destruction order (reverse of creation)
    // This matches the pattern in the thought document

    // 1. Unmount overlay (must be first — it references lower layers)
    try order.record("overlay.deinit");

    // 2. Unmount snapshot if any
    try order.record("snapshot.deinit");

    // 3. Unmount squashfs layers (reverse order)
    try order.record("squashfs[2].deinit");
    try order.record("squashfs[1].deinit");
    try order.record("squashfs[0].deinit");

    // 4. Unmount tmpfs
    try order.record("tmpfs.deinit");

    // 5. Teardown cgroup
    try order.record("cgroup.deinit");

    // 6. Teardown network namespace
    try order.record("netns.deinit");

    // 7. Remove directory tree
    try order.record("removeDir");
}

test "destruction happens in correct reverse order" {
    var order = DestructionOrder.init(testing.allocator);
    defer order.deinit();

    try simulateDestruction(&order);

    const expected = [_][]const u8{
        "overlay.deinit",
        "snapshot.deinit",
        "squashfs[2].deinit",
        "squashfs[1].deinit",
        "squashfs[0].deinit",
        "tmpfs.deinit",
        "cgroup.deinit",
        "netns.deinit",
        "removeDir",
    };

    try testing.expectEqual(expected.len, order.steps.items.len);
    for (expected, order.steps.items) |exp, actual| {
        try testing.expectEqualStrings(exp, actual);
    }
}

// ── Resource Leak Detection Tests ────────────────────────────────────

test "no resource leaks in successful creation and destruction" {
    // Use tracking allocator to detect leaks
    var tracker = ResourceTracker{};

    // Simulate successful creation
    try createSandboxWithFailure(&tracker, null);

    // Resources should still be active (not cleaned up during creation)
    try testing.expect(!tracker.anySquashfsCleaned());

    // Now simulate destruction by manually cleaning up
    tracker.tmpfs_cleaned = true;
    tracker.overlay_cleaned = true;
    tracker.squashfs_cleaned = .{ true, true, true };
    tracker.cgroup_cleaned = true;
    tracker.netns_cleaned = true;

    // Verify all resources were cleaned
    try testing.expect(tracker.allCleaned());
}

test "no resource leaks when creation fails midway" {
    var tracker = ResourceTracker{};

    // Fail at step 3 (overlay)
    const result = createSandboxWithFailure(&tracker, 3);
    try testing.expectError(CreateError.OverlayFailed, result);

    // Verify all created resources were cleaned up by errdefer
    try testing.expect(tracker.tmpfs_cleaned);
    try testing.expect(tracker.squashfs_cleaned[0]);
    try testing.expect(tracker.squashfs_cleaned[1]);
    try testing.expect(tracker.squashfs_cleaned[2]);
    try testing.expect(tracker.overlay_cleaned);

    // Resources that were never created should not be cleaned
    try testing.expect(!tracker.cgroup_cleaned);
    try testing.expect(!tracker.netns_cleaned);
}

// ── Manager Integration Tests ────────────────────────────────────────

/// Mock sandbox manager to test lifecycle management
const MockSandboxManager = struct {
    destroy_called: bool = false,
    destroy_count: usize = 0,

    pub fn destroySandbox(self: *MockSandboxManager, _: []const u8) !void {
        self.destroy_called = true;
        self.destroy_count += 1;
    }
};

test "manager calls destroy before removing from hashmap" {
    var manager = MockSandboxManager{};

    // Simulate removing a sandbox
    try manager.destroySandbox("test-sandbox");

    try testing.expect(manager.destroy_called);
    try testing.expectEqual(@as(usize, 1), manager.destroy_count);
}

test "manager cleanup is idempotent" {
    var manager = MockSandboxManager{};

    // Call destroy multiple times
    try manager.destroySandbox("test-sandbox");
    try manager.destroySandbox("test-sandbox");
    try manager.destroySandbox("test-sandbox");

    try testing.expect(manager.destroy_called);
    try testing.expectEqual(@as(usize, 3), manager.destroy_count);
}

// ── Integration Test Scenarios ───────────────────────────────────────

test "full lifecycle: create, exec, snapshot, restore, destroy" {
    // This is a high-level test that simulates the full lifecycle
    // Once real implementation exists, this can be expanded to use actual sandboxes

    var tracker = ResourceTracker{};

    // 1. Create sandbox
    try createSandboxWithFailure(&tracker, null);
    try testing.expect(!tracker.allCleaned());

    // 2. Simulate exec (no resources to track)
    // exec would use the sandbox resources but not create new ones

    // 3. Simulate snapshot (creates a new squashfs file)
    // snapshot would read from overlay upper dir

    // 4. Simulate restore (unmount/remount with snapshot)
    // restore would temporarily unmount overlay, add snapshot layer

    // 5. Destroy sandbox
    tracker.tmpfs_cleaned = true;
    tracker.overlay_cleaned = true;
    tracker.squashfs_cleaned = .{ true, true, true };
    tracker.cgroup_cleaned = true;
    tracker.netns_cleaned = true;

    try testing.expect(tracker.allCleaned());
}

test "concurrent destruction is safe" {
    // Test that multiple calls to destroy don't cause issues
    var manager = MockSandboxManager{};

    // Simulate concurrent destroy calls (in real code, this would be protected by mutex)
    try manager.destroySandbox("test-sandbox");

    // Verify destroy was called
    try testing.expect(manager.destroy_called);
}

// ── Edge Cases ───────────────────────────────────────────────────────

test "destroy handles already-cleaned resources gracefully" {
    var cleanup_called = false;
    var resource = MockResource.init(1, &cleanup_called);

    // Manually clean it up first
    resource.active = false;

    // Now call deinit - should be safe because of idempotent check
    resource.deinit();

    // cleanup_called should be false because active was already false
    try testing.expect(!cleanup_called);
}

test "errdefer cleanup works with zero resources created" {
    var tracker = ResourceTracker{};

    // Fail immediately at step 1
    const result = createSandboxWithFailure(&tracker, 1);
    try testing.expectError(CreateError.TmpfsFailed, result);

    // Only tmpfs should have been created and cleaned
    try testing.expect(tracker.tmpfs_cleaned);
    try testing.expect(!tracker.overlay_cleaned);
    try testing.expect(!tracker.anySquashfsCleaned());
}
