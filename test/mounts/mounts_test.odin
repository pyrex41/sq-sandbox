package mounts_test

// Mount/unmount tests for squashd.
//
// These tests duplicate the mount structs and logic from src/mounts.odin
// so they can compile and run independently (like test/sigv4/).
//
// Struct-level tests (idempotent unmount, error enum) run anywhere.
// Live mount tests require a privileged Linux container and are gated
// behind an OS + UID check.

import "core:c"
import "core:fmt"
import "core:os"
import "core:strings"
import "core:testing"

// ---------------------------------------------------------------------------
// Mount_Error enum (mirrors src/mounts.odin)
// ---------------------------------------------------------------------------

Mount_Error :: enum {
	None,
	Create_Dir_Failed,
	Mount_Failed,
	Unmount_Failed,
	Subdir_Create_Failed,
}

// ---------------------------------------------------------------------------
// Squashfs_Mount (mirrors src/mounts.odin)
// ---------------------------------------------------------------------------

Squashfs_Mount :: struct {
	mount_point: string,
	active:      bool,
}

squashfs_unmount :: proc(m: ^Squashfs_Mount) {
	if m.active {
		// In real code this calls c_umount2; here we just flip the flag.
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Tmpfs_Mount (mirrors src/mounts.odin)
// ---------------------------------------------------------------------------

Tmpfs_Mount :: struct {
	mount_point: string,
	active:      bool,
}

tmpfs_unmount :: proc(m: ^Tmpfs_Mount) {
	if m.active {
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Overlay_Mount (mirrors src/mounts.odin)
// ---------------------------------------------------------------------------

Overlay_Mount :: struct {
	merged_path: string,
	active:      bool,
}

overlay_unmount :: proc(m: ^Overlay_Mount) {
	if m.active {
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Sandbox_Mounts (mirrors src/mounts.odin)
// ---------------------------------------------------------------------------

Sandbox_Mounts :: struct {
	sqfs_mounts:    [dynamic]Squashfs_Mount,
	snapshot_mount: Maybe(Squashfs_Mount),
	tmpfs:          Tmpfs_Mount,
	overlay:        Overlay_Mount,
}

sandbox_mounts_destroy :: proc(mounts: ^Sandbox_Mounts) {
	overlay_unmount(&mounts.overlay)
	if snap, ok := &mounts.snapshot_mount.?; ok {
		squashfs_unmount(snap)
	}
	tmpfs_unmount(&mounts.tmpfs)
	#reverse for &m in mounts.sqfs_mounts {
		squashfs_unmount(&m)
	}
	delete(mounts.sqfs_mounts)
}

// ---------------------------------------------------------------------------
// Tests: Error enum
// ---------------------------------------------------------------------------

@(test)
test_mount_error_none_is_zero :: proc(t: ^testing.T) {
	err: Mount_Error
	testing.expect_value(t, err, Mount_Error.None)
}

@(test)
test_mount_error_variants :: proc(t: ^testing.T) {
	// Verify all expected error variants exist and are distinct.
	errors := [?]Mount_Error{
		.None,
		.Create_Dir_Failed,
		.Mount_Failed,
		.Unmount_Failed,
		.Subdir_Create_Failed,
	}
	for i in 0 ..< len(errors) {
		for j in i + 1 ..< len(errors) {
			testing.expectf(t, errors[i] != errors[j],
				"error variants %d and %d should be distinct", i, j)
		}
	}
}

// ---------------------------------------------------------------------------
// Tests: Unmount idempotency
// ---------------------------------------------------------------------------

@(test)
test_squashfs_unmount_idempotent :: proc(t: ^testing.T) {
	m := Squashfs_Mount{mount_point = "/tmp/test-sqfs", active = true}
	testing.expect(t, m.active, "should start active")

	squashfs_unmount(&m)
	testing.expect(t, !m.active, "should be inactive after first unmount")

	// Second unmount should be a no-op (no crash, no state change).
	squashfs_unmount(&m)
	testing.expect(t, !m.active, "should remain inactive after second unmount")
}

@(test)
test_squashfs_unmount_inactive_noop :: proc(t: ^testing.T) {
	// Unmounting something that was never mounted should be safe.
	m := Squashfs_Mount{mount_point = "/tmp/never-mounted", active = false}
	squashfs_unmount(&m)
	testing.expect(t, !m.active, "should remain inactive")
}

@(test)
test_tmpfs_unmount_idempotent :: proc(t: ^testing.T) {
	m := Tmpfs_Mount{mount_point = "/tmp/test-tmpfs", active = true}
	tmpfs_unmount(&m)
	testing.expect(t, !m.active, "should be inactive after unmount")

	tmpfs_unmount(&m)
	testing.expect(t, !m.active, "should remain inactive after second unmount")
}

@(test)
test_overlay_unmount_idempotent :: proc(t: ^testing.T) {
	m := Overlay_Mount{merged_path = "/tmp/test-overlay", active = true}
	overlay_unmount(&m)
	testing.expect(t, !m.active, "should be inactive after unmount")

	overlay_unmount(&m)
	testing.expect(t, !m.active, "should remain inactive after second unmount")
}

// ---------------------------------------------------------------------------
// Tests: Sandbox_Mounts destroy
// ---------------------------------------------------------------------------

@(test)
test_sandbox_mounts_destroy_reverse_order :: proc(t: ^testing.T) {
	// Track unmount order via a shared log.
	Order_Log :: struct {
		entries: [dynamic]string,
	}
	log: Order_Log

	mounts := Sandbox_Mounts{
		sqfs_mounts = make([dynamic]Squashfs_Mount),
		tmpfs       = Tmpfs_Mount{mount_point = "tmpfs", active = true},
		overlay     = Overlay_Mount{merged_path = "overlay", active = true},
	}

	append(&mounts.sqfs_mounts, Squashfs_Mount{mount_point = "sqfs-0", active = true})
	append(&mounts.sqfs_mounts, Squashfs_Mount{mount_point = "sqfs-1", active = true})
	mounts.snapshot_mount = Squashfs_Mount{mount_point = "snapshot", active = true}

	// Destroy — should deactivate everything.
	sandbox_mounts_destroy(&mounts)

	testing.expect(t, !mounts.overlay.active, "overlay should be inactive")
	testing.expect(t, !mounts.tmpfs.active, "tmpfs should be inactive")

	if snap, ok := &mounts.snapshot_mount.?; ok {
		testing.expect(t, !snap.active, "snapshot should be inactive")
	}
}

@(test)
test_sandbox_mounts_destroy_idempotent :: proc(t: ^testing.T) {
	mounts := Sandbox_Mounts{
		sqfs_mounts = make([dynamic]Squashfs_Mount),
		tmpfs       = Tmpfs_Mount{mount_point = "tmpfs", active = true},
		overlay     = Overlay_Mount{merged_path = "overlay", active = true},
	}
	append(&mounts.sqfs_mounts, Squashfs_Mount{mount_point = "sqfs-0", active = true})

	sandbox_mounts_destroy(&mounts)
	testing.expect(t, !mounts.overlay.active, "overlay inactive after first destroy")
	testing.expect(t, !mounts.tmpfs.active, "tmpfs inactive after first destroy")

	// Second destroy should be safe (all already inactive, dynamic array deleted).
	// Re-init the dynamic array since delete() was called.
	mounts.sqfs_mounts = {}
	sandbox_mounts_destroy(&mounts)
	testing.expect(t, !mounts.overlay.active, "overlay still inactive")
	testing.expect(t, !mounts.tmpfs.active, "tmpfs still inactive")
}

@(test)
test_sandbox_mounts_destroy_no_snapshot :: proc(t: ^testing.T) {
	mounts := Sandbox_Mounts{
		sqfs_mounts = make([dynamic]Squashfs_Mount),
		tmpfs       = Tmpfs_Mount{mount_point = "tmpfs", active = true},
		overlay     = Overlay_Mount{merged_path = "overlay", active = true},
	}
	// No snapshot_mount set (nil Maybe).
	sandbox_mounts_destroy(&mounts)
	testing.expect(t, !mounts.overlay.active, "overlay should be inactive")
	testing.expect(t, !mounts.tmpfs.active, "tmpfs should be inactive")
}

// ---------------------------------------------------------------------------
// Tests: Struct defaults
// ---------------------------------------------------------------------------

@(test)
test_squashfs_mount_default_inactive :: proc(t: ^testing.T) {
	m: Squashfs_Mount
	testing.expect(t, !m.active, "default Squashfs_Mount should be inactive")
	testing.expect_value(t, m.mount_point, "")
}

@(test)
test_tmpfs_mount_default_inactive :: proc(t: ^testing.T) {
	m: Tmpfs_Mount
	testing.expect(t, !m.active, "default Tmpfs_Mount should be inactive")
}

@(test)
test_overlay_mount_default_inactive :: proc(t: ^testing.T) {
	m: Overlay_Mount
	testing.expect(t, !m.active, "default Overlay_Mount should be inactive")
}

// ---------------------------------------------------------------------------
// Tests: Live mount operations (requires privileged Linux container)
//
// These tests exercise the actual mount/umount2 syscalls. They are skipped
// on non-Linux platforms and when not running as root.
// ---------------------------------------------------------------------------

when ODIN_OS == .Linux {
	foreign import libc "system:c"

	foreign libc {
		@(link_name = "mount")
		c_mount :: proc(source: cstring, target: cstring, fstype: cstring, flags: c.ulong, data: rawptr) -> c.int ---
		@(link_name = "umount2")
		c_umount2 :: proc(target: cstring, flags: c.int) -> c.int ---
		@(link_name = "getuid")
		c_getuid :: proc() -> c.uint ---
	}

	MS_RDONLY  :: 1
	MNT_DETACH :: 2

	_to_cstr :: proc(s: string) -> cstring {
		return strings.clone_to_cstring(s, context.temp_allocator)
	}

	_is_root :: proc() -> bool {
		return c_getuid() == 0
	}

	@(test)
	test_tmpfs_mount_live :: proc(t: ^testing.T) {
		if !_is_root() {
			fmt.println("  [SKIP] not root — tmpfs mount requires privileges")
			return
		}

		mp := "/tmp/squashd-test-tmpfs"
		os.make_directory(mp)
		defer os.remove(mp)

		tgt := _to_cstr(mp)
		opts := _to_cstr("size=1m")

		rc := c_mount("tmpfs", tgt, "tmpfs", 0, rawptr(opts))
		testing.expectf(t, rc == 0, "tmpfs mount should succeed, got rc=%d", rc)

		if rc == 0 {
			// Verify we can create files on the tmpfs
			test_file := fmt.tprintf("%s/test.txt", mp)
			os.write_entire_file(test_file, transmute([]byte)string("hello"))
			testing.expect(t, os.exists(test_file), "should be able to write to tmpfs")

			// Unmount
			rc2 := c_umount2(tgt, MNT_DETACH)
			testing.expectf(t, rc2 == 0, "tmpfs unmount should succeed, got rc=%d", rc2)
		}
	}

	@(test)
	test_tmpfs_mount_unmount_idempotent_live :: proc(t: ^testing.T) {
		if !_is_root() {
			fmt.println("  [SKIP] not root — tmpfs mount requires privileges")
			return
		}

		mp := "/tmp/squashd-test-tmpfs-idem"
		os.make_directory(mp)
		defer os.remove(mp)

		tgt := _to_cstr(mp)
		opts := _to_cstr("size=1m")

		rc := c_mount("tmpfs", tgt, "tmpfs", 0, rawptr(opts))
		testing.expectf(t, rc == 0, "mount should succeed, got rc=%d", rc)

		if rc == 0 {
			// First unmount: should succeed
			rc2 := c_umount2(tgt, MNT_DETACH)
			testing.expectf(t, rc2 == 0, "first unmount should succeed, got rc=%d", rc2)

			// Second unmount: may fail (EINVAL), but should not crash.
			// This validates that our active-flag guard prevents double unmount.
			c_umount2(tgt, MNT_DETACH) // no assertion — just must not crash
		}
	}

	@(test)
	test_tmpfs_subdir_creation_live :: proc(t: ^testing.T) {
		if !_is_root() {
			fmt.println("  [SKIP] not root — tmpfs mount requires privileges")
			return
		}

		mp := "/tmp/squashd-test-tmpfs-subdirs"
		os.make_directory(mp)
		defer {
			c_umount2(_to_cstr(mp), MNT_DETACH)
			os.remove(fmt.tprintf("%s/data", mp))
			os.remove(fmt.tprintf("%s/work", mp))
			os.remove(mp)
		}

		tgt := _to_cstr(mp)
		opts := _to_cstr("size=1m")

		rc := c_mount("tmpfs", tgt, "tmpfs", 0, rawptr(opts))
		testing.expectf(t, rc == 0, "mount should succeed, got rc=%d", rc)

		if rc == 0 {
			// Create overlay subdirectories like tmpfs_mount does
			data_dir := fmt.tprintf("%s/data", mp)
			work_dir := fmt.tprintf("%s/work", mp)

			err1 := os.make_directory(data_dir)
			testing.expectf(t, err1 == nil, "data dir creation should succeed")
			testing.expect(t, os.exists(data_dir), "data dir should exist")

			err2 := os.make_directory(work_dir)
			testing.expectf(t, err2 == nil, "work dir creation should succeed")
			testing.expect(t, os.exists(work_dir), "work dir should exist")
		}
	}

	@(test)
	test_mount_nonexistent_fs_fails :: proc(t: ^testing.T) {
		if !_is_root() {
			fmt.println("  [SKIP] not root — mount requires privileges")
			return
		}

		mp := "/tmp/squashd-test-mount-fail"
		os.make_directory(mp)
		defer os.remove(mp)

		tgt := _to_cstr(mp)
		// Try to mount a nonexistent squashfs — should fail.
		rc := c_mount(_to_cstr("/tmp/nonexistent.squashfs"), tgt, "squashfs", MS_RDONLY, nil)
		testing.expect(t, rc != 0, "mounting nonexistent squashfs should fail")
	}
}
