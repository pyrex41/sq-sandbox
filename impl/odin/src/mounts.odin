package squashd

import "core:fmt"
import "core:os"
import "core:strings"

// ---------------------------------------------------------------------------
// Mount_Error enum
// ---------------------------------------------------------------------------

Mount_Error :: enum {
	None,
	Create_Dir_Failed,
	Mount_Failed,
	Unmount_Failed,
	Subdir_Create_Failed,
}

// ---------------------------------------------------------------------------
// cstring conversion helpers
// ---------------------------------------------------------------------------

// Clone a string to a cstring using the temp allocator (valid until next free_all).
_to_cstr :: proc(s: string) -> cstring {
	return strings.clone_to_cstring(s, context.temp_allocator)
}

// ---------------------------------------------------------------------------
// Squashfs_Mount — uses sq-mount-layer helper
// ---------------------------------------------------------------------------

Squashfs_Mount :: struct {
	mount_point: string,
	active:      bool,
}

squashfs_mount :: proc(
	squashfs_path: string,
	mount_point: string,
	allocator := context.allocator,
) -> (Squashfs_Mount, Mount_Error) {
	if !run_cmd("sq-mount-layer", squashfs_path, mount_point) {
		return {}, .Mount_Failed
	}

	return Squashfs_Mount{
		mount_point = strings.clone(mount_point, allocator),
		active      = true,
	}, .None
}

squashfs_unmount :: proc(m: ^Squashfs_Mount) {
	if m.active {
		run_cmd("sq-mount-layer", "--unmount", m.mount_point)
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Overlay_Mount — uses sq-mount-overlay helper
// ---------------------------------------------------------------------------

Overlay_Mount :: struct {
	merged_path: string,
	active:      bool,
}

// Mount an overlayfs via sq-mount-overlay.
// lower_components: ordered highest-priority first (snapshot, then descending numeric modules).
// sq-mount-overlay takes colon-separated lowerdir string.
overlay_mount :: proc(
	lower_components: []string,
	upper_data: string,
	work: string,
	merged: string,
	allocator := context.allocator,
) -> (Overlay_Mount, Mount_Error) {
	// Build colon-separated lowerdir string for sq-mount-overlay
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	for comp, i in lower_components {
		if i > 0 {
			strings.write_byte(&b, ':')
		}
		strings.write_string(&b, comp)
	}
	lower_str := strings.to_string(b)

	if !run_cmd("sq-mount-overlay", lower_str, upper_data, work, merged) {
		return {}, .Mount_Failed
	}

	return Overlay_Mount{
		merged_path = strings.clone(merged, allocator),
		active      = true,
	}, .None
}

overlay_unmount :: proc(m: ^Overlay_Mount) {
	if m.active {
		run_cmd("sq-mount-overlay", "--unmount", m.merged_path)
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Sandbox_Mounts — aggregate of all mounts for a single sandbox
// (tmpfs removed — unprivileged mode uses plain directories)
// ---------------------------------------------------------------------------

Sandbox_Mounts :: struct {
	sqfs_mounts:    [dynamic]Squashfs_Mount,
	snapshot_mount: Maybe(Squashfs_Mount),
	overlay:        Overlay_Mount,
}

// Tear down all mounts in reverse mount order. Idempotent.
sandbox_mounts_destroy :: proc(mounts: ^Sandbox_Mounts) {
	// 1. Overlay (mounted last, unmounted first)
	overlay_unmount(&mounts.overlay)

	// 2. Snapshot layer (if any)
	if snap, ok := &mounts.snapshot_mount.?; ok {
		squashfs_unmount(snap)
	}

	// 3. Squashfs module layers in reverse order
	#reverse for &m in mounts.sqfs_mounts {
		squashfs_unmount(&m)
	}
	delete(mounts.sqfs_mounts)
}

// ---------------------------------------------------------------------------
// sandbox_mounts_setup — mount all layers with rollback on partial failure
//
// Mount order:
//   1. Squashfs module images
//   2. Snapshot layer (optional)
//   3. Create upper/work directories (plain dirs, no tmpfs)
//   4. Overlay combining all layers
//
// On failure at any step, previously mounted layers are torn down in
// reverse order via sandbox_mounts_destroy (which is idempotent).
// ---------------------------------------------------------------------------

Sandbox_Setup_Params :: struct {
	sandbox_dir:    string,
	modules_dir:    string,
	layers:         []string,   // module names (e.g., ["000-base-alpine", "010-python"])
	snapshot_path:  Maybe(string), // path to snapshot .squashfs if any
}

sandbox_mounts_setup :: proc(
	params: ^Sandbox_Setup_Params,
	allocator := context.allocator,
) -> (result: Sandbox_Mounts, err: Mount_Error) {
	result.sqfs_mounts = make([dynamic]Squashfs_Mount, allocator)

	// Rollback everything on failure.
	defer if err != .None {
		sandbox_mounts_destroy(&result)
	}

	// 1. Mount squashfs module images
	for layer in params.layers {
		sqfs_path := fmt.tprintf("%s/%s.squashfs", params.modules_dir, layer)
		mp := fmt.tprintf("%s/images/%s.squashfs", params.sandbox_dir, layer)

		m: Squashfs_Mount
		m, err = squashfs_mount(sqfs_path, mp, allocator)
		if err != .None {
			return
		}
		append(&result.sqfs_mounts, m)
	}

	// 2. Snapshot layer (optional)
	if snap_path, has_snap := params.snapshot_path.?; has_snap {
		snap_mp := fmt.tprintf("%s/images/_snapshot", params.sandbox_dir)
		snap: Squashfs_Mount
		snap, err = squashfs_mount(snap_path, snap_mp, allocator)
		if err != .None {
			return
		}
		result.snapshot_mount = snap
	}

	// 3. Create upper/work directories (no tmpfs — just plain dirs)
	upper_data := fmt.tprintf("%s/upper/data", params.sandbox_dir)
	work_dir := fmt.tprintf("%s/upper/work", params.sandbox_dir)
	if os.make_directory(upper_data) != nil && !os.exists(upper_data) {
		err = .Subdir_Create_Failed
		return
	}
	if os.make_directory(work_dir) != nil && !os.exists(work_dir) {
		err = .Subdir_Create_Failed
		return
	}

	// 4. Overlay: build lower components list (snapshot first, then modules)
	lower_components: [dynamic]string
	lower_components.allocator = context.temp_allocator

	if snap, has_snap := &result.snapshot_mount.?; has_snap {
		append(&lower_components, snap.mount_point)
	}
	#reverse for &m in result.sqfs_mounts {
		append(&lower_components, m.mount_point)
	}

	merged := fmt.tprintf("%s/merged", params.sandbox_dir)

	result.overlay, err = overlay_mount(lower_components[:], upper_data, work_dir, merged, allocator)
	if err != .None {
		return
	}

	return
}
