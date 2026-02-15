package squashd

import "core:fmt"
import "core:os"
import "core:strings"

// ---------------------------------------------------------------------------
// Mount flags (from <sys/mount.h>)
// ---------------------------------------------------------------------------

MS_RDONLY  :: 1
MNT_DETACH :: 2

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
// Squashfs_Mount
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
	// Ensure mount point directory exists
	if !os.exists(mount_point) {
		if os.make_directory(mount_point) != nil {
			return {}, .Create_Dir_Failed
		}
	}

	src := _to_cstr(squashfs_path)
	tgt := _to_cstr(mount_point)

	rc := c_mount(src, tgt, "squashfs", MS_RDONLY, nil)
	if rc != 0 {
		return {}, .Mount_Failed
	}

	return Squashfs_Mount{
		mount_point = strings.clone(mount_point, allocator),
		active      = true,
	}, .None
}

squashfs_unmount :: proc(m: ^Squashfs_Mount) {
	if m.active {
		tgt := _to_cstr(m.mount_point)
		c_umount2(tgt, MNT_DETACH)
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Tmpfs_Mount
// ---------------------------------------------------------------------------

Tmpfs_Mount :: struct {
	mount_point: string,
	active:      bool,
}

tmpfs_mount :: proc(
	mount_point: string,
	size_mb: u64,
	allocator := context.allocator,
) -> (result: Tmpfs_Mount, err: Mount_Error) {
	// Ensure mount point directory exists
	if !os.exists(mount_point) {
		if os.make_directory(mount_point) != nil {
			return {}, .Create_Dir_Failed
		}
	}

	tgt := _to_cstr(mount_point)
	opts := fmt.ctprintf("size=%dm", size_mb)

	rc := c_mount("tmpfs", tgt, "tmpfs", 0, rawptr(opts))
	if rc != 0 {
		return {}, .Mount_Failed
	}

	// Rollback: unmount if subdir creation fails below.
	result.active = true
	result.mount_point = strings.clone(mount_point, allocator)
	defer if err != .None {
		c_umount2(tgt, MNT_DETACH)
		result.active = false
	}

	// Create overlay subdirectories (data for upperdir, work for workdir)
	if os.make_directory(fmt.tprintf("%s/data", mount_point)) != nil {
		err = .Subdir_Create_Failed
		return
	}
	if os.make_directory(fmt.tprintf("%s/work", mount_point)) != nil {
		err = .Subdir_Create_Failed
		return
	}

	return
}

tmpfs_unmount :: proc(m: ^Tmpfs_Mount) {
	if m.active {
		tgt := _to_cstr(m.mount_point)
		c_umount2(tgt, MNT_DETACH)
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Overlay_Mount
// ---------------------------------------------------------------------------

Overlay_Mount :: struct {
	merged_path: string,
	active:      bool,
}

// Mount an overlayfs.
// lower_components: ordered highest-priority first (snapshot, then descending numeric modules).
overlay_mount :: proc(
	lower_components: []string,
	upper_data: string,
	work: string,
	merged: string,
	allocator := context.allocator,
) -> (Overlay_Mount, Mount_Error) {
	// Ensure merged directory exists
	if !os.exists(merged) {
		if os.make_directory(merged) != nil {
			return {}, .Create_Dir_Failed
		}
	}

	// Build options string: "lowerdir=A:B:C,upperdir=X,workdir=Y"
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_string(&b, "lowerdir=")
	for comp, i in lower_components {
		if i > 0 {
			strings.write_byte(&b, ':')
		}
		strings.write_string(&b, comp)
	}
	fmt.sbprintf(&b, ",upperdir=%s,workdir=%s", upper_data, work)

	tgt := _to_cstr(merged)
	opts := _to_cstr(strings.to_string(b))

	rc := c_mount("overlay", tgt, "overlay", 0, rawptr(opts))
	if rc != 0 {
		return {}, .Mount_Failed
	}

	return Overlay_Mount{
		merged_path = strings.clone(merged, allocator),
		active      = true,
	}, .None
}

overlay_unmount :: proc(m: ^Overlay_Mount) {
	if m.active {
		tgt := _to_cstr(m.merged_path)
		c_umount2(tgt, MNT_DETACH)
		m.active = false
	}
}

// ---------------------------------------------------------------------------
// Sandbox_Mounts — aggregate of all mounts for a single sandbox
// ---------------------------------------------------------------------------

Sandbox_Mounts :: struct {
	sqfs_mounts:    [dynamic]Squashfs_Mount,
	snapshot_mount: Maybe(Squashfs_Mount),
	tmpfs:          Tmpfs_Mount,
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

	// 3. Tmpfs upper layer
	tmpfs_unmount(&mounts.tmpfs)

	// 4. Squashfs module layers in reverse order
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
//   3. Tmpfs upper layer
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
	upper_size_mb:  u64,
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

	// 3. Tmpfs upper layer
	upper_mp := fmt.tprintf("%s/upper", params.sandbox_dir)
	result.tmpfs, err = tmpfs_mount(upper_mp, params.upper_size_mb, allocator)
	if err != .None {
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

	upper_data := fmt.tprintf("%s/upper/data", params.sandbox_dir)
	work_dir := fmt.tprintf("%s/upper/work", params.sandbox_dir)
	merged := fmt.tprintf("%s/merged", params.sandbox_dir)

	result.overlay, err = overlay_mount(lower_components[:], upper_data, work_dir, merged, allocator)
	if err != .None {
		return
	}

	return
}
