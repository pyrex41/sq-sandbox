package squashd

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

// ---------------------------------------------------------------------------
// Init / Recovery — first-boot initialization and sandbox remounting
//
// Replaces: bin/sq-init
//
// Sequence:
//   1. Ensure modules/ and sandboxes/ directories exist
//   2. Check base module version, pull from S3 or build via sq-mkbase
//   3. Check Firecracker components if backend=firecracker
//   4. Remount surviving sandboxes (non-ephemeral mode only)
// ---------------------------------------------------------------------------

Init_Error :: enum {
	None,
	Mkdir_Failed,
	Base_Build_Failed,
	Firecracker_Build_Failed,
}

// Current base module version. Increment when the base image changes.
BASE_MODULE_VERSION :: 2

// Run first-boot initialization. Called once at daemon startup.
init_run :: proc(config: ^Config) -> Init_Error {
	fmt.println("[init] starting")

	// Ensure data directories exist
	mods_dir := modules_dir(config)
	sbox_dir := sandboxes_dir(config)
	_ensure_dir_recursive(mods_dir)
	_ensure_dir_recursive(sbox_dir)

	// Check and provision base module
	base_err := _init_base_module(config)
	if base_err != .None {
		return base_err
	}

	// Check Firecracker components
	if config.backend == .Firecracker {
		fc_path := fmt.tprintf("%s/vm/firecracker", config.data_dir)
		if !os.exists(fc_path) {
			fmt.println("[init] firecracker backend: provisioning VM components")
			if !run_cmd("sq-mkvm", "all") {
				return .Firecracker_Build_Failed
			}
		} else {
			fmt.println("[init] firecracker backend: VM components present")
		}
	}

	// Remount surviving sandboxes (unless ephemeral)
	if !config.ephemeral {
		remount_surviving_sandboxes(config)
	} else {
		fmt.println("[init] ephemeral mode: skipping sandbox remount (S3 is source of truth)")
	}

	// Summary
	mod_count := _count_squashfs_files(modules_dir(config))
	sbox_count := _count_subdirs(sandboxes_dir(config))
	fmt.printfln("[init] ready — modules: %d, sandboxes: %d", mod_count, sbox_count)

	return .None
}

// Check base module version and build/pull if stale or missing.
_init_base_module :: proc(config: ^Config) -> Init_Error {
	mods := modules_dir(config)
	base_path := fmt.tprintf("%s/000-base-alpine.squashfs", mods)
	version_path := fmt.tprintf("%s/000-base-alpine.version", mods)

	// Read existing version (0 if file missing)
	existing_version := 0
	if data, ok := os.read_entire_file(version_path, context.temp_allocator); ok {
		trimmed := strings.trim_space(string(data))
		existing_version, _ = strconv.parse_int(trimmed)
	}

	if os.exists(base_path) && existing_version >= BASE_MODULE_VERSION {
		fmt.println("[init] base module present and up to date")
		return .None
	}

	// Need to fetch or build base module
	if _try_s3_pull_base(config, base_path) {
		return .None
	}

	// S3 not available or failed — build locally
	fmt.println("[init] building Alpine base")
	if !run_cmd("sq-mkbase", "alpine") {
		return .Base_Build_Failed
	}

	return .None
}

// Attempt to pull base module from S3. Returns true on success.
_try_s3_pull_base :: proc(config: ^Config, dest_path: string) -> bool {
	_, has_bucket := config.s3_bucket.?
	if !has_bucket {
		return false
	}

	// Check if sq-s3 command is available
	if !_command_exists("sq-s3") {
		return false
	}

	fmt.println("[init] no base module found, trying S3")
	if run_cmd("sq-s3", "pull", "modules/000-base-alpine.squashfs", dest_path) {
		fmt.println("[init] pulled base from S3")
		return true
	}

	fmt.println("[init] S3 pull failed")
	return false
}

// ---------------------------------------------------------------------------
// Remount surviving sandboxes — restart recovery for non-ephemeral mode
//
// Replaces: bin/sq-init lines 47-109
//
// For each sandbox directory:
//   1. Skip if already mounted (check /proc/mounts)
//   2. Read layers from .meta/layers
//   3. Mount each squashfs module (pull from S3 if missing)
//   4. Mount active snapshot if present
//   5. Rebuild overlay mount
// ---------------------------------------------------------------------------

remount_surviving_sandboxes :: proc(config: ^Config) {
	sbox_dir := sandboxes_dir(config)
	dh, err := os.open(sbox_dir)
	if err != nil {
		return
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return
	}
	defer delete(entries)

	for entry in entries {
		if !entry.is_dir {
			continue
		}

		id := _basename(entry.fullpath)
		sdir := entry.fullpath

		// Must have .meta/layers to be a valid sandbox
		layers_path := fmt.tprintf("%s/.meta/layers", sdir)
		if !os.exists(layers_path) {
			continue
		}

		// Skip if already mounted
		merged_path := fmt.tprintf("%s/merged", sdir)
		if _is_mounted(merged_path) {
			fmt.printfln("[init] %s: already mounted", id)
			continue
		}

		fmt.printfln("[init] %s: remounting", id)

		// Read layer list
		layers_data, layers_ok := os.read_entire_file(layers_path, context.temp_allocator)
		if !layers_ok {
			continue
		}
		layers_str := strings.trim_space(string(layers_data))
		layers := strings.split(layers_str, ",", context.temp_allocator)

		// Remount module images
		mods := modules_dir(config)
		for mod in layers {
			mod_trimmed := strings.trim_space(mod)
			if len(mod_trimmed) == 0 {
				continue
			}

			local_mp := fmt.tprintf("%s/images/%s.squashfs", sdir, mod_trimmed)
			sqfs := fmt.tprintf("%s/%s.squashfs", mods, mod_trimmed)

			if !os.exists(sqfs) {
				if _try_s3_pull_module(config, mod_trimmed, sqfs) {
					// pulled successfully
				} else {
					fmt.printfln("[init]   WARN: module missing: %s", mod_trimmed)
					continue
				}
			}

			_ensure_dir_recursive(local_mp)
			if !_is_mounted(local_mp) {
				src := _to_cstr(sqfs)
				tgt := _to_cstr(local_mp)
				rc := c_mount(src, tgt, "squashfs", MS_RDONLY, nil)
				if rc != 0 {
					fmt.printfln("[init]   WARN: mount failed: %s", mod_trimmed)
					continue
				}
			}
		}

		// Remount active snapshot if any
		snap_meta := fmt.tprintf("%s/.meta/active_snapshot", sdir)
		if os.exists(snap_meta) {
			snap_label_data, snap_ok := os.read_entire_file(snap_meta, context.temp_allocator)
			if snap_ok {
				label := strings.trim_space(string(snap_label_data))
				snap_file := fmt.tprintf("%s/snapshots/%s.squashfs", sdir, label)
				snap_mp := fmt.tprintf("%s/images/_snapshot", sdir)
				if os.exists(snap_file) {
					_ensure_dir_recursive(snap_mp)
					if !_is_mounted(snap_mp) {
						src := _to_cstr(snap_file)
						tgt := _to_cstr(snap_mp)
						c_mount(src, tgt, "squashfs", MS_RDONLY, nil)
					}
				}
			}
		}

		// Rebuild overlay
		upper_data := fmt.tprintf("%s/upper/data", sdir)
		work_dir := fmt.tprintf("%s/upper/work", sdir) if _overlay_work_in_upper(sdir) else fmt.tprintf("%s/work", sdir)
		_ensure_dir_recursive(upper_data)
		_ensure_dir_recursive(work_dir)
		_ensure_dir_recursive(merged_path)

		// Build lowerdir: snapshot first (highest priority), then layers in reverse numeric order
		lower_components: [dynamic]string
		defer delete(lower_components)

		snap_lower := fmt.tprintf("%s/images/_snapshot", sdir)
		if os.exists(snap_lower) && _is_mounted(snap_lower) {
			append(&lower_components, snap_lower)
		}

		// Collect mounted image directories and sort by name descending
		images_dir := fmt.tprintf("%s/images", sdir)
		mounted_images := _find_mounted_images(images_dir)
		defer delete(mounted_images)

		// Sort descending (highest numeric first)
		_sort_strings_descending(mounted_images[:])

		for img in mounted_images {
			append(&lower_components, img)
		}

		if len(lower_components) > 0 {
			_, m_err := overlay_mount(
				lower_components[:],
				upper_data,
				work_dir,
				merged_path,
			)
			if m_err == .None {
				fmt.printfln("[init] %s: ok", id)
			} else {
				fmt.printfln("[init] %s: FAILED", id)
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Run an external command via system(). Returns true on success (exit 0).
run_cmd :: proc(args: ..string) -> bool {
	cmd := strings.join(args[:], " ", context.temp_allocator)
	cstr := strings.clone_to_cstring(cmd, context.temp_allocator)
	return c_system(cstr) == 0
}

// Check if a command exists by running "command -v <name>".
_command_exists :: proc(name: string) -> bool {
	cmd := fmt.tprintf("command -v %s >/dev/null 2>&1", name)
	cstr := strings.clone_to_cstring(cmd, context.temp_allocator)
	return c_system(cstr) == 0
}

// Check if a path is a mountpoint by scanning /proc/mounts.
_is_mounted :: proc(path: string) -> bool {
	data, ok := os.read_entire_file("/proc/mounts", context.temp_allocator)
	if !ok {
		return false
	}
	// Each line: device mountpoint fstype options ...
	content := string(data)
	for line in strings.split_lines_iterator(&content) {
		// Find second field (mountpoint)
		rest := line
		// Skip device field
		idx := strings.index_byte(rest, ' ')
		if idx < 0 {
			continue
		}
		rest = rest[idx + 1:]
		// Read mountpoint field
		end := strings.index_byte(rest, ' ')
		mp: string
		if end >= 0 {
			mp = rest[:end]
		} else {
			mp = rest
		}
		if mp == path {
			return true
		}
	}
	return false
}

// Try to pull a module from S3.
_try_s3_pull_module :: proc(config: ^Config, mod_name: string, dest: string) -> bool {
	_, has_bucket := config.s3_bucket.?
	if !has_bucket || !_command_exists("sq-s3") {
		return false
	}
	s3_key := fmt.tprintf("modules/%s.squashfs", mod_name)
	fmt.printfln("[init]   pulling missing module from S3: %s", mod_name)
	return run_cmd("sq-s3", "pull", s3_key, dest)
}

// Extract the last path component (basename).
_basename :: proc(path: string) -> string {
	idx := strings.last_index_byte(path, '/')
	if idx >= 0 {
		return path[idx + 1:]
	}
	return path
}

// Count .squashfs files in a directory.
_count_squashfs_files :: proc(dir: string) -> int {
	dh, err := os.open(dir)
	if err != nil {
		return 0
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return 0
	}
	defer delete(entries)

	count := 0
	for entry in entries {
		if strings.has_suffix(entry.name, ".squashfs") {
			count += 1
		}
	}
	return count
}

// Count subdirectories in a directory.
_count_subdirs :: proc(dir: string) -> int {
	dh, err := os.open(dir)
	if err != nil {
		return 0
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return 0
	}
	defer delete(entries)

	count := 0
	for entry in entries {
		if entry.is_dir {
			count += 1
		}
	}
	return count
}

// Find mounted image directories matching [0-9]*.squashfs pattern.
_find_mounted_images :: proc(images_dir: string) -> [dynamic]string {
	result: [dynamic]string

	dh, err := os.open(images_dir)
	if err != nil {
		return result
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return result
	}
	defer delete(entries)

	for entry in entries {
		if !entry.is_dir {
			continue
		}
		name := entry.name
		// Match [0-9]*.squashfs pattern
		if len(name) > 0 && name[0] >= '0' && name[0] <= '9' && strings.has_suffix(name, ".squashfs") {
			full_path := entry.fullpath
			if _is_mounted(full_path) {
				append(&result, full_path)
			}
		}
	}

	return result
}

// Simple insertion sort for small string slices, descending order.
_sort_strings_descending :: proc(s: []string) {
	for i := 1; i < len(s); i += 1 {
		key := s[i]
		j := i - 1
		for j >= 0 && s[j] < key {
			s[j + 1] = s[j]
			j -= 1
		}
		s[j + 1] = key
	}
}

// Check if overlay workdir is inside upper/ (the convention used by this codebase).
// Falls back to checking if upper/work exists.
_overlay_work_in_upper :: proc(sdir: string) -> bool {
	work_in_upper := fmt.tprintf("%s/upper/work", sdir)
	return os.exists(work_in_upper)
}
