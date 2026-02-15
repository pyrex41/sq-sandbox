package squashd

import "core:fmt"
import "core:os"
import "core:time"

// ---------------------------------------------------------------------------
// Sandbox_State — tagged union state machine
//
// Exhaustive switching enforced by the compiler. State-specific resources
// (mounts, netns, cgroup) live inside the Ready variant so they are only
// accessible when the sandbox is actually ready.
// ---------------------------------------------------------------------------

Creating :: struct {}

Ready :: struct {
	mounts: Sandbox_Mounts,
	netns:  Maybe(Netns_Handle),
	cgroup: Maybe(Cgroup_Handle),
}

Executing :: struct {
	pid:     i32,
	started: time.Time,
}

Snapshotting :: struct {}

Destroying :: struct {}

Sandbox_State :: union {
	Creating,
	Ready,
	Executing,
	Snapshotting,
	Destroying,
}

// ---------------------------------------------------------------------------
// Create_Options — parameters passed to sandbox_create
// ---------------------------------------------------------------------------

Create_Options :: struct {
	owner:          string,
	task:           string,
	layers:         []string,
	cpu:            f64,
	memory_mb:      u64,
	max_lifetime_s: u64,
	allow_net:      Maybe([]string),
}

// ---------------------------------------------------------------------------
// Sandbox — core sandbox data
// ---------------------------------------------------------------------------

Sandbox :: struct {
	id:             string,
	dir:            string, // "{data_dir}/sandboxes/{id}"
	state:          Sandbox_State,
	created:        time.Time,
	last_active:    time.Time,
	exec_count:     u32,
	owner:          string,
	task:           string,
	max_lifetime_s: u64,
}

// ---------------------------------------------------------------------------
// Create_Error — errors during sandbox creation
// ---------------------------------------------------------------------------

Create_Error :: enum {
	None,
	Dir_Failed,
	Tmpfs_Failed,
	Squashfs_Failed,
	Overlay_Failed,
	Cgroup_Failed,
	Netns_Failed,
	Resolv_Failed,
	Secrets_Failed,
	Meta_Failed,
}

// Transition the sandbox to Ready after successful creation.
sandbox_set_ready :: proc(
	s: ^Sandbox,
	mounts: Sandbox_Mounts,
	netns: Maybe(Netns_Handle),
	cgroup: Maybe(Cgroup_Handle),
) {
	s.state = Ready{
		mounts = mounts,
		netns  = netns,
		cgroup = cgroup,
	}
}

// ---------------------------------------------------------------------------
// sandbox_create — multi-step creation with defer-based rollback
//
// Sequence:
//   1. Create directory tree
//   2. Mount tmpfs for upper layer
//   3. Loop-mount each squashfs module layer
//   4. Mount overlay filesystem
//   5. Create cgroup (best effort — failure is non-fatal)
//   6. Set up network namespace
//   7. Seed /etc/resolv.conf
//   8. Inject secret placeholders
//   9. Write metadata files
//
// On failure at any step, all previously acquired resources are rolled back
// via 'defer if err != .None' guards.
// ---------------------------------------------------------------------------

sandbox_create :: proc(
	config: ^Config,
	sandbox: ^Sandbox,
	opts: Create_Options,
	allocator := context.allocator,
) -> (err: Create_Error) {
	sdir := sandbox.dir
	mods := modules_dir(config)

	// 1. Create directory tree
	_ensure_dir_recursive(fmt.tprintf("%s/images", sdir))
	_ensure_dir_recursive(fmt.tprintf("%s/upper", sdir))
	_ensure_dir_recursive(fmt.tprintf("%s/merged", sdir))
	defer if err != .None {
		_remove_dir_recursive(sdir)
	}

	// 2. Mount tmpfs for upper layer (size-limited writable layer)
	upper_path := fmt.tprintf("%s/upper", sdir)
	tmpfs, tmpfs_err := tmpfs_mount(upper_path, config.upper_limit_mb, allocator)
	if tmpfs_err != .None {
		fmt.printfln("[sandbox] %s: tmpfs mount failed", sandbox.id)
		return .Tmpfs_Failed
	}
	defer if err != .None {
		tmpfs_unmount(&tmpfs)
	}

	// 3. Loop-mount each squashfs module layer
	sqfs_mounts := make([dynamic]Squashfs_Mount, allocator = allocator)
	defer if err != .None {
		#reverse for &m in sqfs_mounts {
			squashfs_unmount(&m)
		}
		delete(sqfs_mounts)
	}

	lower_components := make([dynamic]string, 0, len(opts.layers), context.temp_allocator)

	for layer in opts.layers {
		sqfs_path := fmt.tprintf("%s/%s.squashfs", mods, layer)

		// Check module exists, try S3 pull if missing
		if !os.exists(sqfs_path) {
			if !_try_s3_pull_module(config, layer, sqfs_path) {
				fmt.printfln("[sandbox] %s: module not found: %s", sandbox.id, layer)
				return .Squashfs_Failed
			}
		}

		mp := fmt.tprintf("%s/images/%s.squashfs", sdir, layer)
		m, m_err := squashfs_mount(sqfs_path, mp, allocator)
		if m_err != .None {
			fmt.printfln("[sandbox] %s: squashfs mount failed: %s", sandbox.id, layer)
			return .Squashfs_Failed
		}
		append(&sqfs_mounts, m)
		append(&lower_components, mp)
	}

	// Sort lower components: highest numeric prefix first (descending order for overlay)
	_sort_strings_descending(lower_components[:])

	// 4. Mount overlay filesystem
	upper_data := fmt.tprintf("%s/upper/data", sdir)
	work := fmt.tprintf("%s/upper/work", sdir)
	merged := fmt.tprintf("%s/merged", sdir)
	overlay, ov_err := overlay_mount(lower_components[:], upper_data, work, merged, allocator)
	if ov_err != .None {
		fmt.printfln("[sandbox] %s: overlay mount failed", sandbox.id)
		return .Overlay_Failed
	}
	defer if err != .None {
		overlay_unmount(&overlay)
	}

	// 5. Create cgroup (best effort — failure is non-fatal)
	cgroup: Maybe(Cgroup_Handle)
	cg, cg_err := cgroup_create(sandbox.id, opts.cpu, opts.memory_mb, allocator)
	if cg_err == .None {
		cgroup = cg
	} else {
		fmt.printfln("[sandbox] %s: cgroup creation failed (non-fatal)", sandbox.id)
	}
	defer if err != .None {
		if cg_handle, ok := &cgroup.?; ok {
			cgroup_destroy(cg_handle)
		}
	}

	// 6. Set up network namespace (always created for isolation)
	netns: Maybe(Netns_Handle)
	ns, ns_err := netns_setup(config, sandbox.id, opts.allow_net, allocator)
	if ns_err == .None {
		netns = ns
	} else {
		// Netns failure is non-fatal in matching shell behavior (|| true)
		fmt.printfln("[sandbox] %s: netns setup failed (non-fatal)", sandbox.id)
	}
	defer if err != .None {
		if ns_handle, ok := &netns.?; ok {
			netns_teardown(ns_handle)
		}
	}

	// Get netns index for resolv.conf and secret injection
	netns_index: u8 = 0
	if ns_handle, ok := netns.?; ok {
		netns_index = ns_handle.index
	}

	// 7. Seed /etc/resolv.conf
	resolv_err := seed_resolv_conf(sdir, netns_index)
	if resolv_err != .None {
		fmt.printfln("[sandbox] %s: resolv.conf seeding failed", sandbox.id)
		return .Resolv_Failed
	}

	// 8. Inject secret placeholders
	secrets_err := inject_secret_placeholders(config, sdir, netns_index)
	if secrets_err != .None && secrets_err != .File_Not_Found {
		fmt.printfln("[sandbox] %s: secret injection failed", sandbox.id)
		return .Secrets_Failed
	}

	// 9. Write metadata files
	meta_err := write_meta(sdir, sandbox.id, opts, netns_index)
	if meta_err != .None {
		fmt.printfln("[sandbox] %s: metadata write failed", sandbox.id)
		return .Meta_Failed
	}

	// All succeeded — transition sandbox to Ready state
	// Build Sandbox_Mounts from the acquired resources
	mounts := Sandbox_Mounts{
		sqfs_mounts    = sqfs_mounts,
		snapshot_mount = nil,
		tmpfs          = tmpfs,
		overlay        = overlay,
	}

	sandbox_set_ready(sandbox, mounts, netns, cgroup)
	fmt.printfln("[sandbox] %s: created", sandbox.id)

	return .None
}

// ---------------------------------------------------------------------------
// sandbox_create_firecracker — Firecracker-backend sandbox creation
//
// Sequence:
//   1. Create directory tree (.meta/ only — FS layers are in-VM)
//   2. Allocate CID for vsock communication
//   3. Set up tap networking
//   4. Build squashfs path list from modules
//   5. Start VM via sq-firecracker CLI
//   6. Write metadata files
// ---------------------------------------------------------------------------

sandbox_create_firecracker :: proc(
	config: ^Config,
	sandbox: ^Sandbox,
	opts: Create_Options,
	allocator := context.allocator,
) -> (err: Create_Error) {
	sdir := sandbox.dir
	mods := modules_dir(config)

	// 1. Create directory tree
	_ensure_dir_recursive(sdir)
	meta_dir := fmt.tprintf("%s/.meta", sdir)
	_ensure_dir_recursive(meta_dir)
	_ensure_dir_recursive(fmt.tprintf("%s/.meta/log", sdir))
	defer if err != .None {
		_remove_dir_recursive(sdir)
	}

	// 2. Allocate CID
	cid, cid_err := allocate_cid(config.data_dir)
	if cid_err != .None {
		fmt.printfln("[sandbox-fc] %s: CID allocation failed", sandbox.id)
		return .Meta_Failed
	}

	// Use CID - 100 as tap index for deterministic mapping
	tap_index := int(cid) - 100

	// 3. Set up tap networking
	net_err := firecracker_setup_network(sandbox.id, tap_index, opts.allow_net)
	if net_err != .None {
		fmt.printfln("[sandbox-fc] %s: network setup failed", sandbox.id)
		return .Netns_Failed
	}
	defer if err != .None {
		firecracker_teardown_network(sandbox.id, tap_index)
	}

	// 4. Build squashfs path list
	sqfs_paths := make([dynamic]string, 0, len(opts.layers), context.temp_allocator)
	for layer in opts.layers {
		sqfs_path := fmt.tprintf("%s/%s.squashfs", mods, layer)
		if !os.exists(sqfs_path) {
			if !_try_s3_pull_module(config, layer, sqfs_path) {
				fmt.printfln("[sandbox-fc] %s: module not found: %s", sandbox.id, layer)
				return .Squashfs_Failed
			}
		}
		append(&sqfs_paths, sqfs_path)
	}

	// 5. Start VM
	memory_mb := int(opts.memory_mb)
	if memory_mb == 0 { memory_mb = 1024 }
	cpu := opts.cpu
	if cpu <= 0 { cpu = 2.0 }

	vm_err := firecracker_start_vm(sandbox.id, cpu, memory_mb, sqfs_paths[:], cid, meta_dir)
	if vm_err != .None {
		fmt.printfln("[sandbox-fc] %s: VM start failed", sandbox.id)
		return .Overlay_Failed
	}
	defer if err != .None {
		firecracker_stop_vm(sandbox.id, meta_dir)
	}

	// 6. Write metadata
	meta_err := write_meta(sdir, sandbox.id, opts, 0)
	if meta_err != .None {
		fmt.printfln("[sandbox-fc] %s: metadata write failed", sandbox.id)
		return .Meta_Failed
	}

	// Write FC-specific metadata (best-effort)
	_ = _write_meta_file(meta_dir, "fc.cid", fmt.tprintf("%d", cid))
	_ = _write_meta_file(meta_dir, "fc.tap_index", fmt.tprintf("%d", tap_index))
	_ = _write_meta_file(meta_dir, "backend", "firecracker")

	// Transition to Ready with empty mounts (they exist inside the VM)
	mounts := Sandbox_Mounts{
		sqfs_mounts = make([dynamic]Squashfs_Mount, allocator),
	}
	sandbox_set_ready(sandbox, mounts, nil, nil)
	fmt.printfln("[sandbox-fc] %s: created (cid=%d)", sandbox.id, cid)

	return .None
}

// ---------------------------------------------------------------------------
// sandbox_destroy_firecracker — Firecracker teardown. Idempotent.
//
// Order:
//   1. Stop VM via sq-firecracker stop
//   2. Tear down tap networking
// ---------------------------------------------------------------------------

sandbox_destroy_firecracker :: proc(s: ^Sandbox) {
	meta_dir := fmt.tprintf("%s/.meta", s.dir)

	// Stop VM
	firecracker_stop_vm(s.id, meta_dir)

	// Tear down networking
	if tap_index, ok := read_fc_tap_index(s.dir); ok {
		firecracker_teardown_network(s.id, tap_index)
	}

	s.state = Destroying{}
}

// ---------------------------------------------------------------------------
// sandbox_destroy — reverse-order teardown. Idempotent.
//
// Order (reverse of creation):
//   1. Mounts (overlay first, then snapshot, then tmpfs, then squashfs in reverse)
//   2. Cgroup
//   3. Network namespace
// ---------------------------------------------------------------------------

sandbox_destroy :: proc(s: ^Sandbox) {
	// Only tear down resources if in Ready state
	if ready, ok := &s.state.(Ready); ok {
		// 1. Mounts (reverse order handled by sandbox_mounts_destroy)
		sandbox_mounts_destroy(&ready.mounts)

		// 2. Cgroup
		if cg, cg_ok := &ready.cgroup.?; cg_ok {
			cgroup_destroy(cg)
		}

		// 3. Network namespace
		if ns, ns_ok := &ready.netns.?; ns_ok {
			netns_teardown(ns)
		}
	}
	s.state = Destroying{}
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Recursively remove a directory tree. Best effort, ignores errors.
_remove_dir_recursive :: proc(path: string) {
	run_cmd("rm", "-rf", path)
}
