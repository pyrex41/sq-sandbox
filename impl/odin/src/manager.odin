package squashd

import "core:mem"
import "core:sync"
import "core:time"
import "core:fmt"
import "core:os"
import "core:strings"

// ---------------------------------------------------------------------------
// Managed_Sandbox — per-sandbox wrapper with dedicated arena and mutex
//
// Each sandbox gets its own mem.Arena. All sandbox-related allocations
// (paths, metadata, mount arrays) use this arena via context.allocator.
// On destroy, mem.arena_destroy frees everything at once — no per-allocation
// free() calls, zero per-sandbox memory leaks.
//
// The mutex serializes operations on the same sandbox (exec, snapshot,
// activate, destroy) while allowing concurrent operations across different
// sandboxes.
// ---------------------------------------------------------------------------

Managed_Sandbox :: struct {
	sandbox: Sandbox,
	arena:   mem.Dynamic_Arena,
	lock:    sync.Mutex,
}

// Initialize a Managed_Sandbox with a fresh arena backed by the page allocator.
// Returns a pointer allocated from the manager's own allocator.
managed_sandbox_init :: proc(
	id: string,
	dir: string,
	opts: Create_Options,
	allocator := context.allocator,
) -> ^Managed_Sandbox {
	ms := new(Managed_Sandbox, allocator)
	mem.dynamic_arena_init(&ms.arena)

	// Clone strings into the sandbox's arena so they outlive the request.
	sa := mem.dynamic_arena_allocator(&ms.arena)
	sandbox_id := strings_clone(id, sa)
	sandbox_dir := strings_clone(dir, sa)

	ms.sandbox = Sandbox{
		id             = sandbox_id,
		dir            = sandbox_dir,
		state          = Creating{},
		created        = time.now(),
		last_active    = time.now(),
		exec_count     = 0,
		owner          = strings_clone(opts.owner, sa),
		task           = strings_clone(opts.task, sa),
		max_lifetime_s = opts.max_lifetime_s,
	}

	return ms
}

// Destroy a Managed_Sandbox: tear down sandbox resources, then free the arena.
managed_sandbox_destroy :: proc(ms: ^Managed_Sandbox) {
	sandbox_destroy(&ms.sandbox)
	mem.dynamic_arena_destroy(&ms.arena)
}

// Return the arena allocator for this sandbox's dedicated memory pool.
managed_sandbox_allocator :: proc(ms: ^Managed_Sandbox) -> mem.Allocator {
	return mem.dynamic_arena_allocator(&ms.arena)
}

// ---------------------------------------------------------------------------
// Helper: clone a string with a specific allocator
// ---------------------------------------------------------------------------

strings_clone :: proc(s: string, allocator: mem.Allocator) -> string {
	if len(s) == 0 {
		return ""
	}
	buf := make([]byte, len(s), allocator)
	copy(buf, s)
	return string(buf)
}

// ---------------------------------------------------------------------------
// Sandbox_Manager — central registry of all sandboxes
//
// The global_lock protects the map itself (insert/delete/lookup).
// Per-sandbox operations acquire the Managed_Sandbox.lock instead,
// so different sandboxes can be operated on concurrently.
// ---------------------------------------------------------------------------

Sandbox_Manager :: struct {
	sandboxes:   map[string]^Managed_Sandbox,
	global_lock: sync.Mutex,
	config:      ^Config,
	allocator:   mem.Allocator, // allocator for the map and Managed_Sandbox pointers
}

// Initialize a Sandbox_Manager.
manager_init :: proc(config: ^Config, allocator := context.allocator) -> Sandbox_Manager {
	return Sandbox_Manager{
		sandboxes = make(map[string]^Managed_Sandbox, allocator = allocator),
		config    = config,
		allocator = allocator,
	}
}

// Look up a sandbox by ID. Returns nil if not found.
// Caller must hold global_lock or accept TOCTOU races for read-only checks.
manager_get :: proc(mgr: ^Sandbox_Manager, id: string) -> ^Managed_Sandbox {
	sync.mutex_lock(&mgr.global_lock)
	defer sync.mutex_unlock(&mgr.global_lock)
	return mgr.sandboxes[id]
}

// Register a newly created Managed_Sandbox in the manager's map.
manager_register :: proc(mgr: ^Sandbox_Manager, ms: ^Managed_Sandbox) {
	sync.mutex_lock(&mgr.global_lock)
	defer sync.mutex_unlock(&mgr.global_lock)
	mgr.sandboxes[ms.sandbox.id] = ms
}

// Remove a sandbox from the manager and destroy it.
// Acquires global lock to remove from map, then per-sandbox lock to destroy.
manager_remove :: proc(mgr: ^Sandbox_Manager, id: string) {
	sync.mutex_lock(&mgr.global_lock)
	ms, found := mgr.sandboxes[id]
	if found {
		delete_key(&mgr.sandboxes, id)
	}
	sync.mutex_unlock(&mgr.global_lock)

	if found {
		sync.mutex_lock(&ms.lock)
		managed_sandbox_destroy(ms)
		sync.mutex_unlock(&ms.lock)
		free(ms, mgr.allocator)
	}
}

// Return the number of active sandboxes.
manager_count :: proc(mgr: ^Sandbox_Manager) -> int {
	sync.mutex_lock(&mgr.global_lock)
	defer sync.mutex_unlock(&mgr.global_lock)
	return len(mgr.sandboxes)
}

// Check whether adding another sandbox would exceed the configured limit.
manager_at_capacity :: proc(mgr: ^Sandbox_Manager) -> bool {
	return manager_count(mgr) >= mgr.config.max_sandboxes
}

// Iterate over all sandboxes (snapshot of IDs under lock, then yield each).
// The callback receives the Managed_Sandbox pointer. The per-sandbox lock
// is NOT held — callers that mutate must acquire it themselves.
manager_for_each :: proc(mgr: ^Sandbox_Manager, callback: proc(ms: ^Managed_Sandbox)) {
	// Snapshot the values under the global lock to avoid holding it during callbacks.
	sync.mutex_lock(&mgr.global_lock)
	count := len(mgr.sandboxes)
	snapshot := make([]^Managed_Sandbox, count, context.temp_allocator)
	i := 0
	for _, ms in mgr.sandboxes {
		snapshot[i] = ms
		i += 1
	}
	sync.mutex_unlock(&mgr.global_lock)

	for ms in snapshot {
		callback(ms)
	}
}

// Sandbox directory path for a given ID.
manager_sandbox_dir :: proc(mgr: ^Sandbox_Manager, id: string) -> string {
	return fmt.tprintf("%s/sandboxes/%s", mgr.config.data_dir, id)
}

// ---------------------------------------------------------------------------
// manager_create_sandbox — full API-facing sandbox creation
//
// 1. Check capacity
// 2. Check for duplicate ID
// 3. Allocate Managed_Sandbox with arena
// 4. Run sandbox_create with arena allocator in context
// 5. Register in map on success, clean up on failure
// ---------------------------------------------------------------------------

Manager_Create_Error :: enum {
	None,
	At_Capacity,
	Already_Exists,
	Create_Failed,
}

manager_create_sandbox :: proc(
	mgr: ^Sandbox_Manager,
	id: string,
	opts: Create_Options,
) -> (^Managed_Sandbox, Manager_Create_Error) {
	// Hold global_lock for capacity check, duplicate check, AND slot reservation
	// to prevent TOCTOU races between concurrent create requests.
	sync.mutex_lock(&mgr.global_lock)

	if len(mgr.sandboxes) >= mgr.config.max_sandboxes {
		sync.mutex_unlock(&mgr.global_lock)
		return nil, .At_Capacity
	}

	if mgr.sandboxes[id] != nil {
		sync.mutex_unlock(&mgr.global_lock)
		return nil, .Already_Exists
	}

	// Build sandbox directory path
	sdir := manager_sandbox_dir(mgr, id)

	// Initialize Managed_Sandbox with arena (placeholder in Creating state)
	ms := managed_sandbox_init(id, sdir, opts, mgr.allocator)

	// Reserve the slot with the placeholder before releasing the lock
	mgr.sandboxes[ms.sandbox.id] = ms
	sync.mutex_unlock(&mgr.global_lock)

	// Slow I/O (sandbox_create) runs outside the global lock
	sa := managed_sandbox_allocator(ms)
	context.allocator = sa

	create_err: Create_Error
	if mgr.config.backend == .Firecracker {
		create_err = sandbox_create_firecracker(mgr.config, &ms.sandbox, opts, sa)
	} else {
		create_err = sandbox_create(mgr.config, &ms.sandbox, opts, sa)
	}
	if create_err != .None {
		// Creation failed — remove placeholder from map
		sync.mutex_lock(&mgr.global_lock)
		delete_key(&mgr.sandboxes, ms.sandbox.id)
		sync.mutex_unlock(&mgr.global_lock)

		mem.dynamic_arena_destroy(&ms.arena)
		free(ms, mgr.allocator)
		return nil, .Create_Failed
	}

	return ms, .None
}

// ---------------------------------------------------------------------------
// manager_destroy_sandbox — remove from map and destroy
//
// This is the canonical way to destroy a sandbox from external callers
// (API handlers, reaper thread). It:
//   1. Removes the sandbox from the map under global lock
//   2. Acquires per-sandbox lock
//   3. Calls sandbox_destroy for resource teardown
//   4. Removes the sandbox directory tree
//   5. Frees the arena and Managed_Sandbox pointer
// ---------------------------------------------------------------------------

manager_destroy_sandbox :: proc(mgr: ^Sandbox_Manager, id: string) {
	// Remove from map under global lock
	sync.mutex_lock(&mgr.global_lock)
	ms, found := mgr.sandboxes[id]
	if found {
		delete_key(&mgr.sandboxes, id)
	}
	sync.mutex_unlock(&mgr.global_lock)

	if !found {
		return
	}

	// Acquire per-sandbox lock for teardown
	sync.mutex_lock(&ms.lock)

	// Ephemeral mode: auto-snapshot before destroying so S3 has latest state
	if _ephemeral_enabled(mgr.config) {
		_ephemeral_auto_snapshot(mgr.config, ms)
	}

	// Tear down all sandbox resources — dispatch by backend
	if mgr.config.backend == .Firecracker {
		sandbox_destroy_firecracker(&ms.sandbox)
	} else {
		sandbox_destroy(&ms.sandbox)
	}

	// Remove the sandbox directory tree
	_remove_dir_recursive(ms.sandbox.dir)

	sync.mutex_unlock(&ms.lock)

	// Free arena (reclaims all per-sandbox allocations at once) and the pointer
	mem.dynamic_arena_destroy(&ms.arena)
	free(ms, mgr.allocator)

	fmt.printfln("[manager] destroyed sandbox %s", id)
}

// Recover already-remounted sandboxes from disk into the in-memory manager.
// Intended to run after init_run() remount_surviving_sandboxes.
manager_recover_from_disk :: proc(mgr: ^Sandbox_Manager) {
	sbox_dir := sandboxes_dir(mgr.config)
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

	recovered := 0
	for entry in entries {
		if !entry.is_dir {
			continue
		}
		id := _basename(entry.fullpath)
		if !valid_id(id) {
			continue
		}
		if manager_get(mgr, id) != nil {
			continue
		}

		ms := _recover_managed_sandbox(mgr, id, entry.fullpath)
		if ms == nil {
			continue
		}
		manager_register(mgr, ms)
		recovered += 1
	}

	if recovered > 0 {
		fmt.printfln("[manager] recovered %d sandbox(es) into registry", recovered)
	}
}

_recover_managed_sandbox :: proc(
	mgr: ^Sandbox_Manager,
	id: string,
	sdir: string,
) -> ^Managed_Sandbox {
	owner := _read_meta_string(sdir, "owner", "anon")
	task := _read_meta_string(sdir, "task", "")
	max_lifetime_s := _read_meta_u64(sdir, "max_lifetime_s", 0)

	opts := Create_Options{
		owner          = owner,
		task           = task,
		layers         = nil,
		cpu            = 2.0,
		memory_mb      = 1024,
		max_lifetime_s = max_lifetime_s,
		allow_net      = nil,
	}
	ms := managed_sandbox_init(id, sdir, opts, mgr.allocator)
	if ms == nil {
		return nil
	}

	sa := managed_sandbox_allocator(ms)

	// Firecracker backend recovery: runtime resources live in the VM.
	// Keep manager state lightweight and avoid reconstructing chroot-specific
	// mount/cgroup/netns handles from on-disk layout.
	if mgr.config.backend == .Firecracker {
		empty_sqfs := make([dynamic]Squashfs_Mount, allocator = sa)
		mounts := Sandbox_Mounts{
			sqfs_mounts = empty_sqfs,
		}
		sandbox_set_ready(&ms.sandbox, mounts, nil, nil)
		ms.sandbox.max_lifetime_s = max_lifetime_s
		ms.sandbox.last_active = time.now()
		return ms
	}

	// Reconstruct mount handles from persisted layout.
	sqfs_mounts := make([dynamic]Squashfs_Mount, allocator = sa)
	layers_raw := _read_meta_string(sdir, "layers", "")
	if len(layers_raw) > 0 {
		for layer in strings.split(layers_raw, ",", context.temp_allocator) {
			layer_trimmed := strings.trim_space(layer)
			if len(layer_trimmed) == 0 {
				continue
			}
			mp := fmt.tprintf("%s/images/%s.squashfs", sdir, layer_trimmed)
			if os.exists(mp) {
				append(&sqfs_mounts, Squashfs_Mount{
					mount_point = strings_clone(mp, sa),
					active      = true,
				})
			}
		}
	}

	snapshot_mount: Maybe(Squashfs_Mount)
	snap_mp := fmt.tprintf("%s/images/_snapshot", sdir)
	if os.exists(snap_mp) {
		snapshot_mount = Squashfs_Mount{
			mount_point = strings_clone(snap_mp, sa),
			active      = true,
		}
	}

	upper_mp := fmt.tprintf("%s/upper", sdir)
	merged_mp := fmt.tprintf("%s/merged", sdir)
	mounts := Sandbox_Mounts{
		sqfs_mounts    = sqfs_mounts,
		snapshot_mount = snapshot_mount,
		tmpfs          = Tmpfs_Mount{
			mount_point = strings_clone(upper_mp, sa),
			active      = true,
		},
		overlay        = Overlay_Mount{
			merged_path = strings_clone(merged_mp, sa),
			active      = true,
		},
	}

	// Recreate cgroup/netns handles best-effort from metadata.
	cgroup: Maybe(Cgroup_Handle)
	cg_path := fmt.tprintf("/sys/fs/cgroup/squash-%s", id)
	if os.exists(cg_path) {
		cgroup = Cgroup_Handle{path = strings_clone(cg_path, sa)}
	}

	netns: Maybe(Netns_Handle)
	netns_index := _read_meta_u64(sdir, "netns_index", 0)
	if netns_index > 0 && netns_index <= 254 {
		netns = Netns_Handle{
			name       = strings_clone(fmt.tprintf("squash-%s", id), sa),
			index      = u8(netns_index),
			veth_host  = strings_clone(fmt.tprintf("sq-%s-h", id), sa),
			chain_name = nil,
			host_dns   = nil,
		}
	}

	sandbox_set_ready(&ms.sandbox, mounts, netns, cgroup)
	ms.sandbox.max_lifetime_s = max_lifetime_s
	ms.sandbox.last_active = time.now()
	return ms
}

_read_meta_string :: proc(sdir: string, name: string, fallback: string) -> string {
	path := fmt.tprintf("%s/.meta/%s", sdir, name)
	data, ok := os.read_entire_file(path, context.temp_allocator)
	if !ok {
		return fallback
	}
	trimmed := strings.trim_space(string(data))
	if len(trimmed) == 0 {
		return fallback
	}
	return trimmed
}

_read_meta_u64 :: proc(sdir: string, name: string, fallback: u64) -> u64 {
	s := _read_meta_string(sdir, name, "")
	if len(s) == 0 {
		return fallback
	}
	value: u64
	for ch in s {
		if ch < '0' || ch > '9' {
			return fallback
		}
		value = value * 10 + u64(ch - '0')
	}
	return value
}

// Destroy the manager: destroy all sandboxes and free the map.
manager_destroy :: proc(mgr: ^Sandbox_Manager) {
	sync.mutex_lock(&mgr.global_lock)
	for _, ms in mgr.sandboxes {
		managed_sandbox_destroy(ms)
		free(ms, mgr.allocator)
	}
	delete(mgr.sandboxes)
	sync.mutex_unlock(&mgr.global_lock)
}
