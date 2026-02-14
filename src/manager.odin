package squashd

import "core:mem"
import "core:sync"
import "core:time"
import "core:fmt"

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
