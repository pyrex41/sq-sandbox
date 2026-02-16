package squashd

import "core:fmt"
import "core:sync"
import "core:thread"
import "core:time"

// ---------------------------------------------------------------------------
// Reaper — background thread that destroys expired sandboxes
//
// Replaces: bin/sq-reaper
//
// Runs every 10 seconds, checks each sandbox's max_lifetime_s against its
// created timestamp. If age > max_lifetime_s and max_lifetime_s > 0, the
// sandbox is destroyed via manager_remove.
//
// The reaper collects expired IDs under the global lock, then destroys them
// outside the lock to avoid holding it during teardown (which may involve
// unmounts, iptables calls, etc.).
// ---------------------------------------------------------------------------

REAPER_INTERVAL :: 10 * time.Second

// Start the reaper as a background thread. The thread runs until the process
// exits. The manager pointer must remain valid for the lifetime of the thread.
reaper_start :: proc(manager: ^Sandbox_Manager) -> ^thread.Thread {
	t := thread.create_and_start_with_data(manager, _reaper_loop)
	return t
}

_reaper_loop :: proc(data: rawptr) {
	manager := cast(^Sandbox_Manager)data

	for {
		time.sleep(REAPER_INTERVAL)
		_reaper_tick(manager)
	}
}

// Single reaper pass: find and destroy expired sandboxes.
// Exposed as a separate proc for testability.
_reaper_tick :: proc(manager: ^Sandbox_Manager) {
	// Phase 1: Collect expired sandbox IDs under the global lock.
	// We snapshot IDs rather than destroying inline to avoid holding
	// the global lock during teardown operations.
	expired: [dynamic]string
	defer delete(expired)

	sync.mutex_lock(&manager.global_lock)
	now := time.now()
	for id, ms in manager.sandboxes {
		max_lifetime := ms.sandbox.max_lifetime_s
		if max_lifetime == 0 {
			continue // 0 means unlimited
		}
		age := time.diff(ms.sandbox.created, now)
		if time.duration_seconds(age) > f64(max_lifetime) {
			append(&expired, id)
		}
	}
	sync.mutex_unlock(&manager.global_lock)

	// Phase 2: Destroy expired sandboxes outside the global lock.
	for id in expired {
		fmt.printfln("[reaper] destroying expired sandbox: %s", id)
		manager_destroy_sandbox(manager, id)
	}
}
