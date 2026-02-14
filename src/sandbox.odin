package squashd

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

// Destroy sandbox resources. Tears down in reverse creation order.
sandbox_destroy :: proc(s: ^Sandbox) {
	if ready, ok := &s.state.(Ready); ok {
		sandbox_mounts_destroy(&ready.mounts)
		if cg, cg_ok := &ready.cgroup.?; cg_ok {
			cgroup_destroy(cg)
		}
		if ns, ns_ok := &ready.netns.?; ns_ok {
			netns_teardown(ns)
		}
	}
	s.state = Destroying{}
}
