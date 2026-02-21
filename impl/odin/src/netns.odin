package squashd

// Network namespace — REMOVED in unprivileged mode.
// Sandboxes use bubblewrap for isolation.

Netns_Handle :: struct {
	name: string,
}

setup_netns :: proc(id: string, allow_net: []string) -> (Maybe(Netns_Handle), bool) {
	return nil, true
}

teardown_netns :: proc(handle: ^Netns_Handle) {}
