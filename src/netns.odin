package squashd

// ---------------------------------------------------------------------------
// Network namespace handle
// ---------------------------------------------------------------------------

Netns_Handle :: struct {
	name:       string, // "squash-{id}"
	index:      u8, // 1-254
	veth_host:  string, // "sq-{id}-h"
	chain_name: Maybe(string),
	host_dns:   Maybe(string),
}

// Tear down a network namespace: delete the netns and clean up the veth pair.
netns_teardown :: proc(h: ^Netns_Handle) {
	if len(h.name) > 0 {
		run_cmd("ip", "netns", "delete", h.name)
	}
	if len(h.veth_host) > 0 {
		run_cmd("ip", "link", "delete", h.veth_host)
	}
}
