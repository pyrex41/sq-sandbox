package squashd

import "core:fmt"
import "core:os"
import "core:strings"

// ---------------------------------------------------------------------------
// Network namespace handle and errors
// ---------------------------------------------------------------------------

Netns_Handle :: struct {
	name:       string, // "squash-{id}"
	index:      u8, // 1-254
	veth_host:  string, // "sq-{id}-h"
	chain_name: Maybe(string),
	host_dns:   Maybe(string),
}

Netns_Error :: enum {
	None,
	Index_Exhausted,
	Create_Failed,
	Veth_Failed,
	Config_Failed,
}

// Set up a network namespace for sandbox isolation.
// Creates netns, veth pair, configures addresses, NAT, DNS forwarding,
// and optional egress filtering.
netns_setup :: proc(
	config: ^Config,
	id: string,
	allow_net: Maybe([]string),
	allocator := context.allocator,
) -> (handle: Netns_Handle, err: Netns_Error) {
	// Allocate a unique index under flock
	index, idx_err := _allocate_netns_index(config)
	if idx_err != .None {
		return {}, idx_err
	}

	name := fmt.aprintf("squash-%s", id, allocator = allocator)
	veth_host := fmt.aprintf("sq-%s-h", id, allocator = allocator)
	veth_sandbox := fmt.tprintf("sq-%s-s", id)

	// Create netns
	if !run_cmd("ip", "netns", "add", name) {
		return {}, .Create_Failed
	}

	// Create veth pair
	if !run_cmd("ip", "link", "add", veth_host, "type", "veth", "peer", "name", veth_sandbox) {
		run_cmd("ip", "netns", "delete", name) // rollback
		return {}, .Veth_Failed
	}

	// Move sandbox end into netns
	run_cmd("ip", "link", "set", veth_sandbox, "netns", name)

	// Configure host end
	host_addr := fmt.tprintf("10.200.%d.1/30", index)
	run_cmd("ip", "addr", "add", host_addr, "dev", veth_host)
	run_cmd("ip", "link", "set", veth_host, "up")

	// Configure sandbox end (inside namespace)
	sandbox_addr := fmt.tprintf("10.200.%d.2/30", index)
	gateway := fmt.tprintf("10.200.%d.1", index)
	run_cmd("ip", "netns", "exec", name, "ip", "addr", "add", sandbox_addr, "dev", veth_sandbox)
	run_cmd("ip", "netns", "exec", name, "ip", "link", "set", veth_sandbox, "up")
	run_cmd("ip", "netns", "exec", name, "ip", "link", "set", "lo", "up")
	run_cmd("ip", "netns", "exec", name, "ip", "route", "add", "default", "via", gateway)

	// Enable IP forwarding
	os.write_entire_file("/proc/sys/net/ipv4/ip_forward", transmute([]byte)string("1"))

	// NAT on host for outbound traffic
	subnet := fmt.tprintf("10.200.%d.0/30", index)
	run_cmd("iptables", "-t", "nat", "-A", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE")

	// DNS DNAT — redirect DNS queries to gateway to host's real resolver
	host_dns := _parse_first_nameserver()
	if dns, ok := host_dns.?; ok {
		run_cmd("iptables", "-t", "nat", "-A", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "udp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
		run_cmd("iptables", "-t", "nat", "-A", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "tcp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
	}

	// Egress filtering
	chain_name: Maybe(string)
	if nets, ok := allow_net.?; ok && len(nets) > 0 {
		chain_name = _apply_egress_rules(id, veth_host, nets, allocator)
	}

	return Netns_Handle{
		name       = name,
		index      = index,
		veth_host  = veth_host,
		chain_name = chain_name,
		host_dns   = host_dns,
	}, .None
}

// Tear down a network namespace. Idempotent — safe to call multiple times.
// Reverse-order cleanup: iptables chain, NAT, DNS DNAT, veth, netns.
netns_teardown :: proc(h: ^Netns_Handle) {
	if len(h.name) == 0 {
		return
	}

	// Remove egress chain if present
	if chain, ok := h.chain_name.?; ok {
		run_cmd("iptables", "-D", "FORWARD", "-i", h.veth_host, "-j", chain)
		run_cmd("iptables", "-F", chain)
		run_cmd("iptables", "-X", chain)
	}

	// Remove NAT rules
	subnet := fmt.tprintf("10.200.%d.0/30", h.index)
	run_cmd("iptables", "-t", "nat", "-D", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE")

	// Remove DNS DNAT rules
	gateway := fmt.tprintf("10.200.%d.1", h.index)
	if dns, ok := h.host_dns.?; ok {
		run_cmd("iptables", "-t", "nat", "-D", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "udp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
		run_cmd("iptables", "-t", "nat", "-D", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "tcp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
	}

	// Delete veth pair (host end — peer is auto-deleted)
	if len(h.veth_host) > 0 {
		run_cmd("ip", "link", "delete", h.veth_host)
	}

	// Delete network namespace
	run_cmd("ip", "netns", "delete", h.name)

	h.name = ""
}

// ---------------------------------------------------------------------------
// Netns index allocation — flock-protected sequential scan
// ---------------------------------------------------------------------------

_allocate_netns_index :: proc(config: ^Config) -> (u8, Netns_Error) {
	// Acquire exclusive lock
	lock_path := fmt.tprintf("%s/.netns-index.lock", config.data_dir)
	lock_cstr := strings.clone_to_cstring(lock_path, context.temp_allocator)
	lock_fd := c_open(lock_cstr, O_RDWR | O_CREAT | O_CLOEXEC, 0o644)
	if lock_fd < 0 {
		return 0, .Index_Exhausted
	}
	c_flock(lock_fd, LOCK_EX)
	defer {
		c_flock(lock_fd, LOCK_UN)
		c_close(lock_fd)
	}

	// Scan existing sandbox metadata for used indices
	sbox_dir := sandboxes_dir(config)
	dh, derr := os.open(sbox_dir)
	if derr != nil {
		// No sandboxes dir yet — index 1 is available
		return 1, .None
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return 1, .None
	}
	defer delete(entries)

	// Collect used indices
	used: [256]bool

	for entry in entries {
		if !entry.is_dir {
			continue
		}
		idx_path := fmt.tprintf("%s/.meta/netns_index", entry.fullpath)
		if data, ok := os.read_entire_file(idx_path, context.temp_allocator); ok {
			trimmed := strings.trim_space(string(data))
			idx := 0
			for ch in trimmed {
				if ch >= '0' && ch <= '9' {
					idx = idx * 10 + int(ch - '0')
				} else {
					break
				}
			}
			if idx >= 1 && idx <= 254 {
				used[idx] = true
			}
		}
	}

	// Find first unused index
	for i := u8(1); i <= 254; i += 1 {
		if !used[i] {
			return i, .None
		}
	}

	return 0, .Index_Exhausted
}

// Parse the first nameserver from /etc/resolv.conf.
_parse_first_nameserver :: proc() -> Maybe(string) {
	data, ok := os.read_entire_file("/etc/resolv.conf", context.temp_allocator)
	if !ok {
		return nil
	}

	content := string(data)
	for line in strings.split_lines_iterator(&content) {
		trimmed := strings.trim_left_space(line)
		if strings.has_prefix(trimmed, "nameserver") {
			rest := strings.trim_left_space(trimmed[len("nameserver"):])
			// Take the first whitespace-delimited token
			end := strings.index_byte(rest, ' ')
			if end >= 0 {
				rest = rest[:end]
			}
			if len(rest) > 0 {
				return rest
			}
		}
	}
	return nil
}

// Apply egress filtering rules via iptables.
// Creates a custom chain and links it to FORWARD for the host veth interface.
_apply_egress_rules :: proc(
	id: string,
	iface: string,
	hosts: []string,
	allocator := context.allocator,
) -> string {
	chain := fmt.aprintf("squash-%s", id, allocator = allocator)

	run_cmd("iptables", "-N", chain)
	run_cmd("iptables", "-A", "FORWARD", "-i", iface, "-j", chain)

	// Block ICMP (prevents tunneling)
	run_cmd("iptables", "-A", chain, "-p", "icmp", "-j", "DROP")

	// Rate-limited DNS (prevents DNS tunneling)
	run_cmd("iptables", "-A", chain, "-p", "udp", "--dport", "53",
		"-m", "limit", "--limit", "10/s", "--limit-burst", "20", "-j", "ACCEPT")
	run_cmd("iptables", "-A", chain, "-p", "tcp", "--dport", "53",
		"-m", "limit", "--limit", "10/s", "--limit-burst", "20", "-j", "ACCEPT")

	// Allow established/related connections
	run_cmd("iptables", "-A", chain, "-m", "state", "--state", "ESTABLISHED,RELATED", "-j", "ACCEPT")

	// Allow each host
	for host in hosts {
		if host == "none" {
			continue
		}
		if strings.contains(host, "*") {
			// Wildcards need proxy mode — skip with warning
			fmt.printfln("[net] WARN: wildcard %s requires proxy mode", host)
			continue
		}
		// Shell out to resolve host and apply iptables rules per IP
		run_cmd("sh", "-c",
			fmt.tprintf("for ip in $(getent hosts %s 2>/dev/null | awk '{print $1}'); do iptables -A %s -d $ip -j ACCEPT; done", host, chain))
	}

	// Default: drop
	run_cmd("iptables", "-A", chain, "-j", "DROP")

	return chain
}
