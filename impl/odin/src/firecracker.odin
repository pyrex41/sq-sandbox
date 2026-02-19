package squashd

import "core:c"
import "core:encoding/json"
import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:time"

// ---------------------------------------------------------------------------
// Firecracker backend — VM lifecycle via sq-firecracker CLI, exec via vsock
//
// CID allocation:  file-based counter with flock
// Networking:      tap devices + iptables NAT (10.0.{index}.1/30)
// VM lifecycle:    sq-firecracker start/stop CLI
// Exec:           socat VSOCK-CONNECT to guest agent on port 5000
// Drive hot-add:  sq-firecracker add-drive + vsock __squash_remount
// ---------------------------------------------------------------------------

Firecracker_Error :: enum {
	None,
	CID_Alloc_Failed,
	Network_Setup_Failed,
	Network_Teardown_Failed,
	VM_Start_Failed,
	VM_Stop_Failed,
	Exec_Failed,
	Exec_Parse_Failed,
	Drive_Add_Failed,
	Remount_Failed,
}

// ---------------------------------------------------------------------------
// CID allocation — file-based counter protected by flock
//
// The counter file lives at {data_dir}/.fc-cid-counter.
// CIDs start at 100 to avoid conflicts with well-known CIDs (0=hypervisor,
// 1=reserved, 2=host).
// ---------------------------------------------------------------------------

allocate_cid :: proc(data_dir: string) -> (u32, Firecracker_Error) {
	counter_path := fmt.tprintf("%s/.fc-cid-counter", data_dir)
	lock_path := fmt.tprintf("%s/.fc-cid-counter.lock", data_dir)

	// Acquire exclusive lock
	lock_handle, lock_err := os.open(lock_path, os.O_RDWR | os.O_CREATE | os.O_CLOEXEC, 0o644)
	if lock_err != nil {
		return 0, .CID_Alloc_Failed
	}
	lock_fd := c.int(lock_handle)
	c_flock(lock_fd, LOCK_EX)
	defer {
		c_flock(lock_fd, LOCK_UN)
		c_close(lock_fd)
	}

	// Read current counter value
	current: u32 = 100
	if data, ok := os.read_entire_file(counter_path, context.temp_allocator); ok {
		trimmed := strings.trim_space(string(data))
		if len(trimmed) > 0 {
			val, parse_ok := strconv.parse_uint(trimmed, 10)
			if parse_ok {
				current = u32(val)
				if current < 100 {
					current = 100
				}
			}
		}
	}

	// Allocate and increment
	cid := current
	next_val := fmt.tprintf("%d", current + 1)
	os.write_entire_file(counter_path, transmute([]byte)next_val)

	return cid, .None
}

// ---------------------------------------------------------------------------
// Tap device network setup — 10.0.{index}.1/30 scheme
//
// Creates a tap device (sq-{id}-tap), assigns IP, sets up NAT via iptables.
// The guest gets 10.0.{index}.2 and routes through 10.0.{index}.1.
// ---------------------------------------------------------------------------

firecracker_setup_network :: proc(id: string, index: int, allow_net: Maybe([]string)) -> Firecracker_Error {
	tap_name := fmt.tprintf("sq-%s-tap", id)
	host_addr := fmt.tprintf("10.0.%d.1/30", index)
	subnet := fmt.tprintf("10.0.%d.0/30", index)

	// Create tap device
	if !run_cmd("ip", "tuntap", "add", "dev", tap_name, "mode", "tap") {
		return .Network_Setup_Failed
	}

	// Assign address and bring up
	if !run_cmd("ip", "addr", "add", host_addr, "dev", tap_name) {
		run_cmd("ip", "tuntap", "del", "dev", tap_name, "mode", "tap")
		return .Network_Setup_Failed
	}
	run_cmd("ip", "link", "set", tap_name, "up")

	// Enable IP forwarding
	os.write_entire_file("/proc/sys/net/ipv4/ip_forward", transmute([]byte)string("1"))

	// NAT for outbound traffic
	run_cmd("iptables", "-t", "nat", "-A", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE")

	// DNS DNAT — redirect DNS queries from guest to host resolver
	gateway := fmt.tprintf("10.0.%d.1", index)
	host_dns := _parse_first_nameserver()
	if dns, ok := host_dns.?; ok {
		run_cmd("iptables", "-t", "nat", "-A", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "udp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
		run_cmd("iptables", "-t", "nat", "-A", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "tcp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
	}

	// Egress filtering (if allow_net is specified)
	if nets, ok := allow_net.?; ok && len(nets) > 0 {
		chain := fmt.tprintf("squash-fc-%s", id)
		_apply_egress_rules(chain, tap_name, nets)
	}

	return .None
}

// Tear down tap device networking. Idempotent.
firecracker_teardown_network :: proc(id: string, index: int) -> Firecracker_Error {
	tap_name := fmt.tprintf("sq-%s-tap", id)
	subnet := fmt.tprintf("10.0.%d.0/30", index)
	gateway := fmt.tprintf("10.0.%d.1", index)

	// Remove egress chain if present
	chain := fmt.tprintf("squash-fc-%s", id)
	run_cmd("iptables", "-D", "FORWARD", "-i", tap_name, "-j", chain)
	run_cmd("iptables", "-F", chain)
	run_cmd("iptables", "-X", chain)

	// Remove NAT rules
	run_cmd("iptables", "-t", "nat", "-D", "POSTROUTING", "-s", subnet, "-j", "MASQUERADE")

	// Remove DNS DNAT rules
	host_dns := _parse_first_nameserver()
	if dns, ok := host_dns.?; ok {
		run_cmd("iptables", "-t", "nat", "-D", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "udp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
		run_cmd("iptables", "-t", "nat", "-D", "PREROUTING",
			"-s", subnet, "-d", gateway, "-p", "tcp", "--dport", "53",
			"-j", "DNAT", "--to-destination", dns)
	}

	// Delete tap device
	run_cmd("ip", "tuntap", "del", "dev", tap_name, "mode", "tap")

	return .None
}

// ---------------------------------------------------------------------------
// VM lifecycle — shell out to sq-firecracker CLI
// ---------------------------------------------------------------------------

// Start a Firecracker VM with the given parameters.
// sq-firecracker start {id} {cpu} {mem} {sqfs1} [sqfs2] ...
firecracker_start_vm :: proc(
	id: string,
	cpu: f64,
	memory_mb: int,
	squashfs_paths: []string,
	cid: u32,
	meta_dir: string,
) -> Firecracker_Error {
	// Build args: sq-firecracker start {id} --cpu {cpu} --mem {mem} --cid {cid} {paths...}
	args := make([dynamic]string, 0, 8 + len(squashfs_paths), context.temp_allocator)
	append(&args, "sq-firecracker", "start", id)
	append(&args, "--cpu", fmt.tprintf("%.1f", cpu))
	append(&args, "--mem", fmt.tprintf("%d", memory_mb))
	append(&args, "--cid", fmt.tprintf("%d", cid))
	for path in squashfs_paths {
		append(&args, path)
	}

	if !run_cmd(..args[:]) {
		return .VM_Start_Failed
	}

	// Write CID to metadata
	cid_path := fmt.tprintf("%s/fc.cid", meta_dir)
	cid_str := fmt.tprintf("%d", cid)
	os.write_entire_file(cid_path, transmute([]byte)cid_str)

	return .None
}

// Stop a Firecracker VM.
firecracker_stop_vm :: proc(id: string, meta_dir: string) -> Firecracker_Error {
	if !run_cmd("sq-firecracker", "stop", id) {
		return .VM_Stop_Failed
	}
	return .None
}

// ---------------------------------------------------------------------------
// Exec via vsock — socat to guest agent
//
// Builds a JSON request: {"cmd":"...","workdir":"/","timeout":300}
// Pipes it through: socat -T{timeout} - VSOCK-CONNECT:{cid}:5000
// Parses JSON response: {"exit_code":0,"stdout":"...","stderr":"..."}
// ---------------------------------------------------------------------------

Vsock_Response :: struct {
	exit_code: i32    `json:"exit_code"`,
	stdout:    string `json:"stdout"`,
	stderr:    string `json:"stderr"`,
}

firecracker_exec :: proc(
	cid: u32,
	cmd: string,
	workdir: string,
	timeout_s: int,
	allocator := context.allocator,
) -> (Exec_Result, Firecracker_Error) {
	started := time.now()

	// Build JSON request
	req_buf: [65536]byte
	req_b := strings.builder_from_bytes(req_buf[:])
	strings.write_string(&req_b, `{"cmd":`)
	_write_json_string(&req_b, cmd)
	strings.write_string(&req_b, `,"workdir":`)
	_write_json_string(&req_b, workdir if len(workdir) > 0 else "/")
	fmt.sbprintf(&req_b, `,"timeout":%d}`, timeout_s)
	json_req := strings.to_string(req_b)

	// Create pipes: stdin for socat input, stdout for socat output
	stdin_fds: [2]c.int  // [0]=read, [1]=write
	stdout_fds: [2]c.int
	if c_pipe(&stdin_fds[0]) != 0 {
		return {}, .Exec_Failed
	}
	if c_pipe(&stdout_fds[0]) != 0 {
		c_close(stdin_fds[0])
		c_close(stdin_fds[1])
		return {}, .Exec_Failed
	}

	timeout_arg := fmt.ctprintf("-T%d", timeout_s)
	vsock_arg := fmt.ctprintf("VSOCK-CONNECT:%d:5000", cid)
	argv := [5]cstring{"socat", timeout_arg, "-", vsock_arg, nil}

	pid := c_fork()
	if pid < 0 {
		c_close(stdin_fds[0])
		c_close(stdin_fds[1])
		c_close(stdout_fds[0])
		c_close(stdout_fds[1])
		return {}, .Exec_Failed
	}

	if pid == 0 {
		// Child: wire pipes to stdin/stdout, exec socat
		c_close(stdin_fds[1])   // close write end of stdin pipe
		c_close(stdout_fds[0])  // close read end of stdout pipe
		c_dup2(stdin_fds[0], 0)   // stdin from pipe
		c_dup2(stdout_fds[1], 1)  // stdout to pipe
		c_close(stdin_fds[0])
		c_close(stdout_fds[1])
		c_execvp("socat", &argv[0])
		c_exit(127)
	}

	// Parent: write JSON request to socat's stdin, read response from stdout
	c_close(stdin_fds[0])   // close read end
	c_close(stdout_fds[1])  // close write end

	// Write request
	req_bytes := transmute([]byte)json_req
	c_write(stdin_fds[1], raw_data(req_bytes), c.size_t(len(req_bytes)))
	c_close(stdin_fds[1])  // signal EOF to socat

	// Read response
	resp_buf: [65536]byte
	resp_len: int = 0
	for resp_len < len(resp_buf) {
		n := c_read(stdout_fds[0], raw_data(resp_buf[resp_len:]), c.size_t(len(resp_buf) - resp_len))
		if n <= 0 {
			break
		}
		resp_len += int(n)
	}
	c_close(stdout_fds[0])

	// Wait for socat to exit
	status: c.int
	c_waitpid(pid, &status, 0)

	finished := time.now()

	if resp_len == 0 {
		if status != 0 {
			return Exec_Result{
				exit_code = 124,
				stdout    = "",
				stderr    = "vsock exec failed",
				started   = started,
				finished  = finished,
			}, .None
		}
		return {}, .Exec_Failed
	}

	// Parse JSON response
	resp: Vsock_Response
	if json.unmarshal(resp_buf[:resp_len], &resp, allocator = allocator) != nil {
		return {}, .Exec_Parse_Failed
	}

	return Exec_Result{
		exit_code = resp.exit_code,
		stdout    = resp.stdout,
		stderr    = resp.stderr,
		started   = started,
		finished  = finished,
	}, .None
}

// ---------------------------------------------------------------------------
// Drive hot-add — sq-firecracker CLI + vsock remount trigger
// ---------------------------------------------------------------------------

// Hot-add a squashfs drive to a running VM, then trigger guest remount.
firecracker_add_drive :: proc(
	id: string,
	drive_id: string,
	squashfs_path: string,
	cid: u32,
	meta_dir: string,
) -> Firecracker_Error {
	// Add drive via CLI
	if !run_cmd("sq-firecracker", "add-drive", id, drive_id, squashfs_path) {
		return .Drive_Add_Failed
	}

	// Trigger guest remount via vsock
	_, exec_err := firecracker_exec(cid, "__squash_remount", "/", 30)
	if exec_err != .None {
		return .Remount_Failed
	}

	return .None
}

// ---------------------------------------------------------------------------
// Vsock snapshot trigger
// ---------------------------------------------------------------------------

// Ask the guest agent to create a snapshot via vsock.
firecracker_snapshot_via_vsock :: proc(cid: u32) -> Firecracker_Error {
	_, err := firecracker_exec(cid, "__squash_snapshot", "/", 60)
	if err != .None {
		return .Exec_Failed
	}
	return .None
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Read the CID from a sandbox's metadata directory.
read_fc_cid :: proc(sandbox_dir: string) -> (u32, bool) {
	cid_path := fmt.tprintf("%s/.meta/fc.cid", sandbox_dir)
	data, ok := os.read_entire_file(cid_path, context.temp_allocator)
	if !ok {
		return 0, false
	}
	trimmed := strings.trim_space(string(data))
	val, parse_ok := strconv.parse_uint(trimmed, 10)
	if !parse_ok {
		return 0, false
	}
	return u32(val), true
}

// Read the tap index from a sandbox's metadata directory.
read_fc_tap_index :: proc(sandbox_dir: string) -> (int, bool) {
	idx_path := fmt.tprintf("%s/.meta/fc.tap_index", sandbox_dir)
	data, ok := os.read_entire_file(idx_path, context.temp_allocator)
	if !ok {
		return 0, false
	}
	trimmed := strings.trim_space(string(data))
	val, parse_ok := strconv.parse_int(trimmed, 10)
	if !parse_ok {
		return 0, false
	}
	return val, true
}
