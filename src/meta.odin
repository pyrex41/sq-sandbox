package squashd

import "core:fmt"
import "core:os"
import "core:strings"
import "core:time"

// ---------------------------------------------------------------------------
// Metadata — filesystem-backed metadata read/write for sandboxes
//
// Layout: {sandbox_dir}/.meta/
//   owner, task, layers, created, last_active, cpu, memory_mb,
//   max_lifetime_s, allow_net, netns_index, log/
// ---------------------------------------------------------------------------

Meta_Error :: enum {
	None,
	Mkdir_Failed,
	Write_Failed,
}

// Write all sandbox metadata files into {sandbox_dir}/.meta/.
// Called once during sandbox creation after all resources are acquired.
write_meta :: proc(
	sandbox_dir: string,
	id: string,
	opts: Create_Options,
	netns_index: u8,
) -> Meta_Error {
	meta_dir := fmt.tprintf("%s/.meta", sandbox_dir)
	_ensure_dir_recursive(meta_dir)

	log_dir := fmt.tprintf("%s/log", meta_dir)
	_ensure_dir_recursive(log_dir)

	// Write individual metadata files
	_write_meta_file(meta_dir, "owner", opts.owner) or_return
	_write_meta_file(meta_dir, "task", opts.task) or_return

	// Layers as comma-separated string
	layers_str: string
	if len(opts.layers) > 0 {
		layers_str = strings.join(opts.layers, ",", context.temp_allocator)
	}
	_write_meta_file(meta_dir, "layers", layers_str) or_return

	// Timestamps
	now_str := _format_iso8601(time.now())
	_write_meta_file(meta_dir, "created", now_str) or_return
	_write_meta_file(meta_dir, "last_active", now_str) or_return

	// Resource limits
	_write_meta_file(meta_dir, "cpu", fmt.tprintf("%.1f", opts.cpu)) or_return
	_write_meta_file(meta_dir, "memory_mb", fmt.tprintf("%d", opts.memory_mb)) or_return
	_write_meta_file(meta_dir, "max_lifetime_s", fmt.tprintf("%d", opts.max_lifetime_s)) or_return

	// Netns index (if > 0)
	if netns_index > 0 {
		_write_meta_file(meta_dir, "netns_index", fmt.tprintf("%d", netns_index)) or_return
	}

	// Allow net (JSON array or empty)
	if nets, ok := opts.allow_net.?; ok {
		// Build JSON array: ["host1","host2"]
		buf: [2048]byte
		b := strings.builder_from_bytes(buf[:])
		strings.write_byte(&b, '[')
		for host, i in nets {
			if i > 0 {
				strings.write_byte(&b, ',')
			}
			strings.write_byte(&b, '"')
			strings.write_string(&b, host)
			strings.write_byte(&b, '"')
		}
		strings.write_byte(&b, ']')
		_write_meta_file(meta_dir, "allow_net", strings.to_string(b)) or_return
	}

	return .None
}

// Seed /etc/resolv.conf in the sandbox's overlay upper layer.
// When the sandbox has a network namespace, DNS goes through the gateway;
// otherwise, copy the host's resolv.conf.
seed_resolv_conf :: proc(
	sandbox_dir: string,
	netns_index: u8,
) -> Meta_Error {
	etc_dir := fmt.tprintf("%s/upper/data/etc", sandbox_dir)
	_ensure_dir_recursive(etc_dir)

	resolv_path := fmt.tprintf("%s/resolv.conf", etc_dir)

	content: string
	if netns_index > 0 {
		// DNS through netns gateway
		content = fmt.tprintf("nameserver 10.200.%d.1\n", netns_index)
	} else {
		// Copy host resolv.conf
		if host_data, ok := os.read_entire_file("/etc/resolv.conf", context.temp_allocator); ok {
			content = string(host_data)
		} else {
			content = "nameserver 8.8.8.8\n"
		}
	}

	if !os.write_entire_file(resolv_path, transmute([]byte)content) {
		return .Write_Failed
	}

	return .None
}

// Update the last_active timestamp for a sandbox.
update_last_active :: proc(sandbox_dir: string) {
	meta_dir := fmt.tprintf("%s/.meta", sandbox_dir)
	now_str := _format_iso8601(time.now())
	path := fmt.tprintf("%s/last_active", meta_dir)
	os.write_entire_file(path, transmute([]byte)now_str)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

_write_meta_file :: proc(meta_dir: string, name: string, value: string) -> Meta_Error {
	path := fmt.tprintf("%s/%s", meta_dir, name)
	if !os.write_entire_file(path, transmute([]byte)value) {
		return .Write_Failed
	}
	return .None
}
