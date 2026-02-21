package squashd

import "core:encoding/json"
import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:sync"

_api_config: ^Config
_api_manager: ^Sandbox_Manager

API_ROUTES: [11]Route = {
	{method = "GET", pattern = "/cgi-bin/health", handler = _handle_health},
	{method = "GET", pattern = "/cgi-bin/api/sandboxes", handler = _handle_list_sandboxes},
	{method = "POST", pattern = "/cgi-bin/api/sandboxes", handler = _handle_create_sandbox},
	{method = "GET", pattern = "/cgi-bin/api/sandboxes/*", handler = _handle_get_sandbox},
	{method = "DELETE", pattern = "/cgi-bin/api/sandboxes/*", handler = _handle_delete_sandbox},
	{method = "POST", pattern = "/cgi-bin/api/sandboxes/*/exec", handler = _handle_exec},
	{method = "GET", pattern = "/cgi-bin/api/sandboxes/*/logs", handler = _handle_logs},
	{method = "POST", pattern = "/cgi-bin/api/sandboxes/*/snapshot", handler = _handle_snapshot},
	{method = "POST", pattern = "/cgi-bin/api/sandboxes/*/restore", handler = _handle_restore},
	{method = "POST", pattern = "/cgi-bin/api/sandboxes/*/activate", handler = _handle_activate},
	{method = "GET", pattern = "/cgi-bin/api/modules", handler = _handle_list_modules},
}

api_init :: proc(config: ^Config, manager: ^Sandbox_Manager) {
	_api_config = config
	_api_manager = manager
}

api_routes :: proc() -> []Route {
	return API_ROUTES[:]
}

Create_Request_JSON :: struct {
	id:             string   `json:"id"`,
	owner:          string   `json:"owner"`,
	layers:         string   `json:"layers"`,
	task:           string   `json:"task"`,
	cpu:            f64      `json:"cpu"`,
	memory_mb:      u64      `json:"memory_mb"`,
	max_lifetime_s: u64      `json:"max_lifetime_s"`,
	allow_net:      []string `json:"allow_net"`,
}

Exec_Request_JSON :: struct {
	cmd:     string `json:"cmd"`,
	workdir: string `json:"workdir"`,
	timeout: u64    `json:"timeout"`,
}

Snapshot_Request_JSON :: struct {
	label: string `json:"label"`,
}

Restore_Request_JSON :: struct {
	label: string `json:"label"`,
}

Activate_Request_JSON :: struct {
	module: string `json:"module"`,
}

_handle_health :: proc(_: ^Http_Request, resp: ^Http_Response) {
	backend := "chroot"
	if _api_config.backend == .Firecracker {
		backend = "firecracker"
	}
	base_ready := os.exists(fmt.tprintf("%s/000-base-alpine.squashfs", modules_dir(_api_config)))
	mod_count := _count_squashfs_files(modules_dir(_api_config))
	sb_count := manager_count(_api_manager)

	body := fmt.tprintf(
		`{"status":"ok","backend":"%s","tailscale":{"status":"unknown","ip":""},"sandboxes":%d,"modules":%d,"base_ready":%s}`,
		backend,
		sb_count,
		mod_count,
		"true" if base_ready else "false",
	)
	_json_body(resp, 200, body)
}

_handle_list_sandboxes :: proc(_: ^Http_Request, resp: ^Http_Response) {
	buf: [65536]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_byte(&b, '[')

	first := true
	sync.mutex_lock(&_api_manager.global_lock)
	for _, ms in _api_manager.sandboxes {
		sync.mutex_lock(&ms.lock)
		if !first {
			strings.write_byte(&b, ',')
		}
		first = false
		_write_sandbox_json(&b, &ms.sandbox)
		sync.mutex_unlock(&ms.lock)
	}
	sync.mutex_unlock(&_api_manager.global_lock)

	strings.write_byte(&b, ']')
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] list sandboxes response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_handle_create_sandbox :: proc(req: ^Http_Request, resp: ^Http_Response) {
	if !_require_json(req, resp) {
		return
	}

	body: Create_Request_JSON
	if err := json.unmarshal(req.body, &body, allocator = context.temp_allocator); err != nil {
		_json_error(resp, 400, "invalid JSON body")
		return
	}

	if len(body.id) == 0 {
		_json_error(resp, 400, "id required")
		return
	}
	if !valid_id(body.id) {
		_json_error(resp, 400, "id: alphanumeric/dash/underscore only")
		return
	}

	layers := _parse_layers(body.layers)
	if len(layers) == 0 {
		layers = []string{"000-base-alpine"}
	}
	for layer in layers {
		if !valid_module(layer) {
			_json_error(resp, 400, fmt.tprintf("invalid layer name: %s", layer))
			return
		}
		if !module_exists(_api_config, layer) {
			_json_error(resp, 400, fmt.tprintf("module not found: %s", layer))
			return
		}
	}

	opts := Create_Options{
		owner          = body.owner if len(body.owner) > 0 else "anon",
		task           = body.task,
		layers         = layers,
		cpu            = body.cpu if body.cpu > 0 else 2.0,
		memory_mb      = body.memory_mb if body.memory_mb > 0 else 1024,
		max_lifetime_s = body.max_lifetime_s,
		allow_net      = body.allow_net if len(body.allow_net) > 0 else nil,
	}

	ms, cerr := manager_create_sandbox(_api_manager, body.id, opts)
	switch cerr {
	case .None:
		// Ephemeral mode: auto-restore latest S3 snapshot into the new sandbox
		if _ephemeral_enabled(_api_config) {
			_ephemeral_auto_restore(_api_config, ms)
		}
		sync.mutex_lock(&ms.lock)
		buf: [4096]byte
		b := strings.builder_from_bytes(buf[:])
		_write_sandbox_json(&b, &ms.sandbox)
		sync.mutex_unlock(&ms.lock)
		if _builder_overflowed(&b, len(buf)) {
			fmt.eprintln("[api] create sandbox response truncated — buffer overflow")
			_json_error(resp, 500, "response too large")
			return
		}
		_json_body(resp, 201, strings.to_string(b))
	case .At_Capacity:
		_json_error(resp, 400, fmt.tprintf("sandbox limit reached (%d)", _api_config.max_sandboxes))
	case .Already_Exists:
		_json_error(resp, 400, fmt.tprintf("already exists: %s", body.id))
	case .Create_Failed:
		_json_error(resp, 500, "failed to create sandbox")
	}
}

_handle_get_sandbox :: proc(req: ^Http_Request, resp: ^Http_Response) {
	id := _path_param(req, 0)
	if len(id) == 0 {
		_json_error(resp, 404, "not found")
		return
	}

	ms := manager_get(_api_manager, id)
	if ms == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}

	sync.mutex_lock(&ms.lock)
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	_write_sandbox_json(&b, &ms.sandbox)
	sync.mutex_unlock(&ms.lock)
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] get sandbox response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_handle_delete_sandbox :: proc(req: ^Http_Request, resp: ^Http_Response) {
	id := _path_param(req, 0)
	if len(id) == 0 {
		_json_error(resp, 404, "not found")
		return
	}
	if manager_get(_api_manager, id) == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}
	manager_destroy_sandbox(_api_manager, id)
	resp.status = 204
}

_handle_exec :: proc(req: ^Http_Request, resp: ^Http_Response) {
	if !_require_json(req, resp) {
		return
	}
	id := _path_param(req, 0)
	ms := manager_get(_api_manager, id)
	if ms == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}

	body: Exec_Request_JSON
	if err := json.unmarshal(req.body, &body, allocator = context.temp_allocator); err != nil {
		_json_error(resp, 400, "invalid JSON body")
		return
	}
	if len(body.cmd) == 0 {
		_json_error(resp, 400, "cmd required")
		return
	}

	exec_req := Exec_Request{
		cmd     = body.cmd,
		workdir = body.workdir if len(body.workdir) > 0 else "/",
		timeout = body.timeout if body.timeout > 0 else 300,
	}

	sync.mutex_lock(&ms.lock)
	result: Exec_Result
	exec_err: Exec_Error
	if _api_config.backend == .Firecracker {
		result, exec_err = exec_in_sandbox_firecracker(&ms.sandbox, exec_req)
	} else {
		result, exec_err = exec_in_sandbox(&ms.sandbox, exec_req)
	}
	sync.mutex_unlock(&ms.lock)

	if exec_err != .None {
		switch exec_err {
		case .None:
			// unreachable
		case .Not_Ready:
			_json_error(resp, 400, "sandbox not ready")
		case .Pipe_Failed, .Fork_Failed:
			_json_error(resp, 500, "exec failed")
		}
		return
	}

	body_json := fmt.tprintf(
		`{"seq":%d,"cmd":`,
		result.seq,
	)
	buf: [16384]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_string(&b, body_json)
	_write_json_string(&b, exec_req.cmd)
	strings.write_string(&b, `,"workdir":`)
	_write_json_string(&b, exec_req.workdir)
	strings.write_string(&b, `,"exit_code":`)
	fmt.sbprintf(&b, "%d", result.exit_code)
	strings.write_string(&b, `,"started":`)
	_write_json_string(&b, _format_iso8601(result.started))
	strings.write_string(&b, `,"finished":`)
	_write_json_string(&b, _format_iso8601(result.finished))
	strings.write_string(&b, `,"stdout":`)
	_write_json_string(&b, result.stdout)
	strings.write_string(&b, `,"stderr":`)
	_write_json_string(&b, result.stderr)
	strings.write_string(&b, "}")
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] exec response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_handle_logs :: proc(req: ^Http_Request, resp: ^Http_Response) {
	id := _path_param(req, 0)
	ms := manager_get(_api_manager, id)
	if ms == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}

	sync.mutex_lock(&ms.lock)
	log_dir := fmt.tprintf("%s/.meta/log", ms.sandbox.dir)
	sync.mutex_unlock(&ms.lock)

	dh, err := os.open(log_dir)
	if err != nil {
		_json_body(resp, 200, "[]")
		return
	}
	defer os.close(dh)
	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		_json_body(resp, 200, "[]")
		return
	}
	defer delete(entries)

	buf: [65536]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_byte(&b, '[')
	first := true
	for entry in entries {
		if entry.is_dir {
			continue
		}
		content, ok := os.read_entire_file(entry.fullpath, context.temp_allocator)
		if !ok {
			continue
		}
		if !first {
			strings.write_byte(&b, ',')
		}
		first = false
		strings.write_string(&b, string(content))
	}
	strings.write_byte(&b, ']')
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] logs response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_handle_snapshot :: proc(req: ^Http_Request, resp: ^Http_Response) {
	if !_require_json(req, resp) {
		return
	}
	id := _path_param(req, 0)
	ms := manager_get(_api_manager, id)
	if ms == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}

	body: Snapshot_Request_JSON
	if len(req.body) > 0 {
		_ = json.unmarshal(req.body, &body, allocator = context.temp_allocator)
	}
	label := body.label
	if len(label) == 0 {
		label = generate_snapshot_label()
	}
	if !valid_label(label) {
		_json_error(resp, 400, "label: alphanumeric/dash/underscore/dot only")
		return
	}

	snap_path := snapshot_file_path(ms.sandbox.dir, label)
	sync.mutex_lock(&ms.lock)

	size: u64
	ok: bool
	if _api_config.backend == .Firecracker {
		// Firecracker: ask guest agent to snapshot via vsock
		cid, cid_ok := read_fc_cid(ms.sandbox.dir)
		if !cid_ok {
			sync.mutex_unlock(&ms.lock)
			_json_error(resp, 500, "failed to read CID")
			return
		}
		fc_err := firecracker_snapshot_via_vsock(cid)
		if fc_err != .None {
			sync.mutex_unlock(&ms.lock)
			_json_error(resp, 500, "failed to create snapshot")
			return
		}
		// Guest agent writes snapshot — check if file exists and get size
		ok = os.exists(snap_path)
		if ok {
			info, info_err := os.stat(snap_path)
			if info_err == nil {
				size = u64(info.size)
			}
		}
	} else {
		size, ok = snapshot_create(&ms.sandbox, label)
	}

	if ok {
		_ = os.write_entire_file(fmt.tprintf("%s/.meta/active_snapshot", ms.sandbox.dir), transmute([]byte)label)
		update_last_active(ms.sandbox.dir)
	}
	sync.mutex_unlock(&ms.lock)

	if !ok {
		_json_error(resp, 500, "failed to create snapshot")
		return
	}

	_push_snapshot_s3_bg(_api_config, id, label, snap_path)
	_json_body(resp, 200, fmt.tprintf(`{"snapshot":"%s","size":%d}`, label, size))
}

_handle_restore :: proc(req: ^Http_Request, resp: ^Http_Response) {
	if !_require_json(req, resp) {
		return
	}
	id := _path_param(req, 0)
	ms := manager_get(_api_manager, id)
	if ms == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}

	body: Restore_Request_JSON
	if err := json.unmarshal(req.body, &body, allocator = context.temp_allocator); err != nil {
		_json_error(resp, 400, "invalid JSON body")
		return
	}
	if len(body.label) == 0 || !valid_label(body.label) {
		_json_error(resp, 400, "label required")
		return
	}

	snap_path := snapshot_file_path(ms.sandbox.dir, body.label)
	if !os.exists(snap_path) && !_try_pull_snapshot_s3(_api_config, id, body.label, snap_path) {
		_json_error(resp, 400, fmt.tprintf("not found: %s", body.label))
		return
	}

	sync.mutex_lock(&ms.lock)
	if !os.exists(snap_path) {
		sync.mutex_unlock(&ms.lock)
		_json_error(resp, 400, fmt.tprintf("not found: %s", body.label))
		return
	}
	_ = os.write_entire_file(fmt.tprintf("%s/.meta/active_snapshot", ms.sandbox.dir), transmute([]byte)body.label)
	update_last_active(ms.sandbox.dir)

	ok: bool
	if _api_config.backend == .Firecracker {
		// Firecracker: stop VM + restart with snapshot layer
		ok = _firecracker_restore(ms, snap_path)
	} else {
		ok = _rebuild_mounts_for_metadata(ms)
	}

	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	_write_sandbox_json(&b, &ms.sandbox)
	sync.mutex_unlock(&ms.lock)
	if !ok {
		_json_error(resp, 500, "restore failed")
		return
	}
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] restore response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_handle_activate :: proc(req: ^Http_Request, resp: ^Http_Response) {
	if !_require_json(req, resp) {
		return
	}
	id := _path_param(req, 0)
	ms := manager_get(_api_manager, id)
	if ms == nil {
		_json_error(resp, 404, fmt.tprintf("not found: %s", id))
		return
	}

	body: Activate_Request_JSON
	if err := json.unmarshal(req.body, &body, allocator = context.temp_allocator); err != nil {
		_json_error(resp, 400, "invalid JSON body")
		return
	}
	if len(body.module) == 0 || !valid_module(body.module) {
		_json_error(resp, 400, "module required")
		return
	}
	if !module_exists(_api_config, body.module) {
		_json_error(resp, 400, fmt.tprintf("module not found: %s", body.module))
		return
	}

	sync.mutex_lock(&ms.lock)
	meta_layers := _read_layers(ms.sandbox.dir)
	for m in meta_layers {
		if m == body.module {
			sync.mutex_unlock(&ms.lock)
			_json_error(resp, 400, fmt.tprintf("already active: %s", body.module))
			return
		}
	}
	if len(meta_layers) > 0 {
		meta_str := strings.join(meta_layers, ",", context.temp_allocator)
		meta_str = fmt.tprintf("%s,%s", meta_str, body.module)
		_ = os.write_entire_file(fmt.tprintf("%s/.meta/layers", ms.sandbox.dir), transmute([]byte)meta_str)
	} else {
		_ = os.write_entire_file(fmt.tprintf("%s/.meta/layers", ms.sandbox.dir), transmute([]byte)body.module)
	}
	update_last_active(ms.sandbox.dir)

	ok: bool
	if _api_config.backend == .Firecracker {
		// Firecracker: hot-add drive + vsock remount
		ok = _firecracker_activate(ms, body.module)
	} else {
		ok = _rebuild_mounts_for_metadata(ms)
	}

	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	_write_sandbox_json(&b, &ms.sandbox)
	sync.mutex_unlock(&ms.lock)

	if !ok {
		_json_error(resp, 500, "activate failed")
		return
	}
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] activate response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_handle_list_modules :: proc(_: ^Http_Request, resp: ^Http_Response) {
	mods := list_modules(_api_config, context.temp_allocator)
	if len(mods) == 0 {
		_json_body(resp, 200, "[]")
		return
	}

	buf: [32768]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_byte(&b, '[')
	for mod, i in mods {
		if i > 0 {
			strings.write_byte(&b, ',')
		}
		size := _module_size(mod)
		strings.write_string(&b, `{"name":`)
		_write_json_string(&b, mod)
		strings.write_string(&b, `,"size":`)
		fmt.sbprintf(&b, "%d", size)
		strings.write_string(&b, `,"location":"local"}`)
	}
	strings.write_byte(&b, ']')
	if _builder_overflowed(&b, len(buf)) {
		fmt.eprintln("[api] list modules response truncated — buffer overflow")
		_json_error(resp, 500, "response too large")
		return
	}
	_json_body(resp, 200, strings.to_string(b))
}

_module_size :: proc(name: string) -> i64 {
	path := fmt.tprintf("%s/%s.squashfs", modules_dir(_api_config), name)
	info, err := os.stat(path)
	if err != nil {
		return 0
	}
	return info.size
}

_rebuild_mounts_for_metadata :: proc(ms: ^Managed_Sandbox) -> bool {
	ready, ok := &ms.sandbox.state.(Ready)
	if !ok {
		return false
	}

	// Tear down old mounts first, then recreate from metadata.
	sandbox_mounts_destroy(&ready.mounts)
	layers := _read_layers(ms.sandbox.dir)

	snap_path: Maybe(string)
	active_path := fmt.tprintf("%s/.meta/active_snapshot", ms.sandbox.dir)
	if os.exists(active_path) {
		label_data, has_label := os.read_entire_file(active_path, context.temp_allocator)
		if has_label {
			label := strings.trim_space(string(label_data))
			if len(label) > 0 {
				candidate := snapshot_file_path(ms.sandbox.dir, label)
				if os.exists(candidate) {
					snap_path = candidate
				}
			}
		}
	}

	params := Sandbox_Setup_Params{
		sandbox_dir   = ms.sandbox.dir,
		modules_dir   = modules_dir(_api_config),
		layers        = layers,
		snapshot_path = snap_path,
	}
	new_mounts, m_err := sandbox_mounts_setup(&params, managed_sandbox_allocator(ms))
	if m_err != .None {
		return false
	}
	ready.mounts = new_mounts
	return true
}

_read_layers :: proc(sandbox_dir: string) -> []string {
	layers_path := fmt.tprintf("%s/.meta/layers", sandbox_dir)
	data, ok := os.read_entire_file(layers_path, context.temp_allocator)
	if !ok {
		return nil
	}
	raw := strings.trim_space(string(data))
	if len(raw) == 0 {
		return nil
	}
	parts := strings.split(raw, ",", context.temp_allocator)
	for &part in parts {
		part = strings.trim_space(part)
	}
	return parts
}

_parse_layers :: proc(raw: string) -> []string {
	trimmed := strings.trim_space(raw)
	if len(trimmed) == 0 {
		return nil
	}
	parts := strings.split(trimmed, ",", context.temp_allocator)
	for &part in parts {
		part = strings.trim_space(part)
	}
	return parts
}

_write_sandbox_json :: proc(b: ^strings.Builder, sandbox: ^Sandbox) {
	mounted := false
	if _, ok := &sandbox.state.(Ready); ok {
		mounted = true
	}
	strings.write_string(b, `{"id":`)
	_write_json_string(b, sandbox.id)
	strings.write_string(b, `,"owner":`)
	_write_json_string(b, sandbox.owner)
	strings.write_string(b, `,"task":`)
	_write_json_string(b, sandbox.task)
	strings.write_string(b, `,"created":`)
	_write_json_string(b, _format_iso8601(sandbox.created))
	strings.write_string(b, `,"last_active":`)
	_write_json_string(b, _format_iso8601(sandbox.last_active))
	strings.write_string(b, `,"mounted":`)
	strings.write_string(b, "true" if mounted else "false")
	strings.write_string(b, `,"exec_count":`)
	fmt.sbprintf(b, "%d", sandbox.exec_count)
	strings.write_string(b, `,"max_lifetime_s":`)
	fmt.sbprintf(b, "%d", sandbox.max_lifetime_s)
	strings.write_byte(b, '}')
}

_path_param :: proc(req: ^Http_Request, idx: int) -> string {
	if idx < 0 || idx >= req.path_param_count {
		return ""
	}
	return req.path_params[idx]
}

_require_json :: proc(req: ^Http_Request, resp: ^Http_Response) -> bool {
	if req.method == "POST" {
		ct := req.content_type
		// Case-insensitive prefix check without allocation
		if len(ct) < 16 || !strings.equal_fold(ct[:16], "application/json") {
			_json_error(resp, 415, "Content-Type must be application/json")
			return false
		}
	}
	return true
}

_json_error :: proc(resp: ^Http_Response, status: int, message: string) {
	buf: [1024]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_string(&b, `{"error":`)
	_write_json_string(&b, message)
	strings.write_byte(&b, '}')
	_json_body(resp, status, strings.to_string(b))
}

// Check if a fixed-size builder overflowed. Returns true if truncated.
_builder_overflowed :: proc(b: ^strings.Builder, buf_len: int) -> bool {
	return strings.builder_len(b^) >= buf_len
}

_json_body :: proc(resp: ^Http_Response, status: int, body: string) {
	resp.status = status
	response_set_header(resp, "Content-Type", "application/json")
	response_set_body_string(resp, body)
}

// ---------------------------------------------------------------------------
// Firecracker-specific API helpers
// ---------------------------------------------------------------------------

// Activate a module in a Firecracker sandbox: hot-add drive + vsock remount.
// Caller must hold ms.lock.
_firecracker_activate :: proc(ms: ^Managed_Sandbox, module: string) -> bool {
	cid, cid_ok := read_fc_cid(ms.sandbox.dir)
	if !cid_ok {
		fmt.eprintln("[api-fc] activate: failed to read CID")
		return false
	}

	sqfs_path := fmt.tprintf("%s/%s.squashfs", modules_dir(_api_config), module)
	fc_err := firecracker_add_drive(ms.sandbox.id, module, sqfs_path, cid, fmt.tprintf("%s/.meta", ms.sandbox.dir))
	if fc_err != .None {
		fmt.printfln("[api-fc] activate: drive add failed for %s", module)
		return false
	}

	return true
}

// Restore a snapshot in a Firecracker sandbox: stop VM + restart with snapshot layer.
// Caller must hold ms.lock.
_firecracker_restore :: proc(ms: ^Managed_Sandbox, snap_path: string) -> bool {
	meta_dir := fmt.tprintf("%s/.meta", ms.sandbox.dir)

	// Read current FC metadata before stopping
	cid, cid_ok := read_fc_cid(ms.sandbox.dir)
	if !cid_ok {
		fmt.eprintln("[api-fc] restore: failed to read CID")
		return false
	}

	// Stop the current VM
	firecracker_stop_vm(ms.sandbox.id, meta_dir)

	// Rebuild squashfs path list including snapshot
	layers := _read_layers(ms.sandbox.dir)
	mods := modules_dir(_api_config)
	sqfs_paths := make([dynamic]string, 0, len(layers) + 1, context.temp_allocator)
	for layer in layers {
		append(&sqfs_paths, fmt.tprintf("%s/%s.squashfs", mods, layer))
	}
	// Add snapshot as an additional layer
	append(&sqfs_paths, snap_path)

	// Read resource limits from metadata
	cpu_val := _read_meta_string(ms.sandbox.dir, "cpu", "2.0")
	mem_val := _read_meta_string(ms.sandbox.dir, "memory_mb", "1024")
	cpu: f64 = 2.0
	mem: int = 1024
	if parsed, ok := strconv.parse_f64(cpu_val); ok {
		cpu = parsed
	}
	if parsed, ok := strconv.parse_int(mem_val, 10); ok {
		mem = parsed
	}

	// Restart VM with snapshot layer
	vm_err := firecracker_start_vm(ms.sandbox.id, cpu, mem, sqfs_paths[:], cid, meta_dir)
	if vm_err != .None {
		fmt.printfln("[api-fc] restore: VM restart failed for %s", ms.sandbox.id)
		return false
	}

	return true
}

_s3_enabled :: proc(config: ^Config) -> bool {
	_, has_bucket := config.s3_bucket.?
	return has_bucket && _command_exists("sq-s3")
}

_s3_snapshot_key :: proc(id: string, label: string) -> string {
	return fmt.tprintf("sandboxes/%s/snapshots/%s.squashfs", id, label)
}

_push_snapshot_s3_bg :: proc(config: ^Config, id: string, label: string, local_path: string) {
	if !_s3_enabled(config) {
		return
	}
	_ = run_cmd("sq-s3", "push-bg", local_path, _s3_snapshot_key(id, label))
}

_try_pull_snapshot_s3 :: proc(config: ^Config, id: string, label: string, dest_path: string) -> bool {
	if !_s3_enabled(config) {
		return false
	}
	return run_cmd("sq-s3", "pull", _s3_snapshot_key(id, label), dest_path)
}

// ---------------------------------------------------------------------------
// Ephemeral mode helpers
//
// Ephemeral sandboxes are stateless on disk — S3 is the source of truth.
// On create: auto-restore the latest S3 snapshot (if any).
// On destroy: auto-snapshot the upper layer and push to S3.
// ---------------------------------------------------------------------------

_ephemeral_enabled :: proc(config: ^Config) -> bool {
	return config.ephemeral && _s3_enabled(config)
}

// List snapshot labels for a sandbox from S3. Returns labels sorted by S3 key
// (lexicographic = chronological for our YYYYMMDD-HHMMSS labels).
_s3_list_snapshot_labels :: proc(config: ^Config, id: string) -> []string {
	if !_s3_enabled(config) {
		return nil
	}
	prefix := fmt.tprintf("sandboxes/%s/snapshots/", id)
	output, ok := run_cmd_capture("sq-s3", "list", prefix)
	if !ok || len(output) == 0 {
		return nil
	}

	// Parse output: one S3 key per line, extract label from basename
	result := make([dynamic]string, 0, 8, context.temp_allocator)
	content := output
	for line in strings.split_lines_iterator(&content) {
		trimmed := strings.trim_space(line)
		if len(trimmed) == 0 {
			continue
		}
		// Key format: sandboxes/{id}/snapshots/{label}.squashfs
		// Extract label by taking basename and stripping .squashfs
		base := _basename(trimmed)
		if strings.has_suffix(base, ".squashfs") {
			label := base[:len(base) - len(".squashfs")]
			if len(label) > 0 {
				append(&result, label)
			}
		}
	}
	return result[:]
}

// Find the latest snapshot label for a sandbox in S3.
// Labels are YYYYMMDD-HHMMSS so lexicographic max = latest.
_s3_latest_snapshot_label :: proc(config: ^Config, id: string) -> Maybe(string) {
	labels := _s3_list_snapshot_labels(config, id)
	if len(labels) == 0 {
		return nil
	}
	latest := labels[0]
	for label in labels[1:] {
		if label > latest {
			latest = label
		}
	}
	return latest
}

// Auto-restore: pull the latest S3 snapshot into a newly created sandbox.
// Called after sandbox_create succeeds in ephemeral mode.
_ephemeral_auto_restore :: proc(config: ^Config, ms: ^Managed_Sandbox) {
	id := ms.sandbox.id
	label_maybe := _s3_latest_snapshot_label(config, id)
	label, has_label := label_maybe.?
	if !has_label {
		fmt.printfln("[ephemeral] %s: no S3 snapshots found, starting fresh", id)
		return
	}

	snap_path := snapshot_file_path(ms.sandbox.dir, label)
	snap_dir := fmt.tprintf("%s/snapshots", ms.sandbox.dir)
	_ensure_dir_recursive(snap_dir)

	if !_try_pull_snapshot_s3(config, id, label, snap_path) {
		fmt.printfln("[ephemeral] %s: failed to pull snapshot %s from S3", id, label)
		return
	}

	// Write active_snapshot metadata
	_ = os.write_entire_file(
		fmt.tprintf("%s/.meta/active_snapshot", ms.sandbox.dir),
		transmute([]byte)label,
	)

	// Rebuild mounts with the restored snapshot
	sync.mutex_lock(&ms.lock)
	if config.backend == .Firecracker {
		_firecracker_restore(ms, snap_path)
	} else {
		_rebuild_mounts_for_metadata(ms)
	}
	sync.mutex_unlock(&ms.lock)

	fmt.printfln("[ephemeral] %s: restored snapshot %s from S3", id, label)
}

// Check if the upper layer has meaningful data (anything besides etc/resolv.conf).
_upper_has_data :: proc(sandbox_dir: string) -> bool {
	upper_data := fmt.tprintf("%s/upper/data", sandbox_dir)
	dh, err := os.open(upper_data)
	if err != nil {
		return false
	}
	defer os.close(dh)

	entries, read_err := os.read_dir(dh, -1)
	if read_err != nil {
		return false
	}
	defer delete(entries)

	for entry in entries {
		// Skip etc/ if it only contains resolv.conf (overlay artifact)
		if entry.is_dir && entry.name == "etc" {
			etc_path := fmt.tprintf("%s/etc", upper_data)
			etc_dh, etc_err := os.open(etc_path)
			if etc_err != nil {
				return true // can't open = assume data
			}
			defer os.close(etc_dh)
			etc_entries, etc_read_err := os.read_dir(etc_dh, -1)
			if etc_read_err != nil {
				return true
			}
			defer delete(etc_entries)
			// If etc/ has anything other than resolv.conf, it's real data
			for etc_entry in etc_entries {
				if etc_entry.name != "resolv.conf" {
					return true
				}
			}
			continue
		}
		// Any other file/dir = real data
		return true
	}
	return false
}

// Auto-snapshot: snapshot the upper layer and push to S3 before destroy.
// Called in the destroy path when ephemeral mode is active.
_ephemeral_auto_snapshot :: proc(config: ^Config, ms: ^Managed_Sandbox) {
	id := ms.sandbox.id

	if !_upper_has_data(ms.sandbox.dir) {
		fmt.printfln("[ephemeral] %s: upper layer empty, skipping snapshot", id)
		return
	}

	label := generate_snapshot_label()
	snap_path := snapshot_file_path(ms.sandbox.dir, label)

	size: u64
	ok: bool
	if config.backend == .Firecracker {
		cid, cid_ok := read_fc_cid(ms.sandbox.dir)
		if cid_ok {
			firecracker_snapshot_via_vsock(cid)
			ok = os.exists(snap_path)
			if ok {
				info, info_err := os.stat(snap_path)
				if info_err == nil {
					size = u64(info.size)
				}
			}
		}
	} else {
		size, ok = snapshot_create(&ms.sandbox, label)
	}

	if !ok {
		fmt.printfln("[ephemeral] %s: auto-snapshot failed", id)
		return
	}

	fmt.printfln("[ephemeral] %s: created snapshot %s (%d bytes)", id, label, size)

	// Push snapshot to S3 synchronously (destroy will remove local files)
	s3_key := _s3_snapshot_key(id, label)
	if run_cmd("sq-s3", "push", snap_path, s3_key) {
		fmt.printfln("[ephemeral] %s: pushed %s to S3", id, label)
	} else {
		fmt.printfln("[ephemeral] %s: S3 push failed for %s", id, label)
	}

	// Push manifest (sandbox metadata)
	_ephemeral_push_manifest(config, ms)
}

// Push a manifest.json to S3 with sandbox metadata.
_ephemeral_push_manifest :: proc(config: ^Config, ms: ^Managed_Sandbox) {
	id := ms.sandbox.id
	manifest_path := fmt.tprintf("%s/.meta/manifest.json", ms.sandbox.dir)

	// Build minimal manifest JSON
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	strings.write_string(&b, `{"id":`)
	_write_json_string(&b, id)
	strings.write_string(&b, `,"owner":`)
	_write_json_string(&b, ms.sandbox.owner)
	strings.write_string(&b, `,"task":`)
	_write_json_string(&b, ms.sandbox.task)

	// Include layers
	layers := _read_layers(ms.sandbox.dir)
	strings.write_string(&b, `,"layers":`)
	if len(layers) > 0 {
		strings.write_string(&b, `"`)
		for layer, i in layers {
			if i > 0 {
				strings.write_byte(&b, ',')
			}
			strings.write_string(&b, layer)
		}
		strings.write_string(&b, `"`)
	} else {
		strings.write_string(&b, `""`)
	}
	strings.write_byte(&b, '}')

	content := strings.to_string(b)
	os.write_entire_file(manifest_path, transmute([]byte)content)

	s3_key := fmt.tprintf("sandboxes/%s/manifest.json", id)
	run_cmd("sq-s3", "push", manifest_path, s3_key)
}
