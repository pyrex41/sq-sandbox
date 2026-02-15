package squashd

import "core:encoding/json"
import "core:fmt"
import "core:os"
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
		sync.mutex_lock(&ms.lock)
		buf: [4096]byte
		b := strings.builder_from_bytes(buf[:])
		_write_sandbox_json(&b, &ms.sandbox)
		sync.mutex_unlock(&ms.lock)
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
	result, exec_err := exec_in_sandbox(&ms.sandbox, exec_req)
	sync.mutex_unlock(&ms.lock)

	if exec_err != .None {
		switch exec_err {
		case .None:
			// unreachable
		case .Not_Ready:
			_json_error(resp, 400, "sandbox not ready")
		case .Pipe_Failed, .Fork_Failed, .Netns_Open_Failed:
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
	size, ok := snapshot_create(&ms.sandbox, label)
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
	ok := _rebuild_mounts_for_metadata(ms)
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	_write_sandbox_json(&b, &ms.sandbox)
	sync.mutex_unlock(&ms.lock)
	if !ok {
		_json_error(resp, 500, "restore failed")
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
	ok := _rebuild_mounts_for_metadata(ms)
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])
	_write_sandbox_json(&b, &ms.sandbox)
	sync.mutex_unlock(&ms.lock)

	if !ok {
		_json_error(resp, 500, "activate failed")
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
		upper_size_mb = _api_config.upper_limit_mb,
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
		if !strings.has_prefix(strings.to_lower(req.content_type), "application/json") {
			_json_error(resp, 415, "Content-Type must be application/json")
			return false
		}
	}
	return true
}

_json_error :: proc(resp: ^Http_Response, status: int, message: string) {
	_json_body(resp, status, fmt.tprintf(`{"error":"%s"}`, message))
}

_json_body :: proc(resp: ^Http_Response, status: int, body: string) {
	resp.status = status
	response_set_header(resp, "Content-Type", "application/json")
	response_set_body_string(resp, body)
}

_s3_enabled :: proc(config: ^Config) -> bool {
	_, has_bucket := config.s3_bucket.?
	return has_bucket && _command_exists("sq-s3")
}

_s3_snapshot_key :: proc(id: string, label: string) -> string {
	return fmt.tprintf("snapshots/%s/%s.squashfs", id, label)
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
