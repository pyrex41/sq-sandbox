package squashd

import "core:fmt"
import "core:mem"
import "core:net"
import "core:strings"
import "core:sync"
import "core:thread"
import "core:time"

// ---------------------------------------------------------------------------
// HTTP types
// ---------------------------------------------------------------------------

Http_Handler :: #type proc(req: ^Http_Request, resp: ^Http_Response)

Http_Request :: struct {
	method:       string,
	path:         string,
	headers:      [MAX_HEADERS]Header_Entry,
	header_count: int,
	body:         []byte,
	content_type: string,
	// Wildcard path parameters extracted during route matching.
	// For a route "/sandboxes/*/exec" matching "/sandboxes/abc/exec",
	// path_params[0] = "abc".
	path_params:      [MAX_PATH_PARAMS]string,
	path_param_count: int,
}

Header_Entry :: struct {
	name:  string,
	value: string,
}

Http_Response :: struct {
	status:       int,
	headers:      [MAX_HEADERS]Header_Entry,
	header_count: int,
	body:         []byte,
}

Route :: struct {
	method:  string,
	pattern: string, // exact match, or contains "*" for wildcard segments
	handler: Http_Handler,
}

Thread_Context :: struct {
	routes:  []Route,
	config:  ^Config,
	manager: ^Sandbox_Manager,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

MAX_REQUEST_SIZE    :: 1048576 // 1 MB max request size
MAX_HEADERS         :: 32
MAX_PATH_PARAMS     :: 4
RECV_TIMEOUT_MS     :: 30000   // 30 seconds

// Worker pool size — matches Zig's pool_size for comparable throughput.
// Tuned for typical 4–8 core machines; scales with CPU count.
WORKER_POOL_SIZE :: 8
// Bounded channel capacity — backpressure when workers are saturated.
CONN_QUEUE_CAP   :: 256

// ---------------------------------------------------------------------------
// Bounded channel for connection handoff
// ---------------------------------------------------------------------------

Conn_Work_Item :: struct {
	client: net.TCP_Socket,
	ctx:    ^Thread_Context,
}

Conn_Channel :: struct {
	mu:      sync.Mutex,
	not_empty: sync.Cond,
	not_full:  sync.Cond,
	queue:    [dynamic]Conn_Work_Item,
	cap:     int,
	closed:  bool,
}

conn_channel_init :: proc(ch: ^Conn_Channel, cap: int) {
	ch.cap = cap
	ch.closed = false
	ch.queue = {}
}

conn_channel_close :: proc(ch: ^Conn_Channel) {
	sync.mutex_lock(&ch.mu)
	ch.closed = true
	sync.cond_broadcast(&ch.not_empty)
	sync.cond_broadcast(&ch.not_full)
	sync.mutex_unlock(&ch.mu)
}

conn_channel_send :: proc(ch: ^Conn_Channel, item: Conn_Work_Item) -> bool {
	sync.mutex_lock(&ch.mu)
	for len(ch.queue) >= ch.cap && !ch.closed {
		sync.cond_wait(&ch.not_full, &ch.mu)
	}
	if ch.closed {
		sync.mutex_unlock(&ch.mu)
		return false
	}
	append(&ch.queue, item)
	sync.cond_signal(&ch.not_empty)
	sync.mutex_unlock(&ch.mu)
	return true
}

conn_channel_recv :: proc(ch: ^Conn_Channel) -> (item: Conn_Work_Item, ok: bool) {
	sync.mutex_lock(&ch.mu)
	for len(ch.queue) == 0 && !ch.closed {
		sync.cond_wait(&ch.not_empty, &ch.mu)
	}
	if ch.closed && len(ch.queue) == 0 {
		sync.mutex_unlock(&ch.mu)
		return {}, false
	}
	item = ch.queue[0]
	ordered_remove(&ch.queue, 0)
	sync.cond_signal(&ch.not_full)
	sync.mutex_unlock(&ch.mu)
	return item, true
}

// ---------------------------------------------------------------------------
// serve — TCP accept loop with thread pool + bounded channel
// ---------------------------------------------------------------------------

serve :: proc(port: u16, routes: []Route, config: ^Config, manager: ^Sandbox_Manager) {
	endpoint := net.Endpoint{
		address = net.IP4_Any,
		port    = int(port),
	}

	socket, err := net.listen_tcp(endpoint)
	if err != nil {
		fmt.eprintfln("[http] listen failed on port %d: %v", port, err)
		return
	}

	fmt.printfln("[http] listening on :%d (pool=%d, queue=%d)", port, WORKER_POOL_SIZE, CONN_QUEUE_CAP)

	ctx := Thread_Context{
		routes  = routes,
		config  = config,
		manager = manager,
	}

	ch: Conn_Channel
	conn_channel_init(&ch, CONN_QUEUE_CAP)
	defer conn_channel_close(&ch)

	// Spawn worker threads
	for _ in 0 ..< WORKER_POOL_SIZE {
		thread.create_and_start_with_poly_data(&ch, _worker_loop, self_cleanup = true)
	}

	// Acceptor loop — push connections to channel (blocks on backpressure)
	for {
		client, _, accept_err := net.accept_tcp(socket)
		if accept_err != nil {
			continue
		}

		if !conn_channel_send(&ch, Conn_Work_Item{client = client, ctx = &ctx}) {
			net.close(client)
			break
		}
	}
}

_worker_loop :: proc(ch: ^Conn_Channel) {
	for {
		item, ok := conn_channel_recv(ch)
		if !ok {
			return
		}
		handle_connection(item.client, item.ctx)
	}
}

// ---------------------------------------------------------------------------
// handle_connection — read request, authenticate, route, respond
// ---------------------------------------------------------------------------

handle_connection :: proc(client: net.TCP_Socket, ctx: ^Thread_Context) {
	defer net.close(client)

	// Set receive timeout so we don't block forever on slow/malicious clients
	net.set_option(client, .Receive_Timeout, time.Duration(RECV_TIMEOUT_MS) * time.Millisecond)

	// Read loop: accumulate data until we have full headers + body
	buf := make([]byte, MAX_REQUEST_SIZE, context.temp_allocator)
	total: int = 0
	header_end: int = -1
	content_length: int = 0

	for {
		if total >= MAX_REQUEST_SIZE {
			send_json_error(client, 413, "request too large")
			return
		}

		n, recv_err := net.recv_tcp(client, buf[total:])
		if recv_err != nil || n <= 0 {
			if total == 0 {
				return // Connection closed before any data
			}
			break // Use whatever we have
		}
		total += n

		// Check if we have complete headers (look for \r\n\r\n)
		if header_end < 0 {
			// Search for header terminator in newly received region
			search_start := 0
			if total - n >= 3 {
				search_start = total - n - 3
			}
			s := string(buf[search_start:total])
			idx := strings.index(s, "\r\n\r\n")
			if idx >= 0 {
				header_end = search_start + idx + 4 // points past \r\n\r\n

				// Parse Content-Length from the headers we have
				header_str := string(buf[:header_end])
				content_length = _parse_content_length(header_str)
			}
		}

		// If we have headers, check if body is complete
		if header_end >= 0 {
			body_received := total - header_end
			if body_received >= content_length {
				break // Full request received
			}
		}
	}

	if total <= 0 {
		return
	}

	req: Http_Request
	if !parse_http_request(buf[:total], &req) {
		send_json_error(client, 400, "bad request")
		return
	}

	// Auth check — bearer token required for /cgi-bin/api/ endpoints
	if token, ok := ctx.config.auth_token.?; ok {
		if strings.has_prefix(req.path, "/cgi-bin/api/") {
			auth_val := http_request_header(&req, "Authorization")
			expected := fmt.tprintf("Bearer %s", token)
			if auth_val != expected {
				send_json_error(client, 401, "unauthorized")
				return
			}
		}
	}

	// Route matching
	for &route in ctx.routes {
		if req.method == route.method && match_route(route.pattern, req.path, &req) {
			resp := Http_Response{status = 200}
			route.handler(&req, &resp)
			send_response(client, &resp)
			return
		}
	}

	send_json_error(client, 404, "not found")
}

// Parse Content-Length from raw header string. Returns 0 if not found.
_parse_content_length :: proc(headers: string) -> int {
	rest := headers
	for {
		line_end := strings.index(rest, "\r\n")
		if line_end < 0 || line_end == 0 {
			break
		}
		line := rest[:line_end]
		rest = rest[line_end + 2:]

		colon := strings.index(line, ":")
		if colon < 0 {
			continue
		}
		name := line[:colon]
		if strings.equal_fold(name, "Content-Length") {
			val_str := strings.trim_space(line[colon + 1:])
			val := 0
			for ch in val_str {
				if ch < '0' || ch > '9' {
					return 0
				}
				val = val * 10 + int(ch - '0')
			}
			return val
		}
	}
	return 0
}

// ---------------------------------------------------------------------------
// HTTP/1.1 request parser
//
// Parses: METHOD PATH HTTP/1.1\r\n
//         Header: Value\r\n
//         ...
//         \r\n
//         [body]
//
// All strings point into the original buffer (zero-copy).
// ---------------------------------------------------------------------------

parse_http_request :: proc(data: []byte, req: ^Http_Request) -> bool {
	s := string(data)

	// Request line: "METHOD /path HTTP/1.1\r\n"
	line_end := strings.index(s, "\r\n")
	if line_end < 0 {
		return false
	}
	request_line := s[:line_end]
	rest := s[line_end + 2:]

	// Split request line: METHOD SP PATH SP VERSION
	sp1 := strings.index(request_line, " ")
	if sp1 < 0 {
		return false
	}
	after_method := request_line[sp1 + 1:]
	sp2 := strings.index(after_method, " ")
	if sp2 < 0 {
		return false
	}

	req.method = request_line[:sp1]
	req.path = after_method[:sp2]

	// Strip query string from path
	if qidx := strings.index(req.path, "?"); qidx >= 0 {
		req.path = req.path[:qidx]
	}

	// Parse headers
	req.header_count = 0
	for {
		hdr_end := strings.index(rest, "\r\n")
		if hdr_end < 0 {
			break
		}
		if hdr_end == 0 {
			// Empty line — end of headers
			rest = rest[2:]
			break
		}

		header_line := rest[:hdr_end]
		rest = rest[hdr_end + 2:]

		colon := strings.index(header_line, ":")
		if colon < 0 {
			continue
		}

		if req.header_count >= MAX_HEADERS {
			continue
		}

		name := header_line[:colon]
		value := strings.trim_left(header_line[colon + 1:], " ")

		req.headers[req.header_count] = Header_Entry{name = name, value = value}
		req.header_count += 1

		// Cache Content-Type for quick access
		if strings.equal_fold(name, "Content-Type") {
			req.content_type = value
		}
	}

	// Body is whatever remains
	if len(rest) > 0 {
		req.body = transmute([]byte)rest
	}

	return true
}

// ---------------------------------------------------------------------------
// Header lookup (case-insensitive name match)
// ---------------------------------------------------------------------------

http_request_header :: proc(req: ^Http_Request, name: string) -> string {
	for i in 0 ..< req.header_count {
		if strings.equal_fold(req.headers[i].name, name) {
			return req.headers[i].value
		}
	}
	return ""
}

// ---------------------------------------------------------------------------
// Route matching with wildcard path parameter extraction
//
// Pattern syntax:
//   "/cgi-bin/health"                — exact match
//   "/cgi-bin/api/sandboxes/*/exec"  — "*" matches one path segment
//   "/cgi-bin/api/sandboxes/*"       — trailing wildcard
//
// Matched wildcard values are stored in req.path_params.
// ---------------------------------------------------------------------------

match_route :: proc(pattern: string, path: string, req: ^Http_Request) -> bool {
	req.path_param_count = 0

	// Fast path: no wildcards
	if !strings.contains(pattern, "*") {
		return pattern == path
	}

	// Split both pattern and path into segments and compare
	pat_rest := pattern
	path_rest := path

	// Both must start with /
	if len(pat_rest) == 0 || pat_rest[0] != '/' {
		return false
	}
	if len(path_rest) == 0 || path_rest[0] != '/' {
		return false
	}

	// Skip leading /
	pat_rest = pat_rest[1:]
	path_rest = path_rest[1:]

	for {
		// Get next pattern segment
		pat_seg: string
		pat_done: bool
		pat_seg, pat_rest, pat_done = _next_segment(pat_rest)

		// Get next path segment
		path_seg: string
		path_done: bool
		path_seg, path_rest, path_done = _next_segment(path_rest)

		// Both exhausted — match
		if pat_done && path_done {
			return true
		}

		// One exhausted but not the other — no match
		if pat_done || path_done {
			return false
		}

		if pat_seg == "*" {
			// Wildcard — capture the path segment
			if req.path_param_count < MAX_PATH_PARAMS {
				req.path_params[req.path_param_count] = path_seg
				req.path_param_count += 1
			}
		} else {
			// Literal segment comparison
			if pat_seg != path_seg {
				return false
			}
		}
	}
}

// Split off the next segment from a path (without leading /).
// Returns (segment, remaining_after_slash, is_empty).
_next_segment :: proc(s: string) -> (segment: string, rest: string, done: bool) {
	if len(s) == 0 {
		return "", "", true
	}
	idx := strings.index(s, "/")
	if idx < 0 {
		return s, "", false
	}
	return s[:idx], s[idx + 1:], false
}

// ---------------------------------------------------------------------------
// Response formatting and sending
// ---------------------------------------------------------------------------

send_response :: proc(client: net.TCP_Socket, resp: ^Http_Response) {
	buf: [1024]byte
	b := strings.builder_from_bytes(buf[:])

	// Status line
	reason := _status_reason(resp.status)
	fmt.sbprintf(&b, "HTTP/1.1 %d %s\r\n", resp.status, reason)

	// Default headers
	has_content_type := false
	has_content_length := false
	has_connection := false

	for i in 0 ..< resp.header_count {
		name := resp.headers[i].name
		if strings.equal_fold(name, "Content-Type") {
			has_content_type = true
		}
		if strings.equal_fold(name, "Content-Length") {
			has_content_length = true
		}
		if strings.equal_fold(name, "Connection") {
			has_connection = true
		}
		fmt.sbprintf(&b, "%s: %s\r\n", name, resp.headers[i].value)
	}

	body_len := len(resp.body) if resp.body != nil else 0

	if !has_content_type {
		strings.write_string(&b, "Content-Type: application/json\r\n")
	}
	if !has_content_length {
		fmt.sbprintf(&b, "Content-Length: %d\r\n", body_len)
	}
	if !has_connection {
		strings.write_string(&b, "Connection: close\r\n")
	}

	strings.write_string(&b, "\r\n")

	// Send headers
	header_data := strings.to_string(b)
	net.send_tcp(client, transmute([]byte)header_data)

	// Send body
	if body_len > 0 {
		net.send_tcp(client, resp.body)
	}
}

// ---------------------------------------------------------------------------
// JSON error response helper
// ---------------------------------------------------------------------------

send_json_error :: proc(client: net.TCP_Socket, status: int, message: string) {
	resp := Http_Response{status = status}
	body_str := fmt.tprintf(`{{"error":"%s"}}`, message)
	resp.body = transmute([]byte)body_str
	send_response(client, &resp)
}

// ---------------------------------------------------------------------------
// Response helpers for handlers
// ---------------------------------------------------------------------------

response_set_header :: proc(resp: ^Http_Response, name: string, value: string) {
	if resp.header_count < MAX_HEADERS {
		resp.headers[resp.header_count] = Header_Entry{name = name, value = value}
		resp.header_count += 1
	}
}

response_set_body :: proc(resp: ^Http_Response, data: []byte) {
	resp.body = data
}

response_set_body_string :: proc(resp: ^Http_Response, s: string) {
	resp.body = transmute([]byte)s
}

// ---------------------------------------------------------------------------
// HTTP status code -> reason phrase
// ---------------------------------------------------------------------------

_status_reason :: proc(status: int) -> string {
	switch status {
	case 200: return "OK"
	case 201: return "Created"
	case 204: return "No Content"
	case 400: return "Bad Request"
	case 401: return "Unauthorized"
	case 403: return "Forbidden"
	case 404: return "Not Found"
	case 405: return "Method Not Allowed"
	case 409: return "Conflict"
	case 413: return "Payload Too Large"
	case 500: return "Internal Server Error"
	case 503: return "Service Unavailable"
	case:     return "Unknown"
	}
}
