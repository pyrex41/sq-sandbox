package squashd

import "core:fmt"
import "core:os"
import "core:strings"
import "core:net"

/// Check if the irmin snapshot backend is enabled.
is_irmin_enabled :: proc() -> bool {
	val, ok := os.lookup_env("SQUASH_SNAPSHOT_BACKEND")
	if !ok { return false }
	return val == "irmin"
}

/// Get the sq-store socket path.
get_store_sock_path :: proc() -> string {
	if val, ok := os.lookup_env("SQUASH_STORE_SOCK"); ok {
		return val
	}
	data_dir := os.get_env("SQUASH_DATA") or_else "/data"
	return fmt.tprintf("%s/.sq-store.sock", data_dir)
}

/// Send a request to sq-store via socat and return the response.
/// Uses socat as a helper since Odin's net package doesn't natively
/// support Unix domain sockets in all builds.
store_request :: proc(json_msg: string) -> (response: string, ok: bool) {
	sock_path := get_store_sock_path()

	// Use socat for Unix socket communication
	cmd := fmt.tprintf("printf '%%s\\n' '%s' | socat - UNIX-CONNECT:%s", json_msg, sock_path)
	// Note: In production, this should use a proper Unix socket API.
	// For now, delegate to socat for portability.
	if !run_cmd_output("sh", "-c", cmd, &response) {
		return "", false
	}
	return response, true
}

/// Build snapshot request JSON.
store_snapshot_request :: proc(sandbox_id: string, label: string, upper_data: string) -> string {
	return fmt.tprintf(
		`{{"op":"snapshot","sandbox_id":"%s","label":"%s","upper_data":"%s"}}`,
		sandbox_id, label, upper_data,
	)
}

/// Build restore request JSON.
store_restore_request :: proc(sandbox_id: string, label: string, upper_data: string) -> string {
	return fmt.tprintf(
		`{{"op":"restore","sandbox_id":"%s","label":"%s","upper_data":"%s"}}`,
		sandbox_id, label, upper_data,
	)
}
