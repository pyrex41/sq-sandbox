package squashd

import "core:encoding/json"
import "core:fmt"
import "core:os"
import "core:strings"

// ---------------------------------------------------------------------------
// Secrets — load secrets.json and inject placeholder env vars into sandboxes
//
// Replaces: cgi-bin/common.sh:253-305 (_inject_secret_placeholders)
//
// secrets.json format:
//   { "secrets": { "ANTHROPIC_API_KEY": {
//       "placeholder": "sk-placeholder-anthropic",
//       "value":       "sk-ant-real-key-here",
//       "allowed_hosts": ["api.anthropic.com"]
//   }}}
//
// The Odin daemon never touches real secret values at runtime — those are
// handled by the Go HTTPS proxy sidecar. This module only reads placeholders
// and writes them as environment variables into the sandbox's /etc/profile.d.
// ---------------------------------------------------------------------------

Secret :: struct {
	placeholder:   string   `json:"placeholder"`,
	value:         string   `json:"value"`,
	allowed_hosts: []string `json:"allowed_hosts"`,
}

Secrets_Config :: struct {
	secrets: map[string]Secret `json:"secrets"`,
}

Secrets_Error :: enum {
	None,
	File_Not_Found,
	Read_Failed,
	Parse_Failed,
	Mkdir_Failed,
	Write_Failed,
}

// Load and parse secrets.json. Returns File_Not_Found if absent.
load_secrets :: proc(path: string, allocator := context.allocator) -> (Secrets_Config, Secrets_Error) {
	if !os.exists(path) {
		return {}, .File_Not_Found
	}

	data, ok := os.read_entire_file(path, allocator)
	if !ok {
		return {}, .Read_Failed
	}

	config: Secrets_Config
	err := json.unmarshal(data, &config, allocator = allocator)
	if err != nil {
		return {}, .Parse_Failed
	}

	return config, .None
}

// Inject secret placeholders into a sandbox's filesystem.
//
// Creates /etc/profile.d/squash-secrets.sh in the sandbox's overlay upper
// layer with:
//   1. export KEY=<placeholder> for each secret
//   2. http_proxy / https_proxy pointing at the proxy sidecar
//   3. CA certificate trust env vars (if HTTPS proxy enabled)
//
// netns_index: the sandbox's network namespace index (0 if no netns).
// When present, proxy address is the gateway IP (10.200.{index}.1);
// otherwise falls back to 127.0.0.1.
inject_secret_placeholders :: proc(
	config: ^Config,
	sandbox_dir: string,
	netns_index: u8,
) -> Secrets_Error {
	secrets_file := secrets_path(config)
	secrets, serr := load_secrets(secrets_file, context.temp_allocator)
	if serr == .File_Not_Found {
		return .None // No secrets file — nothing to inject
	}
	if serr != .None {
		return serr
	}

	// Determine proxy host address
	proxy_host: string
	if netns_index > 0 {
		proxy_host = fmt.tprintf("10.200.%d.1", netns_index)
	} else {
		proxy_host = "127.0.0.1"
	}

	// Build the profile script
	buf: [4096]byte
	b := strings.builder_from_bytes(buf[:])

	// Export placeholder values for each secret
	for key, secret in secrets.secrets {
		fmt.sbprintf(&b, "export %s=%s\n", key, secret.placeholder)
	}

	// Proxy env vars (both lower and uppercase for compatibility)
	fmt.sbprintf(&b, "export http_proxy=http://%s:8888\n", proxy_host)
	fmt.sbprintf(&b, "export https_proxy=http://%s:8888\n", proxy_host)
	fmt.sbprintf(&b, "export HTTP_PROXY=http://%s:8888\n", proxy_host)
	fmt.sbprintf(&b, "export HTTPS_PROXY=http://%s:8888\n", proxy_host)

	// CA certificate trust for HTTPS proxy
	ca_cert_path := fmt.tprintf("%s/proxy-ca/ca.crt", config.data_dir)
	if os.exists(ca_cert_path) {
		fmt.sbprintf(&b, "export NODE_EXTRA_CA_CERTS=/usr/local/share/ca-certificates/sq-proxy-ca.crt\n")
		fmt.sbprintf(&b, "export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt\n")
		fmt.sbprintf(&b, "export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt\n")
	}

	script_content := strings.to_string(b)

	// Write profile script into overlay upper layer
	env_dir := fmt.tprintf("%s/upper/data/etc/profile.d", sandbox_dir)
	_ensure_dir_recursive(env_dir)

	script_path := fmt.tprintf("%s/squash-secrets.sh", env_dir)
	if !os.write_entire_file(script_path, transmute([]byte)script_content) {
		return .Write_Failed
	}

	// Copy CA cert into sandbox trust store (if HTTPS proxy enabled)
	if os.exists(ca_cert_path) {
		_inject_ca_cert(sandbox_dir, ca_cert_path)
	}

	return .None
}

// Copy the proxy CA certificate into the sandbox's trust store.
// - Copies to /usr/local/share/ca-certificates/sq-proxy-ca.crt
// - Appends to /etc/ssl/certs/ca-certificates.crt
_inject_ca_cert :: proc(sandbox_dir: string, ca_cert_path: string) {
	ca_data, ok := os.read_entire_file(ca_cert_path, context.temp_allocator)
	if !ok {
		return
	}

	// Copy CA cert file
	ca_dest_dir := fmt.tprintf("%s/upper/data/usr/local/share/ca-certificates", sandbox_dir)
	_ensure_dir_recursive(ca_dest_dir)
	ca_dest := fmt.tprintf("%s/sq-proxy-ca.crt", ca_dest_dir)
	os.write_entire_file(ca_dest, ca_data)

	// Append to system CA bundle (the merged overlay should have the base bundle)
	merged_bundle := fmt.tprintf("%s/merged/etc/ssl/certs/ca-certificates.crt", sandbox_dir)
	if os.exists(merged_bundle) {
		bundle_data, bundle_ok := os.read_entire_file(merged_bundle, context.temp_allocator)
		if bundle_ok {
			ssl_dir := fmt.tprintf("%s/upper/data/etc/ssl/certs", sandbox_dir)
			_ensure_dir_recursive(ssl_dir)
			upper_bundle := fmt.tprintf("%s/ca-certificates.crt", ssl_dir)

			// Concatenate original bundle + CA cert
			combined_len := len(bundle_data) + 1 + len(ca_data)
			combined := make([]byte, combined_len, context.temp_allocator)
			copy(combined, bundle_data)
			combined[len(bundle_data)] = '\n'
			copy(combined[len(bundle_data) + 1:], ca_data)

			os.write_entire_file(upper_bundle, combined)
		}
	}
}

// Recursively ensure a directory path exists.
_ensure_dir_recursive :: proc(path: string) {
	for i := 1; i < len(path); i += 1 {
		if path[i] == '/' {
			segment := path[:i]
			if !os.exists(segment) {
				os.make_directory(segment)
			}
		}
	}
	if !os.exists(path) {
		os.make_directory(path)
	}
}
