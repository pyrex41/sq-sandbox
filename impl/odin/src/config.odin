package squashd

import "core:fmt"
import "core:os"
import "core:strconv"

// ---------------------------------------------------------------------------
// Config — daemon configuration parsed from environment variables
// ---------------------------------------------------------------------------

Backend :: enum {
	Chroot,
	Firecracker,
}

Config :: struct {
	backend:            Backend,
	data_dir:           string,
	port:               u16,
	auth_token:         Maybe(string),
	s3_bucket:          Maybe(string),
	s3_endpoint:        Maybe(string),
	s3_region:          string,
	s3_prefix:          string,
	ephemeral:          bool,
	upper_limit_mb:     u64,
	max_sandboxes:      int,
	proxy_https:        bool,
	tailscale_authkey:  Maybe(string),
	tailscale_hostname: string,
	// Cached derived paths — computed once at startup, never re-allocated.
	_modules_dir:       string,
	_sandboxes_dir:     string,
	_secrets_path:      string,
	_proxy_ca_dir:      string,
}

// Parse configuration from environment variables.
// String values returned by os.lookup_env are allocated with the provided allocator,
// so they persist for the lifetime of the daemon.
config_from_env :: proc(allocator := context.allocator) -> Config {
	context.allocator = allocator

	get_env :: proc(key: string, fallback: string) -> string {
		if val, ok := os.lookup_env(key); ok {
			return val
		}
		return fallback
	}

	maybe_env :: proc(key: string) -> Maybe(string) {
		if val, ok := os.lookup_env(key); ok && len(val) > 0 {
			return val
		}
		return nil
	}

	port_str := get_env("SQUASH_PORT", "8080")
	port_val, _ := strconv.parse_int(port_str, 10)
	if port_val < 1 || port_val > 65535 {
		port_val = 8080
	}

	upper_str := get_env("SQUASH_UPPER_LIMIT_MB", "512")
	upper_val, _ := strconv.parse_u64_of_base(upper_str, 10)

	max_sb_str := get_env("SQUASH_MAX_SANDBOXES", "100")
	max_sb_val, _ := strconv.parse_int(max_sb_str, 10)

	data_dir := get_env("SQUASH_DATA", "/data")

	return Config{
		backend            = .Firecracker if get_env("SQUASH_BACKEND", "") == "firecracker" else .Chroot,
		data_dir           = data_dir,
		port               = u16(port_val),
		auth_token         = maybe_env("SQUASH_AUTH_TOKEN"),
		s3_bucket          = maybe_env("SQUASH_S3_BUCKET"),
		s3_endpoint        = maybe_env("SQUASH_S3_ENDPOINT"),
		s3_region          = get_env("SQUASH_S3_REGION", "us-east-1"),
		s3_prefix          = get_env("SQUASH_S3_PREFIX", ""),
		ephemeral          = get_env("SQUASH_EPHEMERAL", "") == "1",
		upper_limit_mb     = upper_val,
		max_sandboxes      = max_sb_val,
		proxy_https        = get_env("SQUASH_PROXY_HTTPS", "") == "1",
		tailscale_authkey  = maybe_env("TAILSCALE_AUTHKEY"),
		tailscale_hostname = get_env("TAILSCALE_HOSTNAME", "squash"),
		// Cache derived paths once — avoids repeated tprintf allocations.
		_modules_dir       = fmt.aprintf("%s/modules", data_dir),
		_sandboxes_dir     = fmt.aprintf("%s/sandboxes", data_dir),
		_secrets_path      = fmt.aprintf("%s/secrets.json", data_dir),
		_proxy_ca_dir      = fmt.aprintf("%s/proxy-ca", data_dir),
	}
}

// Check whether a secrets.json file exists for this configuration.
secrets_exist :: proc(config: ^Config) -> bool {
	return os.exists(secrets_path(config))
}

modules_dir :: proc(c: ^Config) -> string {
	return c._modules_dir
}

sandboxes_dir :: proc(c: ^Config) -> string {
	return c._sandboxes_dir
}

secrets_path :: proc(c: ^Config) -> string {
	return c._secrets_path
}

proxy_ca_dir :: proc(c: ^Config) -> string {
	return c._proxy_ca_dir
}
