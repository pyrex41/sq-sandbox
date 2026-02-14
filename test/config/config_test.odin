package config_test

// Standalone tests for Config parsing logic.
// Duplicates the core config types and parsing from src/config.odin so we can
// test in isolation without requiring the full squashd package to compile.

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:testing"

// --- Duplicated types from src/config.odin ---

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
}

// --- Duplicated parsing logic ---

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

	upper_str := get_env("SQUASH_UPPER_LIMIT_MB", "512")
	upper_val, _ := strconv.parse_u64_of_base(upper_str, 10)

	max_sb_str := get_env("SQUASH_MAX_SANDBOXES", "100")
	max_sb_val, _ := strconv.parse_int(max_sb_str, 10)

	return Config{
		backend            = .Firecracker if get_env("SQUASH_BACKEND", "") == "firecracker" else .Chroot,
		data_dir           = get_env("SQUASH_DATA", "/data"),
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
	}
}

modules_dir :: proc(c: ^Config) -> string {
	return fmt.tprintf("%s/modules", c.data_dir)
}

sandboxes_dir :: proc(c: ^Config) -> string {
	return fmt.tprintf("%s/sandboxes", c.data_dir)
}

secrets_path :: proc(c: ^Config) -> string {
	return fmt.tprintf("%s/secrets.json", c.data_dir)
}

proxy_ca_dir :: proc(c: ^Config) -> string {
	return fmt.tprintf("%s/proxy-ca", c.data_dir)
}

// --- Helper: set env vars for a test, return cleanup proc ---

set_env :: proc(key: string, value: string) {
	os.set_env(key, value)
}

unset_env :: proc(key: string) {
	os.unset_env(key)
}

// Clear all SQUASH_* and TAILSCALE_* env vars to get a clean slate.
clear_config_env :: proc() {
	keys := []string{
		"SQUASH_BACKEND", "SQUASH_DATA", "SQUASH_PORT",
		"SQUASH_AUTH_TOKEN", "SQUASH_S3_BUCKET", "SQUASH_S3_ENDPOINT",
		"SQUASH_S3_REGION", "SQUASH_S3_PREFIX", "SQUASH_EPHEMERAL",
		"SQUASH_UPPER_LIMIT_MB", "SQUASH_MAX_SANDBOXES",
		"SQUASH_PROXY_HTTPS", "TAILSCALE_AUTHKEY", "TAILSCALE_HOSTNAME",
	}
	for key in keys {
		unset_env(key)
	}
}

// --- Tests ---

@(test)
test_defaults :: proc(t: ^testing.T) {
	clear_config_env()

	cfg := config_from_env()

	testing.expect_value(t, cfg.backend, Backend.Chroot)
	testing.expect_value(t, cfg.data_dir, "/data")
	testing.expect_value(t, cfg.port, u16(8080))
	testing.expect_value(t, cfg.s3_region, "us-east-1")
	testing.expect_value(t, cfg.s3_prefix, "")
	testing.expect_value(t, cfg.ephemeral, false)
	testing.expect_value(t, cfg.upper_limit_mb, u64(512))
	testing.expect_value(t, cfg.max_sandboxes, 100)
	testing.expect_value(t, cfg.proxy_https, false)
	testing.expect_value(t, cfg.tailscale_hostname, "squash")

	// Maybe fields should be nil with no env vars set
	_, has_token := cfg.auth_token.?
	testing.expect(t, !has_token, "auth_token should be nil")
	_, has_bucket := cfg.s3_bucket.?
	testing.expect(t, !has_bucket, "s3_bucket should be nil")
	_, has_endpoint := cfg.s3_endpoint.?
	testing.expect(t, !has_endpoint, "s3_endpoint should be nil")
	_, has_tskey := cfg.tailscale_authkey.?
	testing.expect(t, !has_tskey, "tailscale_authkey should be nil")
}

@(test)
test_custom_values :: proc(t: ^testing.T) {
	clear_config_env()

	set_env("SQUASH_BACKEND", "firecracker")
	set_env("SQUASH_DATA", "/mnt/sandbox-data")
	set_env("SQUASH_PORT", "9090")
	set_env("SQUASH_AUTH_TOKEN", "my-secret-token")
	set_env("SQUASH_S3_BUCKET", "my-bucket")
	set_env("SQUASH_S3_ENDPOINT", "https://s3.custom.endpoint")
	set_env("SQUASH_S3_REGION", "eu-west-1")
	set_env("SQUASH_S3_PREFIX", "prefix/path")
	set_env("SQUASH_EPHEMERAL", "1")
	set_env("SQUASH_UPPER_LIMIT_MB", "1024")
	set_env("SQUASH_MAX_SANDBOXES", "50")
	set_env("SQUASH_PROXY_HTTPS", "1")
	set_env("TAILSCALE_AUTHKEY", "tskey-abc123")
	set_env("TAILSCALE_HOSTNAME", "my-sandbox")
	defer clear_config_env()

	cfg := config_from_env()

	testing.expect_value(t, cfg.backend, Backend.Firecracker)
	testing.expect_value(t, cfg.data_dir, "/mnt/sandbox-data")
	testing.expect_value(t, cfg.port, u16(9090))
	testing.expect_value(t, cfg.s3_region, "eu-west-1")
	testing.expect_value(t, cfg.s3_prefix, "prefix/path")
	testing.expect_value(t, cfg.ephemeral, true)
	testing.expect_value(t, cfg.upper_limit_mb, u64(1024))
	testing.expect_value(t, cfg.max_sandboxes, 50)
	testing.expect_value(t, cfg.proxy_https, true)
	testing.expect_value(t, cfg.tailscale_hostname, "my-sandbox")

	// Maybe fields should be populated
	token, has_token := cfg.auth_token.?
	testing.expect(t, has_token, "auth_token should be set")
	testing.expect_value(t, token, "my-secret-token")

	bucket, has_bucket := cfg.s3_bucket.?
	testing.expect(t, has_bucket, "s3_bucket should be set")
	testing.expect_value(t, bucket, "my-bucket")

	endpoint, has_endpoint := cfg.s3_endpoint.?
	testing.expect(t, has_endpoint, "s3_endpoint should be set")
	testing.expect_value(t, endpoint, "https://s3.custom.endpoint")

	tskey, has_tskey := cfg.tailscale_authkey.?
	testing.expect(t, has_tskey, "tailscale_authkey should be set")
	testing.expect_value(t, tskey, "tskey-abc123")
}

@(test)
test_backend_chroot_explicit :: proc(t: ^testing.T) {
	clear_config_env()
	set_env("SQUASH_BACKEND", "chroot")
	defer clear_config_env()

	cfg := config_from_env()
	testing.expect_value(t, cfg.backend, Backend.Chroot)
}

@(test)
test_backend_unknown_defaults_chroot :: proc(t: ^testing.T) {
	clear_config_env()
	set_env("SQUASH_BACKEND", "something-unknown")
	defer clear_config_env()

	cfg := config_from_env()
	testing.expect_value(t, cfg.backend, Backend.Chroot)
}

@(test)
test_empty_maybe_fields_are_nil :: proc(t: ^testing.T) {
	// Setting an env var to empty string should result in nil Maybe
	clear_config_env()
	set_env("SQUASH_AUTH_TOKEN", "")
	set_env("SQUASH_S3_BUCKET", "")
	set_env("SQUASH_S3_ENDPOINT", "")
	set_env("TAILSCALE_AUTHKEY", "")
	defer clear_config_env()

	cfg := config_from_env()

	_, has_token := cfg.auth_token.?
	testing.expect(t, !has_token, "empty auth_token should be nil")
	_, has_bucket := cfg.s3_bucket.?
	testing.expect(t, !has_bucket, "empty s3_bucket should be nil")
	_, has_endpoint := cfg.s3_endpoint.?
	testing.expect(t, !has_endpoint, "empty s3_endpoint should be nil")
	_, has_tskey := cfg.tailscale_authkey.?
	testing.expect(t, !has_tskey, "empty tailscale_authkey should be nil")
}

@(test)
test_ephemeral_only_on_1 :: proc(t: ^testing.T) {
	clear_config_env()

	// "true" should not enable ephemeral — only "1"
	set_env("SQUASH_EPHEMERAL", "true")
	cfg := config_from_env()
	testing.expect_value(t, cfg.ephemeral, false)
	unset_env("SQUASH_EPHEMERAL")

	// "0" should not enable ephemeral
	set_env("SQUASH_EPHEMERAL", "0")
	cfg2 := config_from_env()
	testing.expect_value(t, cfg2.ephemeral, false)
	unset_env("SQUASH_EPHEMERAL")

	// "1" enables ephemeral
	set_env("SQUASH_EPHEMERAL", "1")
	cfg3 := config_from_env()
	testing.expect_value(t, cfg3.ephemeral, true)

	clear_config_env()
}

@(test)
test_helper_procs :: proc(t: ^testing.T) {
	cfg := Config{
		data_dir = "/my/data",
	}

	testing.expect_value(t, modules_dir(&cfg), "/my/data/modules")
	testing.expect_value(t, sandboxes_dir(&cfg), "/my/data/sandboxes")
	testing.expect_value(t, secrets_path(&cfg), "/my/data/secrets.json")
	testing.expect_value(t, proxy_ca_dir(&cfg), "/my/data/proxy-ca")
}

@(test)
test_proxy_https_only_on_1 :: proc(t: ^testing.T) {
	clear_config_env()

	set_env("SQUASH_PROXY_HTTPS", "true")
	cfg := config_from_env()
	testing.expect_value(t, cfg.proxy_https, false)
	unset_env("SQUASH_PROXY_HTTPS")

	set_env("SQUASH_PROXY_HTTPS", "1")
	cfg2 := config_from_env()
	testing.expect_value(t, cfg2.proxy_https, true)

	clear_config_env()
}
