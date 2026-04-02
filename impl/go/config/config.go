package config

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

// Config holds all runtime configuration derived from environment variables.
type Config struct {
	DataDir       string
	Port          int
	MaxSandboxes  int
	UpperLimitMB  int
	Backend       string // "chroot", "firecracker", "gvisor"
	AuthToken     string // empty = no auth
	S3Bucket      string
	S3Endpoint    string
	S3Region      string
	S3Prefix      string
	ProxyHTTPS    bool
	UpperBackend  string // "tmpfs", "btrfs", "loop"
	LocalCacheDir string
	BusSockPath   string
	SnapshotBackend string // "squashfs", "irmin"
	StoreSockPath string
}

// FromEnv builds a Config from environment variables.
func FromEnv() Config {
	dataDir := envOr("SQUASH_DATA_DIR", envOr("SQUASH_DATA", "/data"))
	c := Config{
		DataDir:         dataDir,
		Port:            envInt("SQUASH_PORT", 8080),
		MaxSandboxes:    envInt("SQUASH_MAX_SANDBOXES", 100),
		UpperLimitMB:    envInt("SQUASH_UPPER_LIMIT_MB", 512),
		Backend:         envOr("SQUASH_BACKEND", "chroot"),
		AuthToken:       os.Getenv("SQUASH_AUTH_TOKEN"),
		S3Bucket:        os.Getenv("SQUASH_S3_BUCKET"),
		S3Endpoint:      os.Getenv("SQUASH_S3_ENDPOINT"),
		S3Region:        envOr("SQUASH_S3_REGION", "us-east-1"),
		S3Prefix:        os.Getenv("SQUASH_S3_PREFIX"),
		ProxyHTTPS:      envBool("SQUASH_PROXY_HTTPS"),
		UpperBackend:    envOr("SQUASH_UPPER_BACKEND", "tmpfs"),
		SnapshotBackend: envOr("SQUASH_SNAPSHOT_BACKEND", "squashfs"),
	}
	c.LocalCacheDir = envOr("SQUASH_LOCAL_CACHE_DIR", filepath.Join(dataDir, "cache"))
	c.BusSockPath = envOr("SQUASH_BUS_SOCK", filepath.Join(dataDir, ".sq-bus.sock"))
	c.StoreSockPath = envOr("SQUASH_STORE_SOCK", filepath.Join(dataDir, ".sq-store.sock"))
	return c
}

// Validate returns an error if the config is invalid.
func (c *Config) Validate() error {
	if c.DataDir == "" || strings.Contains(c.DataDir, "..") {
		return fmt.Errorf("SQUASH_DATA/SQUASH_DATA_DIR must be set and must not contain ..")
	}
	if c.Port < 1 || c.Port > 65535 {
		return fmt.Errorf("SQUASH_PORT must be 1-65535, got %d", c.Port)
	}
	if c.MaxSandboxes < 1 || c.MaxSandboxes > 10000 {
		return fmt.Errorf("SQUASH_MAX_SANDBOXES must be 1-10000, got %d", c.MaxSandboxes)
	}
	if c.UpperLimitMB < 16 || c.UpperLimitMB > 32768 {
		return fmt.Errorf("SQUASH_UPPER_LIMIT_MB must be 16-32768, got %d", c.UpperLimitMB)
	}
	switch c.Backend {
	case "chroot", "firecracker", "gvisor":
	default:
		return fmt.Errorf("SQUASH_BACKEND must be chroot, firecracker, or gvisor, got %q", c.Backend)
	}
	switch c.SnapshotBackend {
	case "squashfs", "irmin":
	default:
		return fmt.Errorf("SQUASH_SNAPSHOT_BACKEND must be squashfs or irmin, got %q", c.SnapshotBackend)
	}
	return nil
}

// ModulesDir returns the path to the modules directory.
func (c *Config) ModulesDir() string { return filepath.Join(c.DataDir, "modules") }

// SandboxesDir returns the path to the sandboxes directory.
func (c *Config) SandboxesDir() string { return filepath.Join(c.DataDir, "sandboxes") }

// ProxyCADir returns the path to the proxy CA directory.
func (c *Config) ProxyCADir() string { return filepath.Join(c.DataDir, "proxy-ca") }

// SecretsFile returns the path to secrets.json.
func (c *Config) SecretsFile() string { return filepath.Join(c.DataDir, "secrets.json") }

// ── env helpers ──────────────────────────────────────────────────────────────

func envOr(key, def string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return def
}

func envInt(key string, def int) int {
	v := os.Getenv(key)
	if v == "" {
		return def
	}
	n, err := strconv.Atoi(v)
	if err != nil {
		return def
	}
	return n
}

func envBool(key string) bool {
	v := strings.ToLower(strings.TrimSpace(os.Getenv(key)))
	return v == "1" || v == "true"
}
