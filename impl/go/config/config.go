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
	DataDir               string
	Port                  int
	MaxSandboxes          int
	UpperLimitMB          int
	Backend               string // "chroot", "firecracker", "gvisor"
	AuthToken             string // empty = no auth
	S3Bucket              string
	S3Endpoint            string
	S3Region              string
	S3Prefix              string
	ProxyHTTPS            bool
	UpperBackend          string // "tmpfs", "btrfs", "loop"
	LocalCacheDir         string
	BusSockPath           string
	SnapshotBackend       string // "squashfs", "irmin"
	StoreSockPath         string
	GUIModule             string   // default GUI layer (e.g. "500-gui-base")
	DefaultFeatures       []string // features auto-applied to every new sandbox
	ControlDBPath         string   // SQLite control-plane DB path
	StripeSecretKey       string
	StripeWebhookSecret   string
	StripeCheckoutPriceID string
	PublicBaseURL         string
	ShenAdmissionCommand  string
	ShenPolicyPath        string
	USDCNetwork           string
	USDCChainID           string
	USDCTokenAddress      string
	USDCReceiveAddress    string
	USDCExplorerTxBaseURL string
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
	c.GUIModule = envOr("SQUASH_GUI_MODULE", "500-gui-base")
	c.ControlDBPath = envOr("SQUASH_CONTROL_DB", filepath.Join(dataDir, "app.db"))
	c.StripeSecretKey = os.Getenv("STRIPE_SECRET_KEY")
	c.StripeWebhookSecret = os.Getenv("STRIPE_WEBHOOK_SECRET")
	c.StripeCheckoutPriceID = os.Getenv("STRIPE_CHECKOUT_PRICE_ID")
	c.PublicBaseURL = envOr("SQUASH_PUBLIC_BASE_URL", "http://localhost:8080")
	c.ShenAdmissionCommand = os.Getenv("SQUASH_SHEN_ADMISSION_CMD")
	c.ShenPolicyPath = envOr("SQUASH_SHEN_POLICY", filepath.Join(dataDir, "shen-backpressure.shen"))
	c.USDCNetwork = envOr("SQUASH_USDC_NETWORK", "base")
	c.USDCChainID = envOr("SQUASH_USDC_CHAIN_ID", "8453")
	c.USDCTokenAddress = envOr("SQUASH_USDC_TOKEN_ADDRESS", "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913")
	c.USDCReceiveAddress = os.Getenv("SQUASH_USDC_RECEIVE_ADDRESS")
	c.USDCExplorerTxBaseURL = envOr("SQUASH_USDC_EXPLORER_TX_BASE_URL", "https://basescan.org/tx/")
	if v := os.Getenv("SQUASH_DEFAULT_FEATURES"); v != "" {
		for _, p := range strings.Split(v, ",") {
			if p = strings.TrimSpace(p); p != "" {
				c.DefaultFeatures = append(c.DefaultFeatures, p)
			}
		}
	}
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
