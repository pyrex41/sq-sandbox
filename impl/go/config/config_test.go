package config

import (
	"os"
	"testing"
)

func TestFromEnvDefaults(t *testing.T) {
	os.Setenv("SQUASH_DATA", "/data")
	defer os.Unsetenv("SQUASH_DATA")

	c := FromEnv()
	if c.DataDir != "/data" {
		t.Errorf("DataDir = %q, want /data", c.DataDir)
	}
	if c.Port != 8080 {
		t.Errorf("Port = %d, want 8080", c.Port)
	}
	if c.Backend != "chroot" {
		t.Errorf("Backend = %q, want chroot", c.Backend)
	}
	if c.SnapshotBackend != "squashfs" {
		t.Errorf("SnapshotBackend = %q, want squashfs", c.SnapshotBackend)
	}
}

func TestValidate(t *testing.T) {
	tests := []struct {
		name    string
		mutate  func(*Config)
		wantErr bool
	}{
		{"valid defaults", func(c *Config) {}, false},
		{"empty data dir", func(c *Config) { c.DataDir = "" }, true},
		{"dotdot in data dir", func(c *Config) { c.DataDir = "/data/../etc" }, true},
		{"port 0", func(c *Config) { c.Port = 0 }, true},
		{"port 65536", func(c *Config) { c.Port = 65536 }, true},
		{"port 1 valid", func(c *Config) { c.Port = 1 }, false},
		{"max sandboxes 0", func(c *Config) { c.MaxSandboxes = 0 }, true},
		{"bad backend", func(c *Config) { c.Backend = "docker" }, true},
		{"gvisor backend valid", func(c *Config) { c.Backend = "gvisor" }, false},
		{"bad snapshot backend", func(c *Config) { c.SnapshotBackend = "s3" }, true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := Config{
				DataDir:         "/data",
				Port:            8080,
				MaxSandboxes:    100,
				UpperLimitMB:    512,
				Backend:         "chroot",
				SnapshotBackend: "squashfs",
			}
			tt.mutate(&c)
			err := c.Validate()
			if (err != nil) != tt.wantErr {
				t.Errorf("Validate() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestModulesDir(t *testing.T) {
	c := Config{DataDir: "/data"}
	if got := c.ModulesDir(); got != "/data/modules" {
		t.Errorf("ModulesDir() = %q, want /data/modules", got)
	}
}

func TestGUIModuleDefault(t *testing.T) {
	os.Setenv("SQUASH_DATA", "/data")
	defer os.Unsetenv("SQUASH_DATA")
	os.Unsetenv("SQUASH_GUI_MODULE")
	os.Unsetenv("SQUASH_DEFAULT_FEATURES")
	c := FromEnv()
	if c.GUIModule != "500-gui-base" {
		t.Errorf("GUIModule default = %q, want 500-gui-base", c.GUIModule)
	}
	if len(c.DefaultFeatures) != 0 {
		t.Errorf("DefaultFeatures default = %v, want empty", c.DefaultFeatures)
	}
}

func TestDefaultFeaturesParsesCommaList(t *testing.T) {
	os.Setenv("SQUASH_DATA", "/data")
	os.Setenv("SQUASH_DEFAULT_FEATURES", "gui, secret-proxy")
	os.Setenv("SQUASH_GUI_MODULE", "510-gui-minimal")
	defer os.Unsetenv("SQUASH_DATA")
	defer os.Unsetenv("SQUASH_DEFAULT_FEATURES")
	defer os.Unsetenv("SQUASH_GUI_MODULE")

	c := FromEnv()
	if c.GUIModule != "510-gui-minimal" {
		t.Errorf("GUIModule = %q, want 510-gui-minimal", c.GUIModule)
	}
	if len(c.DefaultFeatures) != 2 || c.DefaultFeatures[0] != "gui" ||
		c.DefaultFeatures[1] != "secret-proxy" {
		t.Errorf("DefaultFeatures = %v, want [gui secret-proxy]", c.DefaultFeatures)
	}
}
