package manager

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"squashd/config"
)

func TestWriteAndReadMetaWithFeaturesAndGUI(t *testing.T) {
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".meta"), 0755); err != nil {
		t.Fatalf("mkdir meta: %v", err)
	}
	original := &Sandbox{
		ID:           "demo",
		Dir:          dir,
		Owner:        "alice",
		Layers:       []string{"000-base-alpine", "500-gui-base"},
		Backend:      "chroot",
		CPU:          2,
		MemoryMB:     1024,
		MaxLifetimeS: 600,
		AllowNet:     []string{"api.anthropic.com"},
		Features:     []string{"gui"},
		GUI: &GUIState{
			Enabled:    true,
			Desktop:    "xfce",
			Resolution: "1280x720",
			Module:     "500-gui-base",
			JobID:      7,
			NoVNCPort:  6080,
			VNCPort:    5900,
			StartedAt:  time.Now().UTC().Format(time.RFC3339),
		},
		CreatedAt:    time.Now(),
		LastActiveAt: time.Now(),
	}
	if err := writeMeta(dir, original); err != nil {
		t.Fatalf("writeMeta: %v", err)
	}
	loaded, err := readMeta("demo", dir)
	if err != nil {
		t.Fatalf("readMeta: %v", err)
	}
	if len(loaded.Features) != 1 || loaded.Features[0] != "gui" {
		t.Errorf("features round-trip: %v", loaded.Features)
	}
	if loaded.GUI == nil || !loaded.GUI.Enabled || loaded.GUI.Desktop != "xfce" ||
		loaded.GUI.Resolution != "1280x720" || loaded.GUI.JobID != 7 {
		t.Errorf("gui round-trip: %+v", loaded.GUI)
	}
}

func TestInjectSecretsWritesPlaceholderExportsAndCABundle(t *testing.T) {
	dataDir := t.TempDir()
	cfg := &config.Config{
		DataDir:    dataDir,
		ProxyHTTPS: true,
	}

	if err := os.MkdirAll(filepath.Dir(cfg.SecretsFile()), 0755); err != nil {
		t.Fatalf("mkdir secrets dir: %v", err)
	}
	secretsJSON := `{
  "secrets": {
    "XAI_API_KEY": { "placeholder": "sk-placeholder-xai" },
    "GITLAB_TOKEN": { "placeholder": "glpat-placeholder" }
  }
}`
	if err := os.WriteFile(cfg.SecretsFile(), []byte(secretsJSON), 0644); err != nil {
		t.Fatalf("write secrets.json: %v", err)
	}

	if err := os.MkdirAll(cfg.ProxyCADir(), 0755); err != nil {
		t.Fatalf("mkdir proxy ca dir: %v", err)
	}
	caData := []byte("-----BEGIN CERTIFICATE-----\nsq-secret-proxy CA\n-----END CERTIFICATE-----\n")
	if err := os.WriteFile(filepath.Join(cfg.ProxyCADir(), "ca.crt"), caData, 0644); err != nil {
		t.Fatalf("write proxy ca: %v", err)
	}

	sandboxDir := t.TempDir()
	s := &Sandbox{Dir: sandboxDir}

	mergedBundlePath := filepath.Join(s.mergedDir(), "etc", "ssl", "certs", "ca-certificates.crt")
	if err := os.MkdirAll(filepath.Dir(mergedBundlePath), 0755); err != nil {
		t.Fatalf("mkdir merged bundle dir: %v", err)
	}
	if err := os.WriteFile(mergedBundlePath, []byte("system-ca\n"), 0644); err != nil {
		t.Fatalf("write merged bundle: %v", err)
	}

	if err := injectSecrets(cfg, s); err != nil {
		t.Fatalf("injectSecrets: %v", err)
	}

	profileData, err := os.ReadFile(filepath.Join(s.upperDataDir(), "etc", "profile.d", "squash-secrets.sh"))
	if err != nil {
		t.Fatalf("read squash-secrets.sh: %v", err)
	}
	profile := string(profileData)
	for _, want := range []string{
		`export GITLAB_TOKEN="glpat-placeholder"`,
		`export XAI_API_KEY="sk-placeholder-xai"`,
		`export HTTPS_PROXY=http://10.0.2.2:8888`,
		`export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt`,
	} {
		if !strings.Contains(profile, want) {
			t.Fatalf("squash-secrets.sh missing %q:\n%s", want, profile)
		}
	}

	copiedCA, err := os.ReadFile(filepath.Join(s.upperDataDir(), "usr", "local", "share", "ca-certificates", "sq-proxy-ca.crt"))
	if err != nil {
		t.Fatalf("read copied proxy ca: %v", err)
	}
	if string(copiedCA) != string(caData) {
		t.Fatalf("copied proxy ca mismatch:\nwant %q\ngot  %q", string(caData), string(copiedCA))
	}

	upperBundle, err := os.ReadFile(filepath.Join(s.upperDataDir(), "etc", "ssl", "certs", "ca-certificates.crt"))
	if err != nil {
		t.Fatalf("read upper bundle: %v", err)
	}
	for _, want := range []string{"system-ca", "sq-secret-proxy CA"} {
		if !strings.Contains(string(upperBundle), want) {
			t.Fatalf("upper bundle missing %q:\n%s", want, string(upperBundle))
		}
	}
}
