package manager

import (
	"os"
	"strings"
	"syscall"
	"testing"
	"time"

	"squashd/config"
)

func TestHasFeature(t *testing.T) {
	feats := []string{"gui", "secret-proxy"}
	for _, want := range []string{"gui", "GUI", "Secret-Proxy"} {
		if !hasFeature(feats, want) {
			t.Errorf("hasFeature(%v, %q) = false, want true", feats, want)
		}
	}
	for _, want := range []string{"", "tailscale", "guix"} {
		if hasFeature(feats, want) {
			t.Errorf("hasFeature(%v, %q) = true, want false", feats, want)
		}
	}
}

func TestMergeFeatures(t *testing.T) {
	cases := []struct {
		base, extra, want []string
	}{
		{nil, []string{"gui"}, []string{"gui"}},
		{[]string{"gui"}, []string{"gui"}, []string{"gui"}},
		{[]string{"gui"}, []string{"secret-proxy"}, []string{"gui", "secret-proxy"}},
		{[]string{"gui"}, nil, []string{"gui"}},
		{[]string{"a", "b"}, []string{"B", "c"}, []string{"a", "b", "c"}}, // case-insensitive dedup
	}
	for _, c := range cases {
		got := mergeFeatures(c.base, c.extra)
		if strings.Join(got, ",") != strings.Join(c.want, ",") {
			t.Errorf("mergeFeatures(%v, %v) = %v, want %v", c.base, c.extra, got, c.want)
		}
	}
}

func TestValidGUIResolution(t *testing.T) {
	for _, s := range []string{"1920x1080", "800x600", "1x1"} {
		if !validGUIResolution(s) {
			t.Errorf("validGUIResolution(%q) = false, want true", s)
		}
	}
	for _, s := range []string{"", "1920", "1920x", "x1080", "abcxdef", "12345678x1080"} {
		if validGUIResolution(s) {
			t.Errorf("validGUIResolution(%q) = true, want false", s)
		}
	}
}

func TestValidGUIDesktop(t *testing.T) {
	for _, s := range []string{"xfce", "minimal", "kde-plasma", "X1"} {
		if !validGUIDesktop(s) {
			t.Errorf("validGUIDesktop(%q) = false, want true", s)
		}
	}
	for _, s := range []string{"", "with space", "x;y", strings.Repeat("a", 33)} {
		if validGUIDesktop(s) {
			t.Errorf("validGUIDesktop(%q) = true, want false", s)
		}
	}
}

func TestGUIModuleResolution(t *testing.T) {
	// opts.Module wins
	if got := guiModule(nil, &GUIOpts{Module: "custom-gui"}); got != "custom-gui" {
		t.Errorf("guiModule opts: got %q", got)
	}
	// cfg.GUIModule next
	cfg := &config.Config{GUIModule: "510-gui-minimal"}
	if got := guiModule(cfg, nil); got != "510-gui-minimal" {
		t.Errorf("guiModule cfg: got %q", got)
	}
	// fallback
	if got := guiModule(nil, nil); got != "500-gui-base" {
		t.Errorf("guiModule default: got %q", got)
	}
}

func TestBrowserModuleResolution(t *testing.T) {
	cfg := &config.Config{BrowserModule: "520-browser-custom"}
	if got := browserModule(cfg); got != "520-browser-custom" {
		t.Errorf("browserModule cfg: got %q", got)
	}
	if got := browserModule(nil); got != "510-browser-base" {
		t.Errorf("browserModule default: got %q", got)
	}
}

func TestEnableGUIUnknownSandbox(t *testing.T) {
	mgr := New(&config.Config{})
	if _, err := mgr.EnableGUI("missing", nil); err == nil {
		t.Errorf("EnableGUI missing sandbox: want error, got nil")
	}
}

func TestGUIStatusUnknownSandbox(t *testing.T) {
	mgr := New(&config.Config{})
	if _, err := mgr.GUIStatus("missing"); err == nil {
		t.Errorf("GUIStatus missing sandbox: want error, got nil")
	}
}

func TestGUITargetUnknownSandbox(t *testing.T) {
	mgr := New(&config.Config{})
	if _, err := mgr.GUITarget("missing"); err == nil {
		t.Errorf("GUITarget missing sandbox: want error, got nil")
	}
}

func TestGUITargetReturnsRoutingFields(t *testing.T) {
	mgr := New(&config.Config{})
	dir := t.TempDir()
	for _, sub := range []string{".meta", "merged", "upper/data", "upper/work"} {
		if err := mkdirP(dir, sub); err != nil {
			t.Fatalf("mkdir: %v", err)
		}
	}
	s := &Sandbox{
		ID:    "demo",
		Dir:   dir,
		State: "ready",
		GUI: &GUIState{
			Enabled:      true,
			BwrapPID:     12345,
			NoVNCPort:    6080,
			SessionToken: "tok-abc",
		},
	}
	mgr.Register(s)
	tg, err := mgr.GUITarget("demo")
	if err != nil {
		t.Fatalf("GUITarget: %v", err)
	}
	if tg.BwrapPID != 12345 || tg.NoVNCPort != 6080 || tg.SessionToken != "tok-abc" {
		t.Errorf("GUITarget fields: %+v", tg)
	}
}

func TestGUITargetRejectsDisabled(t *testing.T) {
	mgr := New(&config.Config{})
	s := &Sandbox{ID: "demo", State: "ready", GUI: &GUIState{Enabled: false}}
	mgr.Register(s)
	if _, err := mgr.GUITarget("demo"); err == nil {
		t.Errorf("GUITarget on disabled GUI: want error, got nil")
	}
}

func TestOpenGUIBrowserWritesFIFO(t *testing.T) {
	mgr := New(&config.Config{})
	dir := t.TempDir()
	for _, sub := range []string{
		"merged/usr/local/bin",
		"merged/var/lib/sq-gui",
		"upper/data",
		"upper/work",
		".meta",
	} {
		if err := mkdirP(dir, sub); err != nil {
			t.Fatalf("mkdir %s: %v", sub, err)
		}
	}
	cmdPath := filepathJoin(dir, "merged/"+strings.TrimPrefix(browserOpenCommand, "/"))
	if err := os.WriteFile(cmdPath, []byte("#!/bin/sh\n"), 0755); err != nil {
		t.Fatalf("write browser opener: %v", err)
	}
	fifoPath := filepathJoin(dir, "merged/"+strings.TrimPrefix(defaultBrowserFIFO, "/"))
	if err := syscall.Mkfifo(fifoPath, 0600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	reader, err := os.OpenFile(fifoPath, os.O_RDONLY|syscall.O_NONBLOCK, 0)
	if err != nil {
		t.Fatalf("open fifo reader: %v", err)
	}
	defer reader.Close()

	mgr.Register(&Sandbox{
		ID:    "demo",
		Dir:   dir,
		State: "ready",
		GUI:   &GUIState{Enabled: true},
	})
	if err := mgr.OpenGUIBrowser("demo", "https://example.com"); err != nil {
		t.Fatalf("OpenGUIBrowser: %v", err)
	}

	buf := make([]byte, 128)
	var got string
	deadline := time.Now().Add(time.Second)
	for got == "" && time.Now().Before(deadline) {
		n, err := reader.Read(buf)
		if n > 0 {
			got = string(buf[:n])
			break
		}
		if err != nil && !strings.Contains(err.Error(), "resource temporarily unavailable") {
			t.Fatalf("read fifo: %v", err)
		}
		time.Sleep(10 * time.Millisecond)
	}
	if got != "https://example.com\n" {
		t.Fatalf("fifo command = %q, want URL newline", got)
	}
}

func TestOpenGUIBrowserRejectsDisabled(t *testing.T) {
	mgr := New(&config.Config{})
	mgr.Register(&Sandbox{ID: "demo", State: "ready", GUI: &GUIState{Enabled: false}})
	if err := mgr.OpenGUIBrowser("demo", "https://example.com"); err == nil {
		t.Errorf("OpenGUIBrowser disabled GUI: want error, got nil")
	}
}

func TestNewSessionTokenIsDistinct(t *testing.T) {
	a, err := newSessionToken()
	if err != nil {
		t.Fatalf("a: %v", err)
	}
	b, err := newSessionToken()
	if err != nil {
		t.Fatalf("b: %v", err)
	}
	if a == b || len(a) != 32 || len(b) != 32 {
		t.Errorf("session tokens not distinct/sized: a=%q b=%q", a, b)
	}
}

func TestPollPIDZeroTimeoutReadsOnce(t *testing.T) {
	path := filepathJoin(t.TempDir(), "gui.pid")
	if err := os.WriteFile(path, []byte("12345\n"), 0644); err != nil {
		t.Fatalf("write pidfile: %v", err)
	}
	if got := pollPID(path, 0); got != 12345 {
		t.Errorf("pollPID zero timeout = %d, want 12345", got)
	}
}

func TestWriteGUIConfigQuotesPassword(t *testing.T) {
	dir := t.TempDir()
	s := &Sandbox{Dir: dir}
	password := "pa'ss; rm -rf / $(echo bad)"
	if err := writeGUIConfig(s, "xfce", "1280x720", password); err != nil {
		t.Fatalf("writeGUIConfig: %v", err)
	}
	b, err := os.ReadFile(filepathJoin(dir, "merged/etc/sq-gui.conf"))
	if err != nil {
		t.Fatalf("read gui config: %v", err)
	}
	want := "VNC_PASSWORD=" + shellSingleQuote(password) + "\n"
	if !strings.Contains(string(b), want) {
		t.Fatalf("gui config missing quoted password %q:\n%s", want, string(b))
	}
}

func TestCreateSandboxRejectsInvalidGUIModule(t *testing.T) {
	cfg := &config.Config{DataDir: t.TempDir(), Backend: "chroot", GUIModule: "../bad"}
	mgr := New(cfg)
	_, err := mgr.createSandbox("demo", CreateOpts{Features: []string{"gui"}})
	if err == nil || !strings.Contains(err.Error(), "invalid module") {
		t.Fatalf("createSandbox invalid gui module err = %v, want invalid module", err)
	}
}

func mkdirP(base, sub string) error {
	return os.MkdirAll(filepathJoin(base, sub), 0755)
}

// filepathJoin avoids importing filepath at the top of the test file just
// for one helper; we already do path concat in the production code. Inline
// minimal join: assumes unix-style paths.
func filepathJoin(a, b string) string { return a + "/" + b }
