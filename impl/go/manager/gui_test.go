package manager

import (
	"strings"
	"testing"

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
