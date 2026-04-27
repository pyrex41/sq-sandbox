package api

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"squashd/config"
	"squashd/manager"
)

func newTestHandler(cfg *config.Config) http.Handler {
	if cfg == nil {
		cfg = &config.Config{DataDir: "/data", Port: 8080, Backend: "chroot", SnapshotBackend: "squashfs"}
	}
	return NewHandler(cfg, manager.New(cfg))
}

func TestHealthEndpoint(t *testing.T) {
	h := newTestHandler(nil)
	req := httptest.NewRequest("GET", "/cgi-bin/health", nil)
	w := httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code != 200 {
		t.Fatalf("health status = %d, want 200", w.Code)
	}
	body := w.Body.String()
	for _, want := range []string{`"status":"ok"`, `"backend":"chroot"`} {
		if !strings.Contains(body, want) {
			t.Errorf("health body missing %q; got: %s", want, body)
		}
	}
}

func TestAuthMiddleware(t *testing.T) {
	cfg := &config.Config{
		DataDir: "/data", Port: 8080, Backend: "chroot",
		SnapshotBackend: "squashfs",
		AuthToken:       "secret",
	}
	h := newTestHandler(cfg)

	// Health is public — no token needed
	req := httptest.NewRequest("GET", "/cgi-bin/health", nil)
	w := httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code != 200 {
		t.Errorf("health without token = %d, want 200", w.Code)
	}

	// API endpoint without token → 401
	req = httptest.NewRequest("GET", "/cgi-bin/api/sandboxes", nil)
	w = httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code != 401 {
		t.Errorf("api without token = %d, want 401", w.Code)
	}

	// API endpoint with token → passes auth (501 not implemented is fine here)
	req = httptest.NewRequest("GET", "/cgi-bin/api/sandboxes", nil)
	req.Header.Set("Authorization", "Bearer secret")
	w = httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code == 401 {
		t.Errorf("api with token = 401, expected pass-through")
	}
}

func TestValidID(t *testing.T) {
	for _, id := range []string{"abc", "abc-123", "a_b", "A1"} {
		if !validID(id) {
			t.Errorf("validID(%q) = false, want true", id)
		}
	}
	for _, id := range []string{"", "-abc", "abc-", "a/b", "a b", strings.Repeat("a", 65)} {
		if validID(id) {
			t.Errorf("validID(%q) = true, want false", id)
		}
	}
}

func TestParseLayers(t *testing.T) {
	// string form
	layers, ok := parseLayers("000-base,100-python")
	if !ok || len(layers) != 2 || layers[0] != "000-base" {
		t.Errorf("parseLayers string: got %v %v", layers, ok)
	}
	// array form
	layers, ok = parseLayers([]any{"000-base", "100-python"})
	if !ok || len(layers) != 2 {
		t.Errorf("parseLayers array: got %v %v", layers, ok)
	}
	// nil → default
	layers, ok = parseLayers(nil)
	if !ok || len(layers) != 1 || layers[0] != "000-base-alpine" {
		t.Errorf("parseLayers nil: got %v %v", layers, ok)
	}
}

func TestParseFeatures(t *testing.T) {
	// nil → no features, ok
	feats, ok := parseFeatures(nil)
	if !ok || feats != nil {
		t.Errorf("parseFeatures nil: got %v %v", feats, ok)
	}
	// empty string → no features, ok
	feats, ok = parseFeatures("")
	if !ok || feats != nil {
		t.Errorf("parseFeatures empty string: got %v %v", feats, ok)
	}
	// comma-separated string
	feats, ok = parseFeatures("gui, secret-proxy")
	if !ok || len(feats) != 2 || feats[0] != "gui" || feats[1] != "secret-proxy" {
		t.Errorf("parseFeatures string: got %v %v", feats, ok)
	}
	// array of strings
	feats, ok = parseFeatures([]any{"gui"})
	if !ok || len(feats) != 1 || feats[0] != "gui" {
		t.Errorf("parseFeatures array: got %v %v", feats, ok)
	}
	// array with non-string → not ok
	if _, ok := parseFeatures([]any{1}); ok {
		t.Errorf("parseFeatures non-string in array should fail")
	}
	// random type → not ok
	if _, ok := parseFeatures(42); ok {
		t.Errorf("parseFeatures int should fail")
	}
}

func TestValidFeature(t *testing.T) {
	for _, f := range []string{"gui", "secret-proxy", "x", "a_b"} {
		if !validFeature(f) {
			t.Errorf("validFeature(%q) = false, want true", f)
		}
	}
	for _, f := range []string{"", "GUI", "with space", "with/slash", strings.Repeat("a", 33)} {
		if validFeature(f) {
			t.Errorf("validFeature(%q) = true, want false", f)
		}
	}
}

func TestParseGUIOpts(t *testing.T) {
	// nil → no opts, no error
	opts, err := parseGUIOpts(nil)
	if err != nil || opts != nil {
		t.Errorf("parseGUIOpts nil: got %v %v", opts, err)
	}
	// non-object → error
	if _, err := parseGUIOpts("xfce"); err == nil {
		t.Errorf("parseGUIOpts string should error")
	}
	// full object
	opts, err = parseGUIOpts(map[string]any{
		"desktop":      "xfce",
		"resolution":   "1280x720",
		"vnc_password": "secret",
		"module":       "500-gui-custom",
	})
	if err != nil {
		t.Fatalf("parseGUIOpts: %v", err)
	}
	if opts.Desktop != "xfce" || opts.Resolution != "1280x720" ||
		opts.VNCPassword != "secret" || opts.Module != "500-gui-custom" {
		t.Errorf("parseGUIOpts fields: %+v", opts)
	}
}

func TestGUIStatusForUnknownSandbox(t *testing.T) {
	h := newTestHandler(nil)
	req := httptest.NewRequest("GET", "/cgi-bin/api/sandboxes/missing/gui/status", nil)
	w := httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code != 404 {
		t.Errorf("gui status missing sandbox = %d, want 404; body=%s", w.Code, w.Body.String())
	}
}

func TestGUIDisableForUnknownSandbox(t *testing.T) {
	h := newTestHandler(nil)
	req := httptest.NewRequest("POST", "/cgi-bin/api/sandboxes/missing/gui/disable", nil)
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code != 404 {
		t.Errorf("gui disable missing sandbox = %d, want 404; body=%s", w.Code, w.Body.String())
	}
}
