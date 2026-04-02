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
