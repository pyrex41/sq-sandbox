package api

import (
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"

	"squashd/config"
	"squashd/manager"
)

func TestCheckSessionTokenHeaderAndQuery(t *testing.T) {
	cases := []struct {
		name, header, query, expected string
		want                          bool
	}{
		{"empty expected always rejected", "Bearer x", "", "", false},
		{"bearer header match", "Bearer abc", "", "abc", true},
		{"bearer header mismatch", "Bearer wrong", "", "abc", false},
		{"query param match", "", "abc", "abc", true},
		{"query param mismatch", "", "wrong", "abc", false},
		{"missing both", "", "", "abc", false},
		{"non-bearer authz ignored", "Basic abc", "", "abc", false},
		{"header preferred but query also valid", "Bearer abc", "abc", "abc", true},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			req := httptest.NewRequest("GET", "/x?_token="+c.query, nil)
			if c.header != "" {
				req.Header.Set("Authorization", c.header)
			}
			if got := checkSessionToken(req, c.expected); got != c.want {
				t.Errorf("checkSessionToken(%+v) = %v, want %v", c, got, c.want)
			}
		})
	}
}

func TestNoVNCPathDetection(t *testing.T) {
	yes := []string{
		"/cgi-bin/api/sandboxes/abc/novnc",
		"/cgi-bin/api/sandboxes/abc/novnc/",
		"/cgi-bin/api/sandboxes/abc/novnc/vnc.html",
		"/cgi-bin/api/sandboxes/abc/novnc/websockify",
		"/cgi-bin/api/sandboxes/with-dash_x/novnc/core/util/foo.js",
	}
	for _, p := range yes {
		if !isNoVNCPath(p) {
			t.Errorf("isNoVNCPath(%q) = false, want true", p)
		}
	}
	no := []string{
		"",
		"/cgi-bin/api/sandboxes",
		"/cgi-bin/api/sandboxes/",
		"/cgi-bin/api/sandboxes/abc",
		"/cgi-bin/api/sandboxes/abc/exec",
		"/cgi-bin/api/sandboxes/abc/gui/status",
		"/cgi-bin/api/sandboxes/abc/novnc-something/x", // close prefix, not the subtree
		"/some/other/novnc/path",
	}
	for _, p := range no {
		if isNoVNCPath(p) {
			t.Errorf("isNoVNCPath(%q) = true, want false", p)
		}
	}
}

func TestNoVNCBypassesAuthMiddleware(t *testing.T) {
	// With AuthToken set, regular API endpoints should 401 without Bearer,
	// but the /novnc/ subtree must reach its own handler (and that handler
	// then returns 404 because the sandbox doesn't exist).
	cfg := &config.Config{
		DataDir: "/data", Port: 8080, Backend: "chroot",
		SnapshotBackend: "squashfs",
		AuthToken:       "secret",
	}
	h := NewHandler(cfg, manager.New(cfg))

	// Sanity check: /exec without bearer = 401.
	req := httptest.NewRequest("POST", "/cgi-bin/api/sandboxes/abc/exec", strings.NewReader(`{}`))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code != 401 {
		t.Fatalf("exec without bearer = %d, want 401", w.Code)
	}

	// The /novnc/ path should NOT 401 — it must reach the proxy handler,
	// which then 404s because the sandbox doesn't exist.
	req = httptest.NewRequest("GET", "/cgi-bin/api/sandboxes/abc/novnc/vnc.html", nil)
	w = httptest.NewRecorder()
	h.ServeHTTP(w, req)
	if w.Code == 401 {
		t.Errorf("novnc subtree returned 401 — auth middleware should have skipped it")
	}
	if w.Code != 404 {
		t.Logf("novnc on missing sandbox returned %d (body=%s)", w.Code, w.Body.String())
	}
}

func TestGUIProxyAcquireRespectsCap(t *testing.T) {
	gp := newGUIProxy(nil)

	// Saturate the semaphore.
	releases := make([]func(), 0, maxNoVNCPerSandbox)
	for i := 0; i < maxNoVNCPerSandbox; i++ {
		release, ok := gp.acquire("s1")
		if !ok {
			t.Fatalf("acquire %d: ok=false, expected ok=true", i)
		}
		releases = append(releases, release)
	}

	// One more should fail.
	if _, ok := gp.acquire("s1"); ok {
		t.Errorf("acquire over cap: ok=true, want false")
	}

	// A different sandbox is independent.
	release2, ok := gp.acquire("s2")
	if !ok {
		t.Errorf("acquire on different sandbox: ok=false, want true")
	}
	defer release2()

	// Releasing one should free a slot for s1.
	releases[0]()
	release3, ok := gp.acquire("s1")
	if !ok {
		t.Errorf("acquire after release: ok=false, want true")
	}
	defer release3()
	for _, r := range releases[1:] {
		r()
	}
}

// Smoke test: many concurrent acquire/release cycles never deadlock or
// over-allocate.
func TestGUIProxyAcquireConcurrency(t *testing.T) {
	gp := newGUIProxy(nil)
	var wg sync.WaitGroup
	for i := 0; i < 50; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for j := 0; j < 20; j++ {
				if release, ok := gp.acquire("s"); ok {
					release()
				}
			}
		}()
	}
	wg.Wait()
	// Final state: no acquisitions in flight, semaphore empty.
	for i := 0; i < maxNoVNCPerSandbox; i++ {
		if _, ok := gp.acquire("s"); !ok {
			t.Fatalf("post-concurrency acquire %d failed", i)
		}
	}
}

// Compile-time sanity: the proxy should satisfy http.Handler so it can be
// mounted directly on the mux.
var _ http.Handler = (*guiProxy)(nil)
