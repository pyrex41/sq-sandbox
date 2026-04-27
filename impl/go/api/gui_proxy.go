package api

import (
	"context"
	"crypto/subtle"
	"fmt"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"
	"sync"
)

// maxNoVNCPerSandbox caps concurrent /novnc/ connections per sandbox. noVNC
// itself only needs one (HTML + WebSocket); the headroom covers reconnects
// and asset preloads. A misbehaving client cannot starve daemon goroutines.
const maxNoVNCPerSandbox = 4
const noVNCTokenCookie = "sq_novnc_token"

// guiProxy holds the shared httputil.ReverseProxy plus a per-sandbox
// connection-limit semaphore map. Built once at handler-init time so the
// per-sandbox state is consistent across goroutines.
type guiProxy struct {
	h *Handler

	semMu sync.Mutex
	sem   map[string]chan struct{}
}

func newGUIProxy(h *Handler) *guiProxy {
	return &guiProxy{h: h, sem: make(map[string]chan struct{})}
}

// acquire reserves a connection slot for the given sandbox. Returns false
// when the per-sandbox cap is reached; callers must respond 503 in that
// case. The returned release func must be called exactly once when the
// proxy is finished with the connection.
func (g *guiProxy) acquire(id string) (release func(), ok bool) {
	g.semMu.Lock()
	ch, exists := g.sem[id]
	if !exists {
		ch = make(chan struct{}, maxNoVNCPerSandbox)
		g.sem[id] = ch
	}
	g.semMu.Unlock()

	select {
	case ch <- struct{}{}:
		return func() { <-ch }, true
	default:
		return nil, false
	}
}

// ServeHTTP routes /cgi-bin/api/sandboxes/{id}/novnc/{path...} into the
// sandbox netns. Auth is by per-sandbox session token, supplied via either
// `Authorization: Bearer <token>` or `?_token=<token>` (browsers can't set
// custom headers on WebSocket upgrades, so the query param is required).
func (g *guiProxy) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	if id == "" || !validID(id) {
		http.Error(w, "invalid sandbox id", http.StatusBadRequest)
		return
	}

	target, err := g.h.mgr.GUITarget(id)
	if err != nil {
		// Distinguish "sandbox missing" (404) from "GUI not running yet"
		// (503) so clients know whether to retry.
		if strings.Contains(err.Error(), "not found") {
			http.Error(w, err.Error(), http.StatusNotFound)
			return
		}
		http.Error(w, err.Error(), http.StatusServiceUnavailable)
		return
	}

	if !checkSessionToken(r, target.SessionToken) {
		http.Error(w, "invalid session token", http.StatusUnauthorized)
		return
	}
	setNoVNCTokenCookie(w, id, target.SessionToken)

	release, ok := g.acquire(id)
	if !ok {
		http.Error(w, "too many concurrent gui sessions", http.StatusServiceUnavailable)
		return
	}
	defer release()

	pid := target.BwrapPID
	port := target.NoVNCPort
	if port == 0 {
		port = 6080
	}
	upstream := fmt.Sprintf("127.0.0.1:%d", port)

	// httputil.ReverseProxy needs a target URL; the host is unused once we
	// install our own DialContext, but the scheme matters for upgrade
	// handling. Use http — TLS is terminated at the edge.
	targetURL := &url.URL{Scheme: "http", Host: "novnc.invalid"}
	proxy := httputil.NewSingleHostReverseProxy(targetURL)

	// Strip the /cgi-bin/api/sandboxes/{id}/novnc prefix so the upstream
	// (websockify serving the noVNC web UI) sees the path it expects:
	// "/vnc.html", "/websockify", "/core/...", etc.
	prefix := "/cgi-bin/api/sandboxes/" + id + "/novnc"
	origDirector := proxy.Director
	proxy.Director = func(req *http.Request) {
		origDirector(req)
		req.URL.Path = strings.TrimPrefix(req.URL.Path, prefix)
		if req.URL.Path == "" {
			req.URL.Path = "/"
		}
		req.Host = upstream
		// Don't forward proxy-only auth material to websockify.
		req.Header.Del("Authorization")
		req.Header.Del("Cookie")
		stripQueryParam(req.URL, "_token")
	}

	proxy.Transport = &http.Transport{
		DialContext: func(ctx context.Context, network, _ string) (net.Conn, error) {
			return dialInNetns(ctx, pid, network, upstream)
		},
		// noVNC uses a single long-lived WebSocket; pooling buys nothing
		// and would just complicate netns lifetime reasoning.
		DisableKeepAlives: true,
	}

	proxy.ErrorHandler = func(w http.ResponseWriter, _ *http.Request, err error) {
		http.Error(w, "gui proxy: "+err.Error(), http.StatusBadGateway)
	}

	proxy.ServeHTTP(w, r)
}

// checkSessionToken returns true if the request supplied the expected token
// via either Bearer header or `_token` query param. Comparison is constant
// time. An empty expected token (GUI in transitional state) is rejected.
func checkSessionToken(r *http.Request, expected string) bool {
	if expected == "" {
		return false
	}
	if h := r.Header.Get("Authorization"); strings.HasPrefix(h, "Bearer ") {
		got := strings.TrimPrefix(h, "Bearer ")
		if subtle.ConstantTimeCompare([]byte(got), []byte(expected)) == 1 {
			return true
		}
	}
	if got := r.URL.Query().Get("_token"); got != "" {
		if subtle.ConstantTimeCompare([]byte(got), []byte(expected)) == 1 {
			return true
		}
	}
	if cookie, err := r.Cookie(noVNCTokenCookie); err == nil {
		if subtle.ConstantTimeCompare([]byte(cookie.Value), []byte(expected)) == 1 {
			return true
		}
	}
	return false
}

func setNoVNCTokenCookie(w http.ResponseWriter, sandboxID, token string) {
	http.SetCookie(w, &http.Cookie{
		Name:     noVNCTokenCookie,
		Value:    token,
		Path:     "/cgi-bin/api/sandboxes/" + sandboxID + "/novnc",
		HttpOnly: true,
		SameSite: http.SameSiteLaxMode,
	})
}

// stripQueryParam removes a single query-string key from u.RawQuery in place.
func stripQueryParam(u *url.URL, key string) {
	q := u.Query()
	if !q.Has(key) {
		return
	}
	q.Del(key)
	u.RawQuery = q.Encode()
}
