package api

import (
	"net/http"
	"strings"
)

const maxBodySize = 64 * 1024 * 1024 // 64 MiB — matches Go proxy limit

// authMiddleware wraps a handler with Bearer token authentication.
// Routes under /cgi-bin/api/ require the token; others are public.
// If authToken is empty, all requests pass through.
//
// The noVNC reverse-proxy subtree (/cgi-bin/api/sandboxes/.../novnc/...) is
// exempted: it authenticates with a per-sandbox session token issued at
// /gui/enable time, not the daemon-wide token. Mixing the two would force
// the daemon-wide token into shareable URLs.
func authMiddleware(authToken string, next http.Handler) http.Handler {
	if authToken == "" {
		return next
	}
	expected := "Bearer " + authToken
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.HasPrefix(r.URL.Path, "/cgi-bin/api/") && !isNoVNCPath(r.URL.Path) {
			if r.Header.Get("Authorization") != expected {
				jsonError(w, "unauthorized", http.StatusUnauthorized)
				return
			}
		}
		next.ServeHTTP(w, r)
	})
}

// isNoVNCPath returns true for any request path under a sandbox's /novnc/
// subtree. The match is intentionally loose — any malformed path that
// happens to contain "/novnc" will fail validation in the proxy itself.
func isNoVNCPath(p string) bool {
	const prefix = "/cgi-bin/api/sandboxes/"
	if !strings.HasPrefix(p, prefix) {
		return false
	}
	rest := p[len(prefix):]
	// rest looks like "{id}/novnc" or "{id}/novnc/..."
	slash := strings.IndexByte(rest, '/')
	if slash == -1 {
		return false
	}
	tail := rest[slash+1:]
	return tail == "novnc" || strings.HasPrefix(tail, "novnc/")
}

// bodyLimitMiddleware caps request body reads to prevent memory exhaustion.
func bodyLimitMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		r.Body = http.MaxBytesReader(w, r.Body, maxBodySize)
		next.ServeHTTP(w, r)
	})
}

// contentTypeMiddleware enforces application/json for POST/PUT/PATCH bodies.
func contentTypeMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method == http.MethodPost || r.Method == http.MethodPut || r.Method == http.MethodPatch {
			ct := r.Header.Get("Content-Type")
			if !strings.Contains(ct, "application/json") {
				// Allow empty body (e.g. snapshot/restore with no JSON payload)
				if r.ContentLength == 0 {
					next.ServeHTTP(w, r)
					return
				}
				jsonError(w, "unsupported media type", http.StatusUnsupportedMediaType)
				return
			}
		}
		next.ServeHTTP(w, r)
	})
}

// chain applies middlewares in order: chain(a, b, c)(h) = a(b(c(h)))
func chain(h http.Handler, middlewares ...func(http.Handler) http.Handler) http.Handler {
	for i := len(middlewares) - 1; i >= 0; i-- {
		h = middlewares[i](h)
	}
	return h
}
