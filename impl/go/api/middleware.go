package api

import (
	"net/http"
	"strings"
)

const maxBodySize = 64 * 1024 * 1024 // 64 MiB — matches Go proxy limit

// authMiddleware wraps a handler with Bearer token authentication.
// Routes under /cgi-bin/api/ require the token; others are public.
// If authToken is empty, all requests pass through.
func authMiddleware(authToken string, next http.Handler) http.Handler {
	if authToken == "" {
		return next
	}
	expected := "Bearer " + authToken
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.HasPrefix(r.URL.Path, "/cgi-bin/api/") {
			if r.Header.Get("Authorization") != expected {
				jsonError(w, "unauthorized", http.StatusUnauthorized)
				return
			}
		}
		next.ServeHTTP(w, r)
	})
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
