package api

import (
	"encoding/json"
	"net/http"
	"net/url"
	"strings"

	"squashd/manager"
)

// guiResponse is the wire shape returned by the GUI endpoints. It augments
// the runtime state with a convenience noVNC URL fragment so clients don't
// have to assemble it themselves.
type guiResponse struct {
	*manager.GUIState
	NoVNCURL string `json:"novnc_url,omitempty"`
}

type guiBrowserOpenRequest struct {
	URL string `json:"url"`
}

type guiBrowserOpenResponse struct {
	OK  bool   `json:"ok"`
	URL string `json:"url"`
}

func (h *Handler) handleGUIEnable(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)

	// Body is optional. Empty or missing → use defaults.
	var body map[string]any
	if r.ContentLength > 0 {
		_ = json.NewDecoder(r.Body).Decode(&body)
	}
	opts, err := parseGUIOpts(body)
	if err != nil {
		jsonError(w, "invalid body: "+err.Error(), http.StatusBadRequest)
		return
	}

	state, err := h.mgr.EnableGUI(id, opts)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, guiResp(id, state))
}

func (h *Handler) handleGUIDisable(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	if err := h.mgr.DisableGUI(id); err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	state, _ := h.mgr.GUIStatus(id)
	jsonOK(w, guiResp(id, state))
}

func (h *Handler) handleGUIStatus(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	state, err := h.mgr.GUIStatus(id)
	if err != nil {
		jsonError(w, err.Error(), http.StatusNotFound)
		return
	}
	jsonOK(w, guiResp(id, state))
}

func (h *Handler) handleGUIBrowserOpen(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	var body guiBrowserOpenRequest
	if !decodeBody(w, r, &body) {
		return
	}
	body.URL = strings.TrimSpace(body.URL)
	if !validGUIBrowserURL(body.URL) {
		jsonError(w, "url must be http://, https://, or about:blank", http.StatusBadRequest)
		return
	}

	if err := h.mgr.OpenGUIBrowser(id, body.URL); err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		if strings.Contains(err.Error(), "not enabled") ||
			strings.Contains(err.Error(), "not active") ||
			strings.Contains(err.Error(), "not ready") {
			jsonError(w, err.Error(), http.StatusServiceUnavailable)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, guiBrowserOpenResponse{OK: true, URL: body.URL})
}

// guiResp wraps a GUIState in the response envelope, computing a relative
// noVNC URL when the GUI is enabled. The URL is the in-daemon reverse-proxy
// path that crosses into the sandbox netns and forwards to the in-sandbox
// websockify on port 6080. The session token is appended as `?_token=` so
// the first browser request can establish a scoped noVNC cookie; raw HTTP/WS
// clients can also supply the same token as Authorization: Bearer.
func guiResp(id string, state *manager.GUIState) guiResponse {
	resp := guiResponse{GUIState: state}
	if state != nil && state.Enabled && state.SessionToken != "" {
		proxyPrefix := "cgi-bin/api/sandboxes/" + id + "/novnc"
		values := url.Values{}
		values.Set("_token", state.SessionToken)
		values.Set("autoconnect", "true")
		values.Set("path", proxyPrefix+"/websockify")
		values.Set("resize", "scale")
		resp.NoVNCURL = "/" + proxyPrefix + "/vnc.html?" + values.Encode()
	}
	return resp
}

func validGUIBrowserURL(raw string) bool {
	if raw == "about:blank" {
		return true
	}
	u, err := url.Parse(raw)
	if err != nil || u.Host == "" || u.User != nil {
		return false
	}
	return u.Scheme == "http" || u.Scheme == "https"
}
