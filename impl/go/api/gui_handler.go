package api

import (
	"encoding/json"
	"net/http"
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

// guiResp wraps a GUIState in the response envelope, computing a relative
// noVNC URL when the GUI is enabled. The URL is the in-daemon reverse-proxy
// path that crosses into the sandbox netns and forwards to the in-sandbox
// websockify on port 6080. The session token is appended as `?_token=` so
// the URL is directly browser-pasteable for the noVNC web UI; for raw
// HTTP/WS clients, the same token can be supplied as Authorization: Bearer.
func guiResp(id string, state *manager.GUIState) guiResponse {
	resp := guiResponse{GUIState: state}
	if state != nil && state.Enabled && state.SessionToken != "" {
		resp.NoVNCURL = "/cgi-bin/api/sandboxes/" + id + "/novnc/vnc.html?_token=" + state.SessionToken
	}
	return resp
}
