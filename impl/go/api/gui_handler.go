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
// noVNC URL ("/session/{id}/novnc") when the GUI is enabled. Hosts running
// a sq-gui-proxy sidecar can reverse-proxy that path; bare deployments can
// expose port 6080 directly via slirp4netns port mapping.
func guiResp(id string, state *manager.GUIState) guiResponse {
	resp := guiResponse{GUIState: state}
	if state != nil && state.Enabled {
		resp.NoVNCURL = "/session/" + id + "/novnc"
	}
	return resp
}
