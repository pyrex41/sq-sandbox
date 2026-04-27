package api

import (
	"encoding/json"
	"fmt"
	"io/fs"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"squashd/config"
	"squashd/manager"
)


// Handler holds the API dependencies.
type Handler struct {
	cfg *config.Config
	mgr *manager.Manager
}

// NewHandler creates a new Handler and registers all routes on mux.
func NewHandler(cfg *config.Config, mgr *manager.Manager) http.Handler {
	h := &Handler{cfg: cfg, mgr: mgr}

	mux := http.NewServeMux()

	// Health — public
	mux.HandleFunc("GET /cgi-bin/health", h.handleHealth)
	mux.HandleFunc("/cgi-bin/health", h.handleHealth) // method-agnostic fallback

	// Modules
	mux.HandleFunc("GET /cgi-bin/api/modules", h.handleListModules)

	// Sandboxes collection
	mux.HandleFunc("GET /cgi-bin/api/sandboxes", h.handleListSandboxes)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes", h.handleCreateSandbox)

	// Sandbox item
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}", h.handleGetSandbox)
	mux.HandleFunc("DELETE /cgi-bin/api/sandboxes/{id}", h.handleDeleteSandbox)

	// Sandbox actions — synchronous exec
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/exec", h.handleExec)

	// Sandbox actions — background exec + job management
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/exec-bg", h.handleExecBg)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/jobs/{jobId}", h.handleGetJob)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/jobs/{jobId}/logs", h.handleJobLogs)
	mux.HandleFunc("DELETE /cgi-bin/api/sandboxes/{id}/jobs/{jobId}", h.handleKillJob)

	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/activate", h.handleActivate)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/snapshot", h.handleSnapshot)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/restore", h.handleRestore)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/logs", h.handleLogs)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/wg/peers", h.handleWGPeers)

	// GUI mode — native desktop toggle (noVNC + websockify + Xvfb)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/gui/enable", h.handleGUIEnable)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/gui/disable", h.handleGUIDisable)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/gui/status", h.handleGUIStatus)

	// Task runner — autonomous agent execution
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/task", h.handleStartTask)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/task", h.handleGetTask)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/task/events", h.handleTaskEvents)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/task/pause", h.handlePauseTask)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/task/resume", h.handleResumeTask)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/task/kill", h.handleKillTask)
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/task/snapshot", h.handleTaskSnapshot)

	// Irmin-only endpoints
	mux.HandleFunc("POST /cgi-bin/api/sandboxes/{id}/fork", h.handleFork)
	mux.HandleFunc("GET /cgi-bin/api/sandboxes/{id}/diff", h.handleDiff)

	return chain(mux,
		bodyLimitMiddleware,
		contentTypeMiddleware,
		func(next http.Handler) http.Handler {
			return authMiddleware(cfg.AuthToken, next)
		},
	)
}

// ── Health ───────────────────────────────────────────────────────────────────

func (h *Handler) handleHealth(w http.ResponseWriter, r *http.Request) {
	type healthResp struct {
		Status    string `json:"status"`
		Backend   string `json:"backend"`
		Sandboxes int    `json:"sandboxes"`
		Modules   int    `json:"modules"`
		BaseReady bool   `json:"base_ready"`
	}
	mods, _ := listModules(h.cfg.ModulesDir())
	baseReady := false
	for _, m := range mods {
		if strings.HasPrefix(m, "000-") {
			baseReady = true
			break
		}
	}
	jsonOK(w, healthResp{
		Status:    "ok",
		Backend:   h.cfg.Backend,
		Sandboxes: h.mgr.SandboxCount(),
		Modules:   len(mods),
		BaseReady: baseReady,
	})
}

// ── Modules ──────────────────────────────────────────────────────────────────

func (h *Handler) handleListModules(w http.ResponseWriter, r *http.Request) {
	mods, err := listModules(h.cfg.ModulesDir())
	if err != nil && !os.IsNotExist(err) {
		jsonError(w, "failed to list modules: "+err.Error(), http.StatusInternalServerError)
		return
	}
	type resp struct {
		Modules []string `json:"modules"`
	}
	jsonOK(w, resp{Modules: mods})
}

func listModules(dir string) ([]string, error) {
	var names []string
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return nil // skip unreadable entries
		}
		if d.IsDir() && path != dir {
			return fs.SkipDir
		}
		if !d.IsDir() && strings.HasSuffix(d.Name(), ".squashfs") {
			names = append(names, strings.TrimSuffix(d.Name(), ".squashfs"))
		}
		return nil
	})
	sort.Strings(names)
	return names, err
}

// ── Sandboxes collection ─────────────────────────────────────────────────────

func (h *Handler) handleListSandboxes(w http.ResponseWriter, r *http.Request) {
	jsonOK(w, h.mgr.List())
}

func (h *Handler) handleCreateSandbox(w http.ResponseWriter, r *http.Request) {
	var body map[string]any
	if !decodeBody(w, r, &body) {
		return
	}
	id, _ := body["id"].(string)
	if !validID(id) {
		jsonError(w, "invalid id: must be 1-64 alphanumeric/dash/underscore chars", http.StatusBadRequest)
		return
	}
	layers, ok := parseLayers(body["layers"])
	if !ok {
		jsonError(w, "layers must be a comma-separated string or JSON array", http.StatusBadRequest)
		return
	}
	for _, l := range layers {
		if !validModule(l) {
			jsonError(w, "invalid layer: "+l, http.StatusBadRequest)
			return
		}
	}

	opts := manager.CreateOpts{
		Owner:  stringOr(body["owner"], "anon"),
		Task:   stringOr(body["task"], ""),
		Layers: layers,
		CPU:    floatOr(body["cpu"], 2),
		MemoryMB: intOr(body["memory_mb"], 1024),
		MaxLifetimeS: intOr(body["max_lifetime_s"], 0),
	}
	if allow, ok := body["allow_net"].([]any); ok {
		for _, h := range allow {
			if s, ok := h.(string); ok {
				opts.AllowNet = append(opts.AllowNet, s)
			}
		}
	}

	features, fok := parseFeatures(body["features"])
	if !fok {
		jsonError(w, "features must be an array of strings", http.StatusBadRequest)
		return
	}
	for _, f := range features {
		if !validFeature(f) {
			jsonError(w, "invalid feature: "+f, http.StatusBadRequest)
			return
		}
	}
	opts.Features = features

	gui, gerr := parseGUIOpts(body["gui"])
	if gerr != nil {
		jsonError(w, "invalid gui: "+gerr.Error(), http.StatusBadRequest)
		return
	}
	opts.GUI = gui

	info, err := h.mgr.Create(id, opts)
	if err != nil {
		if strings.Contains(err.Error(), "already exists") {
			jsonError(w, err.Error(), http.StatusConflict)
			return
		}
		if strings.Contains(err.Error(), "limit reached") {
			jsonError(w, err.Error(), http.StatusServiceUnavailable)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonCreated(w, info)
}

// ── Sandbox item ─────────────────────────────────────────────────────────────

func (h *Handler) handleGetSandbox(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	info, err := h.mgr.Get(id)
	if err != nil {
		jsonError(w, err.Error(), http.StatusNotFound)
		return
	}
	jsonOK(w, info)
}

func (h *Handler) handleDeleteSandbox(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	if err := h.mgr.Destroy(id); err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonNoContent(w)
}

// ── Sandbox actions ──────────────────────────────────────────────────────────

func (h *Handler) handleExec(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	var body map[string]any
	if !decodeBody(w, r, &body) {
		return
	}
	cmd, _ := body["cmd"].(string)
	if !validCmd(cmd) {
		jsonError(w, "cmd required, max 64KiB", http.StatusBadRequest)
		return
	}
	workdir := stringOr(body["workdir"], "/")
	if !validWorkdir(workdir) {
		jsonError(w, "workdir must be absolute path, no ..", http.StatusBadRequest)
		return
	}
	timeout := clampTimeout(intOr(body["timeout"], 300), 1, 3600)

	result, err := h.mgr.Exec(id, cmd, manager.ExecOpts{WorkDir: workdir, TimeoutS: timeout})
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, result)
}

func (h *Handler) handleExecBg(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	var body map[string]any
	if !decodeBody(w, r, &body) {
		return
	}
	cmd, _ := body["cmd"].(string)
	if !validCmd(cmd) {
		jsonError(w, "cmd required, max 64KiB", http.StatusBadRequest)
		return
	}
	workdir := stringOr(body["workdir"], "/")
	if !validWorkdir(workdir) {
		jsonError(w, "workdir must be absolute path, no ..", http.StatusBadRequest)
		return
	}
	timeout := clampTimeout(intOr(body["timeout"], 7200), 1, 86400)

	jobID, err := h.mgr.ExecBg(id, cmd, workdir, timeout)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, map[string]any{"job_id": jobID})
}

func (h *Handler) handleGetJob(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	jobID, err := jobIDFromPath(r)
	if err != nil {
		jsonError(w, err.Error(), http.StatusBadRequest)
		return
	}
	job, err := h.mgr.GetJob(id, jobID)
	if err != nil {
		jsonError(w, err.Error(), http.StatusNotFound)
		return
	}
	jid, status, exitCode := job.Info()
	jsonOK(w, map[string]any{
		"job_id":    jid,
		"status":    status,
		"exit_code": exitCode,
	})
}

func (h *Handler) handleJobLogs(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	jobID, err := jobIDFromPath(r)
	if err != nil {
		jsonError(w, err.Error(), http.StatusBadRequest)
		return
	}
	job, err := h.mgr.GetJob(id, jobID)
	if err != nil {
		jsonError(w, err.Error(), http.StatusNotFound)
		return
	}

	flusher, ok := w.(http.Flusher)
	if !ok {
		jsonError(w, "streaming not supported", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-cache")
	w.Header().Set("Connection", "keep-alive")
	w.Header().Set("X-Accel-Buffering", "no")

	// Subscribe and get historical lines.
	initial, sub, done, exitCode := job.Subscribe()
	defer job.Unsubscribe(sub)

	for _, line := range initial {
		fmt.Fprintf(w, "data: %s\n\n", line)
	}
	if done {
		fmt.Fprintf(w, "event: done\ndata: {\"exit_code\":%d}\n\n", exitCode)
		flusher.Flush()
		return
	}
	flusher.Flush()

	pos := len(initial)
	ctx := r.Context()

	for {
		select {
		case <-ctx.Done():
			return
		case <-sub.Notify():
			lines, newPos, isDone, code := job.ReadSince(pos)
			for _, line := range lines {
				fmt.Fprintf(w, "data: %s\n\n", line)
			}
			pos = newPos
			if isDone {
				fmt.Fprintf(w, "event: done\ndata: {\"exit_code\":%d}\n\n", code)
				flusher.Flush()
				return
			}
			flusher.Flush()
		}
	}
}

func (h *Handler) handleKillJob(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	jobID, err := jobIDFromPath(r)
	if err != nil {
		jsonError(w, err.Error(), http.StatusBadRequest)
		return
	}
	if err := h.mgr.KillJob(id, jobID); err != nil {
		jsonError(w, err.Error(), http.StatusNotFound)
		return
	}
	jsonNoContent(w)
}

func jobIDFromPath(r *http.Request) (int, error) {
	n, err := strconv.Atoi(r.PathValue("jobId"))
	if err != nil {
		return 0, fmt.Errorf("invalid job_id")
	}
	return n, nil
}

func (h *Handler) handleActivate(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	var body map[string]any
	if !decodeBody(w, r, &body) {
		return
	}
	mod, _ := body["module"].(string)
	if !validModule(mod) {
		jsonError(w, "module required and must be valid name", http.StatusBadRequest)
		return
	}
	info, err := h.mgr.Activate(id, mod)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusBadRequest)
		return
	}
	jsonOK(w, info)
}

func (h *Handler) handleSnapshot(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	var body map[string]any
	// Body is optional for snapshot.
	_ = json.NewDecoder(r.Body).Decode(&body)
	label := ""
	if body != nil {
		label, _ = body["label"].(string)
	}
	result, err := h.mgr.Snapshot(id, label)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, result)
}

func (h *Handler) handleRestore(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	var body map[string]any
	if !decodeBody(w, r, &body) {
		return
	}
	label, _ := body["label"].(string)
	if label == "" {
		jsonError(w, "label required", http.StatusBadRequest)
		return
	}
	info, err := h.mgr.Restore(id, label)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, info)
}

func (h *Handler) handleLogs(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	logs, err := h.mgr.Logs(id)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			jsonError(w, err.Error(), http.StatusNotFound)
			return
		}
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if logs == nil {
		logs = []manager.ExecResult{}
	}
	jsonOK(w, logs)
}

func (h *Handler) handleWGPeers(w http.ResponseWriter, r *http.Request) {
	id := sandboxID(r)
	info, err := h.mgr.Get(id)
	if err != nil {
		jsonError(w, err.Error(), http.StatusNotFound)
		return
	}

	var peers []map[string]any
	// Body is optional (can be empty array).
	_ = json.NewDecoder(r.Body).Decode(&peers)
	if peers == nil {
		peers = []map[string]any{}
	}

	result, err := h.mgr.SetupWireGuard(id, peers)
	if err != nil {
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	_ = info
	jsonOK(w, result)
}

func (h *Handler) handleFork(w http.ResponseWriter, r *http.Request) {
	if h.cfg.SnapshotBackend != "irmin" {
		jsonError(w, "fork requires irmin snapshot backend", http.StatusNotImplemented)
		return
	}
	sourceID := sandboxID(r)
	var body map[string]any
	if !decodeBody(w, r, &body) {
		return
	}
	sourceLabel, _ := body["source_label"].(string)
	targetID, _ := body["target_id"].(string)
	if sourceLabel == "" || targetID == "" {
		jsonError(w, "source_label and target_id required", http.StatusBadRequest)
		return
	}
	if !validID(targetID) {
		jsonError(w, "invalid target_id: must be 1-64 alphanumeric/dash/underscore chars", http.StatusBadRequest)
		return
	}
	if !validLabel(sourceLabel) {
		jsonError(w, "invalid source_label: alphanumeric/dash/underscore/dot only", http.StatusBadRequest)
		return
	}
	if err := h.mgr.Fork(sourceID, sourceLabel, targetID); err != nil {
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, map[string]any{"ok": true})
}

func (h *Handler) handleDiff(w http.ResponseWriter, r *http.Request) {
	if h.cfg.SnapshotBackend != "irmin" {
		jsonError(w, "diff requires irmin snapshot backend", http.StatusNotImplemented)
		return
	}
	id := sandboxID(r)
	from := r.URL.Query().Get("from")
	to := r.URL.Query().Get("to")
	if from == "" || to == "" {
		jsonError(w, "from and to query params required", http.StatusBadRequest)
		return
	}
	if !validLabel(from) || !validLabel(to) {
		jsonError(w, "from/to must be alphanumeric/dash/underscore/dot only", http.StatusBadRequest)
		return
	}
	result, err := h.mgr.Diff(id, from, to)
	if err != nil {
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, result)
}

// ── Response helpers ─────────────────────────────────────────────────────────

func jsonOK(w http.ResponseWriter, v any) {
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(v)
}

func jsonCreated(w http.ResponseWriter, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusCreated)
	json.NewEncoder(w).Encode(v)
}

func jsonNoContent(w http.ResponseWriter) {
	w.WriteHeader(http.StatusNoContent)
}

func jsonError(w http.ResponseWriter, msg string, status int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(map[string]string{"error": msg})
}

// decodeBody decodes a JSON request body into v.
// Returns false (and writes the error response) if decoding fails.
func decodeBody(w http.ResponseWriter, r *http.Request, v any) bool {
	if err := json.NewDecoder(r.Body).Decode(v); err != nil {
		jsonError(w, "invalid json: "+err.Error(), http.StatusBadRequest)
		return false
	}
	return true
}

// sandboxID extracts the {id} path value from a 1.22 pattern request.
func sandboxID(r *http.Request) string {
	return r.PathValue("id")
}

// ── Validation helpers ───────────────────────────────────────────────────────

func validID(id string) bool {
	if id == "" || len(id) > 64 {
		return false
	}
	if strings.HasPrefix(id, "-") || strings.HasSuffix(id, "-") {
		return false
	}
	for _, c := range id {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c >= '0' && c <= '9') || c == '-' || c == '_') {
			return false
		}
	}
	return true
}

func validModule(name string) bool {
	if name == "" || len(name) > 128 {
		return false
	}
	for _, c := range name {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.') {
			return false
		}
	}
	return true
}

func validWorkdir(workdir string) bool {
	return workdir != "" && len(workdir) <= 512 &&
		strings.HasPrefix(workdir, "/") &&
		!strings.Contains(workdir, "..")
}

func validCmd(cmd string) bool {
	return strings.TrimSpace(cmd) != "" && len(cmd) <= 65536
}

func validLabel(label string) bool {
	if label == "" || len(label) > 128 {
		return false
	}
	for _, c := range label {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.') {
			return false
		}
	}
	return true
}

// ── JSON body coercion helpers ────────────────────────────────────────────────

func stringOr(v any, def string) string {
	if s, ok := v.(string); ok && s != "" {
		return s
	}
	return def
}

func floatOr(v any, def float64) float64 {
	switch n := v.(type) {
	case float64:
		return n
	case int:
		return float64(n)
	}
	return def
}

func intOr(v any, def int) int {
	switch n := v.(type) {
	case float64:
		return int(n)
	case int:
		return n
	}
	return def
}

func clampTimeout(v int, min, max int) int {
	if v < min {
		return min
	}
	if v > max {
		return max
	}
	return v
}

// parseFeatures accepts a JSON array of strings or a comma-separated string.
// Returns (nil, true) when the field is missing.
func parseFeatures(raw any) ([]string, bool) {
	switch v := raw.(type) {
	case nil:
		return nil, true
	case string:
		if v == "" {
			return nil, true
		}
		out := make([]string, 0, 4)
		for _, p := range strings.Split(v, ",") {
			if p = strings.TrimSpace(p); p != "" {
				out = append(out, p)
			}
		}
		return out, true
	case []any:
		out := make([]string, 0, len(v))
		for _, item := range v {
			s, ok := item.(string)
			if !ok {
				return nil, false
			}
			if s = strings.TrimSpace(s); s != "" {
				out = append(out, s)
			}
		}
		return out, true
	default:
		return nil, false
	}
}

// validFeature constrains feature names to short alphanumeric tokens.
func validFeature(name string) bool {
	if name == "" || len(name) > 32 {
		return false
	}
	for _, c := range name {
		if !((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' || c == '_') {
			return false
		}
	}
	return true
}

// parseGUIOpts pulls the optional "gui" object out of a create-body map.
func parseGUIOpts(raw any) (*manager.GUIOpts, error) {
	if raw == nil {
		return nil, nil
	}
	m, ok := raw.(map[string]any)
	if !ok {
		return nil, fmt.Errorf("must be an object")
	}
	g := &manager.GUIOpts{}
	if v, ok := m["desktop"].(string); ok {
		g.Desktop = strings.TrimSpace(v)
	}
	if v, ok := m["resolution"].(string); ok {
		g.Resolution = strings.TrimSpace(v)
	}
	if v, ok := m["vnc_password"].(string); ok {
		g.VNCPassword = v
	}
	if v, ok := m["module"].(string); ok {
		g.Module = strings.TrimSpace(v)
	}
	return g, nil
}

// parseLayers accepts either a comma-separated string or a JSON array
// (the body field "layers" can be either form across implementations).
func parseLayers(raw any) ([]string, bool) {
	switch v := raw.(type) {
	case string:
		if v == "" {
			return []string{"000-base-alpine"}, true
		}
		parts := strings.Split(v, ",")
		out := make([]string, 0, len(parts))
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p != "" {
				out = append(out, p)
			}
		}
		return out, true
	case []any:
		out := make([]string, 0, len(v))
		for _, item := range v {
			s, ok := item.(string)
			if !ok {
				return nil, false
			}
			out = append(out, strings.TrimSpace(s))
		}
		return out, true
	case nil:
		return []string{"000-base-alpine"}, true
	default:
		return nil, false
	}
}
