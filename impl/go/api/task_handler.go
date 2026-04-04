package api

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"squashd/runner"
)

// ── Task Runner Endpoints ───────────────────────────────────────────────────

func (h *Handler) handleStartTask(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	_, err := h.mgr.Get(id)
	if err != nil {
		jsonError(w, "sandbox not found: "+id, http.StatusNotFound)
		return
	}

	if h.mgr.GetTask(id) != nil {
		jsonError(w, "task already running", http.StatusConflict)
		return
	}

	var spec runner.TaskSpec
	if err := json.NewDecoder(r.Body).Decode(&spec); err != nil {
		jsonError(w, "invalid body: "+err.Error(), http.StatusBadRequest)
		return
	}

	if spec.Agent == "" {
		jsonError(w, "agent is required", http.StatusBadRequest)
		return
	}
	if spec.Plan == "" {
		jsonError(w, "plan is required", http.StatusBadRequest)
		return
	}

	// Create an executor that runs commands inside this sandbox
	exec := h.mgr.NewSandboxExecutor(id)

	taskID := fmt.Sprintf("task-%s-%d", id, time.Now().Unix())
	tr := runner.NewTaskRunner(taskID, spec, exec)

	// Wire up snapshotter for automatic snapshots at turn boundaries
	tr.SetSnapshotter(h.mgr.NewSandboxSnapshotter(id))

	// Register with manager so other endpoints can find it
	h.mgr.SetTask(id, tr)

	// Launch autonomously
	go tr.Run(context.Background())

	jsonCreated(w, map[string]any{"task_id": taskID})
}

func (h *Handler) handleGetTask(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	tr := h.mgr.GetTask(id)
	if tr == nil {
		jsonError(w, "no task for sandbox "+id, http.StatusNotFound)
		return
	}

	if rt, ok := tr.(*runner.TaskRunner); ok {
		jsonOK(w, rt.TaskState())
	} else {
		jsonOK(w, map[string]string{"status": tr.Status()})
	}
}

func (h *Handler) handleTaskEvents(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	tr := h.mgr.GetTask(id)
	if tr == nil {
		jsonError(w, "no task for sandbox "+id, http.StatusNotFound)
		return
	}

	rt, ok := tr.(*runner.TaskRunner)
	if !ok {
		jsonError(w, "unexpected task type", http.StatusInternalServerError)
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

	sub, history := rt.Events.Subscribe()
	defer rt.Events.Unsubscribe(sub)

	// Replay history
	pos := len(history)
	for _, ev := range history {
		fmt.Fprintf(w, "data: %s\n\n", ev.JSON())
	}
	flusher.Flush()

	// If task is already done, send end marker
	status := rt.StatusEnum()
	if status == runner.StatusSucceeded || status == runner.StatusFailed || status == runner.StatusKilled {
		fmt.Fprintf(w, "event: done\ndata: {\"status\":%q}\n\n", status)
		flusher.Flush()
		return
	}

	// Live stream
	ctx := r.Context()
	for {
		select {
		case <-ctx.Done():
			return
		case <-sub.Notify():
			events := rt.Events.ReadSince(pos)
			for _, ev := range events {
				fmt.Fprintf(w, "data: %s\n\n", ev.JSON())
				pos++
			}
			flusher.Flush()

			status := rt.StatusEnum()
			if status == runner.StatusSucceeded || status == runner.StatusFailed || status == runner.StatusKilled {
				fmt.Fprintf(w, "event: done\ndata: {\"status\":%q}\n\n", status)
				flusher.Flush()
				return
			}
		}
	}
}

func (h *Handler) handlePauseTask(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	tr := h.mgr.GetTask(id)
	if tr == nil {
		jsonError(w, "no task for sandbox "+id, http.StatusNotFound)
		return
	}
	tr.Pause()
	jsonOK(w, map[string]string{"status": "pausing"})
}

func (h *Handler) handleResumeTask(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	tr := h.mgr.GetTask(id)
	if tr == nil {
		jsonError(w, "no task for sandbox "+id, http.StatusNotFound)
		return
	}
	tr.Resume()
	jsonOK(w, map[string]string{"status": "resuming"})
}

func (h *Handler) handleKillTask(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	tr := h.mgr.GetTask(id)
	if tr == nil {
		jsonError(w, "no task for sandbox "+id, http.StatusNotFound)
		return
	}
	tr.Kill()
	jsonOK(w, map[string]string{"status": "killed"})
}

func (h *Handler) handleTaskSnapshot(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	tr := h.mgr.GetTask(id)
	if tr == nil {
		jsonError(w, "no task for sandbox "+id, http.StatusNotFound)
		return
	}

	var body struct {
		Label string `json:"label"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		// Generate a label if none provided
		body.Label = fmt.Sprintf("manual-%d", time.Now().Unix())
	}
	if body.Label == "" {
		body.Label = fmt.Sprintf("manual-%d", time.Now().Unix())
	}

	// Type-assert to get TakeSnapshot
	if rt, ok := tr.(*runner.TaskRunner); ok {
		rt.TakeSnapshot(body.Label, 0)
		jsonOK(w, map[string]any{"label": body.Label, "snapshots": rt.Snapshots()})
	} else {
		jsonError(w, "task does not support snapshots", http.StatusBadRequest)
	}
}
