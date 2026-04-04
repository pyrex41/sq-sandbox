package runner

import (
	"context"
	"fmt"
	"log"
	"math"
	"sync"
	"sync/atomic"
	"time"
)

// TaskStatus represents the lifecycle state of a task.
type TaskStatus string

const (
	StatusQueued    TaskStatus = "queued"
	StatusRunning   TaskStatus = "running"
	StatusSucceeded TaskStatus = "succeeded"
	StatusFailed    TaskStatus = "failed"
	StatusPaused    TaskStatus = "paused"
	StatusKilled    TaskStatus = "killed"
)

// TaskSpec is the input to start an autonomous task in a sandbox.
type TaskSpec struct {
	Agent          string            `json:"agent"`
	Plan           string            `json:"plan"`
	GitRemote      string            `json:"git_remote"`
	Branch         string            `json:"branch"`
	Workdir        string            `json:"workdir"`
	MaxTurns       int               `json:"max_turns"`
	MaxBudgetUsd   float64           `json:"max_budget_usd"`
	TurnsPerBatch  int               `json:"turns_per_batch"`
	EnvVars        map[string]string `json:"env_vars"`
	IssueKey       string            `json:"issue_key"`
	GitLabProject  string            `json:"gitlab_project"`
	SnapshotPolicy SnapshotPolicy    `json:"snapshot_policy"`
}

// TaskState is the queryable snapshot of a running task.
type TaskState struct {
	TaskID       string         `json:"task_id"`
	Status       TaskStatus     `json:"status"`
	TotalTurns   int            `json:"total_turns"`
	TotalCostUsd float64        `json:"total_cost_usd"`
	FilesChanged []string       `json:"files_changed,omitempty"`
	Commits      []Commit       `json:"commits,omitempty"`
	Snapshots    []SnapshotInfo `json:"snapshots,omitempty"`
	MRUrl        string         `json:"mr_url,omitempty"`
	Error        string         `json:"error,omitempty"`
	StartedAt    time.Time      `json:"started_at"`
	ElapsedMs    int64          `json:"elapsed_ms"`
}

// TaskRunner is an autonomous task executor.
type TaskRunner struct {
	ID      string
	Spec    TaskSpec
	Events  *EventLog
	Workdir string // resolved workdir

	exec        Executor
	snapshotter Snapshotter // nil if snapshots not available
	adapter     AgentAdapter
	gitOps      *GitOps
	status      atomic.Value
	cancel      context.CancelFunc

	mu         sync.Mutex
	totalTurns int
	totalCost  float64
	mrURL      string
	errMsg     string
	startedAt  time.Time
	pauseCh    chan struct{}
	snapshots  []SnapshotInfo
}

type SnapshotInfo struct {
	Label string    `json:"label"`
	Size  int64     `json:"size"`
	Batch int       `json:"batch"`
	Time  time.Time `json:"time"`
}

// NewTaskRunner creates a runner for the given spec.
// exec is the Executor that runs commands inside the sandbox.
func NewTaskRunner(id string, spec TaskSpec, exec Executor) *TaskRunner {
	events := NewEventLog()

	var adapter AgentAdapter
	switch spec.Agent {
	case "claude-code":
		adapter = NewClaudeAdapter(exec, events)
	case "rho-xai":
		adapter = NewRhoAdapter(exec, events, "grok-4-1-fast-reasoning")
	case "rho-anthropic":
		adapter = NewRhoAdapter(exec, events, "claude-sonnet")
	case "rho-openai":
		adapter = NewRhoAdapter(exec, events, "gpt-4.1")
	default:
		adapter = NewClaudeAdapter(exec, events)
	}

	// Default limits
	if spec.MaxTurns == 0 {
		spec.MaxTurns = 100
	}
	if spec.TurnsPerBatch == 0 {
		spec.TurnsPerBatch = 5
	}
	if spec.MaxBudgetUsd == 0 {
		spec.MaxBudgetUsd = 10.0
	}
	if spec.GitLabProject == "" {
		spec.GitLabProject = ProjectFromRemote(spec.GitRemote)
	}

	r := &TaskRunner{
		ID:      id,
		Spec:    spec,
		Events:  events,
		exec:    exec,
		adapter: adapter,
		pauseCh: make(chan struct{}),
	}
	// Default snapshot policy
	if spec.SnapshotPolicy.EveryNBatches == 0 {
		spec.SnapshotPolicy.EveryNBatches = 5
	}
	if spec.SnapshotPolicy.MaxSnapshots == 0 {
		spec.SnapshotPolicy.MaxSnapshots = 10
	}

	r.status.Store(StatusQueued)
	return r
}

// SetSnapshotter sets the snapshotter for automatic snapshots at turn boundaries.
func (r *TaskRunner) SetSnapshotter(s Snapshotter) {
	r.snapshotter = s
}

// Run executes the task autonomously. Call this in a goroutine.
func (r *TaskRunner) Run(ctx context.Context) {
	ctx, r.cancel = context.WithCancel(ctx)
	r.startedAt = time.Now()
	r.status.Store(StatusRunning)
	r.Events.Emit("task_start", map[string]any{
		"agent": r.Spec.Agent, "branch": r.Spec.Branch,
	})

	defer func() {
		if r.StatusEnum() == StatusRunning {
			r.status.Store(StatusFailed)
		}
	}()

	// Detect workdir
	r.Workdir = r.Spec.Workdir
	if r.Workdir == "" {
		r.Workdir = DetectWorkdir(r.exec, "/workspace", r.Spec.Branch)
	}

	r.Events.Emit("workdir_detected", map[string]any{"workdir": r.Workdir})

	// Setup workspace
	if err := r.setupWorkspace(); err != nil {
		r.fail(fmt.Sprintf("workspace setup: %v", err))
		return
	}

	// Write plan
	planPath := "/workspace/IMPLEMENTATION_PLAN.md"
	_, err := r.exec.Exec(fmt.Sprintf("cat > %s << 'PLAN_EOF'\n%s\nPLAN_EOF", planPath, r.Spec.Plan), "/", 10)
	if err != nil {
		r.fail(fmt.Sprintf("write plan: %v", err))
		return
	}

	// Main turn loop
	var sessionID string
	for batch := 1; ; batch++ {
		// Pause gate
		if r.StatusEnum() == StatusPaused {
			r.Events.Emit("paused", nil)
			select {
			case <-r.pauseCh:
				r.status.Store(StatusRunning)
				r.Events.Emit("resumed", nil)
			case <-ctx.Done():
				return
			}
		}

		if ctx.Err() != nil {
			r.Events.Emit("cancelled", nil)
			r.status.Store(StatusKilled)
			return
		}

		r.mu.Lock()
		turns := r.totalTurns
		cost := r.totalCost
		r.mu.Unlock()

		if turns >= r.Spec.MaxTurns {
			r.Events.Emit("turn_limit", map[string]any{"turns": turns, "limit": r.Spec.MaxTurns})
			break
		}
		if cost >= r.Spec.MaxBudgetUsd {
			r.Events.Emit("budget_limit", map[string]any{"cost": cost, "limit": r.Spec.MaxBudgetUsd})
			break
		}

		// Workspace state at turn boundary
		if ws, err := InspectWorkspace(r.exec, r.Workdir); err == nil {
			r.Events.Emit("workspace_state", map[string]any{
				"files_changed": ws.FilesChanged,
				"commits":       ws.Commits,
				"dirty":         ws.Dirty,
			})
		}

		// Run a batch
		r.Events.Emit("batch_start", map[string]any{"batch": batch, "turns_so_far": turns})

		result, err := r.adapter.RunBatch(ctx, BatchRequest{
			Workdir:   r.Workdir,
			Plan:      planPath,
			MaxTurns:  r.Spec.TurnsPerBatch,
			SessionID: sessionID,
			EnvVars:   r.Spec.EnvVars,
		})

		if err != nil {
			if IsRetryable(err) && batch <= 5 {
				wait := backoffDuration(batch)
				r.Events.Emit("retry", map[string]any{"error": err.Error(), "wait_ms": wait.Milliseconds()})
				log.Printf("[task %s] retryable error, waiting %v: %v", r.ID, wait, err)
				select {
				case <-time.After(wait):
					continue
				case <-ctx.Done():
					return
				}
			}
			r.fail(fmt.Sprintf("agent error: %v", err))
			return
		}

		r.mu.Lock()
		r.totalTurns += result.TurnsUsed
		r.totalCost += result.CostUsd
		r.mu.Unlock()
		sessionID = result.SessionID

		r.Events.Emit("batch_complete", map[string]any{
			"turns_used":  result.TurnsUsed,
			"total_turns": r.totalTurns,
			"cost_usd":    result.CostUsd,
			"total_cost":  r.totalCost,
			"succeeded":   result.Succeeded,
		})

		// Snapshot at configured intervals
		r.maybeSnapshot(batch)

		if result.Succeeded {
			break
		}

		if result.ExitCode != 0 {
			r.fail(fmt.Sprintf("agent exited %d", result.ExitCode))
			return
		}
	}

	// Post-run: auto-commit, push, create MR
	r.finalize()
}

func (r *TaskRunner) setupWorkspace() error {
	r.gitOps = NewGitOps(r.exec, r.Workdir, r.Spec.GitRemote, r.Spec.Branch)

	r.Events.Emit("git_pull", nil)
	_ = r.gitOps.Pull() // best-effort

	r.Events.Emit("git_branch", map[string]any{"branch": r.Spec.Branch})
	return r.gitOps.SetupBranch()
}

func (r *TaskRunner) finalize() {
	r.Events.Emit("auto_commit", nil)
	sha, err := r.gitOps.AutoCommit(r.Spec.IssueKey)
	if err != nil {
		log.Printf("[task %s] auto-commit error: %v", r.ID, err)
	}
	if sha != "" {
		r.Events.Emit("commit", map[string]any{
			"sha": sha, "message": fmt.Sprintf("fix(%s): agent implementation", r.Spec.IssueKey),
		})
	}

	if !r.gitOps.HasCommits() {
		r.Events.Emit("no_changes", nil)
		r.status.Store(StatusSucceeded)
		r.Events.Emit("task_complete", map[string]any{"status": "succeeded", "no_changes": true})
		return
	}

	r.Events.Emit("pushing", map[string]any{"branch": r.Spec.Branch})
	if err := r.gitOps.Push(); err != nil {
		r.fail(fmt.Sprintf("git push: %v", err))
		return
	}

	r.Events.Emit("creating_mr", nil)
	gitlabURL := GitLabURLFromRemote(r.Spec.GitRemote)
	project := r.Spec.GitLabProject
	title := fmt.Sprintf("%s: agent implementation", r.Spec.IssueKey)
	desc := fmt.Sprintf("## Automated by fg-claude-bot\n\nJira: %s\n\nGenerated autonomously. **Review carefully.**", r.Spec.IssueKey)

	mrURL, err := r.gitOps.CreateDraftMR(gitlabURL, project, title, desc)
	if err != nil {
		log.Printf("[task %s] MR creation error: %v", r.ID, err)
		r.Events.Emit("mr_error", map[string]any{"error": err.Error()})
	} else {
		r.mu.Lock()
		r.mrURL = mrURL
		r.mu.Unlock()
		r.Events.Emit("mr_created", map[string]any{"url": mrURL})
	}

	r.status.Store(StatusSucceeded)
	r.Events.Emit("task_complete", map[string]any{
		"status":      "succeeded",
		"mr_url":      mrURL,
		"total_turns": r.totalTurns,
		"total_cost":  r.totalCost,
	})
}

func (r *TaskRunner) fail(msg string) {
	r.mu.Lock()
	r.errMsg = msg
	r.mu.Unlock()
	r.status.Store(StatusFailed)
	r.Events.Emit("task_complete", map[string]any{"status": "failed", "error": msg})
	log.Printf("[task %s] failed: %s", r.ID, msg)
}

// Status returns the current task status as a string (satisfies manager.TaskRunner).
func (r *TaskRunner) Status() string {
	return string(r.status.Load().(TaskStatus))
}

// StatusEnum returns the typed TaskStatus.
func (r *TaskRunner) StatusEnum() TaskStatus {
	return r.status.Load().(TaskStatus)
}

// EventLog returns the event log (satisfies manager.TaskRunner).
func (r *TaskRunner) EventLog() any {
	return r.Events
}

// State returns a snapshot (satisfies manager.TaskRunner).
func (r *TaskRunner) State() any {
	return r.TaskState()
}

// TaskState returns a typed snapshot of the task state.
func (r *TaskRunner) TaskState() TaskState {
	r.mu.Lock()
	defer r.mu.Unlock()

	elapsed := int64(0)
	if !r.startedAt.IsZero() {
		elapsed = time.Since(r.startedAt).Milliseconds()
	}

	ws, _ := InspectWorkspace(r.exec, r.Workdir)
	var files []string
	var commits []Commit
	if ws != nil {
		files = ws.FilesChanged
		commits = ws.Commits
	}

	snaps := make([]SnapshotInfo, len(r.snapshots))
	copy(snaps, r.snapshots)

	return TaskState{
		TaskID:       r.ID,
		Status:       r.StatusEnum(),
		TotalTurns:   r.totalTurns,
		TotalCostUsd: r.totalCost,
		FilesChanged: files,
		Commits:      commits,
		Snapshots:    snaps,
		MRUrl:        r.mrURL,
		Error:        r.errMsg,
		StartedAt:    r.startedAt,
		ElapsedMs:    elapsed,
	}
}

// Pause pauses the task after the current batch completes.
func (r *TaskRunner) Pause() {
	r.status.Store(StatusPaused)
}

// Resume resumes a paused task.
func (r *TaskRunner) Resume() {
	r.mu.Lock()
	ch := r.pauseCh
	r.pauseCh = make(chan struct{})
	r.mu.Unlock()
	close(ch)
}

// Kill terminates the task immediately.
func (r *TaskRunner) Kill() {
	r.status.Store(StatusKilled)
	if r.cancel != nil {
		r.cancel()
	}
	r.Events.Emit("task_complete", map[string]any{"status": "killed"})
}

func (r *TaskRunner) maybeSnapshot(batch int) {
	policy := r.Spec.SnapshotPolicy
	if policy.EveryNBatches <= 0 || r.snapshotter == nil {
		return
	}
	if batch%policy.EveryNBatches != 0 {
		return
	}

	label := fmt.Sprintf("%s-batch-%d", r.Spec.IssueKey, batch)
	r.TakeSnapshot(label, batch)
}

// TakeSnapshot takes a named snapshot. Can be called externally (on-demand from orchestrator).
func (r *TaskRunner) TakeSnapshot(label string, batch int) {
	if r.snapshotter == nil {
		r.Events.Emit("snapshot_skip", map[string]any{"reason": "no snapshotter"})
		return
	}

	_, size, err := r.snapshotter.Snapshot(label)
	if err != nil {
		log.Printf("[task %s] snapshot error: %v", r.ID, err)
		r.Events.Emit("snapshot_error", map[string]any{"label": label, "error": err.Error()})
		return
	}

	snap := SnapshotInfo{Label: label, Size: size, Batch: batch, Time: time.Now()}
	r.mu.Lock()
	r.snapshots = append(r.snapshots, snap)
	r.mu.Unlock()

	r.Events.Emit("snapshot", map[string]any{"label": label, "size": size, "batch": batch})
	log.Printf("[task %s] snapshot: %s (%d bytes)", r.ID, label, size)
}

// Snapshots returns the list of snapshots taken during this task.
func (r *TaskRunner) Snapshots() []SnapshotInfo {
	r.mu.Lock()
	defer r.mu.Unlock()
	out := make([]SnapshotInfo, len(r.snapshots))
	copy(out, r.snapshots)
	return out
}

func backoffDuration(attempt int) time.Duration {
	secs := math.Pow(2.0, float64(attempt))
	if secs > 60 {
		secs = 60
	}
	return time.Duration(secs) * time.Second
}
