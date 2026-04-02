package manager

import (
	"fmt"
	"sync"

	sqexec "squashd/exec"
)

// Job tracks a background command execution within a sandbox.
type Job struct {
	ID        int    `json:"job_id"`
	SandboxID string `json:"-"`
	Status    string `json:"status"` // "running", "done", "killed"
	ExitCode  int    `json:"exit_code"`

	mu     sync.Mutex
	lines  []string
	subs   []*JobSub
	isDone bool
	cancel func()
}

// JobSub is an opaque subscription handle for SSE log streaming.
type JobSub struct {
	ch chan struct{}
}

// Notify returns the channel that is signalled when new lines are added or
// the job finishes.
func (s *JobSub) Notify() <-chan struct{} { return s.ch }

// Subscribe registers a new SSE subscriber. Returns a snapshot of current log
// lines, the subscription handle, whether the job is already done, and the
// final exit code (only valid if done=true).
func (j *Job) Subscribe() ([]string, *JobSub, bool, int) {
	j.mu.Lock()
	defer j.mu.Unlock()
	sub := &JobSub{ch: make(chan struct{}, 1)}
	j.subs = append(j.subs, sub)
	snapshot := make([]string, len(j.lines))
	copy(snapshot, j.lines)
	return snapshot, sub, j.isDone, j.ExitCode
}

// Unsubscribe removes a subscriber.
func (j *Job) Unsubscribe(sub *JobSub) {
	j.mu.Lock()
	defer j.mu.Unlock()
	for i, s := range j.subs {
		if s == sub {
			j.subs = append(j.subs[:i], j.subs[i+1:]...)
			return
		}
	}
}

// ReadSince returns log lines from position pos onward, the new tail position,
// whether the job is done, and the exit code.
func (j *Job) ReadSince(pos int) ([]string, int, bool, int) {
	j.mu.Lock()
	defer j.mu.Unlock()
	var lines []string
	if pos < len(j.lines) {
		lines = make([]string, len(j.lines)-pos)
		copy(lines, j.lines[pos:])
	}
	return lines, len(j.lines), j.isDone, j.ExitCode
}

// Info returns a stable snapshot of the job's public fields.
func (j *Job) Info() (id int, status string, exitCode int) {
	j.mu.Lock()
	defer j.mu.Unlock()
	return j.ID, j.Status, j.ExitCode
}

// addLine appends a line to the job log and notifies all subscribers.
func (j *Job) addLine(line string) {
	j.mu.Lock()
	j.lines = append(j.lines, line)
	subs := make([]*JobSub, len(j.subs))
	copy(subs, j.subs)
	j.mu.Unlock()
	for _, s := range subs {
		select {
		case s.ch <- struct{}{}:
		default:
		}
	}
}

// finish marks the job complete and notifies all subscribers.
// No-op if already done (e.g. killed eagerly by KillJob).
func (j *Job) finish(exitCode int) {
	j.mu.Lock()
	if j.isDone {
		j.mu.Unlock()
		return
	}
	j.isDone = true
	j.ExitCode = exitCode
	j.Status = "done"
	subs := make([]*JobSub, len(j.subs))
	copy(subs, j.subs)
	j.mu.Unlock()
	for _, s := range subs {
		select {
		case s.ch <- struct{}{}:
		default:
		}
	}
}

// ── Manager methods ──────────────────────────────────────────────────────────

// ExecBg starts a background command in the sandbox and returns the job ID.
func (m *Manager) ExecBg(sandboxID, cmd, workdir string, timeoutS int) (int, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[sandboxID]
	m.mu.Unlock()
	if !exists || s == nil {
		return 0, fmt.Errorf("sandbox not found: %s", sandboxID)
	}

	if workdir == "" {
		workdir = "/"
	}
	if timeoutS <= 0 {
		timeoutS = 7200
	}

	m.jobMu.Lock()
	m.jobSeq++
	jobID := m.jobSeq
	if m.jobs == nil {
		m.jobs = make(map[string]map[int]*Job)
	}
	if m.jobs[sandboxID] == nil {
		m.jobs[sandboxID] = make(map[int]*Job)
	}
	job := &Job{
		ID:        jobID,
		SandboxID: sandboxID,
		Status:    "running",
	}
	m.jobs[sandboxID][jobID] = job
	m.jobMu.Unlock()

	s.updateLastActive()

	cancelFn, exitCh := sqexec.RunBg(s.mergedDir(), cmd, workdir, timeoutS, job.addLine)

	job.mu.Lock()
	job.cancel = cancelFn
	job.mu.Unlock()

	go func() {
		exitCode := <-exitCh
		job.finish(exitCode)
	}()

	return jobID, nil
}

// GetJob returns the job for the given sandbox and job ID.
func (m *Manager) GetJob(sandboxID string, jobID int) (*Job, error) {
	m.jobMu.Lock()
	defer m.jobMu.Unlock()
	sbJobs, ok := m.jobs[sandboxID]
	if !ok {
		return nil, fmt.Errorf("job not found: %d (sandbox %s)", jobID, sandboxID)
	}
	job, ok := sbJobs[jobID]
	if !ok {
		return nil, fmt.Errorf("job not found: %d", jobID)
	}
	return job, nil
}

// KillJob sends SIGKILL to a running job. No-op if already done.
func (m *Manager) KillJob(sandboxID string, jobID int) error {
	job, err := m.GetJob(sandboxID, jobID)
	if err != nil {
		return err
	}
	job.mu.Lock()
	if job.isDone {
		job.mu.Unlock()
		return nil
	}
	cancel := job.cancel
	// Mark killed eagerly so the next status poll sees it immediately.
	job.isDone = true
	job.Status = "killed"
	subs := make([]*JobSub, len(job.subs))
	copy(subs, job.subs)
	job.mu.Unlock()
	if cancel != nil {
		cancel()
	}
	for _, s := range subs {
		select {
		case s.ch <- struct{}{}:
		default:
		}
	}
	return nil
}
