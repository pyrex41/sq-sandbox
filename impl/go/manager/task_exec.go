package manager

import (
	"fmt"

	"squashd/runner"
)

// SandboxSnapshotter adapts Manager.Snapshot to runner.Snapshotter.
type SandboxSnapshotter struct {
	mgr       *Manager
	sandboxID string
}

func (m *Manager) NewSandboxSnapshotter(sandboxID string) *SandboxSnapshotter {
	return &SandboxSnapshotter{mgr: m, sandboxID: sandboxID}
}

func (s *SandboxSnapshotter) Snapshot(label string) (string, int64, error) {
	result, err := s.mgr.Snapshot(s.sandboxID, label)
	if err != nil {
		return "", 0, err
	}
	return result.Snapshot, result.Size, nil
}

// SandboxExecutor adapts the Manager's exec methods to the runner.Executor interface.
// It runs commands inside a specific sandbox's chroot/bwrap environment.
type SandboxExecutor struct {
	mgr       *Manager
	sandboxID string
}

// NewSandboxExecutor returns an Executor that runs commands inside the given sandbox.
func (m *Manager) NewSandboxExecutor(sandboxID string) *SandboxExecutor {
	return &SandboxExecutor{mgr: m, sandboxID: sandboxID}
}

func (e *SandboxExecutor) Exec(cmd string, workdir string, timeoutS int) (*runner.ExecResult, error) {
	result, err := e.mgr.Exec(e.sandboxID, cmd, ExecOpts{
		WorkDir:  workdir,
		TimeoutS: timeoutS,
	})
	if err != nil {
		return nil, err
	}
	return &runner.ExecResult{
		ExitCode: result.ExitCode,
		Stdout:   result.Stdout,
		Stderr:   result.Stderr,
		TimedOut: result.TimedOut,
	}, nil
}

func (e *SandboxExecutor) ExecBg(cmd string, workdir string, timeoutS int) (int, <-chan string, <-chan int, func(), error) {
	jobID, err := e.mgr.ExecBg(e.sandboxID, cmd, workdir, timeoutS)
	if err != nil {
		return 0, nil, nil, nil, err
	}

	job, err := e.mgr.GetJob(e.sandboxID, jobID)
	if err != nil {
		return 0, nil, nil, nil, fmt.Errorf("get job: %w", err)
	}

	lines := make(chan string, 100)
	done := make(chan int, 1)

	history, sub, isDone, exitCode := job.Subscribe()

	go func() {
		defer close(lines)
		defer close(done)
		defer job.Unsubscribe(sub)

		pos := len(history)
		for _, line := range history {
			lines <- line
		}

		if isDone {
			done <- exitCode
			return
		}

		for range sub.Notify() {
			newLines, newPos, jobDone, jobExitCode := job.ReadSince(pos)
			for _, line := range newLines {
				lines <- line
			}
			pos = newPos

			if jobDone {
				done <- jobExitCode
				return
			}
		}
	}()

	cancel := func() {
		_ = e.mgr.KillJob(e.sandboxID, jobID)
	}

	return jobID, lines, done, cancel, nil
}
