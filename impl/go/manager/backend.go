package manager

import (
	"errors"
	"os"
	"path/filepath"

	sqexec "squashd/exec"
)

// ErrCheckpointUnsupported is returned by backends that cannot capture/restore
// process memory (currently the chroot backend; only gvisor supports it).
var ErrCheckpointUnsupported = errors.New("memory checkpoint not supported by this backend")

// Backend abstracts how a sandbox's payload is launched and (optionally)
// memory-checkpointed.
//
// IMPORTANT: both the synchronous Exec path (manager.go) and the background
// ExecBg path (jobs.go) route through Manager.backendFor so a memory checkpoint
// captures the *agent loop* — which runs via ExecBg — and not an empty sandbox.
// Wiring a backend only into Exec would be a latent bug: TaskRunner reaches the
// sandbox exclusively through ExecBg.
type Backend interface {
	// Run executes cmd synchronously in the sandbox and returns its result.
	Run(s *Sandbox, cmd, workdir string, timeoutS int) (*sqexec.Result, error)

	// RunBg starts cmd in the background; onLine receives each merged
	// stdout/stderr line. Returns a cancel func and an exit-code channel.
	RunBg(s *Sandbox, cmd, workdir string, timeoutS int, extraEnv []string, onLine func(string)) (cancel func(), exitCh <-chan int)

	// Checkpoint captures sandbox state (process memory for gvisor) into imgPath.
	// Returns ErrCheckpointUnsupported for backends without memory capture.
	Checkpoint(s *Sandbox, imgPath string) error

	// Restore re-establishes the sandbox from a checkpoint image at imgPath.
	Restore(s *Sandbox, imgPath string) error

	// Destroy tears down backend-specific resources (runsc sandbox, slirp,
	// state dir). No-op for chroot.
	Destroy(s *Sandbox) error
}

// backendFor returns the Backend implementation for a sandbox, keyed by the
// per-sandbox Backend field (stamped at create, persisted in .meta/backend,
// defaulting to "chroot"). The choice is per-sandbox, not global, so existing
// chroot sandboxes keep the exact current code path.
func (m *Manager) backendFor(s *Sandbox) Backend {
	switch s.Backend {
	case "gvisor":
		return &gvisorBackend{cfg: m.cfg}
	default:
		return chrootBackend{}
	}
}

// ── chroot backend (default): behavior-preserving wrapper over sq-exec ────────

type chrootBackend struct{}

func (chrootBackend) Run(s *Sandbox, cmd, workdir string, timeoutS int) (*sqexec.Result, error) {
	return sqexec.Run(s.mergedDir(), cmd, workdir, timeoutS)
}

func (chrootBackend) RunBg(s *Sandbox, cmd, workdir string, timeoutS int, extraEnv []string, onLine func(string)) (func(), <-chan int) {
	return sqexec.RunBg(s.mergedDir(), cmd, workdir, timeoutS, extraEnv, onLine)
}

func (chrootBackend) Checkpoint(s *Sandbox, imgPath string) error { return ErrCheckpointUnsupported }
func (chrootBackend) Restore(s *Sandbox, imgPath string) error    { return ErrCheckpointUnsupported }
func (chrootBackend) Destroy(s *Sandbox) error                    { return nil }

// dirSize returns the total size of regular files under p (best-effort).
func dirSize(p string) int64 {
	var n int64
	_ = filepath.Walk(p, func(_ string, fi os.FileInfo, err error) error {
		if err == nil && fi != nil && !fi.IsDir() {
			n += fi.Size()
		}
		return nil
	})
	return n
}
