package manager

import (
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
)

// Recover scans the sandboxes directory on startup and rebuilds the in-memory
// registry from any sandboxes that are already mounted on disk.
//
// This handles the case where squashd restarts while sandboxes are still running:
// overlay filesystems remain mounted across daemon restarts, so we just need to
// re-register them in memory.
func (m *Manager) Recover() {
	sandboxesDir := m.cfg.SandboxesDir()
	entries, err := os.ReadDir(sandboxesDir)
	if err != nil {
		if !os.IsNotExist(err) {
			slog.Warn("recovery: cannot read sandboxes dir", "dir", sandboxesDir, "err", err)
		}
		return
	}

	recovered := 0
	skipped := 0

	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		id := e.Name()
		sdir := filepath.Join(sandboxesDir, id)

		s, err := readMeta(id, sdir)
		if err != nil {
			slog.Warn("recovery: skip (no metadata)", "id", id, "err", err)
			skipped++
			continue
		}

		// Verify the overlay is actually mounted.
		if !isMounted(s.mergedDir()) {
			slog.Warn("recovery: skip (not mounted)", "id", id)
			skipped++
			continue
		}

		// Check if a snapshot is mounted at images/_snapshot.
		snapMP := filepath.Join(s.imagesDir(), "_snapshot")
		if isMounted(snapMP) {
			s.mu.Lock()
			s.snapshotMounted = true
			s.mu.Unlock()
		}

		m.Register(s)
		recovered++
		slog.Info("recovery: registered", "id", id, "backend", s.Backend)
	}

	slog.Info("recovery complete", "recovered", recovered, "skipped", skipped)
}

// isMounted returns true if path is a mount point.
// Uses mountpoint(1) which is available on Alpine/Debian.
func isMounted(path string) bool {
	if _, err := os.Stat(path); err != nil {
		return false
	}
	err := exec.Command("mountpoint", "-q", path).Run()
	return err == nil
}
