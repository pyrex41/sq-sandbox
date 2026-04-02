// Package reaper enforces sandbox lifetime limits.
// It runs as a background goroutine and destroys sandboxes that have exceeded
// their max_lifetime_s or have been idle beyond the configured TTL.
package reaper

import (
	"log/slog"
	"time"
)

const pollInterval = 10 * time.Second

// Destroyer is the interface the reaper uses to destroy sandboxes.
type Destroyer interface {
	// ExpiredSandboxes returns IDs of sandboxes eligible for reaping.
	ExpiredSandboxes() []string
	// Destroy tears down the named sandbox.
	Destroy(id string) error
}

// Run starts the reaper loop. It blocks until ctx is cancelled.
// Call as: go reaper.Run(ctx, mgr)
func Run(done <-chan struct{}, mgr Destroyer) {
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		select {
		case <-done:
			return
		case <-ticker.C:
			for _, id := range mgr.ExpiredSandboxes() {
				slog.Info("reaper: destroying expired sandbox", "id", id)
				if err := mgr.Destroy(id); err != nil {
					slog.Warn("reaper: destroy failed", "id", id, "err", err)
				}
			}
		}
	}
}
