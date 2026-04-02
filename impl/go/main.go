package main

import (
	"context"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"squashd/api"
	"squashd/config"
	"squashd/manager"
	"squashd/proxy"
	"squashd/reaper"
)

func main() {
	cfg := config.FromEnv()
	if err := cfg.Validate(); err != nil {
		fmt.Fprintf(os.Stderr, "squashd: config error: %v\n", err)
		os.Exit(1)
	}

	slog.SetDefault(slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{
		Level: slog.LevelInfo,
	})))

	mgr := manager.New(&cfg)

	// Graceful shutdown: stop proxy, reaper, and API server on SIGTERM/SIGINT.
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGTERM, syscall.SIGINT)
	defer stop()
	done := ctx.Done()

	// Recover any sandboxes that were running before the last restart.
	mgr.Recover()

	// Launch HTTPS MITM proxy (checks for secrets.json internally).
	go proxy.Run(":8888", cfg.DataDir)

	// Launch sandbox TTL reaper.
	go reaper.Run(done, mgr)

	handler := api.NewHandler(&cfg, mgr)
	addr := fmt.Sprintf(":%d", cfg.Port)
	srv := &http.Server{
		Addr:         addr,
		Handler:      handler,
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 0, // disabled — exec responses can be long-running
		IdleTimeout:  2 * time.Minute,
	}

	go func() {
		<-done
		slog.Info("squashd: shutting down")
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer cancel()
		_ = srv.Shutdown(shutdownCtx)
	}()

	slog.Info("squashd starting", "addr", addr, "backend", cfg.Backend, "snapshot_backend", cfg.SnapshotBackend)
	if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		fmt.Fprintf(os.Stderr, "squashd: server error: %v\n", err)
		os.Exit(1)
	}
}
