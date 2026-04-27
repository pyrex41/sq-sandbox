package controlplane

import (
	"context"
	"path/filepath"
	"testing"
	"time"
)

func newTestControlPlane(t *testing.T) (*Store, *ControlPlane) {
	t.Helper()
	store, err := Open(context.Background(), filepath.Join(t.TempDir(), "app.db"))
	if err != nil {
		t.Fatalf("open store: %v", err)
	}
	t.Cleanup(func() { _ = store.Close() })
	return store, New(store, "", "")
}

func TestRuntimeChargeMicros(t *testing.T) {
	if got := RuntimeChargeMicros(3600, nil); got != BaseSandboxMicrosPerHour {
		t.Fatalf("base one hour charge = %d, want %d", got, BaseSandboxMicrosPerHour)
	}
	if got := RuntimeChargeMicros(5400, []string{"gui"}); got != 180_000 {
		t.Fatalf("gui ninety minute charge = %d, want 180000", got)
	}
	if got := RuntimeChargeMicros(1, nil); got != 12 {
		t.Fatalf("one second base charge = %d, want ceil(40000/3600)=12", got)
	}
}

func TestRecordSandboxStoppedWritesUsageAndDebitsCredits(t *testing.T) {
	ctx := context.Background()
	store, _ := newTestControlPlane(t)
	started := time.Date(2026, 4, 27, 12, 0, 0, 0, time.UTC)

	if err := store.RecordSandboxStarted(ctx, SandboxRecord{
		ID:           "demo",
		OrgID:        DefaultOrgID,
		Owner:        "alice",
		State:        "ready",
		Features:     []string{"gui"},
		CPU:          2,
		MemoryMB:     1024,
		CreatedAt:    started,
		StartedAt:    started,
		LastActiveAt: started,
	}); err != nil {
		t.Fatalf("record start: %v", err)
	}

	event, err := store.RecordSandboxStopped(ctx, DefaultOrgID, "demo", started.Add(90*time.Minute))
	if err != nil {
		t.Fatalf("record stop: %v", err)
	}
	if event.QuantitySeconds != 5400 || event.AmountMicros != 180_000 {
		t.Fatalf("usage event = %+v, want 5400s/180000 micros", event)
	}
	summary, err := store.OrgSummary(ctx, DefaultOrgID)
	if err != nil {
		t.Fatalf("summary: %v", err)
	}
	if summary.CreditBalanceMicros != 9_820_000 {
		t.Fatalf("credits = %d, want 9820000", summary.CreditBalanceMicros)
	}
	if summary.ActiveSandboxes != 0 {
		t.Fatalf("active sandboxes = %d, want 0", summary.ActiveSandboxes)
	}
}

func TestAdmissionDeniesConcurrencyAndInsufficientCredits(t *testing.T) {
	ctx := context.Background()
	store, cp := newTestControlPlane(t)
	if _, err := store.db.ExecContext(ctx, `
UPDATE organizations
SET max_concurrent_sandboxes = 1, credit_balance_micros = 10000000
WHERE id = ?;
`, DefaultOrgID); err != nil {
		t.Fatalf("set limits: %v", err)
	}
	now := time.Now().UTC()
	if err := store.RecordSandboxStarted(ctx, SandboxRecord{
		ID:        "already-running",
		OrgID:     DefaultOrgID,
		Owner:     "alice",
		State:     "ready",
		CreatedAt: now,
		StartedAt: now,
	}); err != nil {
		t.Fatalf("record active sandbox: %v", err)
	}
	decision, err := cp.AdmitCreate(ctx, AdmissionRequest{
		OrgID:               DefaultOrgID,
		SandboxID:           "next",
		EstimatedRunSeconds: 3600,
	})
	if err != nil {
		t.Fatalf("admit concurrency: %v", err)
	}
	if decision.Allowed || decision.Code != "concurrency_limit" {
		t.Fatalf("decision = %+v, want concurrency denial", decision)
	}

	if _, err := store.db.ExecContext(ctx, `
UPDATE organizations
SET max_concurrent_sandboxes = 10, credit_balance_micros = 1
WHERE id = ?;
`, DefaultOrgID); err != nil {
		t.Fatalf("set credits: %v", err)
	}
	decision, err = cp.AdmitCreate(ctx, AdmissionRequest{
		OrgID:               DefaultOrgID,
		SandboxID:           "expensive",
		EstimatedRunSeconds: 3600,
	})
	if err != nil {
		t.Fatalf("admit credits: %v", err)
	}
	if decision.Allowed || decision.Code != "insufficient_credits" {
		t.Fatalf("decision = %+v, want insufficient credits", decision)
	}
}
