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

func TestParseUSDCMicros(t *testing.T) {
	cases := map[string]int64{
		"1":        1_000_000,
		"1.25":     1_250_000,
		"0.000001": 1,
		"$10.5":    10_500_000,
	}
	for raw, want := range cases {
		got, err := ParseUSDCMicros(raw)
		if err != nil {
			t.Fatalf("ParseUSDCMicros(%q): %v", raw, err)
		}
		if got != want {
			t.Fatalf("ParseUSDCMicros(%q) = %d, want %d", raw, got, want)
		}
	}
	for _, raw := range []string{"", "0", "-1", "1.0000001", "abc"} {
		if _, err := ParseUSDCMicros(raw); err == nil {
			t.Fatalf("ParseUSDCMicros(%q) succeeded, want error", raw)
		}
	}
}

func TestConfirmUSDCDepositCreditsOncePerTxHash(t *testing.T) {
	ctx := context.Background()
	store, _ := newTestControlPlane(t)

	first, err := store.ConfirmUSDCDeposit(ctx, USDCDeposit{
		OrgID:        DefaultOrgID,
		TxHash:       "0xabc",
		AmountMicros: 25_000_000,
		Network:      "base",
		ToAddress:    "0xreceive",
	})
	if err != nil {
		t.Fatalf("confirm first deposit: %v", err)
	}
	if !first.Credited || first.CreditBalanceMicros != 35_000_000 {
		t.Fatalf("first result = %+v, want credited balance 35000000", first)
	}

	second, err := store.ConfirmUSDCDeposit(ctx, USDCDeposit{
		OrgID:        DefaultOrgID,
		TxHash:       "0xabc",
		AmountMicros: 25_000_000,
		Network:      "base",
		ToAddress:    "0xreceive",
	})
	if err != nil {
		t.Fatalf("confirm duplicate deposit: %v", err)
	}
	if second.Credited || second.CreditBalanceMicros != 35_000_000 {
		t.Fatalf("duplicate result = %+v, want not credited same balance", second)
	}

	var rows int
	if err := store.db.QueryRowContext(ctx, `SELECT COUNT(*) FROM credit_ledger WHERE reason = 'usdc_deposit';`).Scan(&rows); err != nil {
		t.Fatalf("count credit rows: %v", err)
	}
	if rows != 1 {
		t.Fatalf("usdc credit rows = %d, want 1", rows)
	}
}

func TestRecordUSDCDetectedDepositMarksDuplicateTxs(t *testing.T) {
	ctx := context.Background()
	store, _ := newTestControlPlane(t)

	first, err := store.RecordUSDCDetectedDeposit(ctx, USDCDetectedDeposit{
		OrgID:        DefaultOrgID,
		TxHash:       "0xabc",
		LogIndex:     1,
		BlockNumber:  100,
		BlockHash:    "0xblock",
		Network:      "base",
		TokenAddress: "0xtoken",
		FromAddress:  "0xfrom",
		ToAddress:    "0xto",
		AmountMicros: 1_000_000,
	})
	if err != nil {
		t.Fatalf("record first deposit: %v", err)
	}
	if !first.Inserted || first.Deposit.DuplicateTx {
		t.Fatalf("first record = %+v, want inserted non-duplicate", first)
	}

	second, err := store.RecordUSDCDetectedDeposit(ctx, USDCDetectedDeposit{
		OrgID:        DefaultOrgID,
		TxHash:       "0xabc",
		LogIndex:     2,
		BlockNumber:  100,
		BlockHash:    "0xblock",
		Network:      "base",
		TokenAddress: "0xtoken",
		FromAddress:  "0xfrom",
		ToAddress:    "0xto",
		AmountMicros: 1_000_000,
	})
	if err != nil {
		t.Fatalf("record duplicate tx deposit: %v", err)
	}
	if !second.Inserted || !second.Deposit.DuplicateTx || second.Deposit.Status != "duplicate_tx" {
		t.Fatalf("second record = %+v, want inserted duplicate_tx", second)
	}

	admin, err := store.USDCAdminDeposits(ctx, "base", 10)
	if err != nil {
		t.Fatalf("admin deposits: %v", err)
	}
	if len(admin.RecentDetectedDeposits) != 2 || len(admin.DuplicateTxs) != 1 {
		t.Fatalf("admin deposits = %+v, want 2 recent and 1 duplicate", admin)
	}
}

func TestUSDCWatcherCursorAndHealth(t *testing.T) {
	ctx := context.Background()
	store, _ := newTestControlPlane(t)
	if _, ok, err := store.USDCWatcherCursor(ctx, "base"); err != nil || ok {
		t.Fatalf("empty cursor = ok:%v err:%v, want missing without error", ok, err)
	}
	if err := store.UpdateUSDCWatcherHealth(ctx, USDCWatcherHealth{
		Network:                  "base",
		Enabled:                  true,
		Running:                  true,
		RPCURLConfigured:         true,
		ReceiveAddressConfigured: true,
		LastScannedBlock:         123,
		LatestBlock:              130,
		ConfirmedBlock:           119,
		Confirmations:            12,
		WatchIntervalSeconds:     30,
	}); err != nil {
		t.Fatalf("update watcher health: %v", err)
	}
	cursor, ok, err := store.USDCWatcherCursor(ctx, "base")
	if err != nil || !ok || cursor != 123 {
		t.Fatalf("cursor = %d ok:%v err:%v, want 123 true nil", cursor, ok, err)
	}
	health, err := store.USDCWatcherHealth(ctx, "base")
	if err != nil {
		t.Fatalf("watcher health: %v", err)
	}
	if !health.Enabled || !health.Running || health.LastScannedBlock != 123 || health.Confirmations != 12 {
		t.Fatalf("health = %+v", health)
	}
	if err := store.UpdateUSDCWatcherHealth(ctx, USDCWatcherHealth{
		Network:              "base",
		Enabled:              true,
		Running:              true,
		Confirmations:        12,
		WatchIntervalSeconds: 30,
		LastError:            "rpc unavailable",
	}); err != nil {
		t.Fatalf("update watcher error health: %v", err)
	}
	cursor, ok, err = store.USDCWatcherCursor(ctx, "base")
	if err != nil || !ok || cursor != 123 {
		t.Fatalf("cursor after zero-block health = %d ok:%v err:%v, want preserved 123", cursor, ok, err)
	}
}

func TestUSDCWatcherCreditVerifiedDepositIsReplaySafe(t *testing.T) {
	ctx := context.Background()
	store, _ := newTestControlPlane(t)
	watcher := NewUSDCWatcher(store, USDCWatcherConfig{
		RPCURL:         "http://example.invalid",
		TokenAddress:   "0xtoken",
		ReceiveAddress: "0xto",
	})
	dep := USDCDetectedDeposit{
		OrgID:        DefaultOrgID,
		TxHash:       "0xabc",
		LogIndex:     1,
		BlockNumber:  100,
		BlockHash:    "0xblock",
		Network:      "base",
		TokenAddress: "0xtoken",
		FromAddress:  "0xfrom",
		ToAddress:    "0xto",
		AmountMicros: 1_000_000,
	}
	if err := watcher.creditVerifiedDeposit(ctx, dep); err != nil {
		t.Fatalf("credit first deposit: %v", err)
	}
	if err := watcher.creditVerifiedDeposit(ctx, dep); err != nil {
		t.Fatalf("replay credited deposit: %v", err)
	}
	admin, err := store.USDCAdminDeposits(ctx, "base", 10)
	if err != nil {
		t.Fatalf("admin deposits: %v", err)
	}
	if len(admin.RecentDetectedDeposits) != 1 {
		t.Fatalf("detected deposits = %d, want 1", len(admin.RecentDetectedDeposits))
	}
	got := admin.RecentDetectedDeposits[0]
	if !got.Credited || got.Status != "credited" || got.DuplicateTx {
		t.Fatalf("replayed deposit = %+v, want still credited and non-duplicate", got)
	}
}

func TestUSDCWatcherRetriesDetectedUncreditedDeposit(t *testing.T) {
	ctx := context.Background()
	store, _ := newTestControlPlane(t)
	dep := USDCDetectedDeposit{
		OrgID:        DefaultOrgID,
		TxHash:       "0xabc",
		LogIndex:     1,
		BlockNumber:  100,
		BlockHash:    "0xblock",
		Network:      "base",
		TokenAddress: "0xtoken",
		FromAddress:  "0xfrom",
		ToAddress:    "0xto",
		AmountMicros: 1_000_000,
	}
	if _, err := store.RecordUSDCDetectedDeposit(ctx, dep); err != nil {
		t.Fatalf("record detected deposit: %v", err)
	}
	watcher := NewUSDCWatcher(store, USDCWatcherConfig{
		RPCURL:         "http://example.invalid",
		TokenAddress:   "0xtoken",
		ReceiveAddress: "0xto",
	})
	if err := watcher.creditVerifiedDeposit(ctx, dep); err != nil {
		t.Fatalf("retry detected deposit: %v", err)
	}
	admin, err := store.USDCAdminDeposits(ctx, "base", 10)
	if err != nil {
		t.Fatalf("admin deposits: %v", err)
	}
	got := admin.RecentDetectedDeposits[0]
	if !got.Credited || got.Status != "credited" {
		t.Fatalf("retried deposit = %+v, want credited", got)
	}
}

func TestValidateManualUSDCDeposit(t *testing.T) {
	txHash := "0x" + "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
	dep := USDCDeposit{
		TxHash:       txHash,
		Network:      "base",
		TokenAddress: "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913",
		ToAddress:    "0x1111111111111111111111111111111111111111",
	}
	if err := ValidateManualUSDCDeposit(dep, "base", "0x833589fcD6EdB6E08F4C7C32D4F71B54BDA02913", "0x1111111111111111111111111111111111111111"); err != nil {
		t.Fatalf("valid deposit rejected: %v", err)
	}
	bad := dep
	bad.TxHash = "0xabc"
	if err := ValidateManualUSDCDeposit(bad, "base", dep.TokenAddress, dep.ToAddress); err == nil {
		t.Fatalf("short tx hash accepted")
	}
	bad = dep
	bad.Network = "ethereum"
	if err := ValidateManualUSDCDeposit(bad, "base", dep.TokenAddress, dep.ToAddress); err == nil {
		t.Fatalf("wrong network accepted")
	}
	bad = dep
	bad.ToAddress = "0x2222222222222222222222222222222222222222"
	if err := ValidateManualUSDCDeposit(bad, "base", dep.TokenAddress, dep.ToAddress); err == nil {
		t.Fatalf("wrong recipient accepted")
	}
}
