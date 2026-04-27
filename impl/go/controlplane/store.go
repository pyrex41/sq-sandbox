package controlplane

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	_ "modernc.org/sqlite"
)

const (
	DefaultOrgID  = "default-org"
	DefaultUserID = "default-user"

	BaseSandboxMicrosPerHour = int64(40_000)
	GUISandboxMicrosPerHour  = int64(120_000)
)

type Store struct {
	db *sql.DB
}

type OrgSummary struct {
	ID                     string `json:"id"`
	Name                   string `json:"name"`
	Plan                   string `json:"plan"`
	CreditBalanceMicros    int64  `json:"credit_balance_micros"`
	MonthlyBudgetMicros    int64  `json:"monthly_budget_micros"`
	MonthlyUsageMicros     int64  `json:"monthly_usage_micros"`
	MaxConcurrentSandboxes int    `json:"max_concurrent_sandboxes"`
	MaxRuntimeSeconds      int    `json:"max_runtime_seconds"`
	MaxIdleSeconds         int    `json:"max_idle_seconds"`
	ActiveSandboxes        int    `json:"active_sandboxes"`
}

type SandboxRecord struct {
	ID           string
	OrgID        string
	Owner        string
	State        string
	Features     []string
	CPU          float64
	MemoryMB     int
	CreatedAt    time.Time
	StartedAt    time.Time
	LastActiveAt time.Time
	MaxLifetimeS int
}

type SandboxSummary struct {
	ID           string   `json:"id"`
	OrgID        string   `json:"org_id"`
	Owner        string   `json:"owner"`
	State        string   `json:"state"`
	Features     []string `json:"features,omitempty"`
	CPU          float64  `json:"cpu"`
	MemoryMB     int      `json:"memory_mb"`
	CreatedAt    string   `json:"created_at"`
	StartedAt    string   `json:"started_at,omitempty"`
	StoppedAt    string   `json:"stopped_at,omitempty"`
	LastActiveAt string   `json:"last_active_at,omitempty"`
	MaxLifetimeS int      `json:"max_lifetime_s"`
}

type UsageEvent struct {
	ID              int64  `json:"id"`
	OrgID           string `json:"org_id"`
	SandboxID       string `json:"sandbox_id,omitempty"`
	EventType       string `json:"event_type"`
	QuantitySeconds int64  `json:"quantity_seconds"`
	AmountMicros    int64  `json:"amount_micros"`
	OccurredAt      string `json:"occurred_at"`
	MetadataJSON    string `json:"metadata_json,omitempty"`
}

type Dashboard struct {
	Org             OrgSummary       `json:"org"`
	RecentSandboxes []SandboxSummary `json:"recent_sandboxes"`
	RecentUsage     []UsageEvent     `json:"recent_usage"`
}

type USDCDetectedDeposit struct {
	ID            int64  `json:"id"`
	OrgID         string `json:"org_id"`
	TxHash        string `json:"tx_hash"`
	LogIndex      uint64 `json:"log_index"`
	BlockNumber   uint64 `json:"block_number"`
	BlockHash     string `json:"block_hash"`
	Network       string `json:"network"`
	TokenAddress  string `json:"token_address"`
	FromAddress   string `json:"from_address"`
	ToAddress     string `json:"to_address"`
	AmountMicros  int64  `json:"amount_micros"`
	Confirmations uint64 `json:"confirmations"`
	Credited      bool   `json:"credited"`
	DuplicateTx   bool   `json:"duplicate_tx"`
	Status        string `json:"status"`
	DetectedAt    string `json:"detected_at"`
	CreditedAt    string `json:"credited_at,omitempty"`
	Error         string `json:"error,omitempty"`
}

type USDCDepositRecordResult struct {
	Deposit  USDCDetectedDeposit
	Inserted bool
}

type USDCWatcherHealth struct {
	Network                  string `json:"network"`
	Enabled                  bool   `json:"enabled"`
	Running                  bool   `json:"running"`
	RPCURLConfigured         bool   `json:"rpc_url_configured"`
	ReceiveAddressConfigured bool   `json:"receive_address_configured"`
	LastScannedBlock         uint64 `json:"last_scanned_block"`
	LatestBlock              uint64 `json:"latest_block"`
	ConfirmedBlock           uint64 `json:"confirmed_block"`
	Confirmations            uint64 `json:"confirmations"`
	WatchIntervalSeconds     int64  `json:"watch_interval_seconds"`
	LastCheckedAt            string `json:"last_checked_at,omitempty"`
	LastSuccessAt            string `json:"last_success_at,omitempty"`
	LastError                string `json:"last_error,omitempty"`
}

type USDCAdminDeposits struct {
	RecentDetectedDeposits []USDCDetectedDeposit `json:"recent_detected_deposits"`
	DuplicateTxs           []USDCDetectedDeposit `json:"duplicate_txs"`
	WatcherHealth          USDCWatcherHealth     `json:"watcher_health"`
}

func Open(ctx context.Context, path string) (*Store, error) {
	if path == "" {
		return nil, fmt.Errorf("control db path is required")
	}
	if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
		return nil, fmt.Errorf("mkdir control db dir: %w", err)
	}
	db, err := sql.Open("sqlite", path)
	if err != nil {
		return nil, err
	}
	db.SetMaxOpenConns(1)
	s := &Store{db: db}
	if err := s.init(ctx); err != nil {
		_ = db.Close()
		return nil, err
	}
	return s, nil
}

func (s *Store) Close() error {
	if s == nil || s.db == nil {
		return nil
	}
	return s.db.Close()
}

func (s *Store) EnsureDefaultTenant(ctx context.Context) error {
	_, err := s.db.ExecContext(ctx, `
INSERT INTO organizations (
	id, name, plan, monthly_credit_micros, credit_balance_micros,
	monthly_budget_micros, max_concurrent_sandboxes, max_runtime_seconds,
	max_idle_seconds, created_at
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(id) DO NOTHING;
`, DefaultOrgID, "Default org", "free", 10_000_000, 10_000_000, 20_000_000, 3, 1800, 600, nowString())
	if err != nil {
		return fmt.Errorf("seed default org: %w", err)
	}
	_, err = s.db.ExecContext(ctx, `
INSERT INTO users (id, org_id, email, created_at)
VALUES (?, ?, ?, ?)
ON CONFLICT(id) DO NOTHING;
`, DefaultUserID, DefaultOrgID, "local@example.invalid", nowString())
	if err != nil {
		return fmt.Errorf("seed default user: %w", err)
	}
	return nil
}

func (s *Store) OrgSummary(ctx context.Context, orgID string) (OrgSummary, error) {
	if orgID == "" {
		orgID = DefaultOrgID
	}
	start := monthStart(time.Now()).Format(time.RFC3339)
	var out OrgSummary
	err := s.db.QueryRowContext(ctx, `
SELECT
	o.id, o.name, o.plan, o.credit_balance_micros, o.monthly_budget_micros,
	o.max_concurrent_sandboxes, o.max_runtime_seconds, o.max_idle_seconds,
	COALESCE((SELECT SUM(amount_micros) FROM usage_events WHERE org_id = o.id AND occurred_at >= ?), 0),
	(SELECT COUNT(*) FROM sandboxes WHERE org_id = o.id AND state IN ('creating', 'ready', 'running'))
FROM organizations o
WHERE o.id = ?;
`, start, orgID).Scan(
		&out.ID, &out.Name, &out.Plan, &out.CreditBalanceMicros, &out.MonthlyBudgetMicros,
		&out.MaxConcurrentSandboxes, &out.MaxRuntimeSeconds, &out.MaxIdleSeconds,
		&out.MonthlyUsageMicros, &out.ActiveSandboxes,
	)
	if err != nil {
		return OrgSummary{}, err
	}
	return out, nil
}

func (s *Store) Dashboard(ctx context.Context, orgID string) (Dashboard, error) {
	org, err := s.OrgSummary(ctx, orgID)
	if err != nil {
		return Dashboard{}, err
	}
	sandboxes, err := s.RecentSandboxes(ctx, org.ID, 20)
	if err != nil {
		return Dashboard{}, err
	}
	usage, err := s.RecentUsage(ctx, org.ID, 20)
	if err != nil {
		return Dashboard{}, err
	}
	return Dashboard{Org: org, RecentSandboxes: sandboxes, RecentUsage: usage}, nil
}

func (s *Store) RecentSandboxes(ctx context.Context, orgID string, limit int) ([]SandboxSummary, error) {
	if limit <= 0 || limit > 100 {
		limit = 20
	}
	rows, err := s.db.QueryContext(ctx, `
SELECT id, org_id, owner, state, features_json, cpu, memory_mb, created_at,
       COALESCE(started_at, ''), COALESCE(stopped_at, ''), COALESCE(last_active_at, ''),
       max_lifetime_s
FROM sandboxes
WHERE org_id = ?
ORDER BY created_at DESC
LIMIT ?;
`, orgID, limit)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []SandboxSummary
	for rows.Next() {
		var item SandboxSummary
		var featuresJSON string
		if err := rows.Scan(&item.ID, &item.OrgID, &item.Owner, &item.State, &featuresJSON,
			&item.CPU, &item.MemoryMB, &item.CreatedAt, &item.StartedAt, &item.StoppedAt,
			&item.LastActiveAt, &item.MaxLifetimeS); err != nil {
			return nil, err
		}
		_ = json.Unmarshal([]byte(featuresJSON), &item.Features)
		out = append(out, item)
	}
	return out, rows.Err()
}

func (s *Store) RecentUsage(ctx context.Context, orgID string, limit int) ([]UsageEvent, error) {
	if limit <= 0 || limit > 100 {
		limit = 20
	}
	rows, err := s.db.QueryContext(ctx, `
SELECT id, org_id, COALESCE(sandbox_id, ''), event_type, quantity_seconds,
       amount_micros, occurred_at, COALESCE(metadata_json, '')
FROM usage_events
WHERE org_id = ?
ORDER BY occurred_at DESC, id DESC
LIMIT ?;
`, orgID, limit)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []UsageEvent
	for rows.Next() {
		var item UsageEvent
		if err := rows.Scan(&item.ID, &item.OrgID, &item.SandboxID, &item.EventType,
			&item.QuantitySeconds, &item.AmountMicros, &item.OccurredAt, &item.MetadataJSON); err != nil {
			return nil, err
		}
		out = append(out, item)
	}
	return out, rows.Err()
}

func (s *Store) RecordSandboxStarted(ctx context.Context, rec SandboxRecord) error {
	if rec.ID == "" {
		return fmt.Errorf("sandbox id is required")
	}
	if rec.OrgID == "" {
		rec.OrgID = DefaultOrgID
	}
	if rec.Owner == "" {
		rec.Owner = DefaultUserID
	}
	if rec.State == "" {
		rec.State = "ready"
	}
	if rec.CreatedAt.IsZero() {
		rec.CreatedAt = time.Now()
	}
	if rec.StartedAt.IsZero() {
		rec.StartedAt = rec.CreatedAt
	}
	if rec.LastActiveAt.IsZero() {
		rec.LastActiveAt = rec.StartedAt
	}
	features, err := json.Marshal(rec.Features)
	if err != nil {
		return err
	}
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	defer rollback(tx)
	_, err = tx.ExecContext(ctx, `
INSERT INTO sandboxes (
	id, org_id, owner, state, features_json, cpu, memory_mb, created_at,
	started_at, last_active_at, max_lifetime_s
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(id) DO UPDATE SET
	org_id = excluded.org_id,
	owner = excluded.owner,
	state = excluded.state,
	features_json = excluded.features_json,
	cpu = excluded.cpu,
	memory_mb = excluded.memory_mb,
	started_at = COALESCE(sandboxes.started_at, excluded.started_at),
	last_active_at = excluded.last_active_at,
	max_lifetime_s = excluded.max_lifetime_s;
`, rec.ID, rec.OrgID, rec.Owner, rec.State, string(features), rec.CPU, rec.MemoryMB,
		timeString(rec.CreatedAt), timeString(rec.StartedAt), timeString(rec.LastActiveAt), rec.MaxLifetimeS)
	if err != nil {
		return fmt.Errorf("upsert sandbox: %w", err)
	}
	if _, err = insertUsage(ctx, tx, rec.OrgID, rec.ID, "sandbox_started", 0, 0, rec.StartedAt, "{}"); err != nil {
		return err
	}
	return tx.Commit()
}

func (s *Store) RecordSandboxStopped(ctx context.Context, orgID, sandboxID string, stoppedAt time.Time) (UsageEvent, error) {
	if orgID == "" {
		orgID = DefaultOrgID
	}
	if stoppedAt.IsZero() {
		stoppedAt = time.Now()
	}
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return UsageEvent{}, err
	}
	defer rollback(tx)

	var state, startedRaw, featuresJSON string
	err = tx.QueryRowContext(ctx, `
SELECT state, COALESCE(started_at, created_at), features_json
FROM sandboxes
WHERE id = ? AND org_id = ?;
`, sandboxID, orgID).Scan(&state, &startedRaw, &featuresJSON)
	if errors.Is(err, sql.ErrNoRows) {
		return UsageEvent{}, fmt.Errorf("control-plane sandbox not found: %s", sandboxID)
	}
	if err != nil {
		return UsageEvent{}, err
	}
	if state == "stopped" || state == "deleted" {
		return UsageEvent{}, nil
	}
	startedAt, _ := time.Parse(time.RFC3339, startedRaw)
	seconds := int64(stoppedAt.Sub(startedAt).Seconds())
	if seconds < 0 {
		seconds = 0
	}
	var features []string
	_ = json.Unmarshal([]byte(featuresJSON), &features)
	amount := RuntimeChargeMicros(seconds, features)

	_, err = tx.ExecContext(ctx, `
UPDATE sandboxes
SET state = 'stopped', stopped_at = ?, last_active_at = ?
WHERE id = ? AND org_id = ?;
`, timeString(stoppedAt), timeString(stoppedAt), sandboxID, orgID)
	if err != nil {
		return UsageEvent{}, err
	}
	event, err := insertUsage(ctx, tx, orgID, sandboxID, "sandbox_runtime", seconds, amount, stoppedAt, "{}")
	if err != nil {
		return UsageEvent{}, err
	}
	if amount > 0 {
		if _, err = tx.ExecContext(ctx, `
UPDATE organizations SET credit_balance_micros = credit_balance_micros - ? WHERE id = ?;
`, amount, orgID); err != nil {
			return UsageEvent{}, err
		}
		if _, err = tx.ExecContext(ctx, `
INSERT INTO credit_ledger (org_id, delta_micros, reason, reference, created_at)
VALUES (?, ?, ?, ?, ?);
`, orgID, -amount, "sandbox_runtime", sandboxID, timeString(stoppedAt)); err != nil {
			return UsageEvent{}, err
		}
	}
	return event, tx.Commit()
}

func (s *Store) RecordBillingEvent(ctx context.Context, provider, providerEventID, eventType, status string, amountMicros int64, metadata string) error {
	if provider == "" {
		provider = "stripe"
	}
	_, err := s.db.ExecContext(ctx, `
INSERT INTO billing_events (provider, provider_event_id, event_type, status, amount_micros, metadata_json, created_at)
VALUES (?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(provider_event_id) DO NOTHING;
`, provider, providerEventID, eventType, status, amountMicros, metadata, nowString())
	return err
}

func (s *Store) AddCredits(ctx context.Context, orgID string, deltaMicros int64, reason, reference string) error {
	if orgID == "" {
		orgID = DefaultOrgID
	}
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return err
	}
	defer rollback(tx)
	if _, err = tx.ExecContext(ctx, `UPDATE organizations SET credit_balance_micros = credit_balance_micros + ? WHERE id = ?;`, deltaMicros, orgID); err != nil {
		return err
	}
	if _, err = tx.ExecContext(ctx, `
INSERT INTO credit_ledger (org_id, delta_micros, reason, reference, created_at)
VALUES (?, ?, ?, ?, ?);
`, orgID, deltaMicros, reason, reference, nowString()); err != nil {
		return err
	}
	return tx.Commit()
}

func (s *Store) RecordUSDCDetectedDeposit(ctx context.Context, dep USDCDetectedDeposit) (USDCDepositRecordResult, error) {
	if dep.OrgID == "" {
		dep.OrgID = DefaultOrgID
	}
	if dep.Status == "" {
		dep.Status = "detected"
	}
	if dep.DetectedAt == "" {
		dep.DetectedAt = nowString()
	}
	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return USDCDepositRecordResult{}, err
	}
	defer rollback(tx)

	existing, ok, err := usdcDepositByLog(ctx, tx, dep.TxHash, dep.LogIndex)
	if err != nil {
		return USDCDepositRecordResult{}, err
	}
	if ok {
		return USDCDepositRecordResult{Deposit: existing, Inserted: false}, tx.Commit()
	}

	var matchingRows int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM usdc_deposits WHERE tx_hash = ?;`, dep.TxHash).Scan(&matchingRows); err != nil {
		return USDCDepositRecordResult{}, err
	}
	var creditedRows int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM billing_events WHERE provider = 'usdc' AND provider_event_id = ?;`, dep.TxHash).Scan(&creditedRows); err != nil {
		return USDCDepositRecordResult{}, err
	}
	dep.DuplicateTx = dep.DuplicateTx || matchingRows > 0 || creditedRows > 0
	if dep.DuplicateTx && dep.Status == "detected" {
		dep.Status = "duplicate_tx"
	}

	res, err := tx.ExecContext(ctx, `
INSERT INTO usdc_deposits (
	org_id, tx_hash, log_index, block_number, block_hash, network, token_address,
	from_address, to_address, amount_micros, confirmations, credited, duplicate_tx,
	status, detected_at, credited_at, error
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
`, dep.OrgID, dep.TxHash, dep.LogIndex, dep.BlockNumber, dep.BlockHash, dep.Network,
		dep.TokenAddress, dep.FromAddress, dep.ToAddress, dep.AmountMicros, dep.Confirmations,
		boolInt(dep.Credited), boolInt(dep.DuplicateTx), dep.Status, dep.DetectedAt,
		nullable(dep.CreditedAt), nullable(dep.Error))
	if err != nil {
		return USDCDepositRecordResult{}, err
	}
	dep.ID, _ = res.LastInsertId()
	return USDCDepositRecordResult{Deposit: dep, Inserted: true}, tx.Commit()
}

func (s *Store) MarkUSDCDetectedDeposit(ctx context.Context, txHash string, logIndex uint64, credited bool, status, errMsg string) error {
	if status == "" {
		if credited {
			status = "credited"
		} else {
			status = "detected"
		}
	}
	creditedAt := ""
	if credited {
		creditedAt = nowString()
	}
	_, err := s.db.ExecContext(ctx, `
UPDATE usdc_deposits
SET credited = ?, status = ?, credited_at = CASE WHEN ? = '' THEN credited_at ELSE ? END, error = ?
WHERE tx_hash = ? AND log_index = ?;
`, boolInt(credited), status, creditedAt, creditedAt, errMsg, txHash, logIndex)
	return err
}

func (s *Store) USDCWatcherCursor(ctx context.Context, network string) (uint64, bool, error) {
	if network == "" {
		network = "base"
	}
	var last uint64
	err := s.db.QueryRowContext(ctx, `SELECT last_scanned_block FROM usdc_watcher_state WHERE network = ?;`, network).Scan(&last)
	if errors.Is(err, sql.ErrNoRows) {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, err
	}
	return last, true, nil
}

func (s *Store) UpdateUSDCWatcherHealth(ctx context.Context, health USDCWatcherHealth) error {
	if health.Network == "" {
		health.Network = "base"
	}
	if health.LastCheckedAt == "" {
		health.LastCheckedAt = nowString()
	}
	_, err := s.db.ExecContext(ctx, `
INSERT INTO usdc_watcher_state (
	network, enabled, running, rpc_url_configured, receive_address_configured,
	last_scanned_block, latest_block, confirmed_block, confirmations,
	watch_interval_seconds, last_checked_at, last_success_at, last_error
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(network) DO UPDATE SET
	enabled = excluded.enabled,
	running = excluded.running,
	rpc_url_configured = excluded.rpc_url_configured,
	receive_address_configured = excluded.receive_address_configured,
	last_scanned_block = CASE WHEN excluded.last_scanned_block = 0 THEN usdc_watcher_state.last_scanned_block ELSE excluded.last_scanned_block END,
	latest_block = CASE WHEN excluded.latest_block = 0 THEN usdc_watcher_state.latest_block ELSE excluded.latest_block END,
	confirmed_block = CASE WHEN excluded.confirmed_block = 0 THEN usdc_watcher_state.confirmed_block ELSE excluded.confirmed_block END,
	confirmations = excluded.confirmations,
	watch_interval_seconds = excluded.watch_interval_seconds,
	last_checked_at = excluded.last_checked_at,
	last_success_at = CASE WHEN excluded.last_success_at = '' THEN usdc_watcher_state.last_success_at ELSE excluded.last_success_at END,
	last_error = excluded.last_error;
`, health.Network, boolInt(health.Enabled), boolInt(health.Running), boolInt(health.RPCURLConfigured),
		boolInt(health.ReceiveAddressConfigured), health.LastScannedBlock, health.LatestBlock,
		health.ConfirmedBlock, health.Confirmations, health.WatchIntervalSeconds,
		health.LastCheckedAt, health.LastSuccessAt, health.LastError)
	return err
}

func (s *Store) USDCWatcherHealth(ctx context.Context, network string) (USDCWatcherHealth, error) {
	if network == "" {
		network = "base"
	}
	var h USDCWatcherHealth
	var enabled, running, rpcConfigured, receiveConfigured int
	err := s.db.QueryRowContext(ctx, `
SELECT network, enabled, running, rpc_url_configured, receive_address_configured,
       last_scanned_block, latest_block, confirmed_block, confirmations,
       watch_interval_seconds, COALESCE(last_checked_at, ''), COALESCE(last_success_at, ''),
       COALESCE(last_error, '')
FROM usdc_watcher_state
WHERE network = ?;
`, network).Scan(&h.Network, &enabled, &running, &rpcConfigured, &receiveConfigured,
		&h.LastScannedBlock, &h.LatestBlock, &h.ConfirmedBlock, &h.Confirmations,
		&h.WatchIntervalSeconds, &h.LastCheckedAt, &h.LastSuccessAt, &h.LastError)
	if errors.Is(err, sql.ErrNoRows) {
		return USDCWatcherHealth{Network: network}, nil
	}
	if err != nil {
		return USDCWatcherHealth{}, err
	}
	h.Enabled = enabled != 0
	h.Running = running != 0
	h.RPCURLConfigured = rpcConfigured != 0
	h.ReceiveAddressConfigured = receiveConfigured != 0
	return h, nil
}

func (s *Store) RecentUSDCDetectedDeposits(ctx context.Context, limit int, duplicatesOnly bool) ([]USDCDetectedDeposit, error) {
	if limit <= 0 || limit > 200 {
		limit = 50
	}
	where := ""
	if duplicatesOnly {
		where = "WHERE duplicate_tx = 1"
	}
	rows, err := s.db.QueryContext(ctx, `
SELECT id, org_id, tx_hash, log_index, block_number, block_hash, network, token_address,
       from_address, to_address, amount_micros, confirmations, credited, duplicate_tx,
       status, detected_at, COALESCE(credited_at, ''), COALESCE(error, '')
FROM usdc_deposits
`+where+`
ORDER BY block_number DESC, log_index DESC, id DESC
LIMIT ?;
`, limit)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []USDCDetectedDeposit
	for rows.Next() {
		dep, err := scanUSDCDeposit(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, dep)
	}
	return out, rows.Err()
}

func (s *Store) USDCAdminDeposits(ctx context.Context, network string, limit int) (USDCAdminDeposits, error) {
	recent, err := s.RecentUSDCDetectedDeposits(ctx, limit, false)
	if err != nil {
		return USDCAdminDeposits{}, err
	}
	duplicates, err := s.RecentUSDCDetectedDeposits(ctx, limit, true)
	if err != nil {
		return USDCAdminDeposits{}, err
	}
	health, err := s.USDCWatcherHealth(ctx, network)
	if err != nil {
		return USDCAdminDeposits{}, err
	}
	return USDCAdminDeposits{RecentDetectedDeposits: recent, DuplicateTxs: duplicates, WatcherHealth: health}, nil
}

func RuntimeChargeMicros(seconds int64, features []string) int64 {
	if seconds <= 0 {
		return 0
	}
	rate := BaseSandboxMicrosPerHour
	for _, f := range features {
		if strings.EqualFold(f, "gui") {
			rate = GUISandboxMicrosPerHour
			break
		}
	}
	return (seconds*rate + 3599) / 3600
}

func (s *Store) init(ctx context.Context) error {
	for _, stmt := range []string{
		`PRAGMA journal_mode = WAL;`,
		`PRAGMA foreign_keys = ON;`,
		`PRAGMA busy_timeout = 5000;`,
		schemaSQL,
	} {
		if _, err := s.db.ExecContext(ctx, stmt); err != nil {
			return err
		}
	}
	return s.EnsureDefaultTenant(ctx)
}

func insertUsage(ctx context.Context, tx *sql.Tx, orgID, sandboxID, eventType string, quantitySeconds, amountMicros int64, occurredAt time.Time, metadata string) (UsageEvent, error) {
	res, err := tx.ExecContext(ctx, `
INSERT INTO usage_events (org_id, sandbox_id, event_type, quantity_seconds, amount_micros, occurred_at, metadata_json)
VALUES (?, ?, ?, ?, ?, ?, ?);
`, orgID, nullable(sandboxID), eventType, quantitySeconds, amountMicros, timeString(occurredAt), metadata)
	if err != nil {
		return UsageEvent{}, err
	}
	id, _ := res.LastInsertId()
	return UsageEvent{
		ID:              id,
		OrgID:           orgID,
		SandboxID:       sandboxID,
		EventType:       eventType,
		QuantitySeconds: quantitySeconds,
		AmountMicros:    amountMicros,
		OccurredAt:      timeString(occurredAt),
		MetadataJSON:    metadata,
	}, nil
}

func rollback(tx *sql.Tx) {
	if tx != nil {
		_ = tx.Rollback()
	}
}

func nullable(s string) any {
	if s == "" {
		return nil
	}
	return s
}

type usdcDepositScanner interface {
	Scan(dest ...any) error
}

func usdcDepositByLog(ctx context.Context, tx *sql.Tx, txHash string, logIndex uint64) (USDCDetectedDeposit, bool, error) {
	dep, err := scanUSDCDeposit(tx.QueryRowContext(ctx, `
SELECT id, org_id, tx_hash, log_index, block_number, block_hash, network, token_address,
       from_address, to_address, amount_micros, confirmations, credited, duplicate_tx,
       status, detected_at, COALESCE(credited_at, ''), COALESCE(error, '')
FROM usdc_deposits
WHERE tx_hash = ? AND log_index = ?;
`, txHash, logIndex))
	if errors.Is(err, sql.ErrNoRows) {
		return USDCDetectedDeposit{}, false, nil
	}
	if err != nil {
		return USDCDetectedDeposit{}, false, err
	}
	return dep, true, nil
}

func scanUSDCDeposit(scanner usdcDepositScanner) (USDCDetectedDeposit, error) {
	var dep USDCDetectedDeposit
	var credited, duplicate int
	if err := scanner.Scan(&dep.ID, &dep.OrgID, &dep.TxHash, &dep.LogIndex, &dep.BlockNumber,
		&dep.BlockHash, &dep.Network, &dep.TokenAddress, &dep.FromAddress, &dep.ToAddress,
		&dep.AmountMicros, &dep.Confirmations, &credited, &duplicate, &dep.Status,
		&dep.DetectedAt, &dep.CreditedAt, &dep.Error); err != nil {
		return USDCDetectedDeposit{}, err
	}
	dep.Credited = credited != 0
	dep.DuplicateTx = duplicate != 0
	return dep, nil
}

func boolInt(v bool) int {
	if v {
		return 1
	}
	return 0
}

func nowString() string {
	return timeString(time.Now())
}

func timeString(t time.Time) string {
	return t.UTC().Format(time.RFC3339)
}

func monthStart(t time.Time) time.Time {
	utc := t.UTC()
	return time.Date(utc.Year(), utc.Month(), 1, 0, 0, 0, 0, time.UTC)
}

const schemaSQL = `
CREATE TABLE IF NOT EXISTS organizations (
	id TEXT PRIMARY KEY,
	name TEXT NOT NULL,
	plan TEXT NOT NULL,
	monthly_credit_micros INTEGER NOT NULL DEFAULT 0,
	credit_balance_micros INTEGER NOT NULL DEFAULT 0,
	monthly_budget_micros INTEGER NOT NULL DEFAULT 0,
	max_concurrent_sandboxes INTEGER NOT NULL DEFAULT 1,
	max_runtime_seconds INTEGER NOT NULL DEFAULT 1800,
	max_idle_seconds INTEGER NOT NULL DEFAULT 600,
	created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS users (
	id TEXT PRIMARY KEY,
	org_id TEXT NOT NULL REFERENCES organizations(id),
	email TEXT NOT NULL,
	created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS sandboxes (
	id TEXT PRIMARY KEY,
	org_id TEXT NOT NULL REFERENCES organizations(id),
	owner TEXT NOT NULL,
	state TEXT NOT NULL,
	features_json TEXT NOT NULL DEFAULT '[]',
	cpu REAL NOT NULL DEFAULT 2,
	memory_mb INTEGER NOT NULL DEFAULT 1024,
	created_at TEXT NOT NULL,
	started_at TEXT,
	stopped_at TEXT,
	last_active_at TEXT,
	max_lifetime_s INTEGER NOT NULL DEFAULT 0
);

CREATE INDEX IF NOT EXISTS sandboxes_org_state_idx ON sandboxes(org_id, state);

CREATE TABLE IF NOT EXISTS usage_events (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	org_id TEXT NOT NULL REFERENCES organizations(id),
	sandbox_id TEXT REFERENCES sandboxes(id),
	event_type TEXT NOT NULL,
	quantity_seconds INTEGER NOT NULL DEFAULT 0,
	amount_micros INTEGER NOT NULL DEFAULT 0,
	occurred_at TEXT NOT NULL,
	metadata_json TEXT NOT NULL DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS usage_events_org_time_idx ON usage_events(org_id, occurred_at);

CREATE TABLE IF NOT EXISTS credit_ledger (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	org_id TEXT NOT NULL REFERENCES organizations(id),
	delta_micros INTEGER NOT NULL,
	reason TEXT NOT NULL,
	reference TEXT,
	created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS billing_events (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	provider TEXT NOT NULL,
	provider_event_id TEXT NOT NULL UNIQUE,
	event_type TEXT NOT NULL,
	status TEXT NOT NULL,
	amount_micros INTEGER NOT NULL DEFAULT 0,
	metadata_json TEXT NOT NULL DEFAULT '{}',
	created_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS usdc_deposits (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	org_id TEXT NOT NULL REFERENCES organizations(id),
	tx_hash TEXT NOT NULL,
	log_index INTEGER NOT NULL,
	block_number INTEGER NOT NULL,
	block_hash TEXT NOT NULL,
	network TEXT NOT NULL,
	token_address TEXT NOT NULL,
	from_address TEXT NOT NULL,
	to_address TEXT NOT NULL,
	amount_micros INTEGER NOT NULL,
	confirmations INTEGER NOT NULL DEFAULT 0,
	credited INTEGER NOT NULL DEFAULT 0,
	duplicate_tx INTEGER NOT NULL DEFAULT 0,
	status TEXT NOT NULL,
	detected_at TEXT NOT NULL,
	credited_at TEXT,
	error TEXT,
	UNIQUE(tx_hash, log_index)
);

CREATE INDEX IF NOT EXISTS usdc_deposits_detected_idx ON usdc_deposits(detected_at);
CREATE INDEX IF NOT EXISTS usdc_deposits_tx_idx ON usdc_deposits(tx_hash);

CREATE TABLE IF NOT EXISTS usdc_watcher_state (
	network TEXT PRIMARY KEY,
	enabled INTEGER NOT NULL DEFAULT 0,
	running INTEGER NOT NULL DEFAULT 0,
	rpc_url_configured INTEGER NOT NULL DEFAULT 0,
	receive_address_configured INTEGER NOT NULL DEFAULT 0,
	last_scanned_block INTEGER NOT NULL DEFAULT 0,
	latest_block INTEGER NOT NULL DEFAULT 0,
	confirmed_block INTEGER NOT NULL DEFAULT 0,
	confirmations INTEGER NOT NULL DEFAULT 0,
	watch_interval_seconds INTEGER NOT NULL DEFAULT 0,
	last_checked_at TEXT,
	last_success_at TEXT,
	last_error TEXT
);
`
