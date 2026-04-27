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
`
