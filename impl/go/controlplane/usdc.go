package controlplane

import (
	"context"
	"fmt"
	"strconv"
	"strings"
)

type USDCDeposit struct {
	OrgID        string `json:"org_id"`
	TxHash       string `json:"tx_hash"`
	AmountMicros int64  `json:"amount_micros"`
	Network      string `json:"network"`
	TokenAddress string `json:"token_address,omitempty"`
	FromAddress  string `json:"from_address,omitempty"`
	ToAddress    string `json:"to_address,omitempty"`
}

type USDCDepositResult struct {
	Credited            bool  `json:"credited"`
	CreditBalanceMicros int64 `json:"credit_balance_micros"`
}

func (s *Store) ConfirmUSDCDeposit(ctx context.Context, dep USDCDeposit) (USDCDepositResult, error) {
	if dep.OrgID == "" {
		dep.OrgID = DefaultOrgID
	}
	dep.TxHash = strings.TrimSpace(dep.TxHash)
	if dep.TxHash == "" {
		return USDCDepositResult{}, fmt.Errorf("tx_hash is required")
	}
	if dep.AmountMicros <= 0 {
		return USDCDepositResult{}, fmt.Errorf("amount_micros must be positive")
	}
	if dep.Network == "" {
		dep.Network = "base"
	}
	metadata := fmt.Sprintf(`{"network":%q,"token_address":%q,"from_address":%q,"to_address":%q}`,
		dep.Network, dep.TokenAddress, dep.FromAddress, dep.ToAddress)

	tx, err := s.db.BeginTx(ctx, nil)
	if err != nil {
		return USDCDepositResult{}, err
	}
	defer rollback(tx)

	res, err := tx.ExecContext(ctx, `
INSERT INTO billing_events (provider, provider_event_id, event_type, status, amount_micros, metadata_json, created_at)
VALUES (?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(provider_event_id) DO NOTHING;
`, "usdc", dep.TxHash, "usdc.deposit_confirmed", "confirmed", dep.AmountMicros, metadata, nowString())
	if err != nil {
		return USDCDepositResult{}, err
	}
	inserted, _ := res.RowsAffected()
	if inserted == 0 {
		var balance int64
		if err := tx.QueryRowContext(ctx, `SELECT credit_balance_micros FROM organizations WHERE id = ?;`, dep.OrgID).Scan(&balance); err != nil {
			return USDCDepositResult{}, err
		}
		return USDCDepositResult{Credited: false, CreditBalanceMicros: balance}, tx.Commit()
	}

	if _, err = tx.ExecContext(ctx, `UPDATE organizations SET credit_balance_micros = credit_balance_micros + ? WHERE id = ?;`, dep.AmountMicros, dep.OrgID); err != nil {
		return USDCDepositResult{}, err
	}
	if _, err = tx.ExecContext(ctx, `
INSERT INTO credit_ledger (org_id, delta_micros, reason, reference, created_at)
VALUES (?, ?, ?, ?, ?);
`, dep.OrgID, dep.AmountMicros, "usdc_deposit", dep.TxHash, nowString()); err != nil {
		return USDCDepositResult{}, err
	}
	var balance int64
	if err := tx.QueryRowContext(ctx, `SELECT credit_balance_micros FROM organizations WHERE id = ?;`, dep.OrgID).Scan(&balance); err != nil {
		return USDCDepositResult{}, err
	}
	return USDCDepositResult{Credited: true, CreditBalanceMicros: balance}, tx.Commit()
}

func ParseUSDCMicros(raw string) (int64, error) {
	s := strings.TrimSpace(raw)
	s = strings.TrimPrefix(s, "$")
	if s == "" {
		return 0, fmt.Errorf("amount is required")
	}
	if strings.HasPrefix(s, "-") || strings.HasPrefix(s, "+") {
		return 0, fmt.Errorf("amount must be positive")
	}
	parts := strings.Split(s, ".")
	if len(parts) > 2 {
		return 0, fmt.Errorf("invalid amount")
	}
	whole := parts[0]
	if whole == "" {
		whole = "0"
	}
	if !digitsOnly(whole) {
		return 0, fmt.Errorf("invalid amount")
	}
	fraction := ""
	if len(parts) == 2 {
		fraction = parts[1]
		if !digitsOnly(fraction) {
			return 0, fmt.Errorf("invalid amount")
		}
		if len(fraction) > 6 {
			return 0, fmt.Errorf("USDC supports at most 6 decimal places")
		}
	}
	for len(fraction) < 6 {
		fraction += "0"
	}
	wholeMicros, err := strconv.ParseInt(whole, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("invalid amount: %w", err)
	}
	fracMicros, err := strconv.ParseInt(fraction, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("invalid amount: %w", err)
	}
	out := wholeMicros*1_000_000 + fracMicros
	if out <= 0 {
		return 0, fmt.Errorf("amount must be positive")
	}
	return out, nil
}

func digitsOnly(s string) bool {
	if s == "" {
		return true
	}
	for _, r := range s {
		if r < '0' || r > '9' {
			return false
		}
	}
	return true
}
