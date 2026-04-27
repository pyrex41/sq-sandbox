package controlplane

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
)

type ControlPlane struct {
	store  *Store
	policy AdmissionPolicy
}

type AdmissionRequest struct {
	OrgID               string   `json:"org_id"`
	SandboxID           string   `json:"sandbox_id"`
	Features            []string `json:"features,omitempty"`
	MaxLifetimeSeconds  int      `json:"max_lifetime_seconds"`
	EstimatedRunSeconds int      `json:"estimated_run_seconds"`
}

type AdmissionDecision struct {
	Allowed bool   `json:"allowed"`
	Reason  string `json:"reason,omitempty"`
	Code    string `json:"code,omitempty"`
}

type AdmissionPolicy interface {
	Admit(ctx context.Context, summary OrgSummary, req AdmissionRequest) (AdmissionDecision, error)
}

type GoBackpressurePolicy struct{}

type ShenBackpressurePolicy struct {
	Command    string
	PolicyPath string
	Fallback   AdmissionPolicy
}

func New(store *Store, shenCommand, policyPath string) *ControlPlane {
	return &ControlPlane{
		store: store,
		policy: ShenBackpressurePolicy{
			Command:    shenCommand,
			PolicyPath: policyPath,
			Fallback:   GoBackpressurePolicy{},
		},
	}
}

func (c *ControlPlane) Store() *Store {
	if c == nil {
		return nil
	}
	return c.store
}

func (c *ControlPlane) AdmitCreate(ctx context.Context, req AdmissionRequest) (AdmissionDecision, error) {
	if c == nil || c.store == nil {
		return AdmissionDecision{Allowed: true}, nil
	}
	if req.OrgID == "" {
		req.OrgID = DefaultOrgID
	}
	if req.EstimatedRunSeconds <= 0 {
		req.EstimatedRunSeconds = 3600
	}
	summary, err := c.store.OrgSummary(ctx, req.OrgID)
	if err != nil {
		return AdmissionDecision{}, err
	}
	return c.policy.Admit(ctx, summary, req)
}

func (GoBackpressurePolicy) Admit(_ context.Context, summary OrgSummary, req AdmissionRequest) (AdmissionDecision, error) {
	if summary.MaxConcurrentSandboxes > 0 && summary.ActiveSandboxes >= summary.MaxConcurrentSandboxes {
		return deny("concurrency_limit", "concurrent sandbox limit reached"), nil
	}
	if summary.CreditBalanceMicros <= 0 {
		return deny("credits_exhausted", "credit balance exhausted"), nil
	}
	if summary.MonthlyBudgetMicros > 0 && summary.MonthlyUsageMicros >= summary.MonthlyBudgetMicros {
		return deny("monthly_budget_exhausted", "monthly budget exhausted"), nil
	}
	estimate := RuntimeChargeMicros(int64(req.EstimatedRunSeconds), req.Features)
	if estimate > 0 && summary.CreditBalanceMicros < estimate {
		return deny("insufficient_credits", "credit balance is below estimated session cost"), nil
	}
	if summary.MonthlyBudgetMicros > 0 && summary.MonthlyUsageMicros+estimate > summary.MonthlyBudgetMicros {
		return deny("monthly_budget_would_exceed", "estimated session would exceed monthly budget"), nil
	}
	return AdmissionDecision{Allowed: true}, nil
}

func (p ShenBackpressurePolicy) Admit(ctx context.Context, summary OrgSummary, req AdmissionRequest) (AdmissionDecision, error) {
	if p.Command == "" {
		return p.fallback().Admit(ctx, summary, req)
	}
	payload := struct {
		PolicyPath string           `json:"policy_path,omitempty"`
		Summary    OrgSummary       `json:"summary"`
		Request    AdmissionRequest `json:"request"`
	}{
		PolicyPath: p.PolicyPath,
		Summary:    summary,
		Request:    req,
	}
	input, err := json.Marshal(payload)
	if err != nil {
		return AdmissionDecision{}, err
	}
	cmd := exec.CommandContext(ctx, p.Command)
	cmd.Stdin = bytes.NewReader(input)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return AdmissionDecision{}, fmt.Errorf("shen admission: %w: %s", err, stderr.String())
	}
	var decision AdmissionDecision
	if err := json.Unmarshal(stdout.Bytes(), &decision); err != nil {
		return AdmissionDecision{}, fmt.Errorf("shen admission returned invalid json: %w", err)
	}
	return decision, nil
}

func (p ShenBackpressurePolicy) fallback() AdmissionPolicy {
	if p.Fallback != nil {
		return p.Fallback
	}
	return GoBackpressurePolicy{}
}

func deny(code, reason string) AdmissionDecision {
	return AdmissionDecision{Allowed: false, Code: code, Reason: reason}
}
