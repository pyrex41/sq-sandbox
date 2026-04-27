package api

import (
	"encoding/json"
	"fmt"
	"html"
	"io"
	"net/http"
	"strings"

	"squashd/controlplane"
)

func (h *Handler) handleControlPlaneSummary(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	summary, err := h.cp.Store().OrgSummary(r.Context(), orgIDFromRequest(r))
	if err != nil {
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, summary)
}

func (h *Handler) handleControlPlaneUsage(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	usage, err := h.cp.Store().RecentUsage(r.Context(), orgIDFromRequest(r), 100)
	if err != nil {
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, usage)
}

func (h *Handler) handleBillingCheckout(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	var body struct {
		OrgID string `json:"org_id"`
	}
	if r.ContentLength > 0 {
		if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
			jsonError(w, "invalid json: "+err.Error(), http.StatusBadRequest)
			return
		}
	}
	if body.OrgID == "" {
		body.OrgID = controlplane.DefaultOrgID
	}
	// MVP stub: keep Stripe integration reversible until live keys/products are
	// configured. The shape matches what the frontend needs to redirect later.
	jsonOK(w, map[string]any{
		"mode":     "stripe_checkout_stub",
		"org_id":   body.OrgID,
		"price_id": h.cfg.StripeCheckoutPriceID,
		"url":      strings.TrimRight(h.cfg.PublicBaseURL, "/") + "/app?billing=stub",
	})
}

func (h *Handler) handleBillingWebhook(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	b, err := io.ReadAll(r.Body)
	if err != nil {
		jsonError(w, "read webhook: "+err.Error(), http.StatusBadRequest)
		return
	}
	var payload map[string]any
	_ = json.Unmarshal(b, &payload)
	eventID, _ := payload["id"].(string)
	if eventID == "" {
		eventID = "stub-" + fmt.Sprint(len(b))
	}
	eventType, _ := payload["type"].(string)
	if eventType == "" {
		eventType = "checkout.session.completed"
	}
	if err := h.cp.Store().RecordBillingEvent(r.Context(), "stripe", eventID, eventType, "received", 0, string(b)); err != nil {
		jsonError(w, err.Error(), http.StatusInternalServerError)
		return
	}
	jsonOK(w, map[string]any{"received": true})
}

func (h *Handler) handleDashboard(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	dash, err := h.cp.Store().Dashboard(r.Context(), orgIDFromRequest(r))
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	fmt.Fprintf(w, `<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>sq-sandbox dashboard</title>
  <style>
    body { font-family: system-ui, sans-serif; margin: 2rem; color: #111; }
    .cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 1rem; margin: 1rem 0; }
    .card { border: 1px solid #ddd; border-radius: 10px; padding: 1rem; background: #fafafa; }
    table { border-collapse: collapse; width: 100%%; margin-top: 1rem; }
    th, td { border-bottom: 1px solid #eee; padding: .55rem; text-align: left; }
    .muted { color: #666; }
    code { background: #f1f1f1; padding: .15rem .3rem; border-radius: 4px; }
  </style>
</head>
<body>
  <h1>sq-sandbox</h1>
  <p class="muted">Hosted sandbox control plane MVP. Billing is stubbed until Stripe products are configured.</p>
  <div class="cards">
    <div class="card"><strong>Credits</strong><br>%s</div>
    <div class="card"><strong>Monthly usage</strong><br>%s / %s</div>
    <div class="card"><strong>Active sandboxes</strong><br>%d / %d</div>
    <div class="card"><strong>Plan</strong><br>%s</div>
  </div>
  <h2>Recent sandboxes</h2>
  <table><thead><tr><th>ID</th><th>State</th><th>Owner</th><th>Features</th><th>Created</th></tr></thead><tbody>`,
		formatMicros(dash.Org.CreditBalanceMicros),
		formatMicros(dash.Org.MonthlyUsageMicros),
		formatMicros(dash.Org.MonthlyBudgetMicros),
		dash.Org.ActiveSandboxes,
		dash.Org.MaxConcurrentSandboxes,
		html.EscapeString(dash.Org.Plan),
	)
	for _, sb := range dash.RecentSandboxes {
		fmt.Fprintf(w, `<tr><td><code>%s</code></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>`,
			html.EscapeString(sb.ID),
			html.EscapeString(sb.State),
			html.EscapeString(sb.Owner),
			html.EscapeString(strings.Join(sb.Features, ",")),
			html.EscapeString(sb.CreatedAt),
		)
	}
	fmt.Fprint(w, `</tbody></table>
  <h2>Recent usage</h2>
  <table><thead><tr><th>When</th><th>Event</th><th>Sandbox</th><th>Runtime</th><th>Amount</th></tr></thead><tbody>`)
	for _, ev := range dash.RecentUsage {
		fmt.Fprintf(w, `<tr><td>%s</td><td>%s</td><td><code>%s</code></td><td>%ds</td><td>%s</td></tr>`,
			html.EscapeString(ev.OccurredAt),
			html.EscapeString(ev.EventType),
			html.EscapeString(ev.SandboxID),
			ev.QuantitySeconds,
			formatMicros(ev.AmountMicros),
		)
	}
	fmt.Fprint(w, `</tbody></table>
</body>
</html>`)
}

func (h *Handler) requireControlPlane(w http.ResponseWriter) bool {
	if h.cp == nil || h.cp.Store() == nil {
		jsonError(w, "control plane is not configured", http.StatusServiceUnavailable)
		return false
	}
	return true
}

func orgIDFromRequest(r *http.Request) string {
	if orgID := r.URL.Query().Get("org_id"); orgID != "" {
		return orgID
	}
	return controlplane.DefaultOrgID
}

func admissionStatus(code string) int {
	switch code {
	case "concurrency_limit":
		return http.StatusTooManyRequests
	case "credits_exhausted", "insufficient_credits", "monthly_budget_exhausted", "monthly_budget_would_exceed":
		return http.StatusPaymentRequired
	default:
		return http.StatusForbidden
	}
}

func formatMicros(v int64) string {
	sign := ""
	if v < 0 {
		sign = "-"
		v = -v
	}
	return fmt.Sprintf("%s$%d.%06d", sign, v/1_000_000, v%1_000_000)
}
