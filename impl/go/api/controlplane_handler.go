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

func (h *Handler) handleUSDCInstructions(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	jsonOK(w, h.usdcInstructions(orgIDFromRequest(r)))
}

func (h *Handler) handleUSDCConfirm(w http.ResponseWriter, r *http.Request) {
	if !h.requireControlPlane(w) {
		return
	}
	var body struct {
		OrgID        string `json:"org_id"`
		TxHash       string `json:"tx_hash"`
		AmountUSDC   string `json:"amount_usdc"`
		AmountMicros int64  `json:"amount_micros"`
		Network      string `json:"network"`
		TokenAddress string `json:"token_address"`
		FromAddress  string `json:"from_address"`
		ToAddress    string `json:"to_address"`
	}
	if !decodeBody(w, r, &body) {
		return
	}
	if h.cfg.USDCReceiveAddress == "" {
		jsonError(w, "SQUASH_USDC_RECEIVE_ADDRESS is not configured", http.StatusServiceUnavailable)
		return
	}
	if body.OrgID == "" {
		body.OrgID = controlplane.DefaultOrgID
	}
	if body.Network == "" {
		body.Network = h.cfg.USDCNetwork
	}
	if body.TokenAddress == "" {
		body.TokenAddress = h.cfg.USDCTokenAddress
	}
	if body.ToAddress == "" {
		body.ToAddress = h.cfg.USDCReceiveAddress
	}
	if body.AmountMicros == 0 {
		amount, err := controlplane.ParseUSDCMicros(body.AmountUSDC)
		if err != nil {
			jsonError(w, err.Error(), http.StatusBadRequest)
			return
		}
		body.AmountMicros = amount
	}
	deposit := controlplane.USDCDeposit{
		OrgID:        body.OrgID,
		TxHash:       body.TxHash,
		AmountMicros: body.AmountMicros,
		Network:      body.Network,
		TokenAddress: body.TokenAddress,
		FromAddress:  body.FromAddress,
		ToAddress:    body.ToAddress,
	}
	if err := controlplane.ValidateManualUSDCDeposit(deposit, h.cfg.USDCNetwork, h.cfg.USDCTokenAddress, h.cfg.USDCReceiveAddress); err != nil {
		jsonError(w, err.Error(), http.StatusBadRequest)
		return
	}
	result, err := h.cp.Store().ConfirmUSDCDeposit(r.Context(), deposit)
	if err != nil {
		jsonError(w, err.Error(), http.StatusBadRequest)
		return
	}
	jsonOK(w, result)
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
    input { display: block; box-sizing: border-box; margin: .35rem 0 .75rem; padding: .5rem; width: 100%%; max-width: 44rem; }
    button { padding: .55rem .8rem; cursor: pointer; }
    .muted { color: #666; }
    code { background: #f1f1f1; padding: .15rem .3rem; border-radius: 4px; }
    #usdc-confirm-result { margin-top: .75rem; }
  </style>
</head>
<body>
  <h1>sq-sandbox</h1>
  <p class="muted">Hosted sandbox control plane MVP. USDC top-ups are manually confirmed for beta users.</p>
  <div class="cards">
    <div class="card"><strong>Credits</strong><br>%s</div>
    <div class="card"><strong>Monthly usage</strong><br>%s / %s</div>
    <div class="card"><strong>Active sandboxes</strong><br>%d / %d</div>
    <div class="card"><strong>Plan</strong><br>%s</div>
  </div>
  %s
  %s
  <h2>Recent sandboxes</h2>
  <table><thead><tr><th>ID</th><th>State</th><th>Owner</th><th>Features</th><th>Created</th></tr></thead><tbody>`,
		formatMicros(dash.Org.CreditBalanceMicros),
		formatMicros(dash.Org.MonthlyUsageMicros),
		formatMicros(dash.Org.MonthlyBudgetMicros),
		dash.Org.ActiveSandboxes,
		dash.Org.MaxConcurrentSandboxes,
		html.EscapeString(dash.Org.Plan),
		h.usdcTopUpHTML(),
		h.usdcAdminHTML(),
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

func (h *Handler) usdcInstructions(orgID string) map[string]any {
	if orgID == "" {
		orgID = controlplane.DefaultOrgID
	}
	return map[string]any{
		"mode":                 "manual_usdc_topup",
		"org_id":               orgID,
		"configured":           h.cfg.USDCReceiveAddress != "",
		"network":              h.cfg.USDCNetwork,
		"chain_id":             h.cfg.USDCChainID,
		"token":                "USDC",
		"token_address":        h.cfg.USDCTokenAddress,
		"receive_address":      h.cfg.USDCReceiveAddress,
		"explorer_tx_base_url": h.cfg.USDCExplorerTxBaseURL,
		"confirm_endpoint":     "/cgi-bin/api/billing/usdc/confirm",
		"note":                 "Send native USDC on the configured network, then submit the tx hash for manual admin confirmation.",
	}
}

func (h *Handler) usdcTopUpHTML() string {
	if h.cfg.USDCReceiveAddress == "" {
		return `<div class="card"><strong>USDC top-up</strong><br><span class="muted">Set <code>SQUASH_USDC_RECEIVE_ADDRESS</code> to enable manual deposits.</span></div>`
	}
	return fmt.Sprintf(`<div class="card">
    <strong>USDC top-up</strong><br>
    <span class="muted">Send native USDC on %s (chain %s), then submit the tx hash for manual credit.</span><br>
    <code>%s</code><br>
    <span class="muted">Token: <code>%s</code></span>
  </div>`,
		html.EscapeString(h.cfg.USDCNetwork),
		html.EscapeString(h.cfg.USDCChainID),
		html.EscapeString(h.cfg.USDCReceiveAddress),
		html.EscapeString(h.cfg.USDCTokenAddress),
	)
}

func (h *Handler) usdcAdminHTML() string {
	if h.cfg.USDCReceiveAddress == "" {
		return ""
	}
	return fmt.Sprintf(`<h2>Admin: confirm USDC deposit</h2>
  <div class="card">
    <p class="muted">Verify the transfer on the explorer first. This records credits idempotently by tx hash.</p>
    <label>Transaction hash
      <input id="usdc-tx-hash" placeholder="0x..." autocomplete="off">
    </label>
    <label>Amount USDC
      <input id="usdc-amount" placeholder="25.00" inputmode="decimal">
    </label>
    <label>Sender address (optional)
      <input id="usdc-from-address" placeholder="0x..." autocomplete="off">
    </label>
    <button type="button" onclick="confirmUSDCDeposit()">Confirm deposit</button>
    <div id="usdc-confirm-result" class="muted"></div>
  </div>
  <script>
  async function confirmUSDCDeposit() {
    const out = document.getElementById('usdc-confirm-result');
    out.textContent = 'Confirming...';
    const payload = {
      tx_hash: document.getElementById('usdc-tx-hash').value.trim(),
      amount_usdc: document.getElementById('usdc-amount').value.trim(),
      network: %q,
      token_address: %q,
      to_address: %q,
      from_address: document.getElementById('usdc-from-address').value.trim()
    };
    try {
      const res = await fetch('/cgi-bin/api/billing/usdc/confirm', {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(payload)
      });
      const body = await res.json();
      if (!res.ok) throw new Error(body.error || 'confirmation failed');
      out.textContent = body.credited ? 'Credited. New balance: ' + formatMicros(body.credit_balance_micros) : 'Already confirmed. Balance: ' + formatMicros(body.credit_balance_micros);
    } catch (err) {
      out.textContent = err.message;
    }
  }
  function formatMicros(v) {
    const sign = v < 0 ? '-' : '';
    v = Math.abs(v);
    return sign + '$' + Math.floor(v / 1000000) + '.' + String(v %% 1000000).padStart(6, '0');
  }
  </script>`,
		h.cfg.USDCNetwork,
		h.cfg.USDCTokenAddress,
		h.cfg.USDCReceiveAddress,
	)
}

func formatMicros(v int64) string {
	sign := ""
	if v < 0 {
		sign = "-"
		v = -v
	}
	return fmt.Sprintf("%s$%d.%06d", sign, v/1_000_000, v%1_000_000)
}
