\* sq-sandbox MVP admission policy.
   The Go host passes a normalized snapshot to shen-go; this file keeps the
   quota/budget vocabulary in Shen so the policy can move out of Go cleanly. *\

(define deny
  Code Reason -> [allowed false code Code reason Reason])

(define allow
  -> [allowed true])

(define admit-create
  Active MaxConcurrent Credits MonthlyUsage MonthlyBudget Estimate
  -> (deny "concurrency_limit" "concurrent sandbox limit reached")
     where (and (> MaxConcurrent 0) (>= Active MaxConcurrent))
  Active MaxConcurrent Credits MonthlyUsage MonthlyBudget Estimate
  -> (deny "credits_exhausted" "credit balance exhausted")
     where (<= Credits 0)
  Active MaxConcurrent Credits MonthlyUsage MonthlyBudget Estimate
  -> (deny "monthly_budget_exhausted" "monthly budget exhausted")
     where (and (> MonthlyBudget 0) (>= MonthlyUsage MonthlyBudget))
  Active MaxConcurrent Credits MonthlyUsage MonthlyBudget Estimate
  -> (deny "insufficient_credits" "credit balance is below estimated session cost")
     where (and (> Estimate 0) (< Credits Estimate))
  Active MaxConcurrent Credits MonthlyUsage MonthlyBudget Estimate
  -> (deny "monthly_budget_would_exceed" "estimated session would exceed monthly budget")
     where (and (> MonthlyBudget 0) (> (+ MonthlyUsage Estimate) MonthlyBudget))
  _ _ _ _ _ _ -> (allow))
