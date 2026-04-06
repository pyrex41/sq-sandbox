package runner

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"
)

// AgentAdapter runs a bounded batch of agent turns inside the sandbox.
type AgentAdapter interface {
	Name() string
	RunBatch(ctx context.Context, req BatchRequest) (*BatchResult, error)
}

type BatchRequest struct {
	Workdir   string
	Plan      string // path to plan file inside sandbox
	MaxTurns  int
	SessionID string // for --resume (Claude)
	EnvVars   map[string]string
}

type BatchResult struct {
	ExitCode  int
	TurnsUsed int
	SessionID string
	CostUsd   float64
	Succeeded bool
	Events    []Event
}

// ClaudeAdapter runs `claude -p` with --max-turns and --resume.
type ClaudeAdapter struct {
	exec   Executor
	events *EventLog
}

func NewClaudeAdapter(exec Executor, events *EventLog) *ClaudeAdapter {
	return &ClaudeAdapter{exec: exec, events: events}
}

func (a *ClaudeAdapter) Name() string { return "claude-code" }

func (a *ClaudeAdapter) RunBatch(ctx context.Context, req BatchRequest) (*BatchResult, error) {
	// Build the shell command with env exports + claude invocation
	envExports := buildEnvExports(req.EnvVars)

	var args string
	if req.SessionID != "" {
		args = fmt.Sprintf(`claude -p --resume %s --output-format stream-json --verbose --max-turns %d --allowedTools \"Bash,Read,Write,Edit,Glob,Grep,Agent\"`,
			req.SessionID, req.MaxTurns)
	} else {
		// Pipe plan as prompt via stdin; allowedTools with escaped quotes for sh -c
		args = fmt.Sprintf(`cat %s | claude -p --output-format stream-json --verbose --max-turns %d --allowedTools \"Bash,Read,Write,Edit,Glob,Grep,Agent\"`,
			req.Plan, req.MaxTurns)
	}

	cmd := fmt.Sprintf("cd %s && %s%s", req.Workdir, envExports, args)

	// Run as background job so we can stream output
	_, lines, done, cancel, err := a.exec.ExecBg(cmd, "/", 7200)
	if err != nil {
		return nil, fmt.Errorf("exec claude: %w", err)
	}

	// Cancel on context done
	go func() {
		<-ctx.Done()
		cancel()
	}()

	result := &BatchResult{}

	// Process lines
	for line := range lines {
		a.parseClaudeLine(line, result)
	}

	// Wait for exit code
	exitCode := <-done
	result.ExitCode = exitCode

	// Claude exits 1 even on success when a tool returned non-zero
	if result.Succeeded && result.ExitCode == 1 {
		result.ExitCode = 0
	}

	return result, nil
}

func (a *ClaudeAdapter) parseClaudeLine(line string, result *BatchResult) {
	var raw map[string]any
	if err := json.Unmarshal([]byte(line), &raw); err != nil {
		return
	}

	typ, _ := raw["type"].(string)
	switch typ {
	case "system":
		sub, _ := raw["subtype"].(string)
		if sub == "init" {
			sid, _ := raw["session_id"].(string)
			result.SessionID = sid
			a.events.Emit("session_start", map[string]any{"session_id": sid})
		}

	case "rate_limit_event":
		info, _ := raw["rate_limit_info"].(map[string]any)
		if info != nil {
			status, _ := info["status"].(string)
			limitType, _ := info["rateLimitType"].(string)
			a.events.Emit("rate_limited", map[string]any{
				"status": status, "type": limitType,
			})
		}

	case "stream_event":
		ev, _ := raw["event"].(map[string]any)
		if ev == nil {
			return
		}
		evType, _ := ev["type"].(string)
		if evType == "content_block_start" {
			block, _ := ev["content_block"].(map[string]any)
			if block != nil && block["type"] == "tool_use" {
				name, _ := block["name"].(string)
				a.events.Emit("tool_call", map[string]any{"tool": name})
			}
		}

	case "assistant":
		result.TurnsUsed++
		a.events.Emit("turn_end", map[string]any{"turn": result.TurnsUsed})

	case "user":
		msg, _ := raw["message"].(map[string]any)
		if msg != nil {
			content, _ := msg["content"].([]any)
			for _, c := range content {
				block, _ := c.(map[string]any)
				if block != nil && block["type"] == "tool_result" {
					success := true
					if isErr, ok := block["is_error"].(bool); ok && isErr {
						success = false
					}
					a.events.Emit("tool_result", map[string]any{"success": success})
				}
			}
		}

	case "result":
		sub, _ := raw["subtype"].(string)
		cost, _ := raw["total_cost_usd"].(float64)
		isError, _ := raw["is_error"].(bool)
		result.CostUsd = cost
		if sub == "success" && !isError && cost > 0 {
			result.Succeeded = true
		}
		// Rate-limited / out of credits: cost=0, is_error=true
		if cost == 0 && isError {
			a.events.Emit("error", map[string]any{
				"message": "Claude returned with zero cost — likely rate-limited or out of credits",
			})
		}
		a.events.Emit("batch_done", map[string]any{
			"subtype": sub, "cost_usd": cost, "turns": result.TurnsUsed, "is_error": isError,
		})
	}
}

// RhoAdapter runs `rho-cli loop` with --max-iterations.
type RhoAdapter struct {
	exec   Executor
	events *EventLog
	model  string
}

func NewRhoAdapter(exec Executor, events *EventLog, model string) *RhoAdapter {
	return &RhoAdapter{exec: exec, events: events, model: model}
}

func (a *RhoAdapter) Name() string { return "rho" }

func (a *RhoAdapter) RunBatch(ctx context.Context, req BatchRequest) (*BatchResult, error) {
	envExports := buildEnvExports(req.EnvVars)

	cmd := fmt.Sprintf("%srho-cli --tools read,write,edit,bash,glob,grep loop --mode build --model %s --output-format stream-json -C %s --plan %s --max-iterations %d 2>/dev/null",
		envExports, a.model, req.Workdir, req.Plan, req.MaxTurns)

	_, lines, done, cancel, err := a.exec.ExecBg(cmd, "/", 7200)
	if err != nil {
		return nil, fmt.Errorf("exec rho: %w", err)
	}

	go func() {
		<-ctx.Done()
		cancel()
	}()

	result := &BatchResult{}

	for line := range lines {
		a.parseRhoLine(line, result)
	}

	exitCode := <-done
	result.ExitCode = exitCode

	return result, nil
}

func (a *RhoAdapter) parseRhoLine(line string, result *BatchResult) {
	var raw map[string]any
	if err := json.Unmarshal([]byte(line), &raw); err != nil {
		return
	}

	typ, _ := raw["type"].(string)
	switch typ {
	case "iteration_start":
		iter, _ := raw["iteration"].(float64)
		a.events.Emit("turn_start", map[string]any{"turn": int(iter)})
	case "tool_start":
		name, _ := raw["tool_name"].(string)
		summary, _ := raw["input_summary"].(string)
		a.events.Emit("tool_call", map[string]any{"tool": name, "input": summary})
	case "tool_result":
		success, _ := raw["success"].(bool)
		a.events.Emit("tool_result", map[string]any{"success": success})
	case "iteration_end":
		iter, _ := raw["iteration"].(float64)
		result.TurnsUsed = int(iter)
		a.events.Emit("turn_end", map[string]any{"turn": int(iter)})
	case "loop_done":
		result.Succeeded = true
	}
}

func buildEnvExports(vars map[string]string) string {
	if len(vars) == 0 {
		return ""
	}
	var parts []string
	for k, v := range vars {
		parts = append(parts, fmt.Sprintf("export %s=%q", k, v))
	}
	return strings.Join(parts, "; ") + "; "
}

// IsRetryable checks if an error from an agent batch is worth retrying.
func IsRetryable(err error) bool {
	if err == nil {
		return false
	}
	msg := err.Error()
	return strings.Contains(msg, "SSE stream error") ||
		strings.Contains(msg, "rate_limit") ||
		strings.Contains(msg, "connection reset")
}
