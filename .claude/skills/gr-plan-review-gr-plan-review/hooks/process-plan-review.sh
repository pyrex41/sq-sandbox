#!/bin/bash
#===============================================================================
# Claude Code Hook: GR Plan Review
#===============================================================================
# Automates Grok-powered review of Claude implementation plans.
# Works as:
#   1. PostToolUse hook (automatic on plan save)
#   2. Standalone script (callable from skill or CLI)
#
# Dependencies: repomix, gr, jq
#===============================================================================

set -euo pipefail

#-------------------------------------------------------------------------------
# Configuration
#-------------------------------------------------------------------------------
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
TEMP_DIR="/tmp"
CLEANUP_FILES=()

#-------------------------------------------------------------------------------
# Helper Functions
#-------------------------------------------------------------------------------

log_info() {
    echo "[gr-plan-review] $1" >&2
}

log_error() {
    echo "[gr-plan-review ERROR] $1" >&2
}

cleanup() {
    for file in "${CLEANUP_FILES[@]:-}"; do
        rm -f "$file" 2>/dev/null || true
    done
}

trap cleanup EXIT

#-------------------------------------------------------------------------------
# Parse Input
#-------------------------------------------------------------------------------

# Support both hook JSON input and direct CLI invocation
if [[ -n "${HOOK_INPUT:-}" ]]; then
    INPUT="$HOOK_INPUT"
elif [[ $# -gt 0 ]] && [[ -f "$1" ]]; then
    # Direct file path passed as argument
    FILE_PATH="$1"
    CWD="${2:-.}"
else
    # Read from stdin (Claude Code hook format)
    if [[ ! -t 0 ]]; then
        INPUT=$(cat)
    fi
fi

# Parse Claude Code hook JSON if present
if [[ -n "${INPUT:-}" ]]; then
    FILE_PATH=$(echo "$INPUT" | jq -r '.file_path // ""')
    CWD=$(echo "$INPUT" | jq -r '.cwd // "."')
fi

# Default CWD to current directory
CWD="${CWD:-.}"

cd "$CWD" || {
    log_error "Cannot change to directory: $CWD"
    echo "{}"
    exit 0
}

#-------------------------------------------------------------------------------
# Filter: Only Process Plan Files
#-------------------------------------------------------------------------------

# Only act on plan Markdown files
if [[ -z "$FILE_PATH" ]] || [[ ! "$FILE_PATH" =~ \.md$ ]] || [[ ! "$FILE_PATH" =~ (plan|PLAN) ]]; then
    echo "{}"
    exit 0
fi

# Resolve absolute path if needed
if [[ ! -f "$FILE_PATH" ]]; then
    FILE_PATH="$CWD/$FILE_PATH"
fi

PLAN_PATH="$FILE_PATH"
PLAN_NAME=$(basename "$PLAN_PATH" .md)
REVIEW_PATH="${PLAN_PATH%.md}-review.md"

log_info "Starting Grok review for plan: $PLAN_NAME"

#-------------------------------------------------------------------------------
# Step 1: Repomix Compaction
#-------------------------------------------------------------------------------

# Find Repomix config - prefer project-specific, fall back to default
REPOMIX_CONFIG=".repomix-review.config.json"
if [[ ! -f "$REPOMIX_CONFIG" ]]; then
    REPOMIX_CONFIG=".repomix.config.json"
fi

if [[ ! -f "$REPOMIX_CONFIG" ]]; then
    log_error "No Repomix config found. Create .repomix-review.config.json in project root."
    echo '{"systemMessage": "Error: No Repomix config found. See skill documentation."}'
    exit 0
fi

# Run Repomix with compression
REPOMIX_OUTPUT="/tmp/gr-packed-${TIMESTAMP}.xml"
CLEANUP_FILES+=("$REPOMIX_OUTPUT")

log_info "Running Repomix with config: $REPOMIX_CONFIG"

if ! repomix --config "$REPOMIX_CONFIG" -o "$REPOMIX_OUTPUT" --style xml --compress 2>/dev/null; then
    log_error "Repomix failed. Ensure repomix is installed and in PATH."
    echo '{"systemMessage": "Error: Repomix failed. Check installation."}'
    exit 0
fi

PACKED_CONTEXT=$(cat "$REPOMIX_OUTPUT")
PLAN_CONTENT=$(cat "$PLAN_PATH")

#-------------------------------------------------------------------------------
# Step 2: Build Review Prompt
#-------------------------------------------------------------------------------

PROMPT_FILE="/tmp/gr-review-prompt-${TIMESTAMP}.md"
CLEANUP_FILES+=("$PROMPT_FILE")

# Optional: Include project-specific review instructions
INSTRUCTIONS=""
if [[ -f ".claude/plan-review-instructions.md" ]]; then
    INSTRUCTIONS=$(cat ".claude/plan-review-instructions.md")
fi

# Build the prompt for Grok
cat > "$PROMPT_FILE" << 'EOPROMPT'
You are an expert software architect reviewing a Claude-generated implementation plan.

## Project Context (Repomix XML - compacted codebase):
__PACKED_CONTEXT__

## Claude Plan:
__PLAN_CONTENT__

__INSTRUCTIONS__

## Review Requirements

Provide a professional, constructive review with these exact sections:

1. **Overall Assessment** - Strengths, risk level, and confidence in the plan
2. **Critical Issues** - Direct feedback on security, architecture, or correctness problems
3. **Improvement Suggestions** - Specific recommendations for error handling, testing, maintainability
4. **Missing Steps or Dependencies** - What's not covered that should be
5. **Recommended Revised Plan** - Brief outline if material changes needed

Focus on practicality and real-world implementation concerns.
EOPROMPT

# Substitute placeholders
sed -i "s|__PACKED_CONTEXT__|$PACKED_CONTEXT|g" "$PROMPT_FILE"
sed -i "s|__PLAN_CONTENT__|$PLAN_CONTENT|g" "$PROMPT_FILE"

if [[ -n "$INSTRUCTIONS" ]]; then
    sed -i "s|__INSTRUCTIONS__|## Project-Specific Guidelines\n\n$INSTRUCTIONS|g" "$PROMPT_FILE"
else
    sed -i "s|__INSTRUCTIONS__||g" "$PROMPT_FILE"
fi

#-------------------------------------------------------------------------------
# Step 3: Invoke Grok via gr CLI
#-------------------------------------------------------------------------------

log_info "Sending to Grok for review..."

# Call gr CLI (reads from stdin, outputs markdown)
# Falls back to error message if gr fails
REVIEW=$(gr --format markdown < "$PROMPT_FILE" 2>/dev/null) || {
    log_error "Grok review call failed. Verify 'gr' is authenticated and in PATH."
    REVIEW="## Error

Grok review failed. Please verify:
- \`gr\` is installed and in your PATH
- You are authenticated: run \`gr auth login\`
- The gr CLI is functioning correctly"
}

#-------------------------------------------------------------------------------
# Step 4: Save Review
#-------------------------------------------------------------------------------

cat > "$REVIEW_PATH" << EOF
# Grok Review – $PLAN_NAME

**Generated:** $(date -u +"%Y-%m-%d %H:%M UTC")  
**Project:** $(basename "$CWD")  
**Config:** $REPOMIX_CONFIG

---

$REVIEW
EOF

log_info "Review saved to: $REVIEW_PATH"

#-------------------------------------------------------------------------------
# Step 5: Return Summary to Claude Code
#-------------------------------------------------------------------------------

# Extract first few lines for summary
SUMMARY=$(echo "$REVIEW" | head -c 1200)

# Output JSON for Claude Code async hook
cat << EOF
{
  "systemMessage": "✅ Grok plan review completed for \`$PLAN_NAME\`. Review saved to \`$(basename "$REVIEW_PATH")\`.",
  "additionalContext": "**Grok Review Summary**\n\n$SUMMARY..."
}
EOF

log_info "Done!"
