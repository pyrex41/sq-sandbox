---
name: gr-plan-review
description: Send current plan + Repomix-packed project context to Grok for expert review via gr CLI. Works as both a manual skill invocation and automatic PostToolUse hook.
argument-hint: [optional-plan-path]
allowed-tools: Bash, Read, Write, Glob
category: code-review
tags: [grok, plan-review, repomix, gr, automation]
version: 1.0.0
author: Claude Skills Community
requires: [repomix, gr, jq]
---

# Grok Plan Review (gr-plan-review)

Reviews Claude-generated implementation plans using your `gr` CLI and a full compacted view of the codebase via Repomix. Supports both manual invocation and automatic hook triggers on plan file saves.

## Usage

### Manual Invocation

```
/gr-plan-review
/gr-plan-review .claude/plans/my-plan.md
```

If no plan path is provided, defaults to the most recent plan in `.claude/plans/`.

### What Happens

1. Runs `repomix` using the project's `.repomix-review.config.json` (or `.repomix.config.json` as fallback)
2. Combines the packed context + plan content into a structured prompt
3. Sends the prompt to `gr` (your Grok CLI) for expert review
4. Saves the full review as `<plan-name>-review.md` alongside the plan
5. Displays a confirmation and summary in the chat

## Requirements

- **repomix** - Must be installed and in `$PATH`. Install via: `npm install -g repomix`
- **gr** - Your Grok CLI must be installed and authenticated. Run `gr auth login` once.
- **jq** - Required for parsing hook JSON input

## Per-Project Configuration

### Step 1: Create Repomix Config

Create `.repomix-review.config.json` in your project root (customize `customPatterns` for your codebase):

```json
{
  "output": {
    "style": "xml",
    "filePath": ".gr-packed.xml",
    "removeComments": true,
    "removeEmptyLines": true
  },
  "include": ["**/*"],
  "ignore": {
    "defaultPatterns": true,
    "customPatterns": [
      ".git/**",
      ".claude/**",
      "node_modules/**",
      "build/**",
      "dist/**",
      "target/**",
      "bin/**",
      "*.log",
      "coverage/**",
      "docs/**",
      "README.md",
      "LICENSE",
      "**/*_test.go",
      "**/*.test.*",
      "go.sum",
      "**/vendor/**",
      "**/*.min.*"
    ]
  },
  "compress": true
}
```

### Step 2: (Optional) Create Review Instructions

Create `.claude/plan-review-instructions.md` for project-specific review guidelines:

```markdown
You are reviewing Claude-generated implementation plans for a [project type].
Emphasize: [your priorities like security, error handling, testing, etc.]
```

### Step 3: Enable Automatic Hook (Optional)

Add to your project's `.claude/settings.json`:

```json
{
  "plansDirectory": "./.claude/plans",
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/process-plan-review.sh",
            "async": true,
            "timeout": 240
          }
        ]
      }
    ]
  }
}
```

## Hook Behavior

The hook script (`process-plan-review.sh`) automatically triggers when you write or edit files matching `*plan*.md`. It:

- Only acts on Markdown files containing "plan" in the path
- Runs Repomix to compact the project context
- Sends to Grok via `gr` CLI
- Saves review to `<plan>-review.md`
- Returns a summary to Claude Code via async hook JSON

The hook works independently of the skill and can be used standalone.

## Installation via skm

```bash
skm install gr-plan-review
```

After installation, the skill is available at `/gr-plan-review`.

## Troubleshooting

- **"gr: command not found"** - Ensure `gr` is in your `$PATH` and authenticated
- **"repomix: command not found"** - Install via `npm install -g repomix`
- **Hook not firing** - Verify the hook path in `.claude/settings.json` and reload with `/hooks`
- **Review too slow** - Reduce `customPatterns` in your Repomix config to exclude more files

## Files Included

- `hooks/process-plan-review.sh` - Hook script for automatic plan review
- `templates/repomix-review.config.json` - Per-project Repomix configuration template
- `templates/plan-review-instructions.md` - Per-project review instructions template
