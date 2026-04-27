# gr-plan-review

Send Claude implementation plans to Grok for expert review, powered by Repomix for full codebase context.

## Overview

This skill automates the review of Claude-generated implementation plans using Grok's CLI (`gr`). It packages your entire codebase using Repomix, combines it with the plan, and sends everything to Grok for a comprehensive architectural review.

## Features

- **Manual Review**: Invoke anytime with `/gr-plan-review`
- **Automatic Hook**: Triggers on plan file saves (PostToolUse)
- **Full Context**: Repomix compaction includes entire codebase
- **Per-Project Customization**: Different ignore patterns and review guidelines per repo

## Quick Start

### 1. Install the Skill

```bash
skm install gr-plan-review
```

### 2. Configure Per-Project

Copy the templates to your project root and customize:

```bash
# Repomix config (controls what gets packed)
cp path/to/skills/gr-plan-review/templates/repomix-review.config.json ./

# Optional: Review guidelines
cp path/to/skills/gr-plan-review/templates/plan-review-instructions.md ./.claude/
```

### 3. (Optional) Enable Automatic Hook

Add to `.claude/settings.json`:

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "path/to/skills/gr-plan-review/hooks/process-plan-review.sh",
            "async": true,
            "timeout": 240
          }
        ]
      }
    ]
  }
}
```

### 4. Install Dependencies

```bash
# Repomix for codebase compaction
npm install -g repomix

# gr CLI (Grok client) - follow installation instructions
# https://github.com/pyrex41/gr

# jq for JSON parsing (usually pre-installed)
brew install jq  # macOS
apt install jq   # Linux
```

### 5. Authenticate

```bash
gr auth login
```

## Usage

### Manual

```bash
/gr-plan-review                          # Review latest plan
/gr-plan-review .claude/plans/my-plan.md # Review specific plan
```

### Automatic

Create or edit any `*plan*.md` file — the hook automatically triggers a Grok review.

## Output

Reviews are saved alongside the plan:

```
.claude/plans/
├── my-plan.md          # Original plan
└── my-plan-review.md   # Grok's review
```

## Requirements

| Tool | Purpose | Install |
|------|---------|---------|
| `repomix` | Compact codebase to XML | `npm install -g repomix` |
| `gr` | Grok CLI client | See gr documentation |
| `jq` | Parse JSON (hook input) | Package manager |

## File Structure

```
gr-plan-review/
├── SKILL.md                                    # Main skill definition
├── README.md                                   # This file
├── hooks/
│   └── process-plan-review.sh                  # Hook script (async)
└── templates/
    ├── repomix-review.config.json              # Repomix config template
    └── plan-review-instructions.md             # Review guidelines template
```

## Customization

### Repomix Config

Edit `.repomix-review.config.json` to control what Repomix includes:

- `customPatterns`: Add project-specific ignores
- `include`: File patterns to include
- `output.style`: Use `xml` for Grok (best for long context)

### Review Instructions

Edit `.claude/plan-review-instructions.md` to add project-specific guidelines for Grok (tone, priorities, concerns).

## Troubleshooting

| Issue | Solution |
|-------|----------|
| `gr: command not found` | Ensure `gr` is in PATH, run `gr auth login` |
| `repomix: command not found` | `npm install -g repomix` |
| Hook not firing | Verify path in `settings.json`, run `/hooks` to reload |
| Review too slow | Reduce `customPatterns` in Repomix config |

## License

MIT - Claude Skills Community
