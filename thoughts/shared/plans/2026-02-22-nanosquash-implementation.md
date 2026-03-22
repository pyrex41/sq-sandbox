# nanosquash Implementation Plan

## Overview

Build **nanosquash** — a single deployable product combining sq-sandbox's squashfs layer system with nullclaw's AI agent runtime. nullclaw runs *inside* sandboxes as a baked-in squashfs module. A Zig CLI (`nanosquash`) provides agent-focused commands wrapping sq-sandbox's HTTP API. Preset module combinations offer ready-to-use agent configurations including Claude Code dev containers.

## Current State Analysis

**sq-sandbox** (host): Working squashfs module system, HTTP API, chroot+Firecracker backends, secret proxy for credential injection. Unprivileged sandbox plan in progress (squashfuse/fuse-overlayfs/bubblewrap).

**nullclaw** (agent): 678 KB Zig static binary. 22+ providers, 13+ channels, 18+ tools, SQLite FTS5 memory, daemon/gateway/cron/heartbeat. Config via `~/.nullclaw/config.json` + env overrides.

**Existing integration work**:
- `modules/nullclaw.nix` exists but is broken: hardcoded aarch64, incorrect SQLite build flags (build.zig uses Zig package manager, not system libs), `.squashfs` attribute access mismatch in `modules.nix:24`
- `nix/modules.nix` has `enableNullclaw` flag and conditional export
- `squash-claw/` directory is empty

### Key Discoveries:
- nullclaw's `build.zig:5` uses `standardTargetOptions` — cross-compilation works via `-Dtarget=x86_64-linux-musl` etc.
- SQLite is a Zig package dependency (`build.zig.zon`), compiled from C source by Zig — no system libsqlite needed (`build.zig:9-14`)
- nullclaw's `RuntimeAdapter` vtable is purely descriptive (name, shell_access, fs_access, storage_path, etc.) — has no `execute` method (`runtime.zig:5-41`)
- Shell tool calls `process_util.run` directly, bypassing RuntimeAdapter entirely (`tools/shell.zig:93-98`)
- sq-sandbox injects env vars via `/etc/profile.d/squash-secrets.sh` in the upper layer (`common.sh:234-281`)
- sq-sandbox's secret proxy replaces placeholder env var values with real credentials on outbound HTTP/HTTPS (`shared/proxy/main.go`)
- nullclaw reads config from `$HOME/.nullclaw/config.json` and applies env overrides for `NULLCLAW_PROVIDER`, `NULLCLAW_MODEL`, `NULLCLAW_GATEWAY_PORT`, etc. (`config.zig:153-192`)
- Zig cross-compilation in Nix requires pre-fetched dependencies — nullclaw's `flake.nix` uses `zig2nix` with `build.zig.zon2json-lock` for this

## Desired End State

A user can run:
```sh
# Start the sandbox daemon
nanosquash start

# Spawn an agent sandbox with nullclaw + python + dev tools
nanosquash spawn --preset dev --provider anthropic --model claude-sonnet-4

# Chat with the agent
nanosquash chat my-sandbox "write a fibonacci function in python"

# Snapshot the agent state (memory, workspace, config)
nanosquash snapshot my-sandbox

# List running sandboxes
nanosquash list

# Destroy a sandbox
nanosquash destroy my-sandbox
```

Or using Claude Code inside a sandbox:
```sh
nanosquash spawn --preset claude-dev --layers 000-base-alpine,100-python312,100-nodejs22,200-claude-code
nanosquash exec my-sandbox "claude --help"
```

### Verification:
- `nanosquash spawn --preset agent` creates a sandbox with nullclaw running inside
- nullclaw inside the sandbox can reach AI providers via secret proxy
- `nanosquash snapshot` + `nanosquash spawn --restore` preserves agent memory
- `nanosquash chat` sends messages and returns responses
- `nix build .#nanosquash` produces a single static Zig binary
- `nix build .#module-nullclaw` produces a working squashfs module
- All preset module combinations build and work together

## What We're NOT Doing

- Modifying nullclaw's source code (no RuntimeAdapter changes — nullclaw runs natively inside the sandbox)
- Implementing multi-sandbox orchestration (nullclaw-outside-sandbox pattern)
- Network isolation (per unprivileged sandbox plan — secret proxy is the boundary)
- Firecracker-specific integration (works but not the focus)
- WhatsApp/Telegram channel integration (channels work inside sandbox but not wired to host)
- odin-claw or nanoclaw integration
- GPU passthrough

## Implementation Approach

1. Fix the nullclaw squashfs module so it actually builds
2. Create the nanosquash Zig CLI that wraps sq-sandbox's HTTP API
3. Add agent-focused sandbox presets with config injection
4. Add Claude Code module for dev containers
5. Create the nanosquash Nix flake composing everything
6. Wire snapshot/restore for agent state persistence

---

## Phase 1: Fix nullclaw Squashfs Module

### Overview
Make `modules/nullclaw.nix` produce a working squashfs module containing the nullclaw static binary for the host architecture.

### Changes Required:

#### 1. Add nullclaw flake as input to sq-sandbox flake
**File**: `flake.nix`

Add nullclaw's flake as an input so we get a properly-built binary without reimplementing the Zig build in our module:

```nix
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  flake-utils.url = "github:numtide/flake-utils";
  nullclaw.url = "github:nullclaw/nullclaw";
};
```

Pass nullclaw packages through to modules:

```nix
modules = pkgs.lib.optionalAttrs isLinux
  (import ./nix/modules.nix {
    inherit pkgs;
    enableNullclaw = true;
    nullclaw-pkg = nullclaw.packages.${system}.default or null;
  });
```

#### 2. Rewrite `modules/nullclaw.nix`
**File**: `modules/nullclaw.nix`

Replace the broken cross-compilation with the pre-built package:

```nix
{ pkgs, lib, nullclaw-pkg ? null }:
if nullclaw-pkg == null then { squashfs = null; } else
{
  squashfs = lib.mkSquashfsModule {
    name = "200-nullclaw";
    buildScript = ''
      mkdir -p "$rootfs/usr/local/bin"
      cp ${nullclaw-pkg}/bin/nullclaw "$rootfs/usr/local/bin/"

      # Default config template
      mkdir -p "$rootfs/etc/nullclaw"
      cat > "$rootfs/etc/nullclaw/config.json.template" <<'EOF'
      {
        "default_provider": "anthropic",
        "default_model": "claude-sonnet-4",
        "default_temperature": 0.7,
        "memory": {
          "backend": "sqlite",
          "auto_save": true
        },
        "autonomy": {
          "level": "full",
          "workspace_only": true,
          "max_actions_per_hour": 50
        },
        "tools": {
          "shell_timeout_secs": 120,
          "shell_max_output_bytes": 2097152
        }
      }
      EOF

      # Startup script sourced by agent presets
      cat > "$rootfs/usr/local/bin/nullclaw-init" <<'INIT'
      #!/bin/sh
      # Initialize nullclaw config from template if not present
      NCDIR="${HOME:-/root}/.nullclaw"
      mkdir -p "$NCDIR"
      if [ ! -f "$NCDIR/config.json" ]; then
        cp /etc/nullclaw/config.json.template "$NCDIR/config.json"
      fi
      INIT
      chmod +x "$rootfs/usr/local/bin/nullclaw-init"
    '';
  };
}
```

#### 3. Update `nix/modules.nix` to pass nullclaw-pkg
**File**: `nix/modules.nix`

```nix
{
  pkgs,
  enableNullclaw ? true,
  nullclaw-pkg ? null,
}:
let
  lib = import ./lib-squashfs.nix { inherit pkgs; };
  callModule = path: import path { inherit pkgs lib; };
  callNullclaw = import ../modules/nullclaw.nix { inherit pkgs lib nullclaw-pkg; };

  python = callModule ../modules/python.nix;
  nodejs = callModule ../modules/nodejs.nix;
  sqlite-libs = callModule ../modules/sqlite-libs.nix;
  nullclaw = if enableNullclaw then callNullclaw else { squashfs = null; };
  module-sqlite-libs = sqlite-libs.squashfs;
in
{
  module-base-alpine = callModule ../modules/base-alpine.nix;
  module-python312 = python.python312;
  module-nodejs22 = nodejs.nodejs22;
  module-golang = callModule ../modules/golang.nix;
  module-tailscale = callModule ../modules/tailscale.nix;
  inherit module-sqlite-libs;
}
// (if enableNullclaw && nullclaw.squashfs != null
    then { module-nullclaw = nullclaw.squashfs; }
    else { })
```

### Success Criteria:

#### Automated:
- [ ] `nix build .#module-nullclaw` produces a `.squashfs` file
- [ ] The squashfs contains `/usr/local/bin/nullclaw` (verify with `unsquashfs -l`)
- [ ] `nix flake check` passes
- [ ] `nullclaw --version` works when the module is activated in a sandbox

#### Manual:
- [ ] Create a sandbox with base-alpine + nullclaw modules, run `nullclaw --version` inside

**Implementation Note**: Pause here for manual verification before Phase 2.

---

## Phase 2: nanosquash Zig CLI

### Overview
Create a Zig CLI binary at `squash-claw/` that wraps sq-sandbox's HTTP API with agent-focused commands.

### Changes Required:

#### 1. Initialize Zig project
**Directory**: `squash-claw/`

```
squash-claw/
├── build.zig
├── build.zig.zon
├── src/
│   ├── main.zig          # CLI entry + command routing
│   ├── client.zig        # sq-sandbox HTTP API client
│   ├── presets.zig        # Preset configurations (layer lists + config)
│   └── config.zig        # nanosquash config (daemon URL, defaults)
```

#### 2. `src/main.zig` — CLI commands

```
nanosquash <command> [options]

Commands:
  start               Start sq-sandbox daemon (delegates to squashd)
  spawn [--preset P]  Create sandbox with agent modules
  destroy <id>        Destroy a sandbox
  list                List sandboxes
  exec <id> <cmd>     Execute command in sandbox
  chat <id> <msg>     Send message to agent, return response
  snapshot <id>       Snapshot sandbox state
  restore <id> <snap> Restore from snapshot
  status              Daemon health + sandbox summary
  version             Print version
```

#### 3. `src/client.zig` — HTTP API client

Thin wrapper around `std.http.Client` calling sq-sandbox endpoints:

| Method | Endpoint | nanosquash command |
|--------|----------|-------------------|
| `POST /api/sandboxes` | `spawn` |
| `GET /api/sandboxes` | `list` |
| `GET /api/sandboxes/:id` | `status <id>` |
| `DELETE /api/sandboxes/:id` | `destroy` |
| `POST /api/sandboxes/:id/exec` | `exec`, `chat` |
| `POST /api/sandboxes/:id/activate` | (used internally by spawn) |
| `POST /api/sandboxes/:id/snapshot` | `snapshot` |
| `POST /api/sandboxes/:id/restore` | `restore` |

#### 4. `src/presets.zig` — Agent presets

Each preset defines layers + config + init commands:

```zig
pub const Preset = struct {
    name: []const u8,
    description: []const u8,
    layers: []const []const u8,
    nullclaw_config: ?[]const u8,  // JSON config override
    init_commands: []const []const u8,  // Run after sandbox create
};

pub const presets = [_]Preset{
    .{
        .name = "agent",
        .description = "Minimal nullclaw agent",
        .layers = &.{ "000-base-alpine", "200-nullclaw" },
        .nullclaw_config = null,
        .init_commands = &.{ "nullclaw-init" },
    },
    .{
        .name = "dev",
        .description = "Full dev agent with Python, Node.js, and nullclaw",
        .layers = &.{ "000-base-alpine", "100-python312", "100-nodejs22", "200-nullclaw" },
        .nullclaw_config = null,
        .init_commands = &.{ "nullclaw-init" },
    },
    .{
        .name = "claude-dev",
        .description = "Claude Code dev container with Python and Node.js",
        .layers = &.{ "000-base-alpine", "100-python312", "100-nodejs22", "200-claude-code" },
        .nullclaw_config = null,
        .init_commands = &.{ "claude-code-init" },
    },
    .{
        .name = "claude-agent",
        .description = "Claude Code + nullclaw combined",
        .layers = &.{ "000-base-alpine", "100-python312", "100-nodejs22", "200-nullclaw", "200-claude-code" },
        .nullclaw_config = null,
        .init_commands = &.{ "nullclaw-init", "claude-code-init" },
    },
};
```

#### 5. `chat` command implementation

The `chat` command wraps exec to invoke nullclaw's CLI agent mode:

```zig
// chat sends a message to the nullclaw agent inside the sandbox
fn chat(client: *Client, sandbox_id: []const u8, message: []const u8) ![]const u8 {
    // Escape message for shell
    const escaped = try shellEscape(message);
    // Run nullclaw agent with the message
    const cmd = try std.fmt.allocPrint(
        allocator,
        "nullclaw agent -m {s}",
        .{escaped},
    );
    return client.exec(sandbox_id, cmd, "/workspace", 300);
}
```

#### 6. `build.zig`

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const version = b.option([]const u8, "version", "Version string") orelse "0.1.0";

    const build_options = b.addOptions();
    build_options.addOption([]const u8, "version", version);

    const exe = b.addExecutable(.{
        .name = "nanosquash",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.root_module.addOptions("build_options", build_options);

    if (optimize != .Debug) {
        exe.root_module.strip = true;
        exe.root_module.unwind_tables = .none;
        exe.root_module.omit_frame_pointer = true;
    }

    b.installArtifact(exe);

    const run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    if (b.args) |a| run.addArgs(a);
    b.step("run", "Run nanosquash").dependOn(&run.step);
}
```

### Success Criteria:

#### Automated:
- [ ] `cd squash-claw && zig build` produces `nanosquash` binary
- [ ] `nanosquash version` prints version
- [ ] `nanosquash list` returns sandbox list (when daemon running)
- [ ] `nanosquash spawn --preset agent` creates a sandbox with correct layers
- [ ] `nanosquash chat <id> "hello"` returns a response from nullclaw
- [ ] `nanosquash destroy <id>` cleans up

#### Manual:
- [ ] Full workflow: start → spawn → chat → snapshot → destroy → spawn --restore → chat (memory preserved)

**Implementation Note**: Pause here for manual testing before Phase 3.

---

## Phase 3: Agent Config Injection

### Overview
Wire up config injection so nullclaw inside the sandbox can reach AI providers via the secret proxy and has sensible defaults.

### Changes Required:

#### 1. Secret proxy configuration for AI providers

Add provider API keys to `$SQUASH_DATA/secrets.json`:

```json
{
  "secrets": [
    {
      "name": "anthropic_api_key",
      "placeholder": "ANTHROPIC_PLACEHOLDER_KEY",
      "actual": "sk-ant-api03-...",
      "allowed_hosts": ["api.anthropic.com"]
    },
    {
      "name": "openai_api_key",
      "placeholder": "OPENAI_PLACEHOLDER_KEY",
      "actual": "sk-...",
      "allowed_hosts": ["api.openai.com"]
    }
  ]
}
```

The secret proxy intercepts outbound HTTPS to `api.anthropic.com`, replaces the placeholder `Authorization: Bearer ANTHROPIC_PLACEHOLDER_KEY` with the real key. nullclaw inside the sandbox only ever sees the placeholder.

#### 2. Config injection in nanosquash `spawn`

When `nanosquash spawn` creates a sandbox, it:

1. Creates the sandbox via sq-sandbox API with the requested layers
2. Writes nullclaw config to the sandbox's upper layer via exec:

```sh
# Injected via sq-sandbox exec after sandbox creation
mkdir -p /root/.nullclaw
cat > /root/.nullclaw/config.json <<'EOF'
{
  "default_provider": "anthropic",
  "default_model": "claude-sonnet-4",
  "default_temperature": 0.7,
  "memory": { "backend": "sqlite", "auto_save": true },
  "autonomy": { "level": "full", "workspace_only": true },
  "gateway": { "port": 3000, "host": "0.0.0.0" }
}
EOF
```

3. Sets env vars via profile script:

```sh
cat > /etc/profile.d/nanosquash.sh <<'EOF'
export NULLCLAW_API_KEY=ANTHROPIC_PLACEHOLDER_KEY
export NULLCLAW_PROVIDER=anthropic
export NULLCLAW_WORKSPACE=/workspace
EOF
```

#### 3. nanosquash spawn `--provider` and `--model` flags

```
nanosquash spawn --preset dev --provider anthropic --model claude-sonnet-4
nanosquash spawn --preset dev --provider openrouter --model anthropic/claude-sonnet-4
```

These flags customize the injected config.json and env vars.

### Success Criteria:

#### Automated:
- [ ] `nanosquash spawn --preset agent --provider anthropic` creates a sandbox where `nullclaw agent -m "hello"` returns a response
- [ ] Secret proxy replaces placeholder key with real key on outbound requests
- [ ] `nullclaw` inside sandbox sees only placeholder values, never real API keys
- [ ] Config file is written to sandbox upper layer

#### Manual:
- [ ] Run a multi-turn conversation via `nanosquash chat` and verify context is maintained
- [ ] Verify secret proxy logs show placeholder replacement

---

## Phase 4: Claude Code Module

### Overview
Create a `200-claude-code` squashfs module containing Claude Code (the Anthropic CLI), allowing Claude Code to run inside sandboxes as an alternative or complement to nullclaw.

### Changes Required:

#### 1. `modules/claude-code.nix`
**File**: `modules/claude-code.nix`

Claude Code ships as an npm package (`@anthropic-ai/claude-code`). The module needs Node.js in the sandbox (via `100-nodejs22` layer).

```nix
{ pkgs, lib }:
{
  squashfs = lib.mkSquashfsModule {
    name = "200-claude-code";
    buildScript = ''
      mkdir -p "$rootfs/usr/local/lib/claude-code" "$rootfs/usr/local/bin"

      # Install Claude Code via npm into the module
      export HOME=$TMPDIR
      export npm_config_cache=$TMPDIR/npm-cache
      ${pkgs.nodejs_22}/bin/npm install -g @anthropic-ai/claude-code \
        --prefix "$rootfs/usr/local"

      # Init script for Claude Code in sandbox
      cat > "$rootfs/usr/local/bin/claude-code-init" <<'INIT'
      #!/bin/sh
      # Set up Claude Code workspace
      mkdir -p /workspace
      cd /workspace
      # Write CLAUDE.md if not present
      if [ ! -f CLAUDE.md ]; then
        echo "# Sandbox Agent" > CLAUDE.md
        echo "You are running inside a nanosquash sandbox." >> CLAUDE.md
      fi
      INIT
      chmod +x "$rootfs/usr/local/bin/claude-code-init"
    '';
  };
}
```

#### 2. Register in `nix/modules.nix`

Add alongside existing modules:

```nix
claude-code = callModule ../modules/claude-code.nix;
```

And expose:

```nix
module-claude-code = claude-code.squashfs;
```

#### 3. Secret proxy entry for Claude Code

Claude Code uses either `ANTHROPIC_API_KEY` or `CLAUDE_CODE_OAUTH_TOKEN`. Add to secrets.json:

```json
{
  "name": "claude_code_api_key",
  "placeholder": "CLAUDE_PLACEHOLDER_KEY",
  "actual": "sk-ant-api03-...",
  "allowed_hosts": ["api.anthropic.com", "claude.ai"]
}
```

#### 4. Claude Code presets in nanosquash

The `claude-dev` preset from Phase 2 creates a sandbox with:
- `000-base-alpine` + `100-python312` + `100-nodejs22` + `200-claude-code`
- Injects `ANTHROPIC_API_KEY=CLAUDE_PLACEHOLDER_KEY` via profile script
- Runs `claude-code-init` on create

The `claude-agent` preset stacks both nullclaw and Claude Code:
- `000-base-alpine` + `100-python312` + `100-nodejs22` + `200-nullclaw` + `200-claude-code`

### Success Criteria:

#### Automated:
- [ ] `nix build .#module-claude-code` produces a squashfs module
- [ ] Module contains `/usr/local/bin/claude` binary
- [ ] `nanosquash spawn --preset claude-dev` creates sandbox where `claude --version` works
- [ ] `nanosquash exec <id> "claude -m 'hello'"` returns a response

#### Manual:
- [ ] Interactive Claude Code session inside sandbox: `nanosquash exec <id> "claude"`
- [ ] Claude Code can read/write files in `/workspace` inside the sandbox
- [ ] Combined preset (claude-agent) has both `nullclaw` and `claude` available

**Implementation Note**: Pause here for manual testing before Phase 5.

---

## Phase 5: nanosquash Nix Flake

### Overview
Create `squash-claw/flake.nix` that composes everything into a single deployable unit. One `nix run` gets you the nanosquash CLI + sq-sandbox daemon + all modules.

### Changes Required:

#### 1. `squash-claw/flake.nix`

```nix
{
  description = "nanosquash — sandboxed AI agents from squashfs layers";

  inputs = {
    sq-sandbox.url = "path:../sq-sandbox";  # or github:pyrex41/sq-sandbox
    nixpkgs.follows = "sq-sandbox/nixpkgs";
    flake-utils.follows = "sq-sandbox/flake-utils";
  };

  outputs = { self, sq-sandbox, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        sq = sq-sandbox.packages.${system};
      in {
        packages = {
          # The nanosquash CLI binary
          nanosquash = pkgs.stdenv.mkDerivation {
            pname = "nanosquash";
            version = "0.1.0";
            src = ./.;
            nativeBuildInputs = [ pkgs.zig ];
            buildPhase = ''
              export XDG_CACHE_HOME=$TMPDIR/.cache
              zig build -Doptimize=ReleaseSmall --prefix $out
            '';
            dontInstall = true;
          };

          # Convenience: all modules bundled
          inherit (sq) module-base-alpine module-python312 module-nodejs22
                       module-golang module-tailscale module-nullclaw
                       module-claude-code;

          # Convenience: daemon
          inherit (sq) squashd-shell squashd-rust squashd-zig;

          default = self.packages.${system}.nanosquash;
        };

        # Dev shell with nanosquash + daemon + modules available
        devShells.default = pkgs.mkShell {
          packages = [
            self.packages.${system}.nanosquash
            sq.squashd-shell
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.squashfuse pkgs.fuse-overlayfs pkgs.bubblewrap
          ];
        };
      }
    );
}
```

#### 2. Module preloading script

A helper that preloads all modules for a preset into `$SQUASH_DATA/modules/`:

```sh
#!/bin/sh
# nanosquash-preload — copy squashfs modules to SQUASH_DATA
SQUASH_DATA="${SQUASH_DATA:-/var/lib/sq-sandbox}"
mkdir -p "$SQUASH_DATA/modules"

for mod in "$@"; do
  if [ -f "$mod" ]; then
    name=$(basename "$mod")
    cp -n "$mod" "$SQUASH_DATA/modules/$name"
  fi
done
```

The Nix flake provides an `apps.preload` that copies modules built by Nix into the data directory.

### Success Criteria:

#### Automated:
- [ ] `cd squash-claw && nix build .#nanosquash` produces the CLI binary
- [ ] `nix build .#module-nullclaw` works (inherited from sq-sandbox)
- [ ] `nix develop` drops into a shell with `nanosquash` and `squashd` available
- [ ] `nix flake check` passes

#### Manual:
- [ ] Full end-to-end: `nix develop`, start squashd, preload modules, `nanosquash spawn --preset dev`, `nanosquash chat <id> "hello"`

---

## Phase 6: Snapshot-as-Agent-State

### Overview
Wire snapshot/restore so it captures and recreates the full agent environment: nullclaw's SQLite memory, workspace files, config overrides, conversation history.

### Changes Required:

#### 1. Agent state lives in the writable upper layer

nullclaw's state is all in the filesystem (writable upper layer):
- `~/.nullclaw/config.json` — agent configuration
- `~/.nullclaw/memory.db` — SQLite FTS5 memory database
- `~/.nullclaw/sessions/` — conversation history
- `~/.nullclaw/audit.log` — audit trail
- `/workspace/` — agent workspace (code, files)
- `/etc/profile.d/nanosquash.sh` — injected env vars

sq-sandbox's snapshot already captures the entire upper layer as a squashfs file. Restore mounts it as the topmost read-only layer, with a fresh writable layer on top. This means **all agent state is automatically preserved**.

#### 2. `nanosquash snapshot` enhancements

Add metadata to snapshots:

```
nanosquash snapshot <id> [--label <name>]
```

The CLI calls sq-sandbox's snapshot API and stores a metadata sidecar:

```json
{
  "label": "after-fibonacci",
  "agent": "nullclaw",
  "provider": "anthropic",
  "model": "claude-sonnet-4",
  "created": "2026-02-22T17:00:00Z",
  "sandbox_id": "my-sandbox",
  "layers": ["000-base-alpine", "100-python312", "200-nullclaw"]
}
```

#### 3. `nanosquash spawn --restore`

```
nanosquash spawn --restore <snapshot-label-or-path> [--id <new-id>]
```

Creates a new sandbox using the snapshot as the base state:
1. Create sandbox with the same layers as the original
2. Restore the snapshot (sq-sandbox API)
3. The agent resumes with all memory, workspace, and config intact

#### 4. S3-backed snapshots

When `SQUASH_S3_BUCKET` is set, snapshots are automatically synced to S3 via sq-sandbox's existing `sq-s3` mechanism. This enables:
- Snapshot on one machine, restore on another
- Ephemeral mode: auto-snapshot on destroy, auto-restore on create

### Success Criteria:

#### Automated:
- [ ] `nanosquash snapshot <id>` creates a squashfs snapshot
- [ ] `nanosquash spawn --restore <snapshot>` creates a sandbox with the agent's previous state
- [ ] Memory is preserved: agent recalls information from before the snapshot
- [ ] Workspace files are preserved after restore

#### Manual:
- [ ] Full cycle: spawn → chat (teach agent something) → snapshot → destroy → spawn --restore → chat (agent remembers) → verify

---

## Testing Strategy

### Unit Tests (Zig):
- `client.zig`: Mock HTTP responses, test all API wrappers
- `presets.zig`: Verify preset definitions, layer lists, config generation
- `config.zig`: Config parsing, defaults, env overrides

### Integration Tests:
- Requires sq-sandbox daemon running
- Full lifecycle: spawn → exec → activate → snapshot → restore → destroy
- Secret proxy credential injection
- Multiple concurrent sandboxes
- Preset validation (all presets create working sandboxes)

### Manual Testing:
1. Fresh `nix develop` on bare Linux VM
2. Start daemon, preload modules
3. `nanosquash spawn --preset agent` + multi-turn conversation
4. `nanosquash spawn --preset claude-dev` + Claude Code interactive session
5. Snapshot/restore cycle with memory verification
6. Concurrent sandboxes with different presets

## Performance Considerations

- nanosquash CLI is a thin HTTP client — sub-millisecond overhead per command
- Sandbox spawn time dominated by squashfuse mounts (~50-200ms per layer)
- nullclaw inside sandbox: <2ms startup, 678 KB binary, ~1 MB RSS
- Claude Code inside sandbox: Node.js startup adds ~200-500ms
- Snapshot creation: mksquashfs of upper layer, proportional to written data
- All squashfs modules are read-only and shared across sandboxes (deduplication)

## Migration Notes

- Existing sq-sandbox users: no breaking changes — nanosquash is additive
- The nullclaw module fix (Phase 1) changes `modules/nullclaw.nix` API — but it was broken before
- `modules.nix` gains a `nullclaw-pkg` parameter — defaults to null for backwards compatibility

## References

- Research: `thoughts/shared/research/2026-02-22-squashclaw-synthesis-background.md`
- Unprivileged plan: `thoughts/shared/plans/2026-02-19-unprivileged-sandbox.md`
- Nix monorepo plan: `thoughts/shared/plans/2026-02-16-nix-monorepo-declarative-builds.md`
- nullclaw build: `/Users/reuben/projects/claw/nullclaw/build.zig`
- sq-sandbox modules: `/Users/reuben/projects/claw/sq-sandbox/nix/modules.nix`
- sq-sandbox API: `/Users/reuben/projects/claw/sq-sandbox/shared/cgi-bin/common.sh`
- Secret proxy: `/Users/reuben/projects/claw/sq-sandbox/shared/proxy/main.go`
