---
date: 2026-02-22T03:15:35Z
researcher: reuben
git_commit: ec49f9a
branch: feat/squash-nullclaw-integration
repository: sq-sandbox
topic: "SquashClaw Synthesis: Background Research Across sq-sandbox, nullclaw, odin-claw, and nanoclaw"
tags: [research, codebase, squashclaw, sq-sandbox, nullclaw, odin-claw, nanoclaw, squashfs, firecracker, agent-runtime, synthesis]
status: complete
last_updated: 2026-02-22
last_updated_by: reuben
---

# Research: SquashClaw Synthesis Background

**Date**: 2026-02-22T03:15:35Z
**Researcher**: reuben
**Git Commit**: ec49f9a
**Branch**: feat/squash-nullclaw-integration
**Repository**: sq-sandbox

## Research Question

Document the architecture, capabilities, and integration surfaces of four codebases — sq-sandbox, nullclaw, odin-claw, and nanoclaw — to inform the synthesis of "SquashClaw": a universal, squashfs-based sandbox fabric hosting fully swappable, ultra-lean AI agents.

## Summary

This document provides comprehensive technical documentation of four local codebases and their web-documented capabilities. sq-sandbox provides a composable sandbox runtime built on stacked squashfs layers with overlayfs, dual backends (chroot + Firecracker microVM), and an HTTP API. nullclaw is a 678 KB Zig static binary providing a fully autonomous AI agent runtime with 22+ providers, 13+ channels, 18+ tools, and vtable-based pluggable architecture. odin-claw is an Odin reimplementation of nullclaw's vtable design with 5 providers, 3 channels, 10 tools, and LMDB memory. nanoclaw is a ~500-line TypeScript orchestrator built on Claude's Agent SDK with container isolation per WhatsApp group. The `squash-claw/` directory is currently empty, but sq-sandbox already contains a `feat/squash-nullclaw-integration` branch and a nullclaw module placeholder in `nix/modules.nix`.

---

## Detailed Findings

### 1. sq-sandbox — Composable Sandbox Runtime

**Location**: `/Users/reuben/projects/claw/sq-sandbox/`
**Languages**: Shell (core), Rust, Zig, Odin, Common Lisp, Janet, Go (proxy)
**Branch**: `feat/squash-nullclaw-integration`

#### Architecture

Single daemon process serving an HTTP API that manages sandbox lifecycle. All five daemon implementations shell out to three shared helpers rather than making direct syscalls:

| Helper | File | Purpose |
|--------|------|---------|
| `sq-mount-layer` | `shared/bin/sq-mount-layer` | Mount/unmount squashfs via squashfuse or kernel loop mount |
| `sq-mount-overlay` | `shared/bin/sq-mount-overlay` | Mount/unmount overlay via fuse-overlayfs or kernel overlayfs |
| `sq-exec` | `shared/bin/sq-exec` | Execute via bubblewrap or unshare+chroot fallback |

**Startup sequence** (`shared/bin/sq-start:1-64`): optional secret proxy → optional Tailscale → `sq-init` (first-boot) → `sq-reaper` (background) → daemon.

#### SquashFS Module System

Modules are `.squashfs` files stored in `$SQUASH_DATA/modules/`. Naming convention (`shared/cgi-bin/common.sh:131-134`):

| Prefix | Tier | Examples |
|--------|------|---------|
| `000` | Base rootfs | `000-base-alpine`, `000-base-debian` |
| `10x` | Language runtimes | `100-python312`, `100-nodejs22`, `100-golang` |
| `11x` | Build tools | `110-build-tools` |
| `20x` | Services | `200-tailscale`, `200-nullclaw` |
| `090` | Shared libs | `090-sqlite-libs` |

**Two build paths**:
- Shell: `shared/bin/sq-mkmod` — supports `preset <name>`, `from-dir <path>`, `from-sandbox <id>`
- Nix: `nix/lib-squashfs.nix` — `mkSquashfsModule` with zstd-15, 128K blocks, `-all-root`, reproducible timestamps

**Nix module exports** (`nix/modules.nix`): `module-base-alpine` (Alpine 3.21.3), `module-python312`, `module-nodejs22`, `module-golang`, `module-tailscale`. Two additional modules exist but are not exported: `module-sqlite-libs` and **`module-nullclaw`** (Zig cross-compiled, placeholder hash).

#### Layer Stacking

`_lowerdir()` at `common.sh:198-211` orders layers: snapshot first (highest priority), then numbered modules in reverse lexicographic order. All layers are read-only; a writable tmpfs upper layer (default 512MB) handles writes via copy-on-write.

#### Sandbox Backends

**Chroot backend** (`common.sh:289-455`): No root required. Uses squashfuse + fuse-overlayfs + bubblewrap (`--unshare-pid --unshare-ipc --unshare-uts --die-with-parent`). Falls back to `unshare + chroot` if bwrap unavailable. No network isolation — sandboxes share host network.

**Firecracker backend** (`common.sh:552-798`, `shared/bin/sq-firecracker`): Full VM isolation. Each sandbox runs a Firecracker microVM with its own tap device, vsock (port 5000) for command execution, and virtio block devices for squashfs layers. Requires `/dev/kvm` and root. Guest init at `shared/vm/init:1-81` mounts drives, builds overlayfs, configures networking, execs `sq-vsock-handler`.

#### HTTP API

| Route | Method | Behavior |
|-------|--------|----------|
| `/health` | GET | Backend, tailscale status, sandbox/module counts |
| `/api/modules` | GET | List local + S3 remote modules |
| `/api/sandboxes` | GET/POST | List or create sandboxes |
| `/api/sandboxes/:id` | GET/DELETE | Info or destroy |
| `/api/sandboxes/:id/exec` | POST | Execute command in sandbox |
| `/api/sandboxes/:id/activate` | POST | Hot-add a module |
| `/api/sandboxes/:id/snapshot` | POST | Create squashfs snapshot of upper layer |
| `/api/sandboxes/:id/restore` | POST | Restore from snapshot |

#### Isolation Mechanisms

- **Namespace isolation**: per-exec via bubblewrap (PID, IPC, UTS, pivot_root)
- **cgroups**: stubbed out (`common.sh:229-230`) — requires `CAP_SYS_ADMIN`, deferred to `systemd-run --user --scope`
- **Network**: none for chroot backend; full L3 via tap+NAT for Firecracker
- **Secret proxy**: HTTP/HTTPS MITM proxy (`shared/proxy/main.go`, 526 lines) that replaces placeholder env vars with real credentials for allowed hosts only

#### Snapshot System

Create: `mksquashfs $s/upper/data $snapfile` (zstd or gzip). Restore: unmount overlay → clear upper → mount snapshot as read-only lowerdir → rebuild overlay. Ephemeral mode: auto-snapshot on destroy, auto-restore on create, S3-backed.

#### S3 Integration

`shared/bin/sq-s3` — pure shell SigV4 signing, atomic pull with `flock`, background push. Key namespace: `modules/<name>.squashfs`, `sandboxes/<id>/snapshots/<label>.squashfs`, `sandboxes/<id>/manifest.json`.

#### Implementation Matrix

| Impl | LOC | Binary | HTTP | S3 |
|------|-----|--------|------|-----|
| Shell | ~1.1k | N/A | busybox httpd+CGI | Shell sq-s3 |
| Rust | ~11k | 17MB | Axum+Tokio | Native aws-sdk |
| Zig | ~10k | 2.3MB | std.http | Native SigV4 |
| Odin | ~5k | 838KB | Custom | Shell sq-s3 |
| CL | ~3.5k | ~20MB | Woo/Ningle | Native Ironclad |
| Janet | ~1.9k | 71KB src | net/server | Shell sq-s3 |

#### Deployment Model

Three paths: Nix (`nix build .#image-<impl>`), NixOS module (`services.sq-sandbox`), Docker (`shared/Dockerfile.firecracker` for Firecracker backend). The unprivileged sandbox plan (`thoughts/shared/plans/2026-02-19-unprivileged-sandbox.md`) represents the current direction: dropping Docker entirely in favor of squashfuse/fuse-overlayfs/bubblewrap + Nix-native deployment.

#### Filesystem Layout

```
$SQUASH_DATA/
  modules/               ← .squashfs files
  sandboxes/<id>/
    .meta/               ← flat files: owner, task, layers, cpu, memory_mb, etc.
      log/               ← exec logs: 0001.json, 0002.json
      snapshots.jsonl
    images/              ← squashfuse mount points per module
      _snapshot/         ← snapshot mount point
    upper/data/          ← writable overlay layer (tmpfs)
    upper/work/          ← overlayfs work dir
    merged/              ← chroot/bwrap target
    snapshots/           ← squashfs checkpoint files
  secrets.json           ← credential definitions
  proxy-ca/              ← MITM CA cert + key
  vm/                    ← Firecracker: guest-rootfs.ext4, vmlinux, firecracker binary
```

---

### 2. nullclaw — Autonomous AI Agent Runtime (Zig)

**Location**: `/Users/reuben/projects/claw/nullclaw/`
**Language**: Zig 0.15.2
**Binary**: 678 KB (ReleaseSmall), ~1 MB peak RSS, <2 ms startup
**Tests**: 2,843
**Source**: ~110 files, ~45,000 LOC
**GitHub**: [github.com/nullclaw/nullclaw](https://github.com/nullclaw/nullclaw) — 1,500+ stars, MIT license
**Versioning**: CalVer (`YYYY.M.D`), daily releases

#### Vtable Architecture

Every subsystem is a Zig struct with `ptr: *anyopaque` + `vtable: *const VTable`. Nine major interfaces:

| Subsystem | Interface | Source |
|-----------|-----------|--------|
| AI Models | `Provider` | `src/providers/root.zig` |
| Channels | `Channel` | `src/channels/root.zig` |
| Memory | `Memory` | `src/memory/root.zig` |
| Tools | `Tool` | `src/tools/root.zig` |
| Sandbox | `Sandbox` | `src/security/sandbox.zig` |
| Runtime | `RuntimeAdapter` | `src/runtime.zig` |
| Tunnel | `Tunnel` | `src/tunnel.zig` |
| Observer | `Observer` | `src/observability.zig` |
| Peripheral | `Peripheral` | `src/peripherals.zig` |

All swappable via `~/.nullclaw/config.json` — zero code changes.

#### Provider System (22+)

Dedicated implementations in `src/providers/`:

| File | Provider |
|------|----------|
| `anthropic.zig` | Anthropic (Claude) |
| `openai.zig` | OpenAI (GPT) |
| `openai_codex.zig` | OpenAI Codex |
| `openrouter.zig` | OpenRouter (meta-router) |
| `ollama.zig` | Ollama (local) |
| `gemini.zig` | Google Gemini |
| `claude_cli.zig` | Claude CLI wrapper |
| `codex_cli.zig` | Codex CLI wrapper |
| `compatible.zig` | 40+ OpenAI-compatible endpoints (Groq, DeepSeek, Mistral, xAI, Together, Fireworks, Perplexity, Cohere, Bedrock, Venice, etc.) |

Factory at `src/providers/factory.zig`. Supporting files: `router.zig` (provider routing), `reliable.zig` (retry/fallback), `scrub.zig` (credential scrubbing), `sse.zig` (server-sent events streaming), `helpers.zig`, `api_key.zig`.

#### Channel System (13+)

Implementations in `src/channels/`:

| File | Channel |
|------|---------|
| `cli.zig` | Interactive terminal |
| `telegram.zig` | Telegram Bot API |
| `discord.zig` | Discord Bot+Gateway |
| `slack.zig` | Slack Bolt SDK |
| `imessage.zig` | iMessage via BlueBubbles |
| `matrix.zig` | Matrix protocol |
| `whatsapp.zig` | WhatsApp via Baileys |
| `irc.zig` | IRC with allowlists |
| `lark.zig` | Lark/Feishu WebSocket |
| `dingtalk.zig` | DingTalk Bot API |
| `qq.zig` | QQ |
| `onebot.zig` | OneBot protocol |
| `email.zig` | Email |
| `line.zig` | LINE |
| `maixcam.zig` | MaixCam hardware camera/display |

Plus `dispatch.zig` (channel message routing) and `root.zig` (vtable definition + factory).

#### Tool System (18+)

Located in `src/tools/`. Includes: shell, file_read, file_write, file_edit, memory_store, memory_recall, memory_forget, browser_open, screenshot, composio, http_request, hardware_info, hardware_memory, git, cron/schedule, pushover, delegate (subagents), MCP tools.

#### Memory System

`src/memory/` — SQLite-backed hybrid search:
- `sqlite.zig` — FTS5 virtual tables with BM25 scoring
- `vector.zig` — Embeddings stored as BLOB, cosine similarity
- `embeddings.zig` — `EmbeddingProvider` vtable (OpenAI, custom URL, noop)
- `hygiene.zig` — Automatic archival + purge of stale memories
- `snapshot.zig` — Export/import full memory state
- `markdown.zig` — Markdown file backend
- `cache.zig` — Caching layer
- `chunker.zig` — Text chunking for embedding
- `lucid.zig` — Lucid memory (context-aware)
- `none.zig` — Noop backend

#### Security

`src/security/`:
- `landlock.zig` — Linux Landlock kernel self-restriction (strongest)
- `firejail.zig` — SUID namespace sandbox
- `bubblewrap.zig` — Unprivileged sandboxing
- `docker.zig` — Container isolation
- `detect.zig` — Auto-detect best available backend
- `secrets.zig` — ChaCha20-Poly1305 encryption for API keys
- `pairing.zig` — 6-digit one-time code exchange
- `policy.zig` — Security policy engine
- `audit.zig` — Signed event trail with retention
- `tracker.zig` — Resource usage tracking

#### Autonomous Operation

- `src/daemon.zig` — Daemon supervisor with exponential backoff
- `src/heartbeat.zig` — Periodic wake (configurable interval, e.g., 30m) for proactive monitoring
- `src/cron.zig` — JSON-persisted cron scheduler with standard cron expressions
- `src/agent/dispatcher.zig` — Tool dispatch with subagent delegation
- `src/skillforge.zig` — Skill discovery (GitHub), evaluation, integration

#### Runtime Adapters

`src/runtime.zig` — `RuntimeAdapter` vtable with three implementations: Native (direct OS execution), Docker (containerized, resource-limited), WASM (wasmtime, browser/edge compatible).

#### Build System

`build.zig:1-82`: Uses `std.Build` with SQLite as a Zig dependency (FTS5 enabled). Links SQLite via `b.dependency("sqlite3", ...)`. ReleaseSmall applies: `strip = true`, `unwind_tables = .none`, `omit_frame_pointer = true`, plus macOS post-install strip of local symbols. Version embedded from build option (default: CalVer).

Also includes: `Dockerfile`, `docker-compose.yml`, `flake.nix`/`flake.lock`.

#### Configuration

`~/.nullclaw/config.json` — OpenClaw-compatible format (snake_case). Key sections: `models.providers`, `agents.defaults`, `channels`, `tools`, `mcp_servers`, `memory`, `gateway`, `autonomy`, `runtime`, `tunnel`, `secrets`, `identity`, `security`.

Env overrides: `NULLCLAW_API_KEY`, `NULLCLAW_PROVIDER`, `NULLCLAW_MODEL`, `NULLCLAW_TEMPERATURE`, `NULLCLAW_MEMORY_BACKEND`, `NULLCLAW_AUTONOMY_LEVEL`, etc.

#### Key Source Files

| File | Purpose |
|------|---------|
| `src/main.zig` | CLI entry + command routing |
| `src/root.zig` | Module hierarchy (public API) |
| `src/config.zig` | JSON config loader + 30 sub-config structs |
| `src/agent.zig` | Agent orchestration loop |
| `src/daemon.zig` | Daemon supervisor |
| `src/gateway.zig` | HTTP gateway (rate limiting, pairing) |
| `src/cron.zig` | Cron scheduler |
| `src/heartbeat.zig` | Periodic wake engine |
| `src/mcp.zig` | Model Context Protocol server integration |
| `src/bus.zig` | Internal event bus |
| `src/channel_loop.zig` | Channel message loop |

#### Gateway API

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check |
| `/pair` | POST | 6-digit code → bearer token |
| `/webhook` | POST | Send message (authed) |
| `/whatsapp` | GET/POST | Meta webhook verification + incoming |

---

### 3. odin-claw — Lightweight Agent Runtime (Odin)

**Location**: `/Users/reuben/projects/claw/odin-claw/`
**Language**: Odin
**Binary**: 388 KB (`-o:aggressive`), ~0.2 ms startup
**Tests**: 25 tests across 7 test files
**Build**: `cc -c src/curl_helpers.c -o src/curl_helpers.o && odin build src -out:odin-claw -o:aggressive`

#### Architecture

Single Odin package (`package main`) across ~21 source files. Uses the same vtable pattern as nullclaw: `struct { ptr: rawptr, vtable: ^VTable }`.

Named `"nullclaw"` in `build.odin:8` and uses `NULLCLAW_` env var prefixes. Config path: `~/.odin-claw/config.json`.

#### Provider System (5 implementations + compatible)

| Provider | File location | Status |
|----------|---------------|--------|
| OpenAI | `providers.odin:38-208` | Full (manual JSON parsing) |
| xAI/Grok | `providers.odin:210-253` | Reuses OpenAI format |
| Anthropic | `providers.odin:255-381` | Stub (returns "API integration requires TLS support") |
| Ollama | `providers.odin:383-479` | Full (local) |
| OpenAI-Compatible | `providers.odin:541-734` | 40+ endpoint mappings |
| Mock | `providers.odin:481-538` | For tests |

#### Tool System (10 tools)

| Tool | Location |
|------|----------|
| shell | `tools.odin:48-84` (blocklist: rm, sudo, su, chmod, chown, dd, mkfs) |
| file_read | `tools.odin:87-109` |
| file_write | `tools.odin:112-141` |
| file_edit | `tools.odin:177-234` |
| file_append | `tools.odin:248-283` |
| git | `tools.odin:297-370` (9 subcommands) |
| http_request | `tools.odin:384-426` |
| memory_store | `tools.odin:443-465` (placeholder) |
| memory_recall | `tools.odin:475-487` (placeholder) |
| memory_forget | `tools.odin:497-509` (placeholder) |

Path sandboxing: `validate_path` at `tools.odin:33-45` prefix-matches against workspace + allowed paths.

#### Channel System (3 + CLI)

| Channel | Status |
|---------|--------|
| Slack | Log-only stub (`channels.odin:76`) |
| Telegram | Log-only stub (`channels.odin:128`) |
| Discord | Log-only stub (`channels.odin:175`) |
| CLI | Inline in `main.odin:593-624` (stdin loop) |

#### Memory

- In-memory: `memory.odin:31-99` — `map[string][]byte` with substring search
- LMDB: `lmdb.odin:10-270` — Full FFI bindings, 256MB mmap, vtable-compatible
- SQLite: `memory_db.odin` — Schema stubs only (commented SQL, `Db.connected` never set)

#### Security / Sandbox

- Path sandboxing: `validate_path` prefix matching
- Sandbox vtable: `security.odin:15-82` — `NativeSandbox` only (no network restriction, no resource limits)
- Config accepts `"landlock"` as default backend but no Landlock syscall integration exists
- Docker runtime referenced in validation but not implemented

#### HTTP Layer

Uses libcurl via C FFI helpers (`curl_helpers.c`) — ARM64 ABI workaround for variadic `curl_easy_setopt`. Gateway server in `gateway.odin`.

#### Relationship to nullclaw

Explicitly "inspired by NullClaw (Zig)" per `README.md:7`. Claims 43% smaller binary (388 KB vs 678 KB). Shares vtable design, config structure, env var naming. Implementation coverage is narrower: 5 vs 22+ providers, 3 vs 13 channels, 10 vs 18+ tools, no daemon/cron/heartbeat/skills.

---

### 4. nanoclaw — Claude Agent SDK Orchestrator (TypeScript)

**GitHub**: [github.com/qwibitai/nanoclaw](https://github.com/qwibitai/nanoclaw) — ~10,200+ stars
**Language**: TypeScript/Node.js (>=20)
**Architecture**: ~500 lines core, ~2,000 lines total across 5 files
**Creator**: Gavriel Cohen (Wix veteran, Qwibit AI)

#### Architecture

Host orchestrator with three polling loops (`src/index.ts`): messages (2s), IPC (1s), scheduler (60s). The Claude Agent SDK runs **inside** containers, not on the host.

```
WhatsApp (baileys) → SQLite → Polling loop → Container (Claude Agent SDK) → Response via IPC
```

IPC is filesystem-based JSON queues in `data/ipc/{folder}/`. No Redis, no message broker.

#### Per-Group Container Isolation

Each WhatsApp group gets its own isolated container. Runtime auto-detected: Apple Container on macOS, Docker on Linux.

| Group Type | Filesystem Access |
|-----------|------------------|
| Main group | Full project root + `/workspace/project` |
| Non-main groups | Only `groups/{folder}/` at `/workspace/group` |
| All groups | Read-only `/workspace/global` |

Mount allowlist at `~/.config/nanoclaw/mount-allowlist.json` (outside project — agents cannot tamper). Always-blocked: `.ssh`, `.gnupg`, `.aws`. Container limits: 30min timeout, 10MB max output, 5 concurrent containers.

#### Channels

Ships with WhatsApp only (`@whiskeysockets/baileys ^7.0.0-rc.9`). Other channels added via Skills system: `/add-telegram`, `/add-slack`. QR code pairing for WhatsApp auth.

#### Skills System

`.claude/skills/*/SKILL.md` — Claude Code instructions that **transform the codebase**, not runtime plugins. `/setup`, `/add-telegram`, `/add-slack`, `/convert-to-docker`. The project does not accept feature PRs; new capabilities go as skills.

#### Swarm / Multi-Agent

Uses Anthropic's TeammateTool (Claude Code agent teams). Queen agent spawns sub-agents in isolated containers. `MAX_CONCURRENT_CONTAINERS` (default 5) governs parallelism.

#### Dependencies

`@whiskeysockets/baileys`, `better-sqlite3`, `cron-parser`, `pino`, `qrcode`, `yaml`, `zod`.

---

## Integration Surface Analysis

### sq-sandbox ↔ nullclaw Integration Points

1. **Module delivery**: nullclaw binary as a squashfs module (`200-nullclaw.squashfs`). The Nix build system already has a placeholder at `nix/modules.nix` for cross-compiled Zig → squashfs.

2. **HTTP API orchestration**: nullclaw's `http_request` tool can call sq-sandbox's REST API to spawn, exec, snapshot, and destroy sandboxes. No code changes needed.

3. **Runtime adapter**: nullclaw's `RuntimeAdapter` vtable could gain a `sq-sandbox` adapter that calls the HTTP API instead of local Docker/native execution.

4. **Shared security model**: sq-sandbox provides kernel-level isolation (namespaces, squashfuse, bubblewrap) + credential injection (secret proxy). nullclaw provides application-level security (Landlock, pairing, encrypted secrets, audit).

5. **Configuration**: Both use JSON config. nullclaw's `~/.nullclaw/config.json` and sq-sandbox's env vars (`SQUASH_*`) operate at different layers without conflict.

6. **Agent-in-sandbox pattern**: nullclaw binary baked into a squashfs module runs inside each sandbox. Sandbox provides filesystem isolation; nullclaw provides AI capabilities. Configuration passed via injected env vars or mounted config files.

7. **Snapshot as agent state**: sq-sandbox snapshots capture the writable layer (nullclaw's workspace, memory DB, config) as a squashfs archive. Restore recreates the agent environment from a frozen state.

### odin-claw as Alternative Agent Layer

odin-claw at 388 KB is smaller than nullclaw's 678 KB, making it a viable ultra-minimal alternative for constrained environments. However, its implementation coverage is significantly narrower (5 vs 22+ providers, stub channels, no daemon/cron). It could serve as a "nanobot" variant module alongside the full nullclaw module.

### nanoclaw's Container Model vs SquashClaw

nanoclaw's per-group Docker/Apple Container isolation mirrors the per-sandbox isolation in sq-sandbox, but with different tradeoffs:
- nanoclaw: Docker containers with volume mounts, Claude SDK inside, filesystem-based IPC
- SquashClaw: squashfs layers + overlayfs, any agent binary inside, HTTP API for orchestration

nanoclaw's Skills system (codebase transformation via Claude Code) is orthogonal and could work with SquashClaw — skills could transform the sq-sandbox config or nullclaw config within a sandbox.

---

## Code References

### sq-sandbox Key Files
- `shared/cgi-bin/common.sh` — All sandbox lifecycle operations, both backends
- `shared/bin/sq-exec` — Execution via bwrap or unshare+chroot
- `shared/bin/sq-mount-layer` — squashfuse mount wrapper
- `shared/bin/sq-mount-overlay` — fuse-overlayfs mount wrapper
- `shared/bin/sq-firecracker` — Firecracker VM lifecycle
- `shared/bin/sq-start` — Top-level startup orchestrator
- `shared/bin/sq-mkmod` — Module builder (presets, from-dir, from-sandbox)
- `shared/bin/sq-s3` — S3 client with SigV4 signing
- `shared/proxy/main.go` — HTTPS MITM credential proxy (526 lines)
- `nix/lib-squashfs.nix` — `mkSquashfsModule` Nix helper
- `nix/modules.nix` — Module aggregator (includes nullclaw placeholder)
- `nix/image.nix` — OCI image builder
- `nix/nixos-module.nix` — NixOS system module
- `flake.nix` — Nix flake: 6 daemons, 5+ modules, 7 dev shells

### nullclaw Key Files
- `src/main.zig` — CLI entry + command routing
- `src/agent.zig` — Agent orchestration loop
- `src/config.zig` — JSON config loader
- `src/daemon.zig` — Daemon supervisor with exponential backoff
- `src/gateway.zig` — HTTP gateway with pairing
- `src/cron.zig` — Cron scheduler
- `src/heartbeat.zig` — Periodic wake engine
- `src/runtime.zig` — RuntimeAdapter vtable (native/docker/wasm)
- `src/providers/root.zig` — Provider vtable definition
- `src/channels/root.zig` — Channel vtable definition
- `src/tools/root.zig` — Tool vtable definition
- `src/memory/root.zig` — Memory vtable definition
- `src/security/sandbox.zig` — Sandbox vtable definition
- `src/security/landlock.zig` — Landlock kernel sandboxing
- `src/security/secrets.zig` — ChaCha20-Poly1305 key encryption
- `build.zig` — Zig build with SQLite FTS5

### odin-claw Key Files
- `src/main.odin` — CLI entry, command dispatch, provider factory
- `src/agent.odin` — Agent orchestration, tool dispatch, memory compaction
- `src/providers.odin` — 5 provider implementations + compatible endpoints
- `src/tools.odin` — 10 tool implementations with path sandboxing
- `src/config.odin` — JSON config loading, defaults, env overrides
- `src/lmdb.odin` — Full LMDB FFI + Memory vtable implementation
- `src/channels.odin` — Slack/Telegram/Discord stubs
- `src/gateway.odin` — HTTP server
- `src/security.odin` — Sandbox vtable (NativeSandbox only)
- `src/runtime.odin` — Process execution via libc `system()`
- `src/http.odin` — libcurl FFI bindings
- `src/curl_helpers.c` — ARM64 ABI workaround for libcurl varargs

---

## Historical Context (from thoughts/)

### Plans
- `thoughts/shared/plans/plan-firecracker-and-security.md` — 7-phase plan: backend abstraction, cgroups v2, network egress, guest VM, Firecracker backend, secret proxy, bare metal deploy. All implemented.
- `thoughts/shared/plans/2026-02-16-nix-monorepo-declarative-builds.md` — Consolidating 6 implementations into Nix flake monorepo. Implemented in `flake.nix`.
- `thoughts/shared/plans/2026-02-19-unprivileged-sandbox.md` — Dropping Docker, moving to squashfuse/fuse-overlayfs/bubblewrap. **Current architectural direction**. 6 phases: sq-mount-layer/sq-mount-overlay helpers, sq-exec with bwrap, remove veth/iptables, wire helpers into common.sh, update flake.nix, remove Docker.

### Research
- `thoughts/shared/research/2026-02-19-architecture-isolation-and-nix.md` — Comprehensive analysis of current architecture, container role (purely packaging — zero sandbox isolation), Nix infrastructure, and isolation mechanisms. Written on `feat/squash-nullclaw-integration` branch.
- `thoughts/shared/research/2026-02-14-worktree-scorecard.md` — Scorecard of 5 implementations post-Firecracker. Rust scored highest (4.7/5.0).
- `thoughts/shared/research/2026-02-15-main-vs-janet-vs-odin-review.md` — Comparative review: Shell ~5-15ms latency, Janet ~1-3ms, Odin ~0.5-2ms per health check.
- `thoughts/shared/research/2026-02-15-odin-optimization-deep-dive.md` — 12-dimension deep dive of the sq-sandbox Odin implementation (distinct from odin-claw). Found: aarch64 syscall number bug, temp allocator memory leak, 236 lines of dead SigV4 code.
- `thoughts/shared/research/2026-02-15-ap-homoiconic-architecture.md` — sq-sandbox as execution layer for Autopoiesis (ap) cognitive platform. CL implementation highlighted for same-process embedding potential.

---

## Architecture Documentation

### Comparison Matrix

| | sq-sandbox | nullclaw | odin-claw | nanoclaw |
|---|---|---|---|---|
| **Role** | Sandbox runtime | AI agent runtime | AI agent runtime | AI agent orchestrator |
| **Language** | Shell/Rust/Zig/Odin/CL/Janet | Zig | Odin | TypeScript |
| **Binary size** | N/A (daemon varies) | 678 KB | 388 KB | ~500 lines TS |
| **Isolation** | squashfs+overlayfs+bwrap, Firecracker | Landlock/Firejail/Bwrap/Docker | Path sandbox only | Docker/Apple Container |
| **AI providers** | None (sandbox only) | 22+ | 5 (+compatible) | Claude SDK only |
| **Channels** | HTTP API only | 13+ | 3 stubs + CLI | WhatsApp (+skills) |
| **Tools** | exec, snapshot, restore | 18+ | 10 | Claude tools |
| **Memory** | Filesystem snapshots | SQLite FTS5+vector | LMDB + in-memory | SQLite |
| **Autonomous** | No (daemon only) | Yes (daemon, cron, heartbeat) | No | Cron via scheduler |
| **squashfs** | Native & central | None | None | None |
| **Firecracker** | Full backend | None | None | None |
| **Config** | Env vars (`SQUASH_*`) | JSON (`~/.nullclaw/config.json`) | JSON (`~/.odin-claw/config.json`) | `.env` + JSON |
| **Maturity** | Active (6 implementations) | Very active (daily releases) | Moderate | Very active |

### Existing Integration Work

The `feat/squash-nullclaw-integration` branch on sq-sandbox is the primary evidence of synthesis planning:
1. `nix/modules.nix` contains a `module-nullclaw` entry (Zig cross-compiled, placeholder hash)
2. The `2026-02-19-architecture-isolation-and-nix.md` research was conducted on this branch
3. The `2026-02-19-unprivileged-sandbox.md` plan was written on this branch
4. The `squash-claw/` directory exists but is empty

---

## External Resources

### nullclaw
- [GitHub](https://github.com/nullclaw/nullclaw) — Main repository (1,500+ stars)
- [nullclaw.org](https://nullclaw.org/) — Official site
- [Architecture docs](https://nullclaw.github.io/architecture.html) — Vtable system explained
- [Railway deploy](https://railway.com/deploy/nullclaw) — One-click cloud
- [OpenClaw providers docs](https://docs.openclaw.ai/providers) — Shared provider catalog
- [OpenClaw channels docs](https://docs.openclaw.ai/channels) — Shared channel catalog
- [Cron/heartbeat docs](https://docs.openclaw.ai/automation/cron-jobs)
- [Project history](https://www.datapro.news/p/the-very-strange-tale-of-openclaw-ai-agents) — ClawdBot → MoltBot → OpenClaw

### nanoclaw
- [GitHub](https://github.com/qwibitai/nanoclaw) — Main repository (~10,200+ stars)
- [nanoclaw.dev](https://nanoclaw.dev) — Landing page
- [DeepWiki](https://deepwiki.com/gavrielc/nanoclaw) — Auto-generated technical docs
- [Docker blog post](https://www.docker.com/blog/run-nanoclaw-in-docker-shell-sandboxes/) — Official Docker integration
- [Architecture analysis](https://fumics.in/posts/2026-02-02-nanoclaw-agent-architecture) — "500 Lines vs 50 Modules"
- [Anthropic SDK clarification](https://thenewstack.io/anthropic-agent-sdk-confusion/) — Policy for NanoClaw/OpenClaw usage

### Related Projects
- [ZeroClaw](https://github.com/zeroclaw-labs/zeroclaw) — Rust sibling (3.4 MB binary)
- [KrillClaw](https://github.com/krillclaw/KrillClaw) — 49 KB Zig micro-agent
- [OpenClaw](https://github.com/openclaw/openclaw) — Original TypeScript implementation

---

## Open Questions

1. **nullclaw module build**: The placeholder hash in `nix/modules.nix` needs to be replaced with a real cross-compilation of the nullclaw Zig binary for `x86_64-linux-musl` and `aarch64-linux-musl`. What's the correct `zig build` invocation for cross-compiling the static binary?

2. **RuntimeAdapter extension**: Should a `sq-sandbox` runtime adapter be added to nullclaw that calls the HTTP API? Or is the simpler approach to run nullclaw inside a sandbox and let it use its native runtime?

3. **Configuration injection**: How should nullclaw config be passed into a sandbox? Options: (a) baked into the squashfs module, (b) injected via secret proxy environment variables, (c) mounted from host via overlay upper layer, (d) fetched from S3 at startup.

4. **Memory persistence**: nullclaw's SQLite memory DB lives in the writable upper layer. Snapshots preserve it. Should there be explicit integration so memory persists across sandbox recreations?

5. **Channel routing**: When nullclaw runs inside a sandbox, how do channel messages reach it? Options: (a) gateway runs on host, proxies to sandbox HTTP API, (b) nullclaw runs gateway inside sandbox with port forwarding, (c) Tailscale module provides direct connectivity.

6. **odin-claw role**: Is odin-claw intended as a production alternative to nullclaw, or primarily as a development/learning vehicle? Its narrower feature set and stub implementations suggest the latter.

7. **nanoclaw integration**: Is there value in supporting nanoclaw's Claude SDK model alongside nullclaw's multi-provider model? The Skills system could be useful for configuring sandbox environments.

8. **Unprivileged vs Firecracker**: The unprivileged sandbox plan drops network isolation entirely. For SquashClaw, is the Firecracker backend the preferred path for production (full isolation) while unprivileged is for development/testing?
