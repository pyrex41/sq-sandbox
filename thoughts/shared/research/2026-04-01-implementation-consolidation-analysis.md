---
date: 2026-04-01T19:46:33Z
researcher: reuben
git_commit: 5597f48
branch: main
repository: sq-sandbox
topic: "Implementation Consolidation: Go vs Shell/Busybox vs Zig vs Janet vs CL"
tags: [research, codebase, architecture, consolidation, go, zig, shell, janet, common-lisp]
status: complete
last_updated: 2026-04-01
last_updated_by: reuben
---

# Research: Implementation Consolidation Analysis

**Date**: 2026-04-01T19:46:33Z
**Researcher**: reuben
**Git Commit**: 5597f48
**Branch**: main
**Repository**: sq-sandbox

## Research Question

The project has 6 parallel daemon implementations (Rust, Zig, Odin, Common Lisp, Janet, Shell/CGI) plus a Go HTTPS proxy and an OCaml store sidecar. Which single implementation should the project consolidate on, optimizing for smallest surface area and highest maintainability? Go is proposed as the consolidation target.

## Summary

The codebase currently carries ~38k LOC across 6 daemon implementations, all exposing the same HTTP API. The shared shell scripts (`shared/bin/`, `shared/cgi-bin/`) account for ~4.3k LOC of common infrastructure that all implementations depend on. A Go proxy (`shared/proxy/main.go`, 633 LOC) already exists and works well. An OCaml/Irmin store sidecar adds another 873 LOC. The question is whether to consolidate the daemon itself — currently the Zig impl (11.3k LOC, 2.7MB binary) is deployed on Fly.io per `fly.toml`.

## Current State: What Exists

### Implementation Inventory (on main branch, all present in `impl/`)

| Impl | LOC | Binary Size | Runtime Deps | Scorecard (Feb 2026) | Native S3 | Native Proxy |
|------|-----|-------------|-------------|---------------------|-----------|-------------|
| **Rust** | 12,128 | 22 MB | Tokio runtime | **4.7/5** (highest) | Yes | Yes (HTTPS MITM) |
| **Zig** | 11,313 | 2.7 MB | None (static) | 3.8/5 | Yes (SigV4) | No |
| **Odin** | 5,010 | 838 KB | None (static) | 3.3/5 | No (shells to sq-s3) | No |
| **CL** | 3,503 | ~20 MB | SBCL + Quicklisp | 3.3/5 | Yes | No |
| **Janet** | 2,052 | ~15 MB runtime | Janet interpreter | 2.7/5 | No (shells to sq-s3) | No |
| **Shell/CGI** | ~4,300 | N/A (scripts) | busybox + jq | N/A (baseline) | No (aws cli) | No |

### Already-Go Components

- **`shared/proxy/main.go`** (633 LOC) — HTTPS MITM credential injection proxy. Clean stdlib Go, no external deps. Handles CA generation, cert caching, header replacement, TCP tunneling.
- The proxy is already compiled and shipped alongside whichever daemon is chosen.

### Already-Shell Components (shared across all impls)

- `shared/bin/` — 18 scripts, 3,022 LOC: `sq-exec`, `sq-init`, `sq-mount-layer`, `sq-mount-overlay`, `sq-mkmod`, `sq-s3`, `sq-sync`, `sq-wg`, `sq-reaper`, etc.
- `shared/cgi-bin/` — CGI handlers, 1,326 LOC: `common.sh` (1,165 LOC), `api/sandboxes`, `api/modules`
- All daemon implementations shell out to these for mount, exec, networking operations

### Deployment Status

- `fly.toml` deploys the **Zig** daemon (`SQUASH_BACKEND=chroot`, `SQUASH_MAX_SANDBOXES=20`)
- `Dockerfile.zig` is the production Docker build
- Nix flake builds all 6 as separate packages

### OCaml Store Sidecar

- `store/` — 873 LOC, Irmin-pack based incremental snapshot store
- Optional alternative to squashfs snapshots
- Communicates over Unix socket

## Analysis: The Consolidation Candidates

### Option 1: Go

**What exists**: 633 LOC proxy. No daemon implementation yet — would be built from scratch.

**Arguments for**:
- Single binary, static linking, small binary (~4-8 MB typical)
- Already have working Go code in the project (proxy)
- Rich stdlib: HTTP server, JSON, crypto, testing — no external deps needed
- Excellent cross-compilation
- Huge ecosystem for any future needs
- Could absorb the proxy into the daemon binary (one process instead of two)
- Very readable for anyone who inherits the codebase
- Goroutines map naturally to concurrent sandbox management

**Arguments against**:
- Starting from scratch — none of the existing 6 impls are Go
- Would need to rewrite ~3k-11k LOC of daemon logic
- GC pauses (irrelevant for this workload, but worth noting)
- Binary larger than Zig/Odin (4-8 MB vs 2.7 MB / 838 KB)

**Surface area**: Go stdlib only (no deps). One binary. One language for proxy + daemon.

### Option 2: Stay with Shell/Busybox CGI

**What exists**: Fully working implementation. The `shared/` scripts are the oldest, most battle-tested code. busybox httpd + CGI is proven.

**Arguments for**:
- Already done and deployed as the baseline
- Zero compilation step
- Easy to modify (just edit scripts)
- Every daemon implementation delegates to these scripts anyway
- Minimal dependencies: busybox, jq, squashfs-tools, iproute2, iptables

**Arguments against**:
- ~5-15ms per health check (process spawn + CGI + jq overhead)
- 8+ `jq` invocations per create request
- `eval "$cmd"` in exec path — command injection risk
- No persistent state between requests (CGI model)
- Hard to add features like WebSocket, streaming, connection pooling
- jq is an external dep that adds parsing complexity

**Surface area**: Small in concept, but lots of shell scripts. Spread across many files.

### Option 3: Keep Zig (currently deployed)

**What exists**: 11,313 LOC, the second-most complete implementation. Native S3 with SigV4. Currently what `fly.toml` points at.

**Arguments for**:
- Already deployed and working
- 2.7 MB static binary (smallest compiled option)
- No runtime dependencies
- Native S3, no AWS CLI needed
- Good test coverage (per scorecard)

**Arguments against**:
- 11.3k LOC is a lot for what this does
- Zig 0.15 is pre-1.0, breaking changes between releases (already hit one: `8df53b9`)
- Smaller hiring/contributor pool
- Need to maintain Zig + Go (for proxy) + shell (for shared scripts)
- Custom HTTP routing, JSON parsing — lots of code just for basics

**Surface area**: Zig daemon + Go proxy + shell scripts = 3 languages.

### Option 4: Janet

**What exists**: 2,052 LOC, the smallest daemon implementation.

**Arguments for**:
- Smallest codebase by far
- Fiber-based concurrency
- Easy to iterate on
- No `eval` in exec path

**Arguments against**:
- Lowest scorecard rating (2.7/5)
- Missing tests, weak recovery, gaps in isolation
- Requires Janet runtime (~15 MB)
- Tiny community
- 3 languages: Janet + Go + shell

**Surface area**: Small codebase, but niche runtime dependency.

### Option 5: Common Lisp

**What exists**: 3,503 LOC. Only impl with gVisor support. Native S3.

**Arguments for**:
- Compact codebase (3.5k LOC)
- REPL-driven development is fast for iteration
- Good S3 integration
- gVisor support unique to this impl

**Arguments against**:
- SBCL runtime is ~20 MB
- Quicklisp dependency management
- ~3.3/5 scorecard
- Niche for most contributors
- 3 languages: CL + Go + shell

**Surface area**: Medium codebase, heavy runtime.

## What a Go Consolidation Would Actually Look Like

### The Daemon (~2-3k LOC estimated)

A Go daemon replacing the current impls would:
1. **HTTP server** — stdlib `net/http` with `http.ServeMux` (Go 1.22+ pattern matching). No framework needed. ~200 LOC.
2. **Sandbox manager** — CRUD + state tracking for sandboxes. ~300 LOC.
3. **Exec** — Wrap `os/exec.Command` calling `sq-exec` (shared shell script). ~200 LOC.
4. **Mount operations** — Shell out to `sq-mount-layer`, `sq-mount-overlay`. ~100 LOC.
5. **Network namespace** — Shell out to shared scripts (same as all current impls). ~50 LOC.
6. **S3** — Either shell out to `sq-s3` (simple) or use `aws-sdk-go-v2` (native, adds deps). ~200-500 LOC.
7. **Reaper** — Background goroutine. ~100 LOC.
8. **Recovery/init** — Remount existing sandboxes on startup. ~150 LOC.
9. **Auth middleware** — Bearer token check. ~30 LOC.
10. **Proxy integration** — Could be absorbed into the same binary. ~633 LOC (already written).

Total: ~2-3k LOC for the daemon + 633 LOC proxy = ~3.5k LOC, single binary.

### What Can Be Deleted

- `impl/rust/` (12,128 LOC)
- `impl/zig/` (11,313 LOC)
- `impl/odin/` (5,010 LOC)
- `impl/cl/` (3,503 LOC)
- `impl/janet/` (2,052 LOC)
- `impl/shell/` (wrapper scripts)
- `Dockerfile.zig`
- All per-language Nix build derivations

That's ~34k LOC removed.

### What Stays

- `shared/bin/` shell scripts — these are the actual sandbox primitives. All impls delegate to them.
- `shared/cgi-bin/` — could be dropped if the Go daemon handles all API routes directly
- `shared/proxy/main.go` — absorbed into the Go daemon binary
- `store/` (OCaml sidecar) — stays as optional, or eventually gets a Go port too

## Key Tradeoff: All Impls Shell Out to `shared/bin/` Anyway

This is the most important observation. Every daemon implementation — Rust, Zig, Odin, CL, Janet — delegates mount, exec, network, and S3 operations to the shared shell scripts. The daemons are essentially HTTP API wrappers around `sq-exec`, `sq-mount-layer`, `sq-mount-overlay`, `sq-s3`, etc.

Given this, the daemon language choice matters less for correctness and more for:
1. **HTTP handling quality** (routing, auth, limits, JSON parsing)
2. **State management** (sandbox registry, concurrency)
3. **Ops** (signals, shutdown, recovery)
4. **Maintainability** (readability, contributor access, ecosystem)

Go is strong in all four, with the added benefit of absorbing the proxy.

## Historical Context

- **Feb 14, 2026**: Worktree scorecard ranked implementations. Rust was highest (4.7), then Zig (3.8).
- **Feb 15, 2026**: Main vs Janet vs Odin review recommended Janet or Odin for personal use, main for production.
- **Feb 22, 2026**: nanosquash implementation plan was written using Zig for the CLI wrapper.
- **Current**: Zig daemon is deployed on Fly.io. All impls are on main branch.

No prior research explicitly considered Go as a daemon language — Go was only used for the proxy.

## Code References

- `shared/proxy/main.go:1-633` — Existing Go proxy (potential merge target)
- `shared/bin/sq-exec:1-156` — Exec primitive all impls delegate to
- `shared/bin/sq-mount-layer:1-84` — Mount primitive
- `shared/bin/sq-mount-overlay:1-80` — Overlay primitive
- `shared/cgi-bin/common.sh:1-1165` — Shell CGI implementation
- `impl/zig/src/main.zig:1-201` — Currently deployed daemon entry
- `fly.toml` — Production deployment config (currently Zig)
- `Dockerfile.zig` — Production Docker build

## Related Research

- `thoughts/shared/research/2026-02-14-worktree-scorecard.md` — Implementation comparison scorecard
- `thoughts/shared/research/2026-02-15-main-vs-janet-vs-odin-review.md` — Language review
- `thoughts/shared/research/2026-02-22-squashclaw-synthesis-background.md` — Project synthesis background
- `thoughts/shared/plans/2026-02-22-nanosquash-implementation.md` — nanosquash plan (Zig CLI)

## Open Questions

1. Should the Go daemon absorb the proxy (single binary) or keep it separate?
2. Should S3 operations go native (`aws-sdk-go-v2`) or stay delegated to `sq-s3` shell script?
3. Should the OCaml store sidecar eventually get a Go port too, or remain separate?
4. Does the `nanosquash` CLI (planned in Zig) also move to Go?
5. How much of the `shared/bin/` scripts should migrate into Go vs stay as shell?
