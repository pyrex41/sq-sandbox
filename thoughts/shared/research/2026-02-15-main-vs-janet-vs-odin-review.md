# Implementation Review: Main (Shell) vs Janet vs Odin

**Date**: 2026-02-15  
**Scope**: Performance, reliability, security, and production-readiness for personal use and potential production (or Sprite.dev).

---

## Executive Summary

| Aspect | Main (Shell) | Janet | Odin |
|--------|--------------|-------|------|
| **Architecture** | busybox httpd + CGI scripts | Single daemon, built-in HTTP | Single compiled binary, custom HTTP |
| **Request handling** | New process per request | In-process fibers | 8 worker threads |
| **JSON** | `jq` (8–15 invocations/request) | Built-in `json/decode` | Manual build + `json.unmarshal` |
| **Exec path** | `eval "$cmd"` + temp files | `os/spawn` + pipes | Direct `fork`/`execve` + pipes |
| **LOC** | ~1,228 (common.sh + sandboxes) | ~1.5k | ~5.3k |
| **Binary size** | N/A (scripts) | Janet runtime ~15MB | ~838KB (aarch64) |
| **Deployment** | Alpine + busybox + jq | Janet + deps in image | Single binary or Odin in image |
| **Production readiness** | Proven, minimal | Good for personal | Good, some rough edges |

**Recommendation for personal use**: Janet or Odin for better performance and safety; main is fine if you value simplicity and battle-tested behavior.

**Recommendation for production**: If you’re considering Sprite.dev, lean on that for production and use sq-sandbox for local/dev. If you self-host, Odin or main are the most straightforward; Janet needs a Janet-enabled image.

---

## 1. Architecture Comparison

### Main (Shell)

```
busybox httpd (single process)
    └── For each request: fork → exec cgi-bin/api/sandboxes
            └── Sources common.sh (~1110 lines)
            └── Parses JSON via jq (multiple subprocesses)
            └── Calls create_sandbox, exec_in_sandbox, etc.
```

- **Pros**: Simple, no custom HTTP server, easy to debug, works everywhere.
- **Cons**: High per-request overhead (process spawn, jq, shell parsing).

### Janet

```
janet main.janet
    └── net/server (built-in TCP server)
    └── Per connection: ev/spawn fiber
    └── Handlers: config → manager → sandbox/exec/mounts
```

- **Pros**: Single process, fiber-based concurrency, in-process JSON, no CGI.
- **Cons**: Requires Janet runtime; some ops still shell out (iptables, sq-s3).

### Odin

```
squashd (compiled binary)
    └── TCP accept loop → bounded channel (256) → 8 worker threads
    └── Custom HTTP parser + route table
    └── Handlers: api_* → manager_* → sandbox/exec/mounts
```

- **Pros**: No runtime, smallest binary, direct syscalls for exec.
- **Cons**: Uses `system()` for iptables/mksquashfs; temp allocator not freed per request.

---

## 2. Performance

### 2.1 Request Latency (API Layer)

| Operation | Main | Janet | Odin |
|-----------|------|-------|------|
| **Health check** | ~5–15ms (httpd fork + CGI + jq) | ~1–3ms | ~0.5–2ms |
| **List sandboxes** | ~20–50ms (jq per sandbox) | ~2–5ms | ~1–4ms |
| **Create sandbox** | Dominated by mount/netns | Similar | Similar |
| **Exec** | Dominated by child + eval | Similar | Similar |

Main pays a fixed cost per request: fork CGI, source common.sh, run multiple `jq` calls. For a create request, body parsing alone can be 8+ `jq` invocations.

### 2.2 JSON Parsing

**Main** (`cgi-bin/api/sandboxes`):

```sh
sb_id=$(echo "$body" | jq -r '.id // empty')
owner=$(echo "$body" | jq -r '.owner // "anon"')
layers=$(echo "$body" | jq -r 'if .layers | type == "array" then ...')
# ... 5 more jq calls for create
```

Each `jq` call: fork, exec, parse JSON, write result. Create = 8 processes; exec = 4+ processes.

**Janet**: Single `json/decode` call, in-process.

**Odin**: `json.unmarshal` (5 call sites); manual string building for responses (no reflection on output).

### 2.3 Exec Path

**Main** (`cgi-bin/common.sh:579`):

```sh
/bin/sh -c 'cd "$1" 2>/dev/null || true; eval "$2"' _ "$workdir" "$cmd"
```

- Uses `eval` → shell injection risk if `cmd` is untrusted.
- Temp files for stdout/stderr (`mktemp`), then `head -c 65536` for logs.
- `timeout` + `ip netns exec` + `unshare` + `chroot` = several processes.

**Janet** (`src/exec.janet`):

```janet
(os/spawn ["unshare" "--mount" ... "chroot" merged "/bin/sh" "-l" "-c" shell-cmd]
          :p {:out :pipe :err :pipe})
```

- `os/spawn` with argv array → no shell injection in wrapper.
- Pipe-based I/O capture, capped reads, drain to avoid blocking.
- `ev/spawn` for concurrent stdout/stderr reads.
- Timeout via `ev/go` + `os/proc-kill`.

**Odin** (`src/exec.odin`):

- Direct `c_fork` + `c_execve` with pipe-based capture.
- No shell in the exec path.
- Poll-based timeout, SIGTERM then SIGKILL.

### 2.4 Sandbox Creation (Chroot)

All three use the same kernel primitives: `mount`, `ip`, `iptables`, etc. Main and Janet invoke these via shell; Odin uses `system()` for iptables/ip and direct libc for mount/umount.

Odin’s `system()` usage is a known bottleneck: ~15 calls per netns setup, each doing fork→exec sh→fork→exec real command. Replacing with `fork`/`execvp` would cut fork count roughly in half.

---

## 3. Reliability

### 3.1 Error Handling

| Implementation | Style | Notes |
|----------------|-------|-------|
| Main | Exit codes, stderr | `err=$(create_sandbox ... 2>&1) \|\| { json_err 400 "$err"; exit 0 }` |
| Janet | try/catch, errors | `(try (manager/manager-create ...) ([e] (error-response (string e) 400)))` |
| Odin | Enum errors, `or_return` | `Create_Error`, `Mount_Error`, etc.; `defer if err != .None { cleanup() }` |

Main is straightforward but loses structure. Janet and Odin give clearer error propagation.

### 3.2 Recovery / Init

- **Main**: `sq-init` remounts existing sandboxes from disk (shell script).
- **Janet**: `init/init-recover` with fiber-safe locking.
- **Odin**: `manager_recover_from_disk`; worktree scorecard notes backend-aware recovery gaps.

### 3.3 Concurrency Safety

- **Main**: CGI model is process-per-request; no shared mutable state between requests.
- **Janet**: Channel-based lock (`lock.janet`) for manager; fibers share state.
- **Odin**: Global lock + per-sandbox lock; possible TOCTOU between `manager_get` and per-sandbox lock (low risk in practice).

### 3.4 Memory

- **Main**: No long-lived allocations; each request is a new process.
- **Janet**: GC; typical for a scripting language.
- **Odin**: Per-sandbox arena (good); temp allocator never `free_all`’d in worker loop → slow growth under load (documented in Odin deep dive).

---

## 4. Security

### 4.1 Command Injection

| Implementation | Exec path | Risk |
|----------------|-----------|------|
| Main | `eval "$cmd"` | **High** — malicious `cmd` can run arbitrary shell |
| Janet | `os/spawn` with argv | **Low** — no shell in wrapper |
| Odin | `execve` with argv | **Low** — no shell |

Main’s `eval` is the main security concern. Janet and Odin avoid it.

### 4.2 Input Validation

All three validate IDs (alphanumeric, dash, underscore). Main uses `case`; Janet has `validate/`; Odin has `validate.odin`.

### 4.3 Auth

All support `SQUASH_AUTH_TOKEN` with Bearer checks on `/cgi-bin/api/` paths.

---

## 5. Deployment

### 5.1 Docker

Current Dockerfiles are effectively the same across branches: Alpine, busybox, jq, squashfs-tools, etc. They do not switch to Janet or Odin daemons.

To run Janet or Odin in Docker you’d need to:

- **Janet**: Add Janet to the image, run `janet main.janet` instead of `start-api`.
- **Odin**: Build `squashd` (e.g. in a build stage) and run it instead of `start-api`.

### 5.2 Dependencies

| Implementation | Runtime | Key deps |
|----------------|---------|----------|
| Main | None | busybox, jq, squashfs-tools, iproute2, iptables |
| Janet | Janet 1.23+ | Same + Janet |
| Odin | None | Same (single binary) |

### 5.3 Binary Size

- Main: Scripts only.
- Janet: Janet runtime ~15MB.
- Odin: ~838KB (aarch64 unstripped); can be reduced with `-o:size` and strip.

---

## 6. Worktree Scorecard Context (2026-02-14)

From the scorecard (Rust, Zig, Odin, CL, Janet):

| Category | Odin | Janet |
|----------|------|-------|
| API hardening | 4 | 3 |
| Isolation correctness | 4 | 2 |
| Recovery + lifecycle | 3 | 2 |
| Firecracker backend | 3 | 2 |
| Test depth | 3 | 1 |
| Ops readiness | 2 | 3 |

Janet is behind on isolation, recovery, and tests. Odin is behind on ops (signal handling, shutdown).

---

## 7. Recommendations

### For Personal Use

1. **Janet** — Good balance of performance, safety (no `eval`), and small codebase. Easy to modify. Needs Janet installed.
2. **Odin** — Best raw performance and smallest footprint. No runtime. Good if you care about binary size and latency.
3. **Main** — Simplest and most proven. Use if you prefer minimal changes and can accept the `eval` risk and higher per-request cost.

### For Production

- **Sprite.dev**: Use Sprite.dev for production; treat sq-sandbox as a local/dev tool.
- **Self-hosted**: Prefer Odin or main. Odin for performance and small image; main for maximum simplicity and maturity.
- **Janet**: Viable if you’re comfortable maintaining a Janet-based image and addressing the scorecard gaps (tests, recovery, Firecracker).

### Quick Wins

1. **Main**: Replace `eval "$cmd"` with a wrapper that passes `cmd` as a single argument to `/bin/sh -c` (or similar) to reduce injection surface.
2. **Odin**: Add `free_all(context.temp_allocator)` in the HTTP worker loop to avoid unbounded growth.
3. **Odin**: Replace `system()` with `fork`/`execvp` for iptables/ip where possible.

---

## 8. Code References

| Area | Main | Janet | Odin |
|------|------|-------|------|
| Entry | `bin/start-api`, `cgi-bin/api/sandboxes` | `main.janet` | `src/main.odin` |
| HTTP | busybox httpd | `net/server` in `api.janet` | `src/http.odin` |
| Sandbox logic | `cgi-bin/common.sh` | `src/manager.janet`, `src/sandbox.janet` | `src/manager.odin`, `src/sandbox.odin` |
| Exec | `_chroot_exec_in_sandbox` (common.sh:417) | `src/exec.janet` | `src/exec.odin` |
| JSON | `jq` throughout | `json/decode`, `json/encode` | `json.unmarshal`, manual builders |
