---
date: 2026-02-15T18:09:11+00:00
researcher: claude
git_commit: 34d6de1ed683d73c76420080f47017cb1a4fda28
branch: feat/odin
repository: sq-sandbox
topic: "Odin sq-sandbox deep dive: optimization, performance, and code quality"
tags: [research, codebase, odin, optimization, performance, binary-size]
status: complete
last_updated: 2026-02-15
last_updated_by: claude
---

# Research: Odin sq-sandbox Deep Dive — Optimization, Performance, Code Quality

**Date**: 2026-02-15T18:09:11+00:00
**Researcher**: claude
**Git Commit**: 34d6de1
**Branch**: feat/odin
**Repository**: sq-sandbox (worktree at /Users/reuben/projects/sq-sandbox-odin)

## Research Question

Deep dive into the Odin sq-sandbox implementation across 12 dimensions: binary size, memory allocation, HTTP performance, JSON efficiency, process spawning, string handling, error handling, dead code, syscall efficiency, compiler flags, code cleanliness, and thread pool design. Goal: identify concrete optimization opportunities to make this the leanest, cleanest, most performant implementation.

## Summary

The Odin implementation is 5,306 lines across 21 source files. At 838KB (unstripped ELF aarch64), it is already the smallest binary of the five implementations. The codebase demonstrates strong Odin idioms in several areas (per-sandbox arenas, tagged-union state machine, stack-allocated JSON builders) but has significant optimization opportunities in process spawning (`system()` calls), networking code duplication, dead code (sigv4/s3 modules), and missing compiler optimization flags.

## Detailed Findings

---

### 1. Binary Size Optimization

**Current**: 838 KB (ELF aarch64, unstripped, `src/test_squashd.bin`)

**Odin compiler flags for size reduction**:
- `-o:size` — optimize for binary size (may not be used currently)
- `-no-bounds-check` — removes bounds checking
- `-disable-assert` — removes assert code generation
- `-no-type-assert` — removes type assertion checks
- `-extra-linker-flags:"-s -Wl,--gc-sections"` — strip symbols + remove dead sections
- `-lto:thin` — link-time optimization (added in dev-2026-02)

**Estimated achievable size** (with all optimizations + strip):
- ~200-400 KB stripped with `-o:size`
- Community reports show 10-20x reduction from baseline with aggressive flags

**Key bloat source**: `core:encoding/json` pulls in RTTI (Run-Time Type Information). The codebase already uses manual JSON building for output (good), but still uses `json.unmarshal` for parsing input (5 call sites in `api.odin` and `firecracker.odin`). Replacing these with manual parsing would eliminate the json package dependency entirely.

**`core:fmt` usage**: Unavoidable for this codebase since `fmt.tprintf`/`fmt.sbprintf` are used throughout. The fmt package includes RTTI for all types used in the program.

**Files affected**: Build system (no Makefile/build script found in repo)

---

### 2. Memory Allocation Patterns

**Architecture**: Three-tier allocation strategy.

**Tier 1 — Per-sandbox arena** (`manager.odin:23-69`):
Each `Managed_Sandbox` owns a `mem.Dynamic_Arena`. All sandbox-related allocations (paths, metadata, mount arrays) use this arena via `managed_sandbox_allocator()`. On destroy, `mem.dynamic_arena_destroy()` frees everything at once. This is excellent — zero per-allocation free() calls, zero per-sandbox memory leaks.

**Tier 2 — Temp allocator** (`context.temp_allocator`):
Used for request-scoped allocations: JSON parsing, string splitting, path formatting. **However**, `free_all(context.temp_allocator)` is never explicitly called. In the HTTP handler path, each connection is handled by a worker thread, and temp allocations accumulate. The temp allocator grows monotonically within each worker thread.

**Tier 3 — Stack buffers**:
Fixed-size stack buffers for JSON response building (`[4096]byte`, `[16384]byte`, `[65536]byte`). Zero heap allocation in the response path.

**Issues identified**:
- `manager.odin:75-82` — `strings_clone` is a manual reimplementation of `strings.clone`. The standard library version handles the empty string case identically.
- No `free_all(context.temp_allocator)` call in the worker loop (`http.odin:180-188`). Each `handle_connection` call accumulates temp allocations. Should call `free_all(context.temp_allocator)` after each connection.
- `Dynamic_Arena` (used for per-sandbox) grows by allocating new blocks. Dynamic arrays within the arena (like `sqfs_mounts: [dynamic]Squashfs_Mount`) will leak old backing arrays within the arena on growth. This is fine since the arena frees everything at once, but wastes memory proportional to growth factor.

---

### 3. HTTP Server Performance

**Architecture** (`http.odin:137-178`):
- TCP accept loop on main thread
- Bounded channel (mutex + condvar, capacity 256) for connection handoff
- 8 worker threads consuming from channel
- Per-connection: read full request → parse → route → handle → respond → close
- No HTTP keep-alive (sends `Connection: close`)

**Performance characteristics**:
- `WORKER_POOL_SIZE = 8` is hardcoded. Should scale with CPU count or be configurable.
- `conn_channel_recv` does `ordered_remove(&ch.queue, 0)` which is O(n) — shifts all elements. For a FIFO queue this should use a ring buffer or `pop_front` pattern.
- `MAX_REQUEST_SIZE = 1MB` buffer allocated per connection via `context.temp_allocator` — this is fine but means each worker holds ~1MB while processing.
- Response headers built in 1024-byte stack buffer (`send_response`, `http.odin:502-504`). Adequate for typical responses.

**Bounded channel O(n) dequeue** (`http.odin:127`):
```odin
ordered_remove(&ch.queue, 0)  // Shifts all elements left
```
For 256-capacity queue under load, this copies up to 255 × sizeof(Conn_Work_Item) bytes per dequeue. Should be replaced with a ring buffer.

**Route matching** (`http.odin:425-483`):
Linear scan of 11 routes. Adequate for this scale. The fast path (`!strings.contains(pattern, "*")`) avoids segment splitting for exact matches.

---

### 4. JSON Parsing/Serialization Efficiency

**Input (parsing)**: Uses `core:encoding/json` via `json.unmarshal()` at 5 call sites:
- `api.odin:117` — Create request
- `api.odin:231` — Exec request
- `api.odin:361` — Snapshot request (optional body)
- `api.odin:430` — Restore request
- `api.odin:490` — Activate request
- `firecracker.odin:314` — Vsock response

All use `context.temp_allocator`, so no per-request leaks. But `json.unmarshal` is the heaviest part of the JSON dependency — it pulls in RTTI and reflection.

**Output (serialization)**: Manual string building into stack-allocated buffers via `strings.Builder`. This is optimal — zero heap allocation, predictable performance. Example pattern (`api.odin:654-676`):
```odin
buf: [4096]byte
b := strings.builder_from_bytes(buf[:])
_write_json_string(&b, sandbox.id)
// ...
```

**JSON string escaping** (`exec.odin:457-484`): Handles all required escapes (quotes, backslash, control chars < 0x20 with `\uXXXX`). Uses `fmt.sbprintf` for `\u` escapes which is slower than direct hex writing but only hits control characters.

**Opportunity**: Replace `json.unmarshal` with manual field extraction. The request structs are simple (5-7 fields each, all strings/numbers). A hand-rolled parser scanning for `"field":` patterns would eliminate the entire `core:encoding/json` dependency and its RTTI overhead.

---

### 5. Process Spawning and Exec Path

**Two distinct patterns**:

**Pattern A — `run_cmd()` via `system()`** (`init.odin:286-290`):
```odin
run_cmd :: proc(args: ..string) -> bool {
    cmd := strings.join(args[:], " ", context.temp_allocator)
    cstr := strings.clone_to_cstring(cmd, context.temp_allocator)
    return c_system(cstr) == 0
}
```
Used for: iptables (30+ calls), ip link/addr/netns (20+ calls), sysctl, mksquashfs, sq-firecracker, sq-s3, sq-mkbase, sq-mkvm. **This is the single biggest performance issue.** Each `system()` call:
1. Forks the daemon process
2. Execs `/bin/sh`
3. Shell parses the command string
4. Shell forks again to exec the actual command
5. Two waitpids

That's **2 forks + 2 execs per command**. For sandbox creation with netns, this means ~15 system() calls = ~30 forks.

**Pattern B — Direct fork/execve** (`exec.odin:118-297`):
The sandbox exec path uses direct `fork()` + `execve()` with pipe-based I/O capture. This is optimal. Double-fork for PID namespace isolation is correct.

**Opportunity**: Replace `system()` with direct `fork()/execvp()` or Odin's `os2.process_start()`. This would halve the fork count and eliminate shell parsing overhead.

**Also**: `_command_exists` (`init.odin:293-297`) shells out to `command -v` — could use `os.exists` on known PATH entries instead.

---

### 6. String Handling and Unnecessary Copies

**`fmt.tprintf` usage**: 100+ call sites. Each call allocates from `context.temp_allocator` by creating a new `strings.Builder`. For paths used repeatedly (like `modules_dir`, `sandboxes_dir`), these are recomputed every call:

```odin
modules_dir :: proc(c: ^Config) -> string {
    return fmt.tprintf("%s/modules", c.data_dir)  // Allocates every call
}
```

These path helpers (`config.odin:88-102`) are called from hot paths (every API request). Should be computed once at config creation time and stored as fields.

**`fmt.ctprintf`** (`mounts.odin:31-33`): `_to_cstr` uses `strings.clone_to_cstring(s, context.temp_allocator)` — allocates and copies. Used in mount paths which are called once per sandbox creation, so not a hot-path issue.

**String joins in `run_cmd`**: Every `run_cmd` call does `strings.join(args[:], " ")` which allocates a new string. With 30+ calls during sandbox creation, that's 30+ temp allocations for command strings.

**`_require_json`** (`api.odin:685-693`): Calls `strings.to_lower(req.content_type)` which allocates a lowercase copy. Should use `strings.equal_fold` or `strings.has_prefix` with case-insensitive comparison instead (the Content-Type check is already using `has_prefix` but on the lowered copy).

---

### 7. Error Handling Patterns

**Excellent use of Odin idioms**:
- Enum-based error types throughout: `Create_Error`, `Mount_Error`, `Netns_Error`, `Cgroup_Error`, `Meta_Error`, `Secrets_Error`, `Exec_Error`, `Firecracker_Error`
- `or_return` for chained metadata writes (`meta.odin:37-78`)
- Conditional defer for rollback: `defer if err != .None { cleanup() }` (`sandbox.odin:132-222`)

**Consistent pattern**: All errors are enums with `.None` as the zero/success value. This enables clean switch exhaustiveness checking.

**One inconsistency**: `run_cmd` returns `bool` (true/false) while all other functions return typed error enums. Since `run_cmd` wraps `system()`, it loses the exit code. The Firecracker module uses `run_cmd` but returns `Firecracker_Error` — the error detail (which command failed, what exit code) is lost.

---

### 8. Dead Code Elimination Opportunities

**`s3.odin`** (1 line): Contains only `package squashd`. The S3 client is not implemented. All S3 operations shell out to `sq-s3` CLI via `run_cmd`. Dead file.

**`sigv4.odin`** (236 lines): Complete AWS SigV4 signing implementation. Never called — no S3 HTTP client exists to use it. The entire file is dead code. Removing it would reduce source by ~4.5% and eliminate `core:crypto/hash` and `core:crypto/hmac` imports from the binary.

**`Firecracker_Handle`** (`firecracker.odin:34-37`): Struct defined but never instantiated. CID and tap_index are stored in metadata files instead.

**Unused imports**: Need to verify, but `core:strconv` is imported in `config.odin` and `firecracker.odin` — both use it, so these are valid.

**`_write_meta_file`** return value: In `sandbox.odin:352`, the return value is discarded with `_ = _write_meta_file(...)`. Inconsistent with `meta.odin` where `or_return` is used.

---

### 9. Syscall Efficiency

**Direct syscalls** (optimal):
- `mount/umount2` — direct libc FFI (`linux_extra.odin`)
- `fork/execve/chroot/chdir/unshare/setns` — direct libc FFI (`exec.odin`)
- `pipe/read/poll/waitpid/kill/dup2` — direct libc FFI (`exec.odin`)
- `flock` — direct libc FFI (`linux_extra.odin`)
- `open` via `syscall()` — workaround for namespace conflicts (`linux_extra.odin:40-43`)

**Indirect via shell** (suboptimal):
- All iptables commands — via `system()` (15+ calls per netns setup/teardown)
- All ip link/addr/route/netns commands — via `system()` (10+ calls per netns setup)
- mksquashfs — via `system()`
- rm -rf — via `system()` for directory removal
- sq-firecracker CLI — via `system()`
- sq-s3, sq-mkbase, sq-mkvm — via `system()`

**`c_open` implementation** (`linux_extra.odin:40-43`): Uses `c_syscall(SYS_OPEN, ...)` raw syscall to avoid conflicts with `core:sys/posix` open declaration. On Linux aarch64, `SYS_OPEN` is syscall 2 (correct for x86_64 but **wrong for aarch64** where it's `openat` at 56, not `open` at 2). This is a **potential bug on aarch64 Linux**.

**`O_CLOEXEC` value** (`linux_extra.odin:52`): Hardcoded as `0o2000000` which is correct for Linux x86_64 but architecture-dependent. On some architectures it differs.

---

### 10. Compiler Flags and Build Optimization

**No build script found** in the repository. The existing binary `src/test_squashd.bin` (102,592 bytes on macOS) suggests a test build, not a production build.

**Recommended production build command**:
```bash
odin build src -target=linux_arm64 \
    -o:size \
    -no-bounds-check \
    -disable-assert \
    -no-type-assert \
    -lto:thin \
    -extra-linker-flags:"-s -Wl,--gc-sections"
```

**Expected impact**:
- `-o:size` — significant size reduction (compiler optimizes for code size over speed)
- `-no-bounds-check` — removes all bounds checks (safe for production after testing)
- `-disable-assert` — removes assert code paths
- `-lto:thin` — cross-module optimization (new in dev-2026-02)
- `-s` linker flag — strips all symbols
- `--gc-sections` — removes unreferenced code sections (dead code from imported packages)

**Cross-compilation note**: Building from macOS to Linux requires either native Linux build or manual object file linking. The Odin compiler can generate object files (`-build-mode:object`) but linking requires target toolchain.

---

### 11. Code Cleanliness — Duplication and Inconsistencies

**Networking code duplication** between `netns.odin` and `firecracker.odin`:

Both implement nearly identical iptables egress filtering:
- `netns.odin:240-282` (`_apply_egress_rules`) — creates chain, ICMP drop, DNS rate limit, established/related, per-host allow, default drop
- `firecracker.odin:129-152` (inline in `firecracker_setup_network`) — same exact pattern

Both implement DNS DNAT rules:
- `netns.odin:82-89` — DNS forwarding via iptables
- `firecracker.odin:117-125` — same pattern

**Should extract**: A shared `apply_egress_filter(chain, interface, hosts)` and `apply_dns_dnat(subnet, gateway, dns)` function.

**Networking teardown duplication**:
- `netns_teardown` (`netns.odin:108-144`) and `firecracker_teardown_network` (`firecracker.odin:158-187`) both remove iptables chains, NAT rules, DNS DNAT rules. Different IP schemes (10.200.x vs 10.0.x) but identical structure.

**Pattern: `_is_mounted` called in a loop** (`init.odin:421`):
`_find_mounted_images` reads `/proc/mounts` for every image to check if it's mounted. Each call re-reads the entire file. Should read `/proc/mounts` once and check all images against the parsed set.

**Inconsistent path helpers**:
- `config.odin:88-102` — `modules_dir`, `sandboxes_dir`, `secrets_path`, `proxy_ca_dir` all use `fmt.tprintf` (temp allocator)
- `manager.odin:174-176` — `manager_sandbox_dir` also uses `fmt.tprintf`
- These return temp-allocated strings that can become invalid after `free_all`

**`_ensure_dir_recursive`** (`secrets.odin:176-188`): Manually walks path bytes to find `/` separators. Could use `os.make_directory_all` if available, or at minimum use `strings.split` for clarity. Also defined in secrets.odin but used from sandbox.odin, init.odin, and meta.odin — the compiler handles this since they're all in the same package, but it's an odd placement.

**Global state** (`api.odin:10-11`):
```odin
_api_config: ^Config
_api_manager: ^Sandbox_Manager
```
These package-level globals are set once in `api_init` and read by all handlers. This works but means handlers can't be tested in isolation without setting these globals. The Zig and Rust implementations pass these through handler context instead.

**Snapshot compression** (`snapshot.odin:28`): Uses `-comp gzip` while the README and other implementations use zstd. Inconsistency.

---

### 12. Thread Pool and Concurrency Model

**Custom bounded channel** (`http.odin:74-131`):
Hand-rolled MPSC channel using mutex + two condvars (not_empty, not_full). This is correct but has the O(n) dequeue issue noted above.

**Odin stdlib alternative**: `core:thread` provides `thread.Pool` with `pool_add_task` / `pool_pop_done`. However, the HTTP accept-loop pattern doesn't map cleanly to the task pool API (which is designed for batch work, not streaming connections). The custom channel is a reasonable choice.

**Concurrency model**:
- Global lock (`manager.global_lock`) protects the sandbox map (insert/delete/lookup)
- Per-sandbox lock (`Managed_Sandbox.lock`) serializes operations on the same sandbox
- This allows concurrent operations across different sandboxes

**Potential issue**: `manager_get` acquires and releases the global lock, then callers acquire the per-sandbox lock separately. Between these two lock operations, another thread could destroy the sandbox. This is a classic TOCTOU issue. In practice, the API handlers check for nil after `manager_get` and the only destroyer is the explicit DELETE handler or reaper, so races are unlikely but theoretically possible.

**Reaper thread** (`reaper.odin:26-68`):
Clean two-phase design: snapshot expired IDs under global lock, destroy outside lock. Uses `manager_remove` which is the deprecated path — should use `manager_destroy_sandbox` for consistency with API handlers (which does directory cleanup too).

---

## Optimization Priority Matrix

| # | Area | Impact | Effort | Priority |
|---|------|--------|--------|----------|
| 1 | Replace `system()` with direct fork/exec | High perf (halves forks) | Medium | P0 |
| 2 | Add `-o:size` + strip to build | High size (~50% reduction) | Low | P0 |
| 3 | Remove dead code (sigv4.odin, s3.odin) | Medium size + clarity | Low | P0 |
| 4 | Add `free_all(context.temp_allocator)` in worker loop | High (memory leak fix) | Low | P0 |
| 5 | Cache path helpers (modules_dir etc.) | Medium perf | Low | P1 |
| 6 | Fix ring buffer for conn channel | Medium perf under load | Medium | P1 |
| 7 | Extract shared networking helpers | Medium clarity | Medium | P1 |
| 8 | Replace json.unmarshal with manual parsing | Medium size | High | P2 |
| 9 | Fix `SYS_OPEN` for aarch64 | Critical correctness | Low | P0 |
| 10 | Fix snapshot compression (gzip→zstd) | Correctness | Low | P1 |
| 11 | Replace reaper's `manager_remove` with `manager_destroy_sandbox` | Correctness | Low | P1 |
| 12 | Cache `/proc/mounts` in `_find_mounted_images` | Low perf (init only) | Low | P2 |
| 13 | Fix `_require_json` to avoid alloc | Low perf | Low | P2 |
| 14 | Empty execve environment (like CL bug) | Correctness | Low | P0 |

---

## Code References

- `src/main.odin:1-24` — Entry point, 24 lines
- `src/config.odin:36-81` — Config from env, path helpers
- `src/api.odin:1-807` — All HTTP handlers, JSON request/response
- `src/http.odin:1-604` — TCP server, thread pool, HTTP parser, router
- `src/exec.odin:1-557` — Fork/execve sandbox exec, I/O capture, logging
- `src/manager.odin:1-467` — Sandbox lifecycle, arena management, recovery
- `src/sandbox.odin:1-423` — State machine, create, destroy, Firecracker create/destroy
- `src/mounts.odin:1-310` — squashfs/tmpfs/overlay mount operations
- `src/netns.odin:1-282` — Network namespace setup/teardown, egress filtering
- `src/firecracker.odin:1-398` — Firecracker VM backend (CID, tap, vsock)
- `src/cgroup.odin:1-79` — Cgroup v2 create/destroy/add-process
- `src/init.odin:1-448` — First-boot init, sandbox remounting, utility helpers
- `src/secrets.odin:1-188` — Secret placeholder injection, CA cert injection
- `src/meta.odin:1-133` — Metadata file read/write
- `src/sigv4.odin:1-236` — **DEAD CODE** — complete SigV4 impl, never called
- `src/s3.odin:1` — **DEAD CODE** — empty file
- `src/linux_extra.odin:1-52` — Libc FFI (mount, umount2, system, open, flock)
- `src/modules.odin:1-44` — Module listing
- `src/snapshot.odin:1-36` — Snapshot create (mksquashfs)
- `src/validate.odin:1-49` — ID/label/module validation
- `src/reaper.odin:1-68` — Background expiration thread

## Architecture Documentation

**State machine** (`sandbox.odin:15-38`): Tagged union `Sandbox_State` with variants `Creating`, `Ready`, `Executing`, `Snapshotting`, `Destroying`. State-specific data (mounts, netns, cgroup) lives inside `Ready` variant — only accessible when sandbox is ready. Compiler enforces exhaustive switching.

**Memory model**: Three-tier (per-sandbox arena, temp allocator, stack buffers). Arenas provide bulk deallocation on sandbox destroy. Temp allocator handles request-scoped work. Stack buffers eliminate heap allocation in JSON response paths.

**Concurrency model**: Global lock + per-sandbox locks. Accept loop → bounded channel → worker thread pool. No async I/O — all blocking.

**Process model**: Main process is the HTTP server. Sandbox commands run as fork+execve children in separate namespaces. External tools (iptables, ip, mksquashfs) run via system().

## Open Questions

1. **Build system**: No Makefile or build script found. How is the binary currently built? What flags are used?
2. **aarch64 syscall numbers**: Is `SYS_OPEN = 2` correct for Linux aarch64? (It's x86_64; aarch64 uses `openat` at 56)
3. **`O_CLOEXEC` portability**: Is `0o2000000` correct across all target architectures?
4. **Temp allocator lifetime**: With 8 worker threads and no `free_all`, what's the steady-state memory usage under sustained load?
5. **Child process environment**: `exec.odin:367` passes `envp = [1]cstring{nil}` — empty environment. Was this intentional? The CL implementation had this as a critical bug (no PATH, HOME, etc.).
