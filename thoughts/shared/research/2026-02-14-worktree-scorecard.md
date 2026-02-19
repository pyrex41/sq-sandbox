# Worktree Scorecard (Rust, Lisp, Odin, Zig, Janet)

Scope: current heads as of 2026-02-14 after the Firecracker updates
(`rust:b894f67`, `cl:14153a3`, `odin:34d6de1`, `zig:f5b6902`, `janet:ce2320e`).

Scale: `0-5` where `5 = production-strong for this project`.

| Category | Rust | Zig | Odin | CL | Janet |
|---|---:|---:|---:|---:|---:|
| API hardening (auth/CT/limits/parsing) | 5 | 4 | 4 | 4 | 3 |
| Isolation correctness (mount/cgroup/netns/PID ns) | 5 | 4 | 4 | 3 | 2 |
| Recovery + lifecycle robustness | 4 | 4 | 3 | 3 | 2 |
| S3 integration parity | 5 | 3 | 4 | 4 | 4 |
| Proxy + secrets integration | 5 | 3 | 4 | 4 | 4 |
| Concurrency/state safety | 5 | 4 | 3 | 3 | 3 |
| Test depth | 4 | 5 | 3 | 3 | 1 |
| Ops readiness (startup/shutdown/signals) | 5 | 4 | 2 | 4 | 3 |
| Firecracker backend readiness | 4 | 3 | 3 | 2 | 2 |
| **Overall project readiness** | **4.7** | **3.8** | **3.3** | **3.3** | **2.7** |

## Snapshot

- **Rust**: still the strongest end-to-end implementation; Firecracker wiring is broad (create/exec/activate/snapshot/restore/destroy), with remaining gaps in backend-aware startup recovery and tap rule teardown completeness.
- **Zig**: now has a meaningful Firecracker path and solid host runtime hardening; main gap is backend-aware recovery semantics and Firecracker egress policy parity.
- **Odin**: major feature progress with Firecracker lifecycle support; still held back by ops readiness and command-construction safety in shell-out paths.
- **CL**: Firecracker support exists, but parity is lower due to chroot-shaped recovery assumptions and missing `allow_net` enforcement in Firecracker networking.
- **Janet**: big functionality jump with Firecracker support, but it remains the least production-ready due to limited test coverage and weaker recovery/network hardening.

## Most Important New Gaps (Post-Firecracker)

1. Make startup recovery backend-aware in all non-Rust branches (and in Rust's current init path) instead of always remounting chroot overlay state.
2. Normalize Firecracker network-policy behavior:
   - add real `allow_net` enforcement in Zig/CL/Janet Firecracker paths,
   - ensure teardown removes all per-sandbox rules (Rust currently removes only a subset).
3. Fix backend reporting parity in health endpoints (CL/Janet currently report `"chroot"` unconditionally).
4. Add Firecracker-focused integration tests (create/exec/activate/snapshot/restore/destroy + cleanup) for all non-Rust branches.
5. Remove shell-string command construction where possible (especially Odin/Janet paths) in favor of argv-style exec for safer argument handling.
