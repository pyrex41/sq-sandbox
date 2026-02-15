---
date: 2026-02-13T19:41:45Z
researcher: reuben
git_commit: bd16cc4
branch: main
repository: sq-sandbox
topic: "Security model comparison: Squash Sandbox vs Deno Sandbox vs NanoClaw"
tags: [research, codebase, security, sandbox, deno, nanoclaw, isolation]
status: complete
last_updated: 2026-02-13
last_updated_by: reuben
---

# Research: Security Model Comparison — Squash Sandbox vs Deno Sandbox vs NanoClaw

**Date**: 2026-02-13T19:41:45Z
**Researcher**: reuben
**Git Commit**: bd16cc4
**Branch**: main
**Repository**: sq-sandbox

## Research Question

Compare the Squash Sandbox security model to Deno's sandbox (deno.com/blog/introducing-deno-sandbox) and NanoClaw (github.com/qwibitai/nanoclaw).

## Summary

All three projects sandbox code execution for AI agents, but they occupy fundamentally different points in the design space:

| Dimension | Squash Sandbox | Deno Sandbox | NanoClaw |
|-----------|---------------|--------------|----------|
| **Isolation primitive** | Linux kernel (overlayfs + unshare + cgroups) or Firecracker microVM | Lightweight Linux microVMs in Deno Deploy cloud | OS containers (Apple Container / Docker) |
| **Granularity** | Per-sandbox (whole environment) | Per-subprocess (Deno runtime sandbox) | Per-group (one container per chat group) |
| **Network control** | Per-sandbox host whitelist via iptables | Outbound proxy (httpjail-style) with host whitelist | Inherits container defaults (no explicit policy) |
| **Secret handling** | Placeholder substitution proxy (sq-secret-proxy) | Placeholder substitution proxy (same pattern) | Environment variable filtering (allowlist) |
| **Resource limits** | cgroups v2 (chroot) / VM config (firecracker) | Native VM limits (2 vCPU, 768MB-4GB, 30min max) | Container timeout + idle detection |
| **Filesystem** | Read-only squashfs layers + writable tmpfs overlay | Ephemeral VM filesystem | Per-group mount with read-only global |
| **Target use** | Internal agents, CI, dev environments | SaaS product (Deno Deploy customers) | Personal AI assistant (single user) |
| **Runs on** | Any Linux host or privileged container | Deno's cloud infrastructure only | macOS (Apple Container) or Linux (Docker) |

## Detailed Findings

### 1. Squash Sandbox Security Model

Squash Sandbox provides infrastructure-level isolation with two backends behind an identical API.

#### 1a. Chroot Backend — Process Isolation

The chroot backend uses Linux kernel primitives layered together:

**Namespace isolation** (`cgi-bin/common.sh:352-356`):
```sh
unshare --mount --pid --fork --map-root-user \
    chroot "$s/merged" \
    /bin/sh -c "cd $workdir 2>/dev/null || true; $cmd"
```
- `--mount`: Separate mount namespace — sandbox cannot see host mounts
- `--pid`: Separate PID namespace — sandbox sees only its own processes
- `--fork`: Fork into new namespace (required for PID namespace)
- `--map-root-user`: Map UID 0 in sandbox to unprivileged user outside
- No `--net` in the unshare call itself; network isolation is handled by a separate veth pair + iptables setup at sandbox creation time

**Filesystem isolation** — overlayfs with read-only squashfs layers (`cgi-bin/common.sh:134-141`):
- Base OS and modules are immutable squashfs images, loop-mounted read-only
- Writes land in a `tmpfs` upper layer
- The sandbox sees a merged view via overlayfs
- Destroying the sandbox wipes the upper layer; base images are untouched

**Resource limits** via cgroups v2 (`cgi-bin/common.sh:150-165`):
- CPU: `cpu.max` quota/period (e.g., 1.0 CPU = 100000/100000 microseconds)
- Memory: `memory.max` in bytes, enforced with OOM-kill
- Lifetime: `sq-reaper` background loop checks ages every 10s, destroys expired sandboxes (`bin/sq-reaper:9-30`)

**Network egress control** (`cgi-bin/common.sh:175-264`):
- Each sandbox with `allow_net` set gets a veth pair connecting it to the host
- Per-sandbox iptables chain (`squash-<id>`) with:
  - DNS (UDP/TCP port 53) always allowed
  - Established/related connections allowed
  - Each allowed host resolved via `getent hosts`, IPs added as ACCEPT rules
  - Default policy: DROP
- NAT via MASQUERADE for outbound traffic
- Cleanup removes iptables chains and veth interfaces on destroy

**Secret materialization** (`bin/sq-secret-proxy`):
- socat-based HTTP proxy on port 8888
- Per-connection: reads HTTP request, scans headers for placeholder strings
- Replacement happens only when destination host matches `allowed_hosts` for that secret
- Real credentials never enter the sandbox process or filesystem
- Current limitation: HTTP only; HTTPS CONNECT tunneling needs a compiled binary

#### 1b. Firecracker Backend — VM Isolation

The Firecracker backend provides hardware-level isolation via microVMs:

**VM lifecycle** (`bin/sq-firecracker`):
- Each sandbox is a separate Firecracker process with its own kernel, memory, and vCPUs
- Guest boots a minimal Alpine rootfs with a custom init (`vm/init`) as PID 1
- Squashfs layers passed as virtio block devices (vdb, vdc, ...)
- Guest mounts them as overlayfs, same pattern as chroot mode
- Communication via vsock (port 5000) — no shared filesystem with host

**Resource limits** — native to the VM:
- vCPU count and memory set via Firecracker machine config API
- No cgroups needed; the hypervisor enforces limits

**Network** — tap device per VM (`cgi-bin/common.sh:507-548`):
- Same iptables egress rules as chroot mode, but applied to `sq-<id>-tap` interface
- Same NAT/MASQUERADE pattern

**Isolation boundary**: The Firecracker VMM is the isolation boundary. The guest has its own kernel, its own PID space, its own filesystem, and communicates with the host only via vsock. This is a stronger boundary than namespace isolation.

#### 1c. API Authentication

Bearer token authentication (`cgi-bin/common.sh:39-45`):
- Optional `SQUASH_AUTH_TOKEN` environment variable
- If set, all API requests must include `Authorization: Bearer <token>`
- If unset, API is unauthenticated (designed for internal/Tailscale-protected networks)

### 2. Deno Sandbox Security Model

Deno provides two distinct security layers that should not be confused:

#### 2a. Deno Runtime Permissions (the `--allow-*` system)

This is Deno's long-standing permission model for the `deno` CLI:

- **Deny-by-default**: Programs have no I/O access unless explicitly granted
- **Permission flags**: `--allow-read`, `--allow-write`, `--allow-net`, `--allow-env`, `--allow-run`, `--allow-ffi`, `--allow-sys`
- **Granular scoping**: Paths, hosts, ports, specific env vars, specific executables
- **Deny overrides**: `--deny-net=evil.com` takes precedence over `--allow-net`
- **Runtime prompts**: Interactive permission grants when running in a TTY
- **Permission broker**: External process can make grant/deny decisions via Unix socket (`DENO_PERMISSION_BROKER_PATH`)
- **Audit logging**: `DENO_AUDIT_PERMISSIONS` generates JSONL logs of all permission accesses

This is an **application-level** permission system — it controls what the Deno runtime allows JavaScript/TypeScript code to do. It operates within a single process and relies on the Deno runtime (Rust + V8) to intercept and gate syscalls.

**Key limitation**: `--allow-run` grants subprocess access, and subprocesses inherit OS-level permissions, bypassing the Deno sandbox entirely. Similarly, `--allow-ffi` loads native code that runs outside the sandbox.

#### 2b. Deno Sandbox (the cloud product)

The blog post at deno.com/blog/introducing-deno-sandbox describes a separate product: sandboxed execution environments in Deno Deploy's cloud. Key disclosed details:

- **Lightweight Linux microVMs** with sub-second boot
- **Resource limits**: 2 vCPUs, 768MB-4GB memory, 30-minute maximum lifetime
- **Network egress**: Outbound proxy (similar to coder/httpjail) as chokepoint for policy enforcement
- **Secret protection**: Placeholder substitution — "real key materializes only when the sandbox makes an outbound request to an approved host"
- **Cloud-only**: Runs on Deno's infrastructure, not self-hostable

The blog post explicitly references coder/httpjail for the network proxy pattern. The Squash Sandbox design doc (`docs/plan-firecracker-and-security.md:1135`) also cites both Deno Sandbox and coder/httpjail as references, confirming that Squash's secret proxy and egress control were directly inspired by this approach.

### 3. NanoClaw Security Model

NanoClaw is a personal AI assistant framework where Claude Code agents run inside OS containers.

#### 3a. Container Isolation

- **Apple Container** (macOS) or **Docker** (Linux) — the entire agent process is containerized
- Each chat group gets its own container with its own filesystem
- The container is the entire security boundary — there are no application-level permission checks
- Bash commands, tool execution, file operations all happen inside the container

#### 3b. Filesystem Isolation

Mount strategy enforces per-group boundaries:
- **Main group**: Full project root at `/workspace/project` (read-write) + group folder + global memory (read-only)
- **Non-main groups**: Only their own folder (read-write) + global memory (read-only)
- No cross-group filesystem access
- Per-group `.claude/` sessions directory
- Per-group IPC namespace (messages, tasks, input subdirectories)

#### 3c. Credential Handling

- Environment variable filtering: Only `CLAUDE_CODE_OAUTH_TOKEN` and `ANTHROPIC_API_KEY` are exposed from `.env`
- No proxy-based secret materialization
- No per-host credential scoping

#### 3d. Resource Management

- Hard timeout per group (configurable)
- Idle timeout with activity detection (output markers in stdout)
- Output truncation (capped stdout/stderr size)
- No CPU or memory limits beyond container defaults

#### 3e. Network

- Inherits container runtime defaults — no explicit network policy
- No per-container egress filtering
- No network namespace customization

## Comparison Analysis

### Isolation Strength Spectrum

```
Weakest                                                    Strongest
   |                                                          |
   Deno --allow-*        NanoClaw          Squash chroot    Squash Firecracker
   (process-level        (OS container)    (namespaces +    (microVM +
    runtime checks)                         cgroups +        separate kernel)
                                            iptables)
                                                           Deno Sandbox cloud
                                                           (microVM, but
                                                            not self-hostable)
```

- **Deno `--allow-*`** is application-level — enforced by the Deno runtime, not the kernel. A bug in Deno or use of `--allow-run`/`--allow-ffi` breaks the boundary.
- **NanoClaw** delegates to Docker/Apple Container — kernel-enforced, but with default networking and no resource limits beyond timeouts.
- **Squash chroot** uses multiple kernel primitives (namespaces, cgroups, iptables) composed together. Weaker than a VM (shared kernel) but stronger than a simple container.
- **Squash Firecracker** and **Deno's cloud sandbox** both use microVMs — separate kernel, hardware-enforced isolation. The strongest boundary available.

### Network Egress Control

| Feature | Squash | Deno (cloud) | NanoClaw |
|---------|--------|-------------|----------|
| Per-sandbox host whitelist | Yes (iptables) | Yes (outbound proxy) | No |
| DNS always allowed | Yes | Not disclosed | N/A |
| Wildcard host matching | Warned, not implemented | Not disclosed | N/A |
| Block all egress | `allow_net=["none"]` | Not disclosed | N/A |
| Implementation | iptables chains per sandbox | httpjail-style proxy | None |

Squash and Deno's cloud sandbox solve the same problem (egress filtering) with different mechanisms. Squash uses iptables at the network layer; Deno uses an HTTP proxy at the application layer. The proxy approach can inspect request content (headers, body) while iptables can only filter by IP/port. Squash compensates by adding sq-secret-proxy for content-level inspection.

### Secret Protection

| Feature | Squash | Deno (cloud) | NanoClaw |
|---------|--------|-------------|----------|
| Placeholder substitution | Yes | Yes | No |
| Per-host credential scoping | Yes | Yes | No |
| Exfiltration prevention | Proxy checks host before replacing | Same pattern | Env filtering only |
| Real key inside sandbox | Never | Never | N/A (no proxy) |

Squash and Deno's cloud sandbox use the same placeholder substitution pattern: the sandbox sees `sk-placeholder-anthropic`, the proxy replaces it with the real key only when the request is going to `api.anthropic.com`. NanoClaw filters which env vars are visible but doesn't prevent a compromised agent from reading the API key from its own environment.

### Filesystem Model

| Feature | Squash | Deno (cloud) | NanoClaw |
|---------|--------|-------------|----------|
| Immutable base | squashfs layers (read-only) | Ephemeral VM | Container image |
| Writable layer | tmpfs overlay | VM filesystem | Bind-mounted directories |
| Composable layers | Yes (stack modules) | No | No |
| Snapshots | squashfs of upper layer | Not disclosed | No |
| Layer reuse | Shared across sandboxes | Per-sandbox | Per-group |

Squash's composable squashfs layer system is unique among the three. The PorteuX-inspired pattern allows mixing and matching base OS + runtimes + tools without rebuilding images. Deno and NanoClaw both use monolithic environments.

### Resource Limits

| Feature | Squash | Deno (cloud) | NanoClaw |
|---------|--------|-------------|----------|
| CPU | cgroups v2 / VM config | 2 vCPU fixed | Container default |
| Memory | cgroups v2 / VM config | 768MB-4GB | Container default |
| Lifetime | sq-reaper (configurable) | 30 min max | Hard + idle timeout |
| Per-exec timeout | Yes (configurable) | Not disclosed | Yes |

### Self-Hostability

| Feature | Squash | Deno (cloud) | NanoClaw |
|---------|--------|-------------|----------|
| Self-hostable | Yes | No (cloud-only) | Yes |
| Infrastructure | Any Linux + privileged container | Deno Deploy | macOS or Linux + Docker |
| Target audience | Internal agents, CI | SaaS customers | Single personal user |

### Trust Model

- **Squash**: "Not for untrusted multi-tenant" (README). Designed for internal agents and CI. Firecracker mode provides VM isolation but the system explicitly discourages multi-tenant use.
- **Deno cloud sandbox**: Designed for SaaS — sandboxes may run untrusted user code. Strongest trust boundaries needed.
- **NanoClaw**: "Built for one user." Trust model is auditing — the codebase is designed to be understood in 8 minutes so the user can verify it themselves.

## Code References

- `cgi-bin/common.sh:150-165` — cgroup setup (CPU + memory limits)
- `cgi-bin/common.sh:175-264` — Network namespace + iptables egress control
- `cgi-bin/common.sh:330-376` — chroot exec with unshare isolation
- `cgi-bin/common.sh:507-548` — Firecracker tap device + network setup
- `bin/sq-secret-proxy` — Full secret materialization proxy implementation
- `bin/sq-reaper` — Lifetime enforcement background loop
- `bin/sq-firecracker` — Firecracker VM lifecycle management
- `docs/security.md` — User-facing security documentation
- `docs/plan-firecracker-and-security.md` — Design doc with Deno Sandbox and httpjail references

## Architecture Documentation

### Squash's Layered Security Model

Squash composes security from independent, stackable mechanisms:

1. **Filesystem layer**: squashfs (immutable) + overlayfs (copy-on-write) + chroot (path restriction) — or separate VM filesystem
2. **Process layer**: PID namespace (process isolation) + mount namespace (mount isolation) — or separate kernel
3. **Resource layer**: cgroups v2 (CPU/memory) + sq-reaper (lifetime) — or VM resource config
4. **Network layer**: veth pair + iptables per-sandbox chains (host whitelist) — or tap device + same iptables
5. **Credential layer**: sq-secret-proxy (placeholder substitution with host verification)

Each layer operates independently. A sandbox can have resource limits without network filtering, or network filtering without secret proxy. The Firecracker backend replaces layers 1-3 with a microVM while reusing layers 4-5.

### NanoClaw's Container-as-Boundary Model

NanoClaw uses a single boundary — the container — and does not add application-level security layers on top. Security properties derive entirely from the container runtime:
- Filesystem isolation: mount configuration
- Process isolation: container namespace
- Network: container defaults
- Credentials: env filtering at container creation

### Deno's Dual-Model Approach

Deno has two distinct security systems:
1. **Runtime permissions** (`--allow-*`): Application-level, built into the Deno CLI, works everywhere
2. **Cloud sandbox**: Infrastructure-level, microVM-based, cloud-only

The cloud sandbox is architecturally closest to Squash's Firecracker mode. The runtime permissions have no analog in Squash or NanoClaw (they don't enforce what code *inside* the sandbox can do, only what the sandbox itself can access externally).

## External References

- Deno Sandbox announcement: https://deno.com/blog/introducing-deno-sandbox
- Deno security/permissions docs: https://docs.deno.com/runtime/fundamentals/security/
- NanoClaw repository: https://github.com/qwibitai/nanoclaw
- Firecracker: https://github.com/firecracker-microvm/firecracker
- coder/httpjail (referenced by both Deno and Squash): https://github.com/coder/httpjail

## Open Questions

- Deno's cloud sandbox blog post discloses very few implementation details — the actual VM technology, seccomp profile, and filesystem strategy are not public. A deeper comparison would require access to Deno's infrastructure documentation.
- NanoClaw's Apple Container backend may have different security properties than Docker, but the container-runner.ts abstracts over both and the security model documentation doesn't differentiate.
- Squash's chroot mode explicitly does not use a separate network namespace in the `unshare` call (`--net` is absent at `common.sh:353`); network isolation relies on the veth+iptables setup at creation time rather than namespace-level separation.
