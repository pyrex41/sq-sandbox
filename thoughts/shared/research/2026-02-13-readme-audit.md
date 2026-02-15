---
date: 2026-02-13T12:00:00-08:00
researcher: reuben
git_commit: 610a968a6ad6f38a21aa8bc28cda247e6aa5ce1c
branch: main
repository: sq-sandbox
topic: "README accuracy audit against actual codebase"
tags: [research, codebase, readme, documentation]
status: complete
last_updated: 2026-02-13
last_updated_by: reuben
---

# Research: README Accuracy Audit

**Date**: 2026-02-13
**Git Commit**: 610a968
**Branch**: main
**Repository**: sq-sandbox

## Research Question
Is the README comprehensive and accurate relative to the actual codebase?

## Summary

The README is well-structured and covers the core architecture accurately. However, there are several discrepancies between what the README documents and what the code actually implements. The issues range from a non-existent API endpoint to incorrect base image names and missing fields.

## Discrepancies Found

### 1. Non-existent API endpoint: `POST /cgi-bin/api/modules/build`

**README (line 151)**: Lists `POST /cgi-bin/api/modules/build — build module from preset`

**Actual code** (`cgi-bin/api/modules`): Only supports `GET` — returns `400 "GET only"` for all other methods. There is no build endpoint. Module building is done via the `bin/sq-mkmod` CLI script, not through the HTTP API.

The `static/index.html` landing page also does NOT list this endpoint, confirming it doesn't exist.

### 2. Incorrect base image names in Bases table

**README (lines 57-62)**:
| Base | Name |
|------|------|
| `base-debian-slim` | ~30MB |
| `base-ubuntu-noble` | ~45MB |

**Actual code** (`bin/sq-mkbase:84-85`):
- Debian produces `000-base-debian` (not `base-debian-slim`)
- Ubuntu (noble/jammy) produces `000-base-ubuntu` (not `base-ubuntu-noble`)

The names in the README don't match what `sq-mkbase` actually generates. The suite name is NOT included in the output filename.

### 3. `base-porteux-core` doesn't exist

**README (line 61)**: Lists `base-porteux-core (~200MB, installpkg, glibc)`

**Actual code**: `sq-mkbase` supports: `alpine`, `debian`, `ubuntu`, `void`, `from-dir`. There is no PorteuX build function. This base cannot be built with the current code.

### 4. Missing `task` field in create sandbox example

**README (lines 156-164)**: Create sandbox JSON body doesn't include `task` field.

**Actual code** (`cgi-bin/api/sandboxes`): Accepts a `task` field. The `static/index.html` also documents `{id, owner, layers, task}` for the create endpoint. The `sandbox_info()` function in `common.sh` returns the task field.

### 5. Missing documentation: Environment variables

The README documents `SQUASH_BACKEND` but doesn't document these environment variables used throughout the codebase:

| Variable | Default | Where Used |
|----------|---------|-----------|
| `SQUASH_DATA` | `/data` | Every script |
| `SQUASH_PORT` | `8080` | `start-api`, `sq-ctl` |
| `SQUASH_AUTH_TOKEN` | `""` (no auth) | `start-api`, `common.sh`, `sq-ctl` |
| `SQUASH_API` | `http://localhost:8080` | `sq-ctl` |
| `TAILSCALE_AUTHKEY` | none | `setup-tailscale`, `entrypoint.sh` |
| `TAILSCALE_HOSTNAME` | `squash` | `setup-tailscale` |

### 6. Missing files in Files section

The README's file tree omits:
- `static/index.html` — API landing page served at root URL
- `docs/plan-firecracker-and-security.md` — design document for Firecracker + security features

### 7. Layers field accepts array or string (undocumented)

**README**: Shows `"layers": "000-base-alpine,100-python312"` (string only)

**Actual code** (`cgi-bin/api/sandboxes`): The `layers` field can be either a comma-separated string OR a JSON array. The handler normalizes arrays to comma-separated strings.

### 8. Module presets not fully documented

The README modules table mentions examples like `python312, nodejs22, golang` but doesn't document the full list of available presets or note that `rust` and `build-tools` exist as stubs that instruct users to install inside the sandbox.

Available presets in `sq-mkmod`:
- `python3.12` (or `python312`) — downloads pre-built binary
- `nodejs22` — downloads from nodejs.org
- `golang` — downloads from go.dev
- `rust` — stub (recommends rustup in sandbox)
- `build-tools` — stub (recommends apk add in sandbox)
- `tailscale` — downloads from pkgs.tailscale.com

### 9. Exec response format not documented

The API section doesn't document what the exec endpoint returns. The actual response includes:
```json
{
  "exit_code": 0,
  "stdout": "...",
  "stderr": "...",
  "started": "2024-01-01T00:00:00Z",
  "ended": "2024-01-01T00:00:01Z",
  "duration_ms": 1000
}
```

### 10. Sandbox info response format not documented

The README doesn't document the response format from `GET /api/sandboxes/:id`. The actual response includes: `id`, `owner`, `task`, `layers`, `created`, `last_active`, `mounted`, `exec_count`, `upper_bytes`, `snapshots`, `active_snapshot`, `cpu`, `memory_mb`, `max_lifetime_s`, `allow_net`.

## Items That Are Accurate

- Architecture diagram (squashfs layers + overlayfs) is correct
- Backend table (chroot vs firecracker) is accurate
- Operations list (create, exec, activate, snapshot, restore, destroy) matches code
- Security features (resource limits, network egress, secret proxy) match implementation
- Firecracker description accurately reflects `sq-firecracker` + `vm/init` + `vm/sq-vsock-handler`
- Deploy directory descriptions match actual configurations
- "What this is NOT" section is appropriate

## Code References
- `cgi-bin/api/modules:9-10` — GET-only handler, no build endpoint
- `bin/sq-mkbase:84-85` — Base naming: `000-base-debian`, `000-base-ubuntu`
- `cgi-bin/api/sandboxes:19-26` — Create body parsing with task, cpu, memory_mb, etc.
- `cgi-bin/common.sh:729-768` — sandbox_info() response fields
- `cgi-bin/common.sh:309-355` — exec response construction
- `bin/sq-mkmod:34-170` — All preset definitions
- `static/index.html` — Landing page API reference
- `bin/start-api:4-8` — Environment variable usage
