# API Reference

Squash Sandbox exposes a REST API via busybox httpd + CGI scripts.
All endpoints live under `/cgi-bin/`.

## Endpoints

```
GET    /cgi-bin/health                      status
GET    /cgi-bin/api/sandboxes               list all sandboxes
POST   /cgi-bin/api/sandboxes               create sandbox
GET    /cgi-bin/api/sandboxes/:id           sandbox info
DELETE /cgi-bin/api/sandboxes/:id           destroy sandbox
POST   /cgi-bin/api/sandboxes/:id/exec      run command
POST   /cgi-bin/api/sandboxes/:id/activate  add module layer
POST   /cgi-bin/api/sandboxes/:id/snapshot  checkpoint upper layer
POST   /cgi-bin/api/sandboxes/:id/restore   restore from checkpoint
GET    /cgi-bin/api/sandboxes/:id/logs      execution history
GET    /cgi-bin/api/modules                 available modules
```

## Authentication

If `SQUASH_AUTH_TOKEN` is set, all requests must include:

```
Authorization: Bearer <token>
```

## Create sandbox

**POST /cgi-bin/api/sandboxes**

```json
{
    "id": "dev",
    "owner": "alice",
    "layers": "000-base-alpine,100-python312",
    "task": "run tests",
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800,
    "allow_net": ["api.anthropic.com", "pypi.org"]
}
```

| Field            | Required | Default     | Description                              |
|------------------|----------|-------------|------------------------------------------|
| `id`             | yes      | —           | Unique sandbox identifier                |
| `layers`         | yes      | —           | Comma-separated string or JSON array     |
| `owner`          | no       | `""`        | Owner label                              |
| `task`           | no       | `""`        | Task description                         |
| `cpu`            | no       | `2.0`       | CPU cores (cgroups v2 / VM config)       |
| `memory_mb`      | no       | `1024`      | Memory in MB (OOM-killed on exceed)      |
| `max_lifetime_s` | no       | `0`         | Auto-destroy after N seconds (0 = never) |
| `allow_net`      | no       | `[]`        | Outbound host whitelist (see [security](security.md)) |

## Sandbox info

**GET /cgi-bin/api/sandboxes/:id**

```json
{
    "id": "dev",
    "owner": "alice",
    "task": "run tests",
    "layers": ["000-base-alpine", "100-python312"],
    "created": "2025-01-01T00:00:00Z",
    "last_active": "2025-01-01T00:05:00Z",
    "mounted": true,
    "exec_count": 3,
    "upper_bytes": 4096,
    "snapshots": [],
    "active_snapshot": null,
    "cpu": 1.0,
    "memory_mb": 512,
    "max_lifetime_s": 1800,
    "allow_net": ["api.anthropic.com", "pypi.org"]
}
```

## Exec command

**POST /cgi-bin/api/sandboxes/:id/exec**

Request:

```json
{
    "cmd": "python3 -c \"print(42)\""
}
```

Response:

```json
{
    "exit_code": 0,
    "stdout": "42\n",
    "stderr": "",
    "started": "2025-01-01T00:05:00Z",
    "ended": "2025-01-01T00:05:01Z",
    "duration_ms": 1000
}
```

Output is truncated to 64KB per stream.

## Activate module

**POST /cgi-bin/api/sandboxes/:id/activate**

Adds a module layer to a running sandbox. The overlay is remounted with the
new layer; the writable upper directory is preserved.

```json
{
    "module": "100-nodejs22"
}
```

## Snapshot

**POST /cgi-bin/api/sandboxes/:id/snapshot**

Compresses the writable upper layer into a numbered squashfs checkpoint.
Returns the snapshot name (e.g. `900-snapshot-1`).

## Restore

**POST /cgi-bin/api/sandboxes/:id/restore**

Mounts a snapshot as a layer, clears the upper directory, and remounts.

```json
{
    "snapshot": "900-snapshot-1"
}
```

## Logs

**GET /cgi-bin/api/sandboxes/:id/logs**

Returns the execution history for the sandbox (commands run, exit codes,
timestamps).

## List modules

**GET /cgi-bin/api/modules**

Returns all available `.squashfs` files in the modules directory.

## Health check

**GET /cgi-bin/health**

Returns `{"status":"ok"}` when the API is running.
