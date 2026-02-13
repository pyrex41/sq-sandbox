# S3 Sync

Optional S3 integration makes modules and snapshots durable and portable
across hosts. Local disk acts as a cache; S3 is the source of truth.
Works with any S3-compatible service (AWS S3, Cloudflare R2, MinIO, Backblaze B2).

**Disabled by default.** Set `SQUASH_S3_BUCKET` to enable.

## Architecture

```
Local (fast, ephemeral)          S3 (durable, universal)
┌──────────────────┐             ┌──────────────────┐
│ modules/ (cache)  │──push-bg──>│ modules/*.sqfs    │
│                   │<───pull────│                   │
│ snapshots/ (cache)│──push-bg──>│ sandboxes/*/snap/ │
│                   │<───pull────│                   │
└──────────────────┘             └──────────────────┘
```

## Auto-sync behavior

- **After module build** (`sq-mkbase`, `sq-mkmod`): background push to S3
- **After snapshot**: background push to S3
- **On sandbox create**: if a requested module is missing locally, pull from S3
- **On restore**: if snapshot is missing locally, pull from S3
- **On startup** (`sq-init`): pull missing base/modules from S3 before building

## Manual CLI

```
sq-ctl push [modules|snapshots <id>]    push all to S3
sq-ctl pull [modules|snapshots <id>]    pull all from S3
sq-ctl sync [sandbox-id]                bi-directional sync
```

## Provider examples

### AWS S3

```sh
SQUASH_S3_BUCKET=my-squash-modules
SQUASH_S3_REGION=us-west-2
# Credentials via IAM role, env vars, or ~/.aws/credentials
```

### Cloudflare R2

```sh
SQUASH_S3_BUCKET=my-squash-modules
SQUASH_S3_ENDPOINT=https://ACCOUNT.r2.cloudflarestorage.com
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
```

### MinIO (self-hosted)

```sh
SQUASH_S3_BUCKET=squash
SQUASH_S3_ENDPOINT=http://minio:9000
AWS_ACCESS_KEY_ID=minioadmin
AWS_SECRET_ACCESS_KEY=minioadmin
```

## Transport

Uses `aws` CLI if available, falls back to `curl` + AWS Signature V4 signing
via `openssl`. No additional dependencies required beyond what's in the base
image.

## Environment variables

| Variable             | Default      | Description                        |
|----------------------|--------------|------------------------------------|
| `SQUASH_S3_BUCKET`   | `""` (off)   | S3 bucket name                     |
| `SQUASH_S3_ENDPOINT` | `""` (AWS)   | Custom endpoint for R2/MinIO/B2    |
| `SQUASH_S3_REGION`   | `us-east-1`  | AWS region                         |
| `SQUASH_S3_PREFIX`   | `""`         | Key prefix (e.g. `prod/`)         |
| `SQUASH_EPHEMERAL`   | `""` (off)   | Enable ephemeral mode (set `1`)    |

## Ephemeral Mode

Set `SQUASH_EPHEMERAL=1` to make S3 the sole durable storage. Local disk
becomes a disposable cache — no persistent volume required. Requires
`SQUASH_S3_BUCKET`.

### Lifecycle

```
create "dev"
  └─ mount layers (pull from S3 on miss, as usual)
  └─ check S3 for sandboxes/dev/snapshots/
  └─ if latest snapshot exists → auto-restore it

exec "dev" "pip install flask"
  └─ changes written to upper/ (local, ephemeral)

destroy "dev"
  └─ check if upper/ has meaningful changes
  └─ if yes → mksquashfs upper/ → stage to /tmp → background push to S3
  └─ push manifest.json to S3 (records sandbox config)
  └─ rm -rf sandbox dir (local state gone)

create "dev"  (again, anywhere)
  └─ mount layers
  └─ auto-restore from latest S3 snapshot
  └─ upper/ now has flask installed
```

### Manifest

Each sandbox gets a `sandboxes/<id>/manifest.json` in S3 that records its
configuration:

```json
{
  "id": "dev",
  "layers": "000-base-alpine,100-python312",
  "owner": "alice",
  "task": "run tests",
  "cpu": 2,
  "memory_mb": 1024,
  "max_lifetime_s": 0,
  "allow_net": [],
  "latest_snapshot": "20250101-120000"
}
```

This allows the sandbox to be fully recreated from S3 without the caller
specifying all parameters again.

### Example: stateless container

```sh
# No -v flag — no persistent volume needed
docker run --rm --privileged \
  -e SQUASH_EPHEMERAL=1 \
  -e SQUASH_S3_BUCKET=my-squash \
  -e SQUASH_S3_ENDPOINT=http://minio:9000 \
  -p 8080:8080 \
  ghcr.io/pyrex41/sq-sandbox:latest
```

### When to use

- **Stateless containers** (ECS, Fly, Lambda): no EBS/EFS needed
- **Multi-host**: work on sandbox A from host 1, continue from host 2
- **Disaster recovery**: host dies, spin up anywhere, state is in S3
- **Cost optimization**: no persistent volume charges

### Backward compatibility

Without `SQUASH_EPHEMERAL=1`, all behavior is unchanged. The existing
persistent-volume workflow works exactly as before.
