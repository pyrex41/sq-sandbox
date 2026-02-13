# Deployment

Squash Sandbox runs as a privileged Docker container. Pre-built configs live
in `deploy/` for several providers.

## Requirements

- Docker with `--privileged` (for mount, overlayfs, loop devices, unshare)
- Persistent volume for `/data` (modules, sandboxes, snapshots)
- Firecracker backend additionally requires `/dev/kvm`

## Generic Docker

```sh
docker run -d --privileged \
  -p 8080:8080 \
  -v squash-data:/data \
  -e SQUASH_AUTH_TOKEN=your-secret-token \
  ghcr.io/pyrex41/sq-sandbox:latest
```

Or build from source:

```sh
git clone https://github.com/pyrex41/sq-sandbox.git
cd sq-sandbox
docker build -t sq-sandbox .
docker run -d --privileged -p 8080:8080 -v squash-data:/data sq-sandbox
```

## Hetzner VPS (chroot backend)

**Config:** `deploy/hetzner/docker-compose.yml`
**Cost:** CX32 (4c/8GB) + 100GB volume ~12 EUR/mo

```sh
cd deploy/hetzner
cp .env.example .env   # edit with your tokens
docker compose up -d
```

The compose file passes through all environment variables via `${VAR:-default}`
substitution. Set them in `.env` or your shell.

## Hetzner bare metal (firecracker backend)

**Config:** `deploy/hetzner-baremetal/docker-compose.yml`
**Cost:** AX42 (Ryzen 5 3600 / 64GB) ~47 EUR/mo

```sh
cd deploy/hetzner-baremetal
cp .env.example .env   # edit with your tokens
docker compose up -d
```

Key differences from the VPS config:
- Uses `Dockerfile.firecracker` (includes Firecracker binary + KVM deps)
- Maps `/dev/kvm` into the container
- Sets `SQUASH_BACKEND=firecracker`

## Fly.io

**Config:** `deploy/fly/fly.toml`
**Cost:** Performance-2x (2c/4GB) + 100GB volume ~$62/mo

```sh
cd deploy/fly
fly launch --copy-config
fly secrets set SQUASH_AUTH_TOKEN=your-secret-token
fly volumes create squash_data --size 100 --region iad
fly deploy
```

The fly.toml configures:
- Auto-start/stop machines
- Minimum 1 machine running
- Health checks on `/cgi-bin/health`
- TLS termination on port 443

## AWS ECS

**Config:** `deploy/aws/task-definition.json`
**Cost:** c6g.xlarge (4c/8GB) ~$170/mo (includes EFS)

1. Push the image to ECR:
   ```sh
   docker build -t squash .
   docker tag squash ACCOUNT.dkr.ecr.REGION.amazonaws.com/squash:v3
   docker push ACCOUNT.dkr.ecr.REGION.amazonaws.com/squash:v3
   ```

2. Store secrets in SSM Parameter Store:
   ```sh
   aws ssm put-parameter --name /squash/auth-token --value "your-token" --type SecureString
   ```

3. Create an EFS filesystem and note the `fs-XXXXX` ID. Update the task
   definition with your account ID, region, and EFS ID.

4. Register and run:
   ```sh
   aws ecs register-task-definition --cli-input-json file://task-definition.json
   aws ecs create-service --cluster default --service-name squash \
     --task-definition squash --desired-count 1 --launch-type EC2
   ```

The task definition uses:
- EC2 launch type (required for `--privileged`)
- Host networking
- EFS for persistent `/data` volume
- SSM Parameter Store for secrets
