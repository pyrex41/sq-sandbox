# Security Features

Squash Sandbox provides per-sandbox resource limits, network egress control,
and secret materialization. These work identically across both backends
(chroot and firecracker) unless noted.

## Resource limits

Set at sandbox creation time via the [create API](api.md):

| Field            | Default | Enforcement                                      |
|------------------|---------|--------------------------------------------------|
| `cpu`            | `2.0`   | cgroups v2 (chroot), native VM config (firecracker) |
| `memory_mb`      | `1024`  | cgroups v2 (chroot), native VM config (firecracker) |
| `max_lifetime_s` | `0`     | `sq-reaper` background process (both backends)   |

- **CPU** — fractional cores allowed (e.g. `0.5`). In chroot mode, enforced
  via cgroups v2 `cpu.max`. In firecracker mode, set as vCPU count on the VM.
- **Memory** — in megabytes. Processes are OOM-killed if they exceed the limit.
  In chroot mode, enforced via cgroups v2 `memory.max`. In firecracker mode,
  set as VM memory size.
- **Lifetime** — auto-destroy after N seconds. The `sq-reaper` process runs
  in the background and checks sandbox ages periodically. Set to `0` to
  disable (sandbox lives until explicitly destroyed).

## Network egress control

Per-sandbox outbound network whitelisting via the `allow_net` field:

| Value                                    | Behavior                           |
|------------------------------------------|------------------------------------|
| `[]` (default)                           | Allow all outbound traffic         |
| `["api.anthropic.com", "pypi.org"]`      | Allow only listed hosts            |
| `["none"]`                               | Block all outbound traffic         |

Implementation:

- **chroot**: Each sandbox gets its own network namespace. nftables rules
  restrict outbound connections to resolved IPs of allowed hosts. DNS is
  always permitted for hostname resolution.
- **firecracker**: Each VM gets a tap device. nftables rules on the host
  filter traffic on the tap interface.

DNS resolution happens at sandbox creation time for the allowed hosts. If a
host resolves to multiple IPs, all are permitted.

## Secret materialization

The `sq-secret-proxy` injects real credentials into outbound HTTP requests
without exposing them inside the sandbox. Sandboxes see only placeholder
values.

### How it works

1. Configure secrets in `$SQUASH_DATA/secrets.json`
2. The proxy runs on the host (port 8888)
3. Sandbox HTTP traffic is routed through the proxy
4. The proxy replaces placeholder values with real credentials, but only
   when the destination host is in the secret's `allowed_hosts` list
5. Real keys never enter the sandbox

### Configuration

Create `$SQUASH_DATA/secrets.json`:

```json
{
    "secrets": {
        "ANTHROPIC_API_KEY": {
            "placeholder": "sk-placeholder-anthropic",
            "value": "sk-ant-real-key-here",
            "allowed_hosts": ["api.anthropic.com"]
        }
    }
}
```

Each secret has:

| Field           | Description                                          |
|-----------------|------------------------------------------------------|
| `placeholder`   | The fake value the sandbox sees and sends in requests |
| `value`         | The real credential injected by the proxy             |
| `allowed_hosts` | Hosts where replacement is allowed                   |

The proxy only replaces the placeholder with the real value when the
outbound request targets a host in `allowed_hosts`. This prevents a
malicious sandbox from exfiltrating credentials to an attacker-controlled
server.
