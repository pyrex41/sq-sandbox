#!/bin/sh
# squash/cgi-bin/common.sh
# Shared functions for all CGI handlers and CLI tools.
# Depends: jq, mksquashfs, sq-mount-layer, sq-mount-overlay, sq-exec
# Optional: squashfuse, fuse-overlayfs, bubblewrap (for unprivileged mode)

set -eu

DATA="${SQUASH_DATA:-/data}"
MODULES="$DATA/modules"
SANDBOXES="$DATA/sandboxes"
BACKEND="${SQUASH_BACKEND:-chroot}"

# ── Input validation ──────────────────────────────────────────────────

valid_id() {
    case "$1" in "") return 1 ;; *[!a-zA-Z0-9_-]*) return 1 ;; esac
}

valid_label() {
    case "$1" in "") return 1 ;; *[!a-zA-Z0-9_.-]*) return 1 ;; esac
}

valid_module() {
    case "$1" in "") return 1 ;; *[!a-zA-Z0-9_.-]*) return 1 ;; esac
}

# ── S3 sync ──────────────────────────────────────────────────────────

_s3_enabled() { [ -n "${SQUASH_S3_BUCKET:-}" ] && command -v sq-s3 >/dev/null 2>&1; }

# ── Ephemeral mode ────────────────────────────────────────────────────

_ephemeral_enabled() { [ "${SQUASH_EPHEMERAL:-}" = "1" ] && _s3_enabled; }

_s3_push_manifest() {
    local id="$1"
    local s=$(sdir "$id")
    [ ! -d "$s/.meta" ] && return 1
    local tmp=$(mktemp)
    local layers=$(cat "$s/.meta/layers" 2>/dev/null || echo "")
    local owner=$(cat "$s/.meta/owner" 2>/dev/null || echo "")
    local task=$(cat "$s/.meta/task" 2>/dev/null || echo "")
    local cpu=$(cat "$s/.meta/cpu" 2>/dev/null || echo "2")
    local memory_mb=$(cat "$s/.meta/memory_mb" 2>/dev/null || echo "1024")
    local max_lifetime_s=$(cat "$s/.meta/max_lifetime_s" 2>/dev/null || echo "0")
    local allow_net=$(cat "$s/.meta/allow_net" 2>/dev/null || echo "[]")
    echo "$allow_net" | jq empty 2>/dev/null || allow_net="[]"
    local latest_snapshot=$(_s3_latest_snapshot "$id" 2>/dev/null || echo "")
    jq -n \
        --arg id "$id" \
        --arg layers "$layers" \
        --arg owner "$owner" \
        --arg task "$task" \
        --argjson cpu "$cpu" \
        --argjson memory_mb "$memory_mb" \
        --argjson max_lifetime_s "$max_lifetime_s" \
        --argjson allow_net "$allow_net" \
        --arg latest_snapshot "$latest_snapshot" \
        '{id:$id,layers:$layers,owner:$owner,task:$task,cpu:$cpu,
          memory_mb:$memory_mb,max_lifetime_s:$max_lifetime_s,
          allow_net:$allow_net,latest_snapshot:$latest_snapshot}' \
        > "$tmp"
    sq-s3 push "$tmp" "sandboxes/$id/manifest.json"
    local rc=$?
    rm -f "$tmp"
    return $rc
}

_s3_pull_manifest() {
    local id="$1"
    local tmp=$(mktemp)
    sq-s3 pull "sandboxes/$id/manifest.json" "$tmp" 2>/dev/null || { rm -f "$tmp"; return 1; }
    echo "$tmp"
}

_s3_latest_snapshot() {
    local id="$1"
    sq-s3 list "sandboxes/$id/snapshots/" 2>/dev/null \
        | sed -n 's|.*/\([^/]*\)\.squashfs$|\1|p' \
        | sort \
        | tail -1
}

_ephemeral_stage_and_push() {
    local src="$1" key="$2"
    local tmp="/tmp/sq-push-$(basename "$src")"
    cp "$src" "$tmp"
    # Background push from temp copy; self-cleans after
    (sq-s3 push "$tmp" "$key" >> "$DATA/.s3-push.log" 2>&1; rm -f "$tmp") &
}

# ── HTTP ───────────────────────────────────────────────────────────────

json_response() {
    local status="${1:-200}" body="$2"
    case "$status" in
        200) printf 'Status: 200 OK\r\n' ;;
        201) printf 'Status: 201 Created\r\n' ;;
        204) printf 'Status: 204 No Content\r\n' ;;
        400) printf 'Status: 400 Bad Request\r\n' ;;
        401) printf 'Status: 401 Unauthorized\r\n' ;;
        404) printf 'Status: 404 Not Found\r\n' ;;
        409) printf 'Status: 409 Conflict\r\n' ;;
        415) printf 'Status: 415 Unsupported Media Type\r\n' ;;
        500) printf 'Status: 500 Internal Server Error\r\n' ;;
    esac
    printf 'Content-Type: application/json\r\n\r\n'
    echo "$body"
}

json_ok()    { json_response 200 "$1"; }
json_err()   { json_response "$1" "{\"error\":$(echo "$2" | jq -Rs .)}"; }
read_body()  { dd bs=1 count="${CONTENT_LENGTH:-0}" 2>/dev/null; }
require_json() {
    case "${CONTENT_TYPE:-}" in
        application/json*) ;;
        *) json_err 415 "expected Content-Type: application/json"; exit 0 ;;
    esac
}

check_auth() {
    local token="${SQUASH_AUTH_TOKEN:-}"
    [ -z "$token" ] && return 0
    [ "Bearer $token" = "${HTTP_AUTHORIZATION:-}" ] && return 0
    json_err 401 "unauthorized"
    exit 0
}

# ── Module operations ──────────────────────────────────────────────────

# Module filename convention:
#   NNN-name.squashfs
#   NNN = sort order (000=base, 1xx=runtime, 2xx=service, 9xx=checkpoint)
#
# Stored in $MODULES/. Loop-mounted read-only per sandbox.

mod_path() { echo "$MODULES/$1.squashfs"; }
mod_exists() {
    valid_module "$1" || return 1
    [ -f "$(mod_path "$1")" ] && return 0
    _s3_enabled && sq-s3 pull "modules/$1.squashfs" "$(mod_path "$1")" 2>/dev/null && return 0
    return 1
}

list_modules() {
    local seen=""
    {
        for f in "$MODULES"/*.squashfs; do
            [ ! -f "$f" ] && continue
            local name=$(basename "$f" .squashfs)
            local size=$(stat -c%s "$f" 2>/dev/null || echo 0)
            jq -n --arg name "$name" --argjson size "$size" --arg loc "local" \
                '{name:$name,size:$size,location:$loc}'
            seen="$seen $name "
        done
        if _s3_enabled; then
            sq-s3 list "modules/" 2>/dev/null | while read -r key; do
                [ -z "$key" ] && continue
                local rname=$(basename "$key" .squashfs)
                case "$seen" in *" $rname "*) continue ;; esac
                jq -n --arg name "$rname" --argjson size 0 --arg loc "remote" \
                    '{name:$name,size:$size,location:$loc}'
            done
        fi
    } | jq -s '.'
}

# ── Sandbox filesystem ─────────────────────────────────────────────────
#
# Layout: $SANDBOXES/<id>/
#   .meta/
#     owner          plain text
#     task           plain text
#     layers         newline-delimited module names (mount order)
#     created        ISO timestamp
#     last_active    ISO timestamp
#     cpu            CPU limit (float, e.g. 2.0)
#     memory_mb      Memory limit in MB
#     max_lifetime_s Max lifetime in seconds (0=unlimited)
#     allow_net      JSON array of allowed egress hosts
#     log/
#       0001.json    execution log
#       0002.json
#     snapshots.jsonl   snapshot manifest
#   images/
#     000-base-alpine.squashfs/    <- loop mount point
#     100-python312.squashfs/      <- loop mount point
#     _snapshot/                   <- active snapshot mount
#   upper/           writable overlay layer
#   work/            overlayfs workdir
#   merged/          final chroot target

sdir()    { echo "$SANDBOXES/$1"; }
exists()  { [ -d "$(sdir "$1")/.meta" ]; }

# Build overlayfs lowerdir string from images/ directory.
# Sort: _snapshot first (highest priority), then by name descending.
_lowerdir() {
    local id="$1" s=$(sdir "$1") lower=""
    # Snapshot layer first
    if [ -d "$s/images/_snapshot" ] && mountpoint -q "$s/images/_snapshot" 2>/dev/null; then
        lower="$s/images/_snapshot"
    fi
    # Module layers: highest number prefix first
    for d in $(ls -1d "$s"/images/[0-9]*.squashfs 2>/dev/null | sort -r); do
        mountpoint -q "$d" 2>/dev/null || continue
        [ -n "$lower" ] && lower="$lower:" || true
        lower="$lower$d"
    done
    echo "$lower"
}

_mount_overlay() {
    local id="$1" s=$(sdir "$1")
    local lower=$(_lowerdir "$id")
    [ -z "$lower" ] && return 1
    sq-mount-overlay "$lower" "$s/upper/data" "$s/upper/work" "$s/merged"
}

_umount_overlay() {
    local s=$(sdir "$1")
    sq-mount-overlay --unmount "$s/merged" 2>/dev/null || true
}

# ── cgroup helpers (disabled — requires root) ─────────────────────────
# Resource limits via cgroups v2 require CAP_SYS_ADMIN.
# For unprivileged mode, use systemd-run --user --scope instead.

_cgroup_setup() { :; }
_cgroup_teardown() { :; }

# ── Secret proxy injection ────────────────────────────────────────────

_inject_secret_placeholders() {
    local id="$1"
    local s=$(sdir "$id")
    local secrets_file="$DATA/secrets.json"
    [ ! -f "$secrets_file" ] && return 0

    local env_dir="$s/upper/data/etc/profile.d"
    mkdir -p "$env_dir"

    # Proxy address — sandboxes use slirp4netns; host loopback is at the gateway IP
    local proxy_host="10.0.2.2"

    # Generate env script with placeholders + proxy config
    {
        jq -r '.secrets | to_entries[] | "export \(.key)=\(.value.placeholder)"' \
            "$secrets_file" 2>/dev/null
        echo "export http_proxy=http://${proxy_host}:8888"
        echo "export https_proxy=http://${proxy_host}:8888"
        echo "export HTTP_PROXY=http://${proxy_host}:8888"
        echo "export HTTPS_PROXY=http://${proxy_host}:8888"
    } > "$env_dir/squash-secrets.sh"
    chmod 644 "$env_dir/squash-secrets.sh"

    # Inject proxy CA into sandbox trust store (only if HTTPS proxy is enabled)
    local ca_cert="$DATA/proxy-ca/ca.crt"
    if [ -f "$ca_cert" ]; then
        # Copy CA cert into sandbox
        mkdir -p "$s/upper/data/usr/local/share/ca-certificates"
        cp "$ca_cert" "$s/upper/data/usr/local/share/ca-certificates/sq-proxy-ca.crt"

        # Append to system CA bundle (used by curl, wget, python, etc.)
        if [ -f "$s/merged/etc/ssl/certs/ca-certificates.crt" ]; then
            mkdir -p "$s/upper/data/etc/ssl/certs"
            cp "$s/merged/etc/ssl/certs/ca-certificates.crt" \
                "$s/upper/data/etc/ssl/certs/ca-certificates.crt"
            cat "$ca_cert" >> "$s/upper/data/etc/ssl/certs/ca-certificates.crt"
        fi

        # Env vars for runtimes that don't use the system bundle
        echo "export NODE_EXTRA_CA_CERTS=/usr/local/share/ca-certificates/sq-proxy-ca.crt" \
            >> "$env_dir/squash-secrets.sh"

        # Python: requests uses certifi, not system bundle; ssl module checks SSL_CERT_FILE
        echo "export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt" \
            >> "$env_dir/squash-secrets.sh"
        echo "export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt" \
            >> "$env_dir/squash-secrets.sh"
    fi
}

# ── Network (simplified — sandboxes inherit host networking) ──────────
# Egress filtering via iptables has been removed. The secret proxy is the
# credential security boundary. Sandboxes use host networking directly.

# ── Chroot backend: Create ────────────────────────────────────────────

_chroot_create_sandbox() {
    local id="$1" owner="$2" layer_csv="$3" task="${4:-}"
    local cpu="${5:-2}" memory_mb="${6:-1024}" max_lifetime_s="${7:-0}" allow_net="${8:-}"
    local s=$(sdir "$id")

    exists "$id" && { echo "already exists" >&2; return 1; }

    local max_sandboxes="${SQUASH_MAX_SANDBOXES:-100}"
    local current_count=$(ls -1d "$SANDBOXES"/*/ 2>/dev/null | wc -l)
    [ "$current_count" -ge "$max_sandboxes" ] && {
        echo "sandbox limit reached ($max_sandboxes)" >&2; return 1
    }

    # Validate layers
    local mod
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        mod_exists "$mod" || { echo "module not found: $mod" >&2; return 2; }
    done

    # Build directory tree — upper layer uses SQUASH_UPPER_BACKEND (default: tmpfs)
    mkdir -p "$s/images" "$s/upper" "$s/merged" "$s/.meta/log"
    local storage_sh
    storage_sh="$(dirname "$(dirname "$0")")/storage.sh"
    if [ -f "$storage_sh" ]; then
        . "$storage_sh"
        upper_mount "$s/upper"
    else
        local upper_limit="${SQUASH_UPPER_LIMIT_MB:-512}"
        mount -t tmpfs -o "size=${upper_limit}M" tmpfs "$s/upper"
        mkdir -p "$s/upper/data" "$s/upper/work"
    fi

    # Metadata as files
    echo "$owner"          > "$s/.meta/owner"
    echo "$task"           > "$s/.meta/task"
    echo "$layer_csv"      > "$s/.meta/layers"
    date -Iseconds         > "$s/.meta/created"
    date -Iseconds         > "$s/.meta/last_active"
    echo "$cpu"            > "$s/.meta/cpu"
    echo "$memory_mb"      > "$s/.meta/memory_mb"
    echo "$max_lifetime_s" > "$s/.meta/max_lifetime_s"
    [ -n "$allow_net" ] && echo "$allow_net" > "$s/.meta/allow_net"

    # Set up cgroups for resource limits
    _cgroup_setup "$id" "$cpu" "$memory_mb" >/dev/null 2>&1 || true

    # Mount each module (squashfuse for unprivileged, kernel mount as fallback)
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        local mp="$s/images/$mod.squashfs"
        sq-mount-layer "$(mod_path "$mod")" "$mp" || {
            echo "mount failed: $mod" >&2
            destroy_sandbox "$id"
            return 3
        }
    done

    # Mount overlay
    _mount_overlay "$id" || {
        echo "overlay mount failed" >&2
        destroy_sandbox "$id"
        return 3
    }

    # Seed /etc/resolv.conf — sandboxes inherit host networking
    if [ -f /etc/resolv.conf ]; then
        mkdir -p "$s/upper/data/etc"
        cp /etc/resolv.conf "$s/upper/data/etc/resolv.conf"
    fi

    # Inject secret placeholders + proxy config if secrets.json exists
    _inject_secret_placeholders "$id"

    # Ephemeral mode: auto-restore from latest S3 snapshot
    if _ephemeral_enabled; then
        local latest=$(_s3_latest_snapshot "$id")
        if [ -n "$latest" ]; then
            echo "[ephemeral] restoring $id from S3 snapshot: $latest" >&2
            _chroot_restore_sandbox "$id" "$latest" || \
                echo "[ephemeral] restore failed, continuing with clean sandbox" >&2
        fi
    fi
}

# ── Chroot backend: Execute ───────────────────────────────────────────

_chroot_exec_in_sandbox() {
    local id="$1" cmd="$2" workdir="${3:-/}" timeout_s="${4:-300}"
    local s=$(sdir "$id")

    _chroot_mounted "$id" || { echo "not mounted" >&2; return 1; }

    date -Iseconds > "$s/.meta/last_active"

    # Log sequence
    local seq=$(( $(ls "$s/.meta/log/" 2>/dev/null | wc -l) + 1 ))
    local logf="$s/.meta/log/$(printf '%04d' $seq).json"

    local t0=$(date -Iseconds)
    local out=$(mktemp) err=$(mktemp)
    local rc=0

    # Determine if sandbox has network access (allow_net is non-empty JSON array)
    local allow_net_raw net_flag="0"
    allow_net_raw=$(cat "$s/.meta/allow_net" 2>/dev/null || echo "[]")
    if [ "$allow_net_raw" != "[]" ] && [ -n "$allow_net_raw" ]; then
        net_flag="1"
    fi

    # Execute via bubblewrap (unprivileged) or unshare+chroot (fallback)
    sq-exec "$s/merged" "$cmd" "$workdir" "$timeout_s" "$net_flag" \
        >"$out" 2>"$err" || rc=$?

    local t1=$(date -Iseconds)

    # Write log entry
    jq -n \
        --argjson seq "$seq" \
        --arg cmd "$cmd" \
        --arg workdir "$workdir" \
        --argjson rc "$rc" \
        --arg t0 "$t0" \
        --arg t1 "$t1" \
        --arg stdout "$(head -c 65536 "$out")" \
        --arg stderr "$(head -c 65536 "$err")" \
        '{seq:$seq,cmd:$cmd,workdir:$workdir,exit_code:$rc,started:$t0,finished:$t1,stdout:$stdout,stderr:$stderr}' \
        > "$logf"

    rm -f "$out" "$err"
    cat "$logf"
    return $rc
}

# ── Chroot backend: Activate module ───────────────────────────────────

_chroot_activate_module() {
    local id="$1" mod="$2"
    local s=$(sdir "$id")

    mod_exists "$mod" || { echo "module not found: $mod" >&2; return 1; }

    local mp="$s/images/$mod.squashfs"
    [ -d "$mp" ] && mountpoint -q "$mp" 2>/dev/null && {
        echo "already active: $mod" >&2; return 2
    }

    # Mount new module
    sq-mount-layer "$(mod_path "$mod")" "$mp"

    # Remount overlay (upper preserved)
    _umount_overlay "$id"
    _mount_overlay "$id"

    # Update layers list
    printf ',%s' "$mod" >> "$s/.meta/layers"
}

# ── Chroot backend: Snapshot ──────────────────────────────────────────

_chroot_snapshot_sandbox() {
    local id="$1" label="${2:-$(date +%Y%m%d-%H%M%S)}"
    valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
    local s=$(sdir "$id")

    # Irmin backend: delegate to sq-store sidecar
    if [ "${SQUASH_SNAPSHOT_BACKEND:-squashfs}" = "irmin" ]; then
        local store_sock="${SQUASH_STORE_SOCK:-$DATA/.sq-store.sock}"
        local request
        request=$(jq -n --arg sandbox_id "$id" --arg label "$label" \
            --arg upper_data "$s/upper/data" \
            '{op:"snapshot",sandbox_id:$sandbox_id,label:$label,upper_data:$upper_data}')
        local response
        response=$(printf '%s\n' "$request" | socat - UNIX-CONNECT:"$store_sock" 2>/dev/null) || {
            echo "sq-store unreachable" >&2; return 1; }
        local ok
        ok=$(echo "$response" | jq -r '.ok')
        if [ "$ok" != "true" ]; then
            echo "sq-store error: $(echo "$response" | jq -r '.error')" >&2; return 1
        fi
        local size
        size=$(echo "$response" | jq -r '.size // 0')
        jq -n --arg label "$label" --arg created "$(date -Iseconds)" \
            --argjson size "$size" --arg backend "irmin" \
            '{label:$label,created:$created,size:$size,backend:$backend}' >> "$s/.meta/snapshots.jsonl"
        echo "$label"
        return 0
    fi

    local snapdir="$s/snapshots"
    local snapfile="$snapdir/$label.squashfs"

    [ -f "$snapfile" ] && { echo "exists: $label" >&2; return 1; }

    mkdir -p "$snapdir"
    # Use zstd if kernel supports it, otherwise fall back to gzip
    local comp_args="-comp gzip -b 256K"
    if zgrep -q CONFIG_SQUASHFS_ZSTD=y /proc/config.gz 2>/dev/null; then
        comp_args="-comp zstd -Xcompression-level 3 -b 128K"
    fi
    mksquashfs "$s/upper/data" "$snapfile" $comp_args -noappend -quiet

    local size=$(stat -c%s "$snapfile")
    jq -n --arg label "$label" --arg created "$(date -Iseconds)" --argjson size "$size" \
        '{label:$label,created:$created,size:$size}' >> "$s/.meta/snapshots.jsonl"

    # Notify sidecar for background S3 push (preferred) or push directly
    if command -v sq-sync >/dev/null 2>&1; then
        sq-sync --notify "{\"op\":\"push\",\"path\":\"$snapfile\",\"key\":\"sandboxes/$id/snapshots/$label.squashfs\"}"
    elif _s3_enabled; then
        sq-s3 push-bg "$snapfile" "sandboxes/$id/snapshots/$label.squashfs"
    fi

    echo "$snapfile"
}

# ── Chroot backend: Restore ──────────────────────────────────────────

_chroot_restore_sandbox() {
    local id="$1" label="$2"
    valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
    local s=$(sdir "$id")

    # Irmin backend: delegate to sq-store sidecar
    if [ "${SQUASH_SNAPSHOT_BACKEND:-squashfs}" = "irmin" ]; then
        local store_sock="${SQUASH_STORE_SOCK:-$DATA/.sq-store.sock}"
        local request
        request=$(jq -n --arg sandbox_id "$id" --arg label "$label" \
            --arg upper_data "$s/upper/data" \
            '{op:"restore",sandbox_id:$sandbox_id,label:$label,upper_data:$upper_data}')

        # Tear down current overlay before restore
        _umount_overlay "$id"

        local response
        response=$(printf '%s\n' "$request" | socat - UNIX-CONNECT:"$store_sock" 2>/dev/null) || {
            echo "sq-store unreachable" >&2; return 1; }
        local ok
        ok=$(echo "$response" | jq -r '.ok')
        if [ "$ok" != "true" ]; then
            echo "sq-store error: $(echo "$response" | jq -r '.error')" >&2; return 1
        fi

        echo "$label" > "$s/.meta/active_snapshot"
        _mount_overlay "$id"

        # Re-seed DNS
        if [ -f /etc/resolv.conf ]; then
            mkdir -p "$s/upper/data/etc"
            cp /etc/resolv.conf "$s/upper/data/etc/resolv.conf"
        fi
        _inject_secret_placeholders "$id"
        return 0
    fi

    local snapfile="$s/snapshots/$label.squashfs"

    if [ ! -f "$snapfile" ]; then
        if _s3_enabled; then
            mkdir -p "$(dirname "$snapfile")"
            sq-s3 pull "sandboxes/$id/snapshots/$label.squashfs" "$snapfile" 2>/dev/null || {
                echo "not found: $label" >&2; return 1; }
        else
            echo "not found: $label" >&2; return 1
        fi
    fi

    # Tear down current overlay
    _umount_overlay "$id"

    # Unmount previous snapshot layer if any
    if mountpoint -q "$s/images/_snapshot" 2>/dev/null; then
        sq-mount-layer --unmount "$s/images/_snapshot" 2>/dev/null || true
    fi

    # Clear upper contents + reset work
    find "$s/upper/data" -mindepth 1 -delete 2>/dev/null || true
    find "$s/upper/work" -mindepth 1 -delete 2>/dev/null || true

    # Mount snapshot as top read-only layer
    sq-mount-layer "$snapfile" "$s/images/_snapshot"

    # Record active snapshot
    echo "$label" > "$s/.meta/active_snapshot"

    # Remount overlay
    _mount_overlay "$id"

    # Re-seed DNS (host networking)
    if [ -f /etc/resolv.conf ]; then
        mkdir -p "$s/upper/data/etc"
        cp /etc/resolv.conf "$s/upper/data/etc/resolv.conf"
    fi

    # Re-inject secret placeholders
    _inject_secret_placeholders "$id"
}

# ── Chroot backend: Destroy ──────────────────────────────────────────

_chroot_destroy_sandbox() {
    local id="$1"
    local s=$(sdir "$id")
    [ ! -d "$s" ] && return 0

    # Ephemeral mode: auto-snapshot to S3 before destroying
    if _ephemeral_enabled && [ -d "$s/upper/data" ]; then
        local upper_files=$(find "$s/upper/data" -mindepth 1 \
            -not -path "$s/upper/data/etc" \
            -not -path "$s/upper/data/etc/resolv.conf" | head -1)
        if [ -n "$upper_files" ]; then
            echo "[ephemeral] auto-snapshot $id before destroy" >&2
            local snapfile=$(_chroot_snapshot_sandbox "$id" 2>/dev/null) || true
            if [ -n "$snapfile" ] && [ -f "$snapfile" ]; then
                _ephemeral_stage_and_push "$snapfile" \
                    "sandboxes/$id/snapshots/$(basename "$snapfile")"
            fi
        fi
        _s3_push_manifest "$id" 2>/dev/null || true
    fi

    # Unmount overlay
    _umount_overlay "$id"

    # Unmount snapshot
    [ -d "$s/images/_snapshot" ] && mountpoint -q "$s/images/_snapshot" 2>/dev/null && {
        sq-mount-layer --unmount "$s/images/_snapshot" 2>/dev/null || true
    }

    # Unmount all module images
    for d in "$s"/images/*.squashfs; do
        [ -d "$d" ] && mountpoint -q "$d" 2>/dev/null && {
            sq-mount-layer --unmount "$d" 2>/dev/null || true
        }
    done

    rm -rf "$s"
}

# ── Chroot backend: Mounted check ────────────────────────────────────

_chroot_mounted() {
    mountpoint -q "$(sdir "$1")/merged" 2>/dev/null
}

# ── Firecracker backend: Network ─────────────────────────────────────
# NOTE: Firecracker networking requires root (tap devices, iptables).
# This is separate from the unprivileged chroot path.

_allocate_netns_index() {
    # Find next free /30 subnet index (1-254)
    local idx=1
    while [ $idx -le 254 ]; do
        if ! ip link show "sq-*-tap" 2>/dev/null | grep -q "10.0.${idx}."; then
            echo "$idx"; return 0
        fi
        idx=$((idx + 1))
    done
    return 1
}

_firecracker_setup_network() {
    local id="$1"
    local s=$(sdir "$id")
    local tap="sq-${id}-tap"
    local sandbox_index
    sandbox_index=$(_allocate_netns_index) || { echo "subnet exhausted" >&2; return 1; }

    echo "$sandbox_index" > "$s/.meta/netns_index"

    # Create tap device
    ip tuntap add dev "$tap" mode tap
    ip addr add "10.0.${sandbox_index}.1/30" dev "$tap"
    ip link set "$tap" up

    # Enable IP forwarding
    echo 1 > /proc/sys/net/ipv4/ip_forward 2>/dev/null || true

    # NAT for outbound
    iptables -t nat -A POSTROUTING -s "10.0.${sandbox_index}.0/30" -j MASQUERADE
}

_firecracker_teardown_network() {
    local id="$1"
    local tap="sq-${id}-tap"
    local sandbox_index
    sandbox_index=$(cat "$(sdir "$id")/.meta/netns_index" 2>/dev/null || echo "")

    iptables -D FORWARD -i "$tap" -j "squash-${id}" 2>/dev/null || true
    iptables -F "squash-${id}" 2>/dev/null || true
    iptables -X "squash-${id}" 2>/dev/null || true

    if [ -n "$sandbox_index" ]; then
        iptables -t nat -D POSTROUTING -s "10.0.${sandbox_index}.0/30" -j MASQUERADE 2>/dev/null || true
    fi

    ip link delete "$tap" 2>/dev/null || true
}

# ── Firecracker backend: Create ──────────────────────────────────────

_firecracker_create_sandbox() {
    local id="$1" owner="$2" layer_csv="$3" task="${4:-}"
    local cpu="${5:-2}" memory_mb="${6:-1024}" max_lifetime_s="${7:-0}" allow_net="${8:-}"
    local s=$(sdir "$id")

    exists "$id" && { echo "already exists" >&2; return 1; }

    local max_sandboxes="${SQUASH_MAX_SANDBOXES:-100}"
    local current_count=$(ls -1d "$SANDBOXES"/*/ 2>/dev/null | wc -l)
    [ "$current_count" -ge "$max_sandboxes" ] && {
        echo "sandbox limit reached ($max_sandboxes)" >&2; return 1
    }

    # Validate layers
    local mod
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        mod_exists "$mod" || { echo "module not found: $mod" >&2; return 2; }
    done

    mkdir -p "$s/.meta/log"
    echo "$owner"          > "$s/.meta/owner"
    echo "$task"           > "$s/.meta/task"
    echo "$layer_csv"      > "$s/.meta/layers"
    date -Iseconds         > "$s/.meta/created"
    date -Iseconds         > "$s/.meta/last_active"
    echo "$cpu"            > "$s/.meta/cpu"
    echo "$memory_mb"      > "$s/.meta/memory_mb"
    echo "$max_lifetime_s" > "$s/.meta/max_lifetime_s"
    [ -n "$allow_net" ] && echo "$allow_net" > "$s/.meta/allow_net"

    # Set up tap device for networking
    _firecracker_setup_network "$id"

    # Collect squashfs paths (in layer order, base first)
    local sqfs_args=""
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        sqfs_args="$sqfs_args $(mod_path "$mod")"
    done

    sq-firecracker start "$id" "$cpu" "$memory_mb" $sqfs_args

    # TODO: Firecracker secret injection requires passing proxy config via vsock
    # or kernel cmdline. The guest would need to write /etc/profile.d/ on boot.
    # For now, secrets.json-based proxy injection only works in chroot mode.

    # Ephemeral mode: auto-restore from latest S3 snapshot
    if _ephemeral_enabled; then
        local latest=$(_s3_latest_snapshot "$id")
        if [ -n "$latest" ]; then
            echo "[ephemeral] restoring $id from S3 snapshot: $latest" >&2
            _firecracker_restore_sandbox "$id" "$latest" || \
                echo "[ephemeral] restore failed, continuing with clean sandbox" >&2
        fi
    fi
}

# ── Firecracker backend: Execute ─────────────────────────────────────

_firecracker_exec_in_sandbox() {
    local id="$1" cmd="$2" workdir="${3:-/}" timeout_s="${4:-300}"
    local s=$(sdir "$id")

    date -Iseconds > "$s/.meta/last_active"

    local seq=$(( $(ls "$s/.meta/log/" 2>/dev/null | wc -l) + 1 ))
    local logf="$s/.meta/log/$(printf '%04d' $seq).json"
    local t0=$(date -Iseconds)

    # Send command to VM
    local result
    result=$(sq-firecracker exec "$id" "$cmd" "$workdir" "$timeout_s")

    local t1=$(date -Iseconds)
    local rc=$(echo "$result" | jq -r '.exit_code')
    local out=$(echo "$result" | jq -r '.stdout')
    local err=$(echo "$result" | jq -r '.stderr')

    jq -n \
        --argjson seq "$seq" --arg cmd "$cmd" --arg workdir "$workdir" \
        --argjson rc "$rc" --arg t0 "$t0" --arg t1 "$t1" \
        --arg stdout "$out" --arg stderr "$err" \
        '{seq:$seq,cmd:$cmd,workdir:$workdir,exit_code:$rc,started:$t0,finished:$t1,stdout:$stdout,stderr:$stderr}' \
        > "$logf"

    cat "$logf"
    return $rc
}

# ── Firecracker backend: Activate module ─────────────────────────────

_firecracker_activate_module() {
    local id="$1" mod="$2"
    local s=$(sdir "$id")

    mod_exists "$mod" || { echo "module not found: $mod" >&2; return 1; }

    sq-firecracker add-drive "$id" "$mod" "$(mod_path "$mod")"

    # Update layers list
    printf ',%s' "$mod" >> "$s/.meta/layers"
}

# ── Firecracker backend: Snapshot ────────────────────────────────────

_firecracker_snapshot_sandbox() {
    local id="$1" label="${2:-$(date +%Y%m%d-%H%M%S)}"
    valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
    local s=$(sdir "$id")
    local snapdir="$s/snapshots"
    local snapfile="$snapdir/$label.squashfs"

    [ -f "$snapfile" ] && { echo "exists: $label" >&2; return 1; }
    mkdir -p "$snapdir"

    # Ask the VM to create a snapshot of its upper layer
    sq-firecracker exec "$id" "__squash_snapshot $snapfile" "/" "60"

    local size=$(stat -c%s "$snapfile" 2>/dev/null || echo 0)
    jq -n --arg label "$label" --arg created "$(date -Iseconds)" --argjson size "$size" \
        '{label:$label,created:$created,size:$size}' >> "$s/.meta/snapshots.jsonl"

    # Notify sidecar for background S3 push (preferred) or push directly
    if command -v sq-sync >/dev/null 2>&1; then
        sq-sync --notify "{\"op\":\"push\",\"path\":\"$snapfile\",\"key\":\"sandboxes/$id/snapshots/$label.squashfs\"}"
    elif _s3_enabled; then
        sq-s3 push-bg "$snapfile" "sandboxes/$id/snapshots/$label.squashfs"
    fi

    echo "$snapfile"
}

# ── Firecracker backend: Restore ─────────────────────────────────────

_firecracker_restore_sandbox() {
    local id="$1" label="$2"
    valid_label "$label" || { echo "label: alphanumeric/dash/underscore/dot only" >&2; return 1; }
    local s=$(sdir "$id")
    local snapfile="$s/snapshots/$label.squashfs"

    if [ ! -f "$snapfile" ]; then
        if _s3_enabled; then
            mkdir -p "$(dirname "$snapfile")"
            sq-s3 pull "sandboxes/$id/snapshots/$label.squashfs" "$snapfile" 2>/dev/null || {
                echo "not found: $label" >&2; return 1; }
        else
            echo "not found: $label" >&2; return 1
        fi
    fi

    # Stop VM
    sq-firecracker stop "$id"

    # Record active snapshot
    echo "$label" > "$s/.meta/active_snapshot"

    # Restart VM (it will pick up the snapshot on remount)
    local cpu=$(cat "$s/.meta/cpu" 2>/dev/null || echo 2)
    local mem=$(cat "$s/.meta/memory_mb" 2>/dev/null || echo 1024)
    local sqfs_args=""
    for mod in $(cat "$s/.meta/layers" | tr ',' ' '); do
        sqfs_args="$sqfs_args $(mod_path "$mod")"
    done
    # Add snapshot as additional layer
    sqfs_args="$sqfs_args $snapfile"

    sq-firecracker start "$id" "$cpu" "$mem" $sqfs_args
}

# ── Firecracker backend: Destroy ─────────────────────────────────────

_firecracker_destroy_sandbox() {
    local id="$1"
    local s=$(sdir "$id")
    [ ! -d "$s" ] && return 0

    # Ephemeral mode: auto-snapshot to S3 before destroying
    if _ephemeral_enabled; then
        echo "[ephemeral] auto-snapshot $id before destroy" >&2
        local snapfile=$(_firecracker_snapshot_sandbox "$id" 2>/dev/null) || true
        if [ -n "$snapfile" ] && [ -f "$snapfile" ]; then
            _ephemeral_stage_and_push "$snapfile" \
                "sandboxes/$id/snapshots/$(basename "$snapfile")"
        fi
        _s3_push_manifest "$id" 2>/dev/null || true
    fi

    sq-firecracker stop "$id" 2>/dev/null || true
    _firecracker_teardown_network "$id" 2>/dev/null || true
    rm -rf "$s"
}

# ── Firecracker backend: Mounted check ───────────────────────────────

_firecracker_mounted() {
    local pid_file="$(sdir "$1")/.meta/fc.pid"
    [ -f "$pid_file" ] && kill -0 "$(cat "$pid_file")" 2>/dev/null
}

# ── gVisor backend ────────────────────────────────────────────────────
#
# Uses runsc (gVisor) for process isolation with the same overlayfs
# infrastructure as the chroot backend. Provides user-space kernel
# isolation (Sentry) without requiring /dev/kvm like Firecracker.

RUNSC_ROOT="${SQUASH_DATA:-/data}/runsc"

_gvisor_container_id() { echo "sq-$1"; }

_gvisor_write_oci_config() {
    local id="$1" merged_path="$2" netns_name="$3" cpu="$4" mem_mb="$5"
    local s=$(sdir "$id")
    local bundle_dir="$s/oci"
    local cpu_quota=$(( ${cpu:-2} * 100000 ))
    local mem_bytes=$(( ${mem_mb:-1024} * 1048576 ))
    mkdir -p "$bundle_dir"

    local netns_json=""
    if [ -n "$netns_name" ]; then
        netns_json=',{"type":"network","path":"/var/run/netns/'"$netns_name"'"}'
    fi

    cat > "$bundle_dir/config.json" <<EOCFG
{"ociVersion":"1.0.0","process":{"terminal":false,"user":{"uid":0,"gid":0},"args":["sleep","infinity"],"env":["PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin","HOME=/root","USER=root","TERM=xterm"],"cwd":"/"},"root":{"path":"$merged_path","readonly":false},"mounts":[{"destination":"/proc","type":"proc","source":"proc"},{"destination":"/dev","type":"tmpfs","source":"tmpfs","options":["nosuid","strictatime","mode=755","size=65536k"]},{"destination":"/dev/pts","type":"devpts","source":"devpts","options":["nosuid","noexec","newinstance","ptmxmode=0666","mode=0620"]},{"destination":"/sys","type":"sysfs","source":"sysfs","options":["nosuid","noexec","nodev","ro"]}],"linux":{"namespaces":[{"type":"pid"},{"type":"ipc"},{"type":"uts"},{"type":"mount"}${netns_json}],"resources":{"memory":{"limit":${mem_bytes}},"cpu":{"quota":${cpu_quota},"period":100000}}}}
EOCFG
}

_gvisor_create_sandbox() {
    local id="$1" layers="$2" cpu="$3" mem="$4" lifetime="$5" allow_net="$6"
    local s=$(sdir "$id")
    local cid=$(_gvisor_container_id "$id")
    local merged="$s/merged"
    local bundle="$s/oci"

    mkdir -p "$s/upper" "$s/merged" "$s/.meta/log" "$s/images" "$s/snapshots"

    # Mount tmpfs + squashfs + overlay (same as chroot)
    _chroot_mount_layers "$id" "$layers"

    # Setup netns (same as chroot)
    _setup_network "$id" "$allow_net"
    local netns_name="squash-$id"

    # Seed resolv.conf
    _seed_resolv_conf "$id"

    # Write OCI config
    _gvisor_write_oci_config "$id" "$merged" "$netns_name" "$cpu" "$mem"

    # Start runsc container
    mkdir -p "$RUNSC_ROOT"
    runsc --root="$RUNSC_ROOT" --overlay2=none create --bundle="$bundle" "$cid" || {
        echo "runsc create failed" >&2; return 1
    }
    runsc --root="$RUNSC_ROOT" start "$cid" || {
        runsc --root="$RUNSC_ROOT" delete --force "$cid" 2>/dev/null
        echo "runsc start failed" >&2; return 1
    }

    # Write metadata
    _write_meta "$id" "$layers" "$cpu" "$mem" "$lifetime"
}

_gvisor_exec_in_sandbox() {
    local id="$1" cmd="$2" workdir="${3:-/}" timeout_s="${4:-300}"
    local cid=$(_gvisor_container_id "$id")
    local s=$(sdir "$id")

    local stdout_file=$(mktemp) stderr_file=$(mktemp)
    local start_ts=$(date +%s)

    timeout "${timeout_s}s" runsc --root="$RUNSC_ROOT" exec \
        --user=root "--cwd=$workdir" "$cid" \
        /bin/sh -c "$cmd" \
        >"$stdout_file" 2>"$stderr_file"
    local exit_code=$?

    local end_ts=$(date +%s)
    local stdout_str=$(head -c 65536 "$stdout_file")
    local stderr_str=$(head -c 65536 "$stderr_file")
    rm -f "$stdout_file" "$stderr_file"

    # Write exec log
    local seq=$(_next_seq "$id")
    local duration_ms=$(( (end_ts - start_ts) * 1000 ))
    _write_exec_log "$id" "$seq" "$cmd" "$workdir" "$exit_code" \
                    "$start_ts" "$end_ts" "$duration_ms" \
                    "$stdout_str" "$stderr_str"

    # Output JSON result
    jq -n --arg ec "$exit_code" --arg out "$stdout_str" --arg err "$stderr_str" \
           --argjson started "$start_ts" --argjson finished "$end_ts" \
           --argjson dur "$duration_ms" --argjson seq "$seq" \
        '{exit_code:($ec|tonumber),stdout:$out,stderr:$err,started:$started,finished:$finished,duration_ms:$dur,seq:$seq}'
}

_gvisor_activate_module() {
    local id="$1" module="$2"
    local cid=$(_gvisor_container_id "$id")

    # Stop the container
    runsc --root="$RUNSC_ROOT" kill "$cid" SIGKILL 2>/dev/null || true
    sleep 0.1
    runsc --root="$RUNSC_ROOT" delete --force "$cid" 2>/dev/null || true

    # Remount overlay with new layer (same as chroot)
    _chroot_activate_module "$id" "$module"

    # Regenerate OCI config and restart
    local s=$(sdir "$id")
    local netns_name="squash-$id"
    _gvisor_write_oci_config "$id" "$s/merged" "$netns_name" 2 1024
    runsc --root="$RUNSC_ROOT" --overlay2=none create --bundle="$s/oci" "$cid"
    runsc --root="$RUNSC_ROOT" start "$cid"
}

_gvisor_snapshot_sandbox() {
    # gVisor uses same host-side overlayfs as chroot — snapshot is identical
    _chroot_snapshot_sandbox "$@"
}

_gvisor_restore_sandbox() {
    local id="$1" label="$2"
    local cid=$(_gvisor_container_id "$id")

    # Stop the container
    runsc --root="$RUNSC_ROOT" kill "$cid" SIGKILL 2>/dev/null || true
    sleep 0.1
    runsc --root="$RUNSC_ROOT" delete --force "$cid" 2>/dev/null || true

    # Restore overlay (same as chroot)
    _chroot_restore_sandbox "$id" "$label"

    # Regenerate OCI config and restart
    local s=$(sdir "$id")
    local netns_name="squash-$id"
    _gvisor_write_oci_config "$id" "$s/merged" "$netns_name" 2 1024
    runsc --root="$RUNSC_ROOT" --overlay2=none create --bundle="$s/oci" "$cid"
    runsc --root="$RUNSC_ROOT" start "$cid"
}

_gvisor_destroy_sandbox() {
    local id="$1"
    local cid=$(_gvisor_container_id "$id")

    # Stop runsc container
    runsc --root="$RUNSC_ROOT" kill "$cid" SIGKILL 2>/dev/null || true
    sleep 0.1
    runsc --root="$RUNSC_ROOT" delete --force "$cid" 2>/dev/null || true

    # Unmount filesystems (same as chroot)
    _chroot_destroy_sandbox "$id"
}

_gvisor_mounted() {
    local id="$1"
    local cid=$(_gvisor_container_id "$id")
    runsc --root="$RUNSC_ROOT" state "$cid" 2>/dev/null | grep -q '"running"'
}

# ── Backend dispatch wrappers ─────────────────────────────────────────

create_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_create_sandbox "$@" ;;
        firecracker) _firecracker_create_sandbox "$@" ;;
        gvisor)      _gvisor_create_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

exec_in_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_exec_in_sandbox "$@" ;;
        firecracker) _firecracker_exec_in_sandbox "$@" ;;
        gvisor)      _gvisor_exec_in_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

activate_module() {
    case "$BACKEND" in
        chroot)      _chroot_activate_module "$@" ;;
        firecracker) _firecracker_activate_module "$@" ;;
        gvisor)      _gvisor_activate_module "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

snapshot_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_snapshot_sandbox "$@" ;;
        firecracker) _firecracker_snapshot_sandbox "$@" ;;
        gvisor)      _gvisor_snapshot_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

restore_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_restore_sandbox "$@" ;;
        firecracker) _firecracker_restore_sandbox "$@" ;;
        gvisor)      _gvisor_restore_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

destroy_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_destroy_sandbox "$@" ;;
        firecracker) _firecracker_destroy_sandbox "$@" ;;
        gvisor)      _gvisor_destroy_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

mounted() {
    case "$BACKEND" in
        chroot)      _chroot_mounted "$@" ;;
        firecracker) _firecracker_mounted "$@" ;;
        gvisor)      _gvisor_mounted "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

# ── Info ───────────────────────────────────────────────────────────────

sandbox_info() {
    local id="$1"
    local s=$(sdir "$id")
    [ ! -d "$s/.meta" ] && return 1

    local layers=$(cat "$s/.meta/layers" 2>/dev/null || echo "")
    local layers_json=$(echo "$layers" | tr ',' '\n' | jq -R . | jq -s '.')
    local snapshots="[]"
    [ -f "$s/.meta/snapshots.jsonl" ] && snapshots=$(jq -s '.' "$s/.meta/snapshots.jsonl" 2>/dev/null || echo '[]')
    local active_snap="null"
    [ -f "$s/.meta/active_snapshot" ] && active_snap="\"$(cat "$s/.meta/active_snapshot")\""
    local upper_bytes=$(du -sb "$s/upper/data" 2>/dev/null | cut -f1 || echo 0)

    local cpu=$(cat "$s/.meta/cpu" 2>/dev/null || echo "2")
    local memory_mb=$(cat "$s/.meta/memory_mb" 2>/dev/null || echo "1024")
    local max_lifetime_s=$(cat "$s/.meta/max_lifetime_s" 2>/dev/null || echo "0")
    local allow_net=$(cat "$s/.meta/allow_net" 2>/dev/null || echo "null")
    # Validate allow_net is valid JSON, otherwise wrap as string
    echo "$allow_net" | jq empty 2>/dev/null || allow_net="null"

    jq -n \
        --arg id "$id" \
        --arg owner "$(cat "$s/.meta/owner" 2>/dev/null)" \
        --arg task "$(cat "$s/.meta/task" 2>/dev/null)" \
        --argjson layers "$layers_json" \
        --arg created "$(cat "$s/.meta/created" 2>/dev/null)" \
        --arg last_active "$(cat "$s/.meta/last_active" 2>/dev/null)" \
        --argjson mounted "$(mounted "$id" && echo true || echo false)" \
        --argjson exec_count "$(ls "$s/.meta/log/" 2>/dev/null | wc -l)" \
        --argjson upper_bytes "${upper_bytes:-0}" \
        --argjson snapshots "$snapshots" \
        --argjson active_snapshot "$active_snap" \
        --argjson cpu "$cpu" \
        --argjson memory_mb "$memory_mb" \
        --argjson max_lifetime_s "$max_lifetime_s" \
        --argjson allow_net "$allow_net" \
        '{id:$id,owner:$owner,task:$task,layers:$layers,created:$created,
          last_active:$last_active,mounted:$mounted,exec_count:$exec_count,
          upper_bytes:$upper_bytes,snapshots:$snapshots,active_snapshot:$active_snapshot,
          cpu:$cpu,memory_mb:$memory_mb,max_lifetime_s:$max_lifetime_s,allow_net:$allow_net}'
}

list_sandboxes() {
    {
        for d in "$SANDBOXES"/*/; do
            [ ! -d "$d/.meta" ] && continue
            sandbox_info "$(basename "$d")"
        done
    } | jq -s '.'
}
