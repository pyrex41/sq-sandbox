#!/bin/sh
# squash/cgi-bin/common.sh
# Shared functions for all CGI handlers and CLI tools.
# Depends: jq, mksquashfs, losetup, mount, unshare, chroot

set -eu

DATA="${SQUASH_DATA:-/data}"
MODULES="$DATA/modules"
SANDBOXES="$DATA/sandboxes"
BACKEND="${SQUASH_BACKEND:-chroot}"

# ── S3 sync ──────────────────────────────────────────────────────────

_s3_enabled() { [ -n "${SQUASH_S3_BUCKET:-}" ] && command -v sq-s3 >/dev/null 2>&1; }

# ── Ephemeral mode ────────────────────────────────────────────────────

_ephemeral_enabled() { [ "${SQUASH_EPHEMERAL:-}" = "1" ] && _s3_enabled; }

_s3_push_manifest() {
    local id="$1" s=$(sdir "$id")
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
        500) printf 'Status: 500 Internal Server Error\r\n' ;;
    esac
    printf 'Content-Type: application/json\r\n\r\n'
    echo "$body"
}

json_ok()    { json_response 200 "$1"; }
json_err()   { json_response "$1" "{\"error\":$(echo "$2" | jq -Rs .)}"; }
read_body()  { dd bs=1 count="${CONTENT_LENGTH:-0}" 2>/dev/null; }

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
    [ -f "$(mod_path "$1")" ] && return 0
    _s3_enabled && sq-s3 pull "modules/$1.squashfs" "$(mod_path "$1")" 2>/dev/null && return 0
    return 1
}

list_modules() {
    local out="["
    local first=true
    local seen=""
    for f in "$MODULES"/*.squashfs; do
        [ ! -f "$f" ] && continue
        local name=$(basename "$f" .squashfs)
        local size=$(stat -c%s "$f" 2>/dev/null || echo 0)
        $first || out="$out,"
        first=false
        out="$out{\"name\":\"$name\",\"size\":$size,\"location\":\"local\"}"
        seen="$seen $name "
    done
    # Include remote-only modules from S3
    if _s3_enabled; then
        sq-s3 list "modules/" 2>/dev/null | while read -r key; do
            [ -z "$key" ] && continue
            local rname=$(basename "$key" .squashfs)
            case "$seen" in *" $rname "*) continue ;; esac
            $first || out="$out,"
            first=false
            out="$out{\"name\":\"$rname\",\"size\":0,\"location\":\"remote\"}"
        done
    fi
    echo "$out]"
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
    mount -t overlay overlay \
        -o "lowerdir=$lower,upperdir=$s/upper,workdir=$s/work" \
        "$s/merged"
}

_umount_overlay() {
    local s=$(sdir "$1")
    umount "$s/merged" 2>/dev/null || umount -l "$s/merged" 2>/dev/null || true
}

# ── cgroup helpers (Phase 2) ──────────────────────────────────────────

_cgroup_setup() {
    local id="$1" cpu="$2" mem_mb="$3"
    local cg="/sys/fs/cgroup/squash-$id"
    mkdir -p "$cg"

    # CPU limit (cpu.max: quota period_us)
    # cpu=1.0 means 100000us per 100000us period
    local quota=$(echo "$cpu * 100000" | bc | cut -d. -f1)
    echo "$quota 100000" > "$cg/cpu.max"

    # Memory limit
    local mem_bytes=$((mem_mb * 1024 * 1024))
    echo "$mem_bytes" > "$cg/memory.max"

    echo "$cg"
}

_cgroup_teardown() {
    local id="$1"
    local cg="/sys/fs/cgroup/squash-$id"
    rmdir "$cg" 2>/dev/null || true
}

# ── Network namespace helpers (Phase 3) ───────────────────────────────

_chroot_setup_netns() {
    local id="$1" allow_net="$2"
    local veth_host="sq-${id}-h"
    local veth_sandbox="sq-${id}-s"

    # Derive a unique index from the sandbox id (hash to 1-254 range)
    local sandbox_index=$(printf '%s' "$id" | cksum | awk '{print ($1 % 254) + 1}')

    # Create veth pair
    ip link add "$veth_host" type veth peer name "$veth_sandbox"
    ip addr add "10.200.${sandbox_index}.1/30" dev "$veth_host"
    ip link set "$veth_host" up

    # Store the sandbox index for later use
    echo "$sandbox_index" > "$(sdir "$id")/.meta/netns_index"

    # Note: In production, the sandbox end would be moved into the sandbox's
    # network namespace. For chroot mode with --net in unshare, the veth_sandbox
    # end is configured when exec runs. Store interface names for cleanup.
    echo "$veth_host" > "$(sdir "$id")/.meta/veth_host"
    echo "$veth_sandbox" > "$(sdir "$id")/.meta/veth_sandbox"

    # Configure the sandbox end on the host side for now
    ip addr add "10.200.${sandbox_index}.2/30" dev "$veth_sandbox"
    ip link set "$veth_sandbox" up

    # Enable IP forwarding
    echo 1 > /proc/sys/net/ipv4/ip_forward 2>/dev/null || true

    # NAT on host for outbound
    iptables -t nat -A POSTROUTING -s "10.200.${sandbox_index}.0/30" -j MASQUERADE

    # Egress filtering
    if [ -n "$allow_net" ] && [ "$allow_net" != "[]" ]; then
        _apply_egress_rules "$id" "$veth_host" "$allow_net"
    fi
}

_apply_egress_rules() {
    local id="$1" iface="$2" allow_net="$3"
    local chain="squash-${id}"

    iptables -N "$chain" 2>/dev/null || true
    iptables -A FORWARD -i "$iface" -j "$chain"

    # Allow DNS (needed to resolve allowed hosts)
    iptables -A "$chain" -p udp --dport 53 -j ACCEPT
    iptables -A "$chain" -p tcp --dport 53 -j ACCEPT

    # Allow established/related connections
    iptables -A "$chain" -m state --state ESTABLISHED,RELATED -j ACCEPT

    # Allow each host pattern
    echo "$allow_net" | jq -r '.[]' 2>/dev/null | while read -r host; do
        case "$host" in
            none) ;; # block everything
            *\**)
                # Wildcard: resolve at connection time via ipset or DNS-based matching
                # For MVP: log + warn that wildcards need sq-secret-proxy
                echo "[net] WARN: wildcard $host requires proxy mode" >&2
                ;;
            *)
                # Resolve and allow
                for ip in $(getent hosts "$host" 2>/dev/null | awk '{print $1}'); do
                    iptables -A "$chain" -d "$ip" -j ACCEPT
                done
                ;;
        esac
    done

    # Default: drop
    iptables -A "$chain" -j DROP
}

_chroot_teardown_netns() {
    local id="$1"
    local sandbox_index
    sandbox_index=$(cat "$(sdir "$id")/.meta/netns_index" 2>/dev/null || echo "")

    iptables -D FORWARD -i "sq-${id}-h" -j "squash-${id}" 2>/dev/null || true
    iptables -F "squash-${id}" 2>/dev/null || true
    iptables -X "squash-${id}" 2>/dev/null || true

    # Remove NAT rule
    if [ -n "$sandbox_index" ]; then
        iptables -t nat -D POSTROUTING -s "10.200.${sandbox_index}.0/30" -j MASQUERADE 2>/dev/null || true
    fi

    ip link delete "sq-${id}-h" 2>/dev/null || true
}

# ── Chroot backend: Create ────────────────────────────────────────────

_chroot_create_sandbox() {
    local id="$1" owner="$2" layer_csv="$3" task="${4:-}"
    local cpu="${5:-2}" memory_mb="${6:-1024}" max_lifetime_s="${7:-0}" allow_net="${8:-}"
    local s=$(sdir "$id")

    exists "$id" && { echo "already exists" >&2; return 1; }

    # Validate layers
    local mod
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        mod_exists "$mod" || { echo "module not found: $mod" >&2; return 2; }
    done

    # Build directory tree
    mkdir -p "$s/images" "$s/upper" "$s/work" "$s/merged" "$s/.meta/log"

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

    # Loop-mount each module
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        local mp="$s/images/$mod.squashfs"
        mkdir -p "$mp"
        mount -o loop,ro -t squashfs "$(mod_path "$mod")" "$mp" || {
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

    # Seed /etc/resolv.conf for network access
    if [ -f /etc/resolv.conf ]; then
        mkdir -p "$s/upper/etc"
        cp /etc/resolv.conf "$s/upper/etc/resolv.conf"
    fi

    # Set up network namespace if allow_net is specified
    if [ -n "$allow_net" ] && [ "$allow_net" != "[]" ]; then
        _chroot_setup_netns "$id" "$allow_net" 2>/dev/null || true
    fi

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

    # Put process in cgroup before exec if cgroup exists
    local cg="/sys/fs/cgroup/squash-$id"
    if [ -d "$cg" ]; then
        echo $$ > "$cg/cgroup.procs" 2>/dev/null || true
    fi

    timeout "$timeout_s" \
        unshare --mount --pid --fork --map-root-user \
        chroot "$s/merged" \
        /bin/sh -c "cd $workdir 2>/dev/null || true; $cmd" \
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
    mkdir -p "$mp"
    mount -o loop,ro -t squashfs "$(mod_path "$mod")" "$mp"

    # Remount overlay (upper preserved)
    _umount_overlay "$id"
    _mount_overlay "$id"

    # Update layers list
    printf ',%s' "$mod" >> "$s/.meta/layers"
}

# ── Chroot backend: Snapshot ──────────────────────────────────────────

_chroot_snapshot_sandbox() {
    local id="$1" label="${2:-$(date +%Y%m%d-%H%M%S)}"
    local s=$(sdir "$id")
    local snapdir="$s/snapshots"
    local snapfile="$snapdir/$label.squashfs"

    [ -f "$snapfile" ] && { echo "exists: $label" >&2; return 1; }

    mkdir -p "$snapdir"
    mksquashfs "$s/upper" "$snapfile" -comp gzip -b 256K -noappend -quiet

    local size=$(stat -c%s "$snapfile")
    printf '{"label":"%s","created":"%s","size":%s}\n' \
        "$label" "$(date -Iseconds)" "$size" >> "$s/.meta/snapshots.jsonl"

    _s3_enabled && sq-s3 push-bg "$snapfile" "sandboxes/$id/snapshots/$label.squashfs"

    echo "$snapfile"
}

# ── Chroot backend: Restore ──────────────────────────────────────────

_chroot_restore_sandbox() {
    local id="$1" label="$2"
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

    # Tear down current overlay
    _umount_overlay "$id"

    # Unmount previous snapshot layer if any
    if mountpoint -q "$s/images/_snapshot" 2>/dev/null; then
        umount "$s/images/_snapshot" 2>/dev/null || umount -l "$s/images/_snapshot"
    fi

    # Clear upper + work
    rm -rf "$s/upper" "$s/work"
    mkdir -p "$s/upper" "$s/work"

    # Mount snapshot as top read-only layer
    mkdir -p "$s/images/_snapshot"
    mount -o loop,ro -t squashfs "$snapfile" "$s/images/_snapshot"

    # Record active snapshot
    echo "$label" > "$s/.meta/active_snapshot"

    # Remount overlay
    _mount_overlay "$id"

    # Re-seed DNS
    if [ -f /etc/resolv.conf ]; then
        mkdir -p "$s/upper/etc"
        cp /etc/resolv.conf "$s/upper/etc/resolv.conf"
    fi
}

# ── Chroot backend: Destroy ──────────────────────────────────────────

_chroot_destroy_sandbox() {
    local id="$1" s=$(sdir "$id")
    [ ! -d "$s" ] && return 0

    # Ephemeral mode: auto-snapshot to S3 before destroying
    if _ephemeral_enabled && [ -d "$s/upper" ]; then
        local upper_files=$(find "$s/upper" -mindepth 1 \
            -not -path "$s/upper/etc" \
            -not -path "$s/upper/etc/resolv.conf" | head -1)
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

    # Tear down network namespace
    _chroot_teardown_netns "$id" 2>/dev/null || true

    # Tear down cgroup
    _cgroup_teardown "$id"

    # Unmount overlay
    _umount_overlay "$id"

    # Unmount snapshot
    [ -d "$s/images/_snapshot" ] && {
        umount "$s/images/_snapshot" 2>/dev/null || umount -l "$s/images/_snapshot" 2>/dev/null || true
    }

    # Unmount all module images
    for d in "$s"/images/*.squashfs; do
        [ -d "$d" ] && { umount "$d" 2>/dev/null || umount -l "$d" 2>/dev/null || true; }
    done

    rm -rf "$s"
}

# ── Chroot backend: Mounted check ────────────────────────────────────

_chroot_mounted() {
    grep -q "$(sdir "$1")/merged" /proc/mounts 2>/dev/null
}

# ── Firecracker backend: Network ─────────────────────────────────────

_firecracker_setup_network() {
    local id="$1"
    local s=$(sdir "$id")
    local tap="sq-${id}-tap"
    local sandbox_index=$(printf '%s' "$id" | cksum | awk '{print ($1 % 254) + 1}')

    echo "$sandbox_index" > "$s/.meta/netns_index"

    # Create tap device
    ip tuntap add dev "$tap" mode tap
    ip addr add "10.0.${sandbox_index}.1/30" dev "$tap"
    ip link set "$tap" up

    # Enable IP forwarding
    echo 1 > /proc/sys/net/ipv4/ip_forward 2>/dev/null || true

    # NAT for outbound
    iptables -t nat -A POSTROUTING -s "10.0.${sandbox_index}.0/30" -j MASQUERADE

    # Egress rules if allow_net is set
    local allow_net=$(cat "$s/.meta/allow_net" 2>/dev/null || echo "")
    if [ -n "$allow_net" ] && [ "$allow_net" != "[]" ]; then
        _apply_egress_rules "$id" "$tap" "$allow_net"
    fi
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
    local s=$(sdir "$id")
    local snapdir="$s/snapshots"
    local snapfile="$snapdir/$label.squashfs"

    [ -f "$snapfile" ] && { echo "exists: $label" >&2; return 1; }
    mkdir -p "$snapdir"

    # Ask the VM to create a snapshot of its upper layer
    sq-firecracker exec "$id" "__squash_snapshot $snapfile" "/" "60"

    local size=$(stat -c%s "$snapfile" 2>/dev/null || echo 0)
    printf '{"label":"%s","created":"%s","size":%s}\n' \
        "$label" "$(date -Iseconds)" "$size" >> "$s/.meta/snapshots.jsonl"

    _s3_enabled && sq-s3 push-bg "$snapfile" "sandboxes/$id/snapshots/$label.squashfs"

    echo "$snapfile"
}

# ── Firecracker backend: Restore ─────────────────────────────────────

_firecracker_restore_sandbox() {
    local id="$1" label="$2"
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
    local id="$1" s=$(sdir "$id")
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

# ── Backend dispatch wrappers ─────────────────────────────────────────

create_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_create_sandbox "$@" ;;
        firecracker) _firecracker_create_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

exec_in_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_exec_in_sandbox "$@" ;;
        firecracker) _firecracker_exec_in_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

activate_module() {
    case "$BACKEND" in
        chroot)      _chroot_activate_module "$@" ;;
        firecracker) _firecracker_activate_module "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

snapshot_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_snapshot_sandbox "$@" ;;
        firecracker) _firecracker_snapshot_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

restore_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_restore_sandbox "$@" ;;
        firecracker) _firecracker_restore_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

destroy_sandbox() {
    case "$BACKEND" in
        chroot)      _chroot_destroy_sandbox "$@" ;;
        firecracker) _firecracker_destroy_sandbox "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

mounted() {
    case "$BACKEND" in
        chroot)      _chroot_mounted "$@" ;;
        firecracker) _firecracker_mounted "$@" ;;
        *) echo "unknown backend: $BACKEND" >&2; return 1 ;;
    esac
}

# ── Info ───────────────────────────────────────────────────────────────

sandbox_info() {
    local id="$1" s=$(sdir "$id")
    [ ! -d "$s/.meta" ] && return 1

    local layers=$(cat "$s/.meta/layers" 2>/dev/null || echo "")
    local layers_json=$(echo "$layers" | tr ',' '\n' | jq -R . | jq -s '.')
    local snapshots="[]"
    [ -f "$s/.meta/snapshots.jsonl" ] && snapshots=$(jq -s '.' "$s/.meta/snapshots.jsonl" 2>/dev/null || echo '[]')
    local active_snap="null"
    [ -f "$s/.meta/active_snapshot" ] && active_snap="\"$(cat "$s/.meta/active_snapshot")\""
    local upper_bytes=$(du -sb "$s/upper" 2>/dev/null | cut -f1 || echo 0)

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
    local out="[" first=true
    for d in "$SANDBOXES"/*/; do
        [ ! -d "$d/.meta" ] && continue
        local id=$(basename "$d")
        $first || out="$out,"
        first=false
        out="$out$(sandbox_info "$id")"
    done
    echo "$out]"
}
