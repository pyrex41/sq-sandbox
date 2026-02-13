#!/bin/sh
# squash/cgi-bin/common.sh
# Shared functions for all CGI handlers and CLI tools.
# Depends: jq, mksquashfs, losetup, mount, unshare, chroot

set -eu

DATA="${SQUASH_DATA:-/data}"
MODULES="$DATA/modules"
SANDBOXES="$DATA/sandboxes"

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
mod_exists() { [ -f "$(mod_path "$1")" ]; }

list_modules() {
    local out="["
    local first=true
    for f in "$MODULES"/*.squashfs; do
        [ ! -f "$f" ] && continue
        local name=$(basename "$f" .squashfs)
        local size=$(stat -c%s "$f" 2>/dev/null || echo 0)
        $first || out="$out,"
        first=false
        out="$out{\"name\":\"$name\",\"size\":$size}"
    done
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
#     log/
#       0001.json    execution log
#       0002.json
#     snapshots.jsonl   snapshot manifest
#   images/
#     000-base-alpine.squashfs/    ← loop mount point
#     100-python312.squashfs/      ← loop mount point
#     _snapshot/                   ← active snapshot mount
#   upper/           writable overlay layer
#   work/            overlayfs workdir
#   merged/          final chroot target

sdir()    { echo "$SANDBOXES/$1"; }
exists()  { [ -d "$(sdir "$1")/.meta" ]; }
mounted() { grep -q "$(sdir "$1")/merged" /proc/mounts 2>/dev/null; }

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

# ── Create ─────────────────────────────────────────────────────────────

create_sandbox() {
    local id="$1" owner="$2" layer_csv="$3" task="${4:-}"
    local s=$(sdir "$id")

    exists "$id" && { echo "already exists" >&2; return 1; }

    # Validate layers
    local mod
    for mod in $(echo "$layer_csv" | tr ',' ' '); do
        mod_exists "$mod" || { echo "module not found: $mod" >&2; return 2; }
    done

    # Build directory tree
    mkdir -p "$s"/{images,upper,work,merged,.meta/log}

    # Metadata as files
    echo "$owner"      > "$s/.meta/owner"
    echo "$task"       > "$s/.meta/task"
    echo "$layer_csv"  > "$s/.meta/layers"
    date -Iseconds     > "$s/.meta/created"
    date -Iseconds     > "$s/.meta/last_active"

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
}

# ── Execute ────────────────────────────────────────────────────────────

exec_in_sandbox() {
    local id="$1" cmd="$2" workdir="${3:-/}" timeout_s="${4:-300}"
    local s=$(sdir "$id")

    mounted "$id" || { echo "not mounted" >&2; return 1; }

    date -Iseconds > "$s/.meta/last_active"

    # Log sequence
    local seq=$(( $(ls "$s/.meta/log/" 2>/dev/null | wc -l) + 1 ))
    local logf="$s/.meta/log/$(printf '%04d' $seq).json"

    local t0=$(date -Iseconds)
    local out=$(mktemp) err=$(mktemp)
    local rc=0

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

# ── Activate module on running sandbox ─────────────────────────────────

activate_module() {
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

# ── Snapshot ───────────────────────────────────────────────────────────

snapshot_sandbox() {
    local id="$1" label="${2:-$(date +%Y%m%d-%H%M%S)}"
    local s=$(sdir "$id")
    local snapdir="$s/snapshots"
    local snapfile="$snapdir/$label.squashfs"

    [ -f "$snapfile" ] && { echo "exists: $label" >&2; return 1; }

    mkdir -p "$snapdir"
    mksquashfs "$s/upper" "$snapfile" -comp zstd -b 256K -noappend -quiet

    local size=$(stat -c%s "$snapfile")
    printf '{"label":"%s","created":"%s","size":%s}\n' \
        "$label" "$(date -Iseconds)" "$size" >> "$s/.meta/snapshots.jsonl"

    echo "$snapfile"
}

# ── Restore ────────────────────────────────────────────────────────────

restore_sandbox() {
    local id="$1" label="$2"
    local s=$(sdir "$id")
    local snapfile="$s/snapshots/$label.squashfs"

    [ -f "$snapfile" ] || { echo "not found: $label" >&2; return 1; }

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

# ── Destroy ────────────────────────────────────────────────────────────

destroy_sandbox() {
    local id="$1" s=$(sdir "$id")
    [ ! -d "$s" ] && return 0

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
        '{id:$id,owner:$owner,task:$task,layers:$layers,created:$created,
          last_active:$last_active,mounted:$mounted,exec_count:$exec_count,
          upper_bytes:$upper_bytes,snapshots:$snapshots,active_snapshot:$active_snapshot}'
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
