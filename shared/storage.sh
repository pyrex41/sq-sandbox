#!/bin/sh
# squash/storage.sh — Hybrid local-first storage for sandbox upper layers.
# Supports three backends: tmpfs (default), btrfs, loop.
# Also provides local caching and background delta-sync to S3.
set -eu

UPPER_BACKEND="${SQUASH_UPPER_BACKEND:-tmpfs}"
UPPER_LIMIT_MB="${SQUASH_UPPER_LIMIT_MB:-512}"
LOCAL_CACHE="${SQUASH_LOCAL_CACHE_DIR:-${HOME:+$HOME/.cache/sq-sandbox}}"
LOCAL_CACHE="${LOCAL_CACHE:-/tmp/sq-sandbox-cache}"
DATA="${SQUASH_DATA:-/data}"

log() { echo "[storage] $*" >&2; }

# ── Upper backend: mount writable layer ─────────────────────────────

# Mount the upper layer for a sandbox.
# Args: $1 = sandbox upper dir (e.g. /data/sandboxes/foo/upper)
upper_mount() {
    local target="$1"
    mkdir -p "$target"

    case "$UPPER_BACKEND" in
        btrfs)  _upper_btrfs  "$target" ;;
        loop)   _upper_loop   "$target" ;;
        *)      _upper_tmpfs  "$target" ;;
    esac

    mkdir -p "$target/data" "$target/work"
}

_upper_tmpfs() {
    local target="$1"
    mount -t tmpfs -o "size=${UPPER_LIMIT_MB}M" tmpfs "$target"
    log "tmpfs upper: $target (${UPPER_LIMIT_MB}M)"
}

_upper_btrfs() {
    local target="$1"
    # Ensure /data/upper is on a btrfs filesystem; create subvolume per sandbox
    local subvol_base="$DATA/upper"
    mkdir -p "$subvol_base"

    # Extract sandbox name from path
    local sb_name
    sb_name=$(basename "$(dirname "$target")")

    local subvol="$subvol_base/$sb_name"
    if [ ! -d "$subvol" ]; then
        btrfs subvolume create "$subvol" >/dev/null 2>&1 || {
            log "WARN: btrfs subvolume create failed, falling back to tmpfs"
            _upper_tmpfs "$target"
            return
        }
    fi

    mount --bind "$subvol" "$target"
    log "btrfs upper: $target -> $subvol"
}

_upper_loop() {
    local target="$1"
    local loop_base="$DATA/upper"
    mkdir -p "$loop_base"

    local sb_name
    sb_name=$(basename "$(dirname "$target")")
    local img="$loop_base/${sb_name}.img"

    if [ ! -f "$img" ]; then
        # Create sparse file
        dd if=/dev/zero of="$img" bs=1M count=0 seek="$UPPER_LIMIT_MB" 2>/dev/null
        mkfs.ext4 -q -F "$img" 2>/dev/null
    fi

    mount -o loop "$img" "$target"
    log "loop upper: $target -> $img (${UPPER_LIMIT_MB}M)"
}

# ── Btrfs instant snapshot ──────────────────────────────────────────

# Create a near-instant btrfs snapshot of a sandbox's upper layer.
# Args: $1 = sandbox id, $2 = label
# Returns 0 on success with the snapshot subvolume path on stdout.
btrfs_snapshot() {
    local id="$1" label="$2"
    local subvol="$DATA/upper/$id"
    local snap_dir="$DATA/sandboxes/$id/snapshots"
    local snap_path="$snap_dir/$label"

    mkdir -p "$snap_dir"

    if [ "$UPPER_BACKEND" != "btrfs" ]; then
        log "ERROR: btrfs_snapshot requires SQUASH_UPPER_BACKEND=btrfs"
        return 1
    fi

    if [ ! -d "$subvol" ]; then
        log "ERROR: no btrfs subvolume at $subvol"
        return 1
    fi

    btrfs subvolume snapshot -r "$subvol" "$snap_path" >/dev/null 2>&1 || {
        log "ERROR: btrfs snapshot failed"
        return 1
    }

    log "btrfs snapshot: $subvol -> $snap_path"
    echo "$snap_path"
}

# ── Local cache ─────────────────────────────────────────────────────

# Ensure local cache directory structure exists.
cache_init() {
    mkdir -p "$LOCAL_CACHE/modules" "$LOCAL_CACHE/snapshots"
}

# Copy a module to the local cache.
# Args: $1 = module squashfs path
cache_module() {
    local src="$1"
    cache_init
    local name
    name=$(basename "$src")
    local dest="$LOCAL_CACHE/modules/$name"
    [ -f "$dest" ] && return 0
    cp "$src" "$dest"
    log "cached module: $name"
}

# Copy a snapshot to the local cache.
# Args: $1 = snapshot squashfs path, $2 = sandbox id
cache_snapshot() {
    local src="$1" id="$2"
    cache_init
    local name
    name=$(basename "$src")
    local dest="$LOCAL_CACHE/snapshots/${id}__${name}"
    [ -f "$dest" ] && return 0
    cp "$src" "$dest"
    log "cached snapshot: ${id}/${name}"
}

# Try to pull a module from local cache first, then S3.
# Args: $1 = module name (e.g. 000-base-alpine.squashfs), $2 = destination path
cache_pull_module() {
    local name="$1" dest="$2"
    cache_init

    # Check local cache first
    local cached="$LOCAL_CACHE/modules/$name"
    if [ -f "$cached" ]; then
        cp "$cached" "$dest"
        log "cache hit (module): $name"
        return 0
    fi

    # Fall through to S3 if configured
    if [ -n "${SQUASH_S3_BUCKET:-}" ]; then
        . "$(dirname "$0")/sq-s3"
        s3_pull "modules/$name" "$dest" && {
            # Backfill local cache
            cp "$dest" "$cached" 2>/dev/null || true
            return 0
        }
    fi

    return 1
}

# ── Background delta sync ──────────────────────────────────────────

SYNC_DB="$LOCAL_CACHE/sync.db"

# Initialize the sync queue database.
sync_db_init() {
    cache_init
    if [ ! -f "$SYNC_DB" ]; then
        sqlite3 "$SYNC_DB" <<'SQL'
CREATE TABLE IF NOT EXISTS sync_queue (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    action TEXT NOT NULL,       -- 'push' or 'pull'
    local_path TEXT NOT NULL,
    s3_key TEXT NOT NULL,
    status TEXT DEFAULT 'pending',  -- pending, running, done, failed
    created_at TEXT DEFAULT (datetime('now')),
    finished_at TEXT
);
SQL
        log "sync db initialized: $SYNC_DB"
    fi
}

# Enqueue a file for background push to S3.
# Args: $1 = local path, $2 = S3 key
sync_enqueue_push() {
    local path="$1" key="$2"
    sync_db_init
    sqlite3 "$SYNC_DB" \
        "INSERT INTO sync_queue (action, local_path, s3_key) VALUES ('push', '$path', '$key');"
    log "enqueued push: $key"
}

# Process pending items in the sync queue (runs in background).
sync_process_queue() {
    [ -z "${SQUASH_S3_BUCKET:-}" ] && return 0
    sync_db_init

    local row
    while true; do
        row=$(sqlite3 "$SYNC_DB" \
            "SELECT id, action, local_path, s3_key FROM sync_queue WHERE status='pending' LIMIT 1;" 2>/dev/null) || break
        [ -z "$row" ] && break

        local qid action lpath s3key
        qid=$(echo "$row" | cut -d'|' -f1)
        action=$(echo "$row" | cut -d'|' -f2)
        lpath=$(echo "$row" | cut -d'|' -f3)
        s3key=$(echo "$row" | cut -d'|' -f4)

        sqlite3 "$SYNC_DB" "UPDATE sync_queue SET status='running' WHERE id=$qid;"

        if [ "$action" = "push" ] && [ -f "$lpath" ]; then
            if sq-s3 push "$lpath" "$s3key" 2>/dev/null; then
                sqlite3 "$SYNC_DB" \
                    "UPDATE sync_queue SET status='done', finished_at=datetime('now') WHERE id=$qid;"
                log "sync push done: $s3key"
            else
                sqlite3 "$SYNC_DB" "UPDATE sync_queue SET status='failed' WHERE id=$qid;"
                log "sync push failed: $s3key"
            fi
        else
            sqlite3 "$SYNC_DB" "UPDATE sync_queue SET status='failed' WHERE id=$qid;"
        fi
    done
}

# ── CLI dispatch ────────────────────────────────────────────────────

case "${1:-}" in
    mount-upper)
        [ $# -lt 2 ] && { echo "usage: storage.sh mount-upper <target-dir>" >&2; exit 1; }
        upper_mount "$2"
        ;;
    btrfs-snapshot)
        [ $# -lt 3 ] && { echo "usage: storage.sh btrfs-snapshot <id> <label>" >&2; exit 1; }
        btrfs_snapshot "$2" "$3"
        ;;
    cache-init)
        cache_init
        ;;
    sync-init)
        sync_db_init
        ;;
    sync-enqueue)
        [ $# -lt 3 ] && { echo "usage: storage.sh sync-enqueue <local-path> <s3-key>" >&2; exit 1; }
        sync_enqueue_push "$2" "$3"
        ;;
    sync-process)
        sync_process_queue
        ;;
    "")
        # Sourced by other scripts — functions are available
        ;;
    *)
        echo "usage: storage.sh {mount-upper|btrfs-snapshot|cache-init|sync-init|sync-enqueue|sync-process}" >&2
        exit 1
        ;;
esac
