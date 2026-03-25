# Database Alternatives to Snapshot Images

**Date**: 2026-03-24
**Trigger**: [Kyle Mistele (@0xblacklight) thread](https://x.com/0xblacklight/status/2036534699582255329) on rethinking sandbox filesystem storage
**Status**: Research / exploration

## The Insight

Kyle's core argument:

> Agents don't see POSIX APIs — they just see tokens in and tokens out. So you don't need a real filesystem, just something that *looks like* a filesystem but frontends whatever backend you want.

Key points from the thread:
- You don't need a filesystem — you need a **filesystem API** that frontends a database
- ACID compliance, indexability matter when your state is valuable
- FUSE lets you bridge the gap, but agents don't even need FUSE — they see tool results, not syscalls
- If you **own the harness**, you can separate tool INTERFACE from tool EXECUTION
- The tool looks like `fs.read("/foo/bar")` to the agent but backends to S3, Postgres, SQLite, whatever

## Current sq-sandbox Snapshot Architecture

### How it works today

```
upper/              (writable, tmpfs/btrfs/loop)
images/_snapshot    (read-only squashfs, restored snapshot)
images/100-xxx.squashfs
images/000-base.squashfs
```

1. **Create**: `mksquashfs upper/data → snapshots/<label>.squashfs`
2. **Restore**: loop-mount squashfs → `images/_snapshot`, clear upper, re-inject DNS/secrets
3. **Sync**: push/pull `.squashfs` files to/from S3 (async via `sq-sync` sidecar)
4. **Metadata**: `snapshots.jsonl` (append-only), `active_snapshot` label file
5. **Destroy**: optional auto-snapshot of non-trivial upper layers before teardown

### What snapshots capture

The **entire `upper/data` directory** — all files written/modified since last create/restore. This includes user files plus injected infrastructure (resolv.conf, CA certs, proxy env). It's a full capture, not incremental.

### Pain points with squashfs snapshots

- **Creation is slow** — `mksquashfs` compresses the entire upper layer every time (no incremental)
- **Opaque** — can't query contents without mounting, no metadata indexing
- **No dedup** — consecutive snapshots share most data but store it independently
- **Kernel dependency** — squashfs mount requires either kernel module or squashfuse
- **Btrfs alternative exists** — the codebase already has a fast path using `btrfs subvolume snapshot -r` (~instant), suggesting squashfs speed is a known issue

## Two Different Questions

Kyle's thread actually raises **two separate ideas** worth evaluating independently:

### Question 1: Replace squashfs with an embedded DB for storage

Keep the kernel filesystem (overlayfs), but use a DB-backed format instead of squashfs for the read-only layers.

| Candidate | Strengths | Weaknesses |
|-----------|-----------|------------|
| **SQLite (sqlar)** | Single-file, ACID, built-in compression (zlib/zstd via extension), `sqlar` archive format stores files natively, widely supported, queryable metadata | No kernel mount (need FUSE — [littlefs-fuse](https://github.com/nicmcd/littlefs-fuse), [sqlarfs](https://github.com/nicmcd/sqlarfs)), slower random read than mmap'd squashfs |
| **LMDB** | Zero-copy mmap reads, crash-safe, extremely fast reads, B+ tree with sorted keys (natural path ordering) | No built-in compression, values limited to ~4GB, no SQL query layer, needs FUSE for mount |
| **DuckDB** | Excellent for analytical queries over metadata, columnar compression | Designed for analytics not blob storage, no FUSE ecosystem, overkill for file serving |
| **RocksDB** | LSM-tree, great write throughput, prefix compression, used by CockroachDB/TiKV | Complex tuning, not single-file, no FUSE ecosystem |
| **erofs** | Kernel-native (like squashfs but newer), better random read, chunk-based dedup | Same opacity problems as squashfs, less widely supported |

**SQLite/sqlar is the most practical DB replacement** — it has the richest ecosystem, single-file portability, and existing FUSE implementations. But it loses squashfs's key advantage: **zero-overhead kernel-native mounting**.

### Question 2: Skip the filesystem entirely (the deeper insight)

This is Kyle's bigger point. If the consumer is an **agent harness** (not a human with `ls`), you don't need a mounted filesystem at all. You need:

1. A **tool interface** that looks like file operations: `read_file`, `write_file`, `list_dir`, `search`
2. A **backend** that stores the data however is optimal: SQLite, S3, Postgres, content-addressable store
3. **Snapshot = transaction boundary** — just commit/tag the current state

This changes the architecture fundamentally:

```
BEFORE (filesystem-centric):
  Agent → harness → tool call → POSIX fs → overlayfs → squashfs layers

AFTER (database-centric):
  Agent → harness → tool call → DB adapter → SQLite/S3/Postgres
                                              ↓
                                    snapshot = db checkpoint / tag
```

**Advantages of the DB-centric approach:**
- **Instant snapshots** — SQLite WAL checkpoint, Postgres SAVEPOINT, or just tag a commit hash in a content-addressed store
- **Incremental by nature** — only changed blobs are stored
- **Content-addressable dedup** — hash each file, store once, reference many times
- **Queryable** — "which snapshot has file X?", "diff two snapshots", "find all .py files"
- **ACID** — no partial snapshots, no corruption from crashes mid-`mksquashfs`
- **Portable** — single SQLite file vs opaque squashfs blob
- **No kernel dependencies** — no squashfuse, no overlay module, no mount privileges

**The catch — what we lose:**
- **`exec` / process execution** — agents that run arbitrary binaries need a real mounted filesystem. You can't `python script.py` against a SQLite database. This is the fundamental blocker for coding agents.
- **Shell commands** — `grep`, `find`, `git` etc. all expect a POSIX filesystem
- **Performance at scale** — thousands of small file reads through a DB adapter vs direct mmap
- **Compatibility** — every existing tool assumes files on disk

## Relevance to sq-sandbox

sq-sandbox runs **coding agents** that execute real processes (compilers, test runners, package managers). These need a real mounted filesystem — we can't go fully DB-backed for the execution environment.

BUT there are **hybrid opportunities**:

### Hybrid Approach: DB-backed snapshots, FS-mounted execution

```
[active sandbox]
  overlayfs with real upper/ layer (processes run here)
       │
       │ snapshot
       ▼
  SQLite content-addressed store
       │
       │ restore
       ▼
  Materialize back to overlayfs upper/
```

**Snapshot creation**: Instead of `mksquashfs`, walk `upper/data`, hash each file, store content-addressed blobs in SQLite. Record the tree (path → hash → permissions) as a snapshot record.

**Snapshot restore**: Read the tree from SQLite, materialize files back into `upper/data`.

**Benefits over current approach**:
- **Incremental snapshots** — only store changed files (hash comparison)
- **Dedup across snapshots** — same file content stored once
- **Queryable snapshot history** — SQL queries over snapshot metadata
- **Faster creation** for small changes — don't recompress unchanged files
- **Single portable file** for all snapshots of a sandbox

**Trade-offs**:
- Restore requires materializing (slower than squashfs mount for large snapshots)
- More complex implementation than `mksquashfs` one-liner
- Need to handle special files (symlinks, permissions, device nodes)

### Alternative Hybrid: SQLite for metadata + S3/CAS for blobs

```
SQLite: snapshots table, file_tree table (path, hash, mode, size, mtime)
CAS:    content-addressed blob store (local dir or S3)
          blobs/{sha256-first2}/{sha256}.zst
```

This separates concerns:
- SQLite handles the **tree structure** and **snapshot metadata** (fast queries, ACID)
- CAS handles **file content** (dedup, compression, S3-syncable)
- Snapshots become lightweight — just a new set of tree entries pointing to existing blobs

## Datom/DAG Model (from pyrex41/autopoiesis)

The [autopoiesis](https://github.com/pyrex41/autopoiesis) project implements exactly the primitives we need, applied to agent cognitive state. The architecture transfers directly to filesystem snapshots:

### Autopoiesis → Filesystem Mapping

| Autopoiesis concept | Filesystem equivalent |
|---|---|
| Datom `(E, A, V, Tx, added)` | `(path, attr, value, snapshot_id, asserted)` |
| Entity | A file or directory |
| Snapshot DAG node | Tree root hash → set of `(path → content_hash, mode, mtime)` |
| Branch (named mutable ref) | A sandbox's current state pointer |
| `take!` (atomic find-and-claim) | Atomic snapshot commit |
| O(1) fork (new ref to same node) | New sandbox sharing all history |
| Structural diff (`sexpr-diff`) | Tree diff → changed paths only |
| Lazy snapshot proxy | Lazy blob fetch on first access |
| SHA-256 structural hashing | Content-addressable file store |
| FSet weight-balanced trees | Persistent sorted path tree (structural sharing) |

### The Key Insight: Snapshots as a DAG, Not a List

Today, snapshots are a **flat list** of opaque `.squashfs` blobs per sandbox:
```
sandbox-1/snapshots/snap-1.squashfs  (full, independent)
sandbox-1/snapshots/snap-2.squashfs  (full, independent)
sandbox-2/snapshots/snap-1.squashfs  (full, independent, duplicates data from sandbox-1)
```

With the Datom/DAG model, snapshots form a **content-addressed DAG with structural sharing**:
```
                    ┌─ snap-3a (sandbox-1 branch)
         ┌─ snap-2 ┤
snap-1 ──┤         └─ snap-3b (sandbox-2, forked from snap-2)
         └─ snap-2alt (different branch)

Each node is: { tree_hash, parent_hash, timestamp, label }
Tree is:      { path → (content_hash, mode, size) }
Blobs:        content_hash → compressed bytes (stored once globally)
```

**O(1) forking**: Creating sandbox-2 from sandbox-1's snap-2 is just creating a new branch ref pointing to the same DAG node. Both sandboxes share all blob storage. They only diverge when new files are written.

**Structural diffing**: Comparing snap-3a and snap-3b walks both trees from their common ancestor (snap-2), collecting only paths where content_hash differs. This is O(changed files), not O(total files).

### Concrete Data Model

```sql
-- The datom-inspired schema

-- Content-addressed blob store (global, shared across all sandboxes)
CREATE TABLE blobs (
    hash       BLOB PRIMARY KEY,  -- SHA-256 of uncompressed content
    data       BLOB NOT NULL,     -- zstd-compressed file content
    size       INTEGER NOT NULL,  -- uncompressed size
    refcount   INTEGER DEFAULT 1  -- for GC (optional)
);

-- Tree entries: the "datoms" — (entity=path, attributes, tx=snapshot)
CREATE TABLE tree (
    snapshot_id INTEGER NOT NULL,
    path        TEXT NOT NULL,
    hash        BLOB,             -- NULL for directories
    mode        INTEGER NOT NULL, -- unix permissions
    size        INTEGER NOT NULL,
    mtime       INTEGER NOT NULL,
    link_target TEXT,             -- for symlinks
    PRIMARY KEY (snapshot_id, path)
);

-- DAG nodes: each snapshot points to parent(s)
CREATE TABLE snapshots (
    id          INTEGER PRIMARY KEY,
    tree_hash   BLOB NOT NULL,    -- merkle root of the tree
    parent_id   INTEGER,          -- NULL for root snapshot
    label       TEXT,
    created     TEXT NOT NULL,     -- ISO 8601
    sandbox_id  TEXT,
    FOREIGN KEY (parent_id) REFERENCES snapshots(id)
);

-- Branches: named mutable refs into the DAG (like git refs)
CREATE TABLE branches (
    name        TEXT PRIMARY KEY,  -- "sandbox-abc123" or "sandbox-abc123/autosave"
    snapshot_id INTEGER NOT NULL,
    FOREIGN KEY (snapshot_id) REFERENCES snapshots(id)
);

-- Indexes for fast operations
CREATE INDEX idx_tree_hash ON tree(hash);              -- find all paths containing a blob
CREATE INDEX idx_snap_parent ON snapshots(parent_id);  -- walk DAG children
CREATE INDEX idx_snap_sandbox ON snapshots(sandbox_id); -- list sandbox history
```

### Operations Mapped to Autopoiesis Primitives

**Snapshot creation** (≈ autopoiesis `snapshot-agent`):
```
1. Walk upper/data, hash each file
2. For each file:
   - If blob.hash exists in blobs table → skip (dedup!)
   - Else → INSERT compressed content
3. INSERT tree entries for this snapshot
4. Compute tree_hash (merkle root of sorted path→hash pairs)
5. INSERT snapshot node pointing to parent
6. UPDATE branch ref to new snapshot
All in one SQLite transaction → ACID, atomic
```

**Restore** (≈ autopoiesis `checkout-snapshot`):
```
1. SELECT all tree entries for snapshot_id
2. Clear upper/data
3. Materialize: for each entry, decompress blob → write to upper/data path
4. Remount overlayfs
```

**Fork** (≈ autopoiesis O(1) fork via `branch-head` pointer):
```
1. INSERT INTO branches (name, snapshot_id)
   VALUES ('sandbox-NEW', (SELECT snapshot_id FROM branches WHERE name = 'sandbox-ORIG'))
-- That's it. Zero data copying. Both sandboxes share everything.
-- On next snapshot of either, they diverge.
```

**Diff** (≈ autopoiesis `sexpr-diff`):
```sql
-- Files changed between two snapshots
SELECT COALESCE(a.path, b.path) as path,
       a.hash as old_hash, b.hash as new_hash
FROM tree a FULL OUTER JOIN tree b
  ON a.path = b.path
WHERE a.snapshot_id = :snap_old AND b.snapshot_id = :snap_new
  AND (a.hash IS DISTINCT FROM b.hash
       OR a.path IS NULL OR b.path IS NULL);
```

**S3 sync** (content-addressed, like autopoiesis structural hashing):
```
-- Only push blobs that the remote doesn't have
-- Each blob's S3 key is its hash → natural dedup
s3://bucket/cas/{hash[0:2]}/{hash}.zst

-- Snapshot metadata is tiny — just the tree + DAG node
-- Push as: s3://bucket/snapshots/{sandbox_id}/{snapshot_id}.json
```

### Why This Is Better Than "Just Use SQLite for Snapshots"

The autopoiesis model adds three things that a naive SQLite store doesn't:

1. **DAG structure with parent pointers** — enables history traversal, common ancestor finding, branching. Without this you just have a flat list of snapshots (what we have today, but in SQLite).

2. **Structural sharing via content hashing** — the blob store is global, not per-snapshot. Two sandboxes that started from the same base share 95%+ of their blobs automatically. This is the FSet/persistent-data-structure insight.

3. **O(1) forking via branch refs** — forking isn't "copy the snapshot", it's "point a new name at the same DAG node". The fork only costs storage when the branches diverge. This enables speculative branching, A/B testing of agent strategies, rollback-and-retry — all the things autopoiesis uses for its evolutionary swarm.

### Performance Characteristics

| Operation | Current (squashfs) | Datom/DAG (SQLite CAS) |
|---|---|---|
| **Snapshot create (cold)** | O(total_size) — full mksquashfs | O(total_size) — hash + compress all files |
| **Snapshot create (warm)** | O(total_size) — still full rebuild | **O(changed_size)** — only new/modified blobs |
| **Restore** | O(1) mount — near-instant | O(total_files) — materialize to upper/ |
| **Fork sandbox** | Copy .squashfs file — O(size) | **O(1)** — new branch pointer |
| **Diff two snapshots** | Mount both + `diff -r` — O(total) | **O(changed)** — SQL join on tree tables |
| **S3 push** | Full .squashfs — O(size) each time | **O(new_blobs)** — only push new content hashes |
| **S3 pull for restore** | Full .squashfs — O(size) | **O(needed_blobs)** — only pull blobs not in local cache |
| **Disk usage (10 similar snaps)** | 10x snapshot size | **~1.2x** — shared blobs |
| **Query "which snap has file X?"** | Mount each, check — O(n × mount) | **O(log n)** — SQL index lookup |

The trade-off: **restore is slower** (materialize vs mount). But for coding agents, snapshot creation happens far more often than restore, and the creation speedup for incremental changes is dramatic.

### Lazy Materialization (Future: Eliminate the Restore Penalty)

Autopoiesis uses `lazy-snapshot` proxies that defer I/O until access. The filesystem equivalent:

```
FUSE mount backed by SQLite CAS
  ├── On open(path): lookup tree entry, decompress blob on demand
  ├── On readdir(path): query tree WHERE path LIKE 'dir/%'
  └── On write(path): copy-up to overlayfs upper/ (like overlay whiteouts)
```

This eliminates the restore materialization step entirely. Files are fetched from the CAS on first access. Combined with overlayfs for writes, this gives:

- **O(1) restore** (just point FUSE at new snapshot_id)
- **O(accessed) read cost** (only decompress what's actually read)
- **Full POSIX compat** (processes see a real filesystem via FUSE)
- **exec works** (binaries are materialized to page cache on demand)

This is the "lazy loading via MOP slot-unbound hooks" from autopoiesis, but for files instead of CLOS slots.

## Embedded DB Comparison (Detailed)

Research from web sources on each candidate:

| Property | SQLite/sqlar | LMDB | RocksDB | BoltDB/bbolt |
|---|---|---|---|---|
| **Architecture** | B-tree, single-file | B+ tree, mmap'd single-file | LSM-tree, multi-file | B+ tree, mmap'd single-file |
| **Read perf** | Fast with mmap pragma | Fastest (zero-copy mmap) | Moderate (block cache) | Fast (zero-copy mmap) |
| **Write perf** | Single-writer, moderate | Single-writer, fast for sorted | Multi-writer, highest throughput | Single-writer, moderate |
| **Compression** | zlib only (sqlar) | None (app must compress) | LZ4/ZSTD/Snappy (excellent) | None (app must compress) |
| **Multi-process** | WAL mode (limited) | Yes (shared mmap) | Yes (read-only instances) | No (exclusive file lock) |
| **FUSE ecosystem** | sqlarfs, libsqlfs (mature) | None (must build) | None (must build) | None |
| **Snapshot semantics** | WAL checkpoint | MVCC read txn = snapshot | Checkpoint = hard-linked SSTs | COW paging |
| **Max blob size** | ~1-2 GB (must chunk larger) | ~4 GB value limit | Multi-GB via BlobDB | Virtual addr space |
| **Container ecosystem use** | None in production | None | Alluxio, Ceph (metadata) | containerd (metadata) |
| **Single-file portable** | Yes | Yes | No (directory of files) | Yes |

**Recommendation for sq-sandbox**: **SQLite** for the tree/metadata store + DAG, with **application-level zstd compression** of blob values. SQLite gives us:
- Single portable `.db` file per sandbox (or global)
- SQL queries for diffing, history, search
- FUSE mount option via sqlarfs for backward compat
- Widest language support (C, Rust, Zig, shell via `sqlite3` CLI)
- ACID transactions for atomic snapshot commits
- Already used in sq-sandbox for the sync queue (`storage.sh:174-237`)

LMDB would win on raw read performance (zero-copy mmap) but loses on queryability, compression, and ecosystem. RocksDB is overkill for our scale and adds operational complexity (not single-file).

## Concrete Next Steps

### Phase 1: Foundation (compatible with current arch)

1. **Add content-addressed blob cache alongside squashfs** — when creating a snapshot, also index file hashes. Enable "fast diff" between snapshots.

2. **Replace `snapshots.jsonl` with SQLite** — already have SQLite for sync queue in `storage.sh`. Unify metadata storage. Add the `snapshots`, `tree`, `blobs`, `branches` tables.

3. **Implement incremental squashfs creation** — use the hash index to `mksquashfs` only changed files (via `-pf` pseudo file definitions or `sqfstar` with filtered input).

### Phase 2: Datom/DAG snapshot format

4. **SQLite CAS snapshot store** — implement the schema above. Snapshot creation walks `upper/data`, hashes files, stores new blobs, records tree entries. Restore materializes from the store.

5. **O(1) fork via branch refs** — `POST /api/sandboxes/:id/fork` creates a new sandbox by pointing a new branch at the source's current snapshot. Enables cheap sandbox cloning for parallel agent exploration.

6. **Content-addressed S3 sync** — push/pull individual blobs by hash. Dramatically reduces bandwidth. Manifest is just the snapshot DAG metadata (tiny).

### Phase 3: Lazy materialization

7. **FUSE mount backed by SQLite CAS** — eliminate the materialization step on restore. Files decompressed on demand. Combined with overlayfs for writes.

8. **Virtual filesystem tools for agent harness** — for agents that only need read/write/search (not exec), bypass FUSE entirely. Serve file operations from the DB. Only materialize to disk when `exec` is requested (Kyle's insight).

## References

- [pyrex41/autopoiesis](https://github.com/pyrex41/autopoiesis) — Datom store, snapshot DAG, O(1) forking, structural diffing
- [SQLite Archive (sqlar)](https://www.sqlite.org/sqlar.html) — built-in archive format
- [SQLite Faster Than Filesystem](https://sqlite.org/fasterthanfs.html) — SQLite benchmark for blob access
- [sqlarfs FUSE](https://github.com/nicmcd/sqlarfs) — mount sqlar as filesystem
- [libsqlfs](https://github.com/guardianproject/libsqlfs) — full POSIX filesystem on SQLite
- [LMDB](http://www.lmdb.tech/doc/) — zero-copy memory-mapped KV store
- [RocksDB BlobDB](https://rocksdb.org/blog/2021/05/26/integrated-blob-db.html) — key-value separation for large values
- [Alluxio + RocksDB](https://www.alluxio.io/blog/scalable-metadata-service-in-alluxio-storing-billions-of-files) — billion-file filesystem metadata in RocksDB
- [erofs](https://erofs.docs.kernel.org/) — next-gen read-only filesystem (kernel-native)
- [composefs](https://github.com/containers/composefs) — EROFS + overlayfs for containers (state of the art)
- [casync](https://github.com/systemd/casync) — content-addressable sync tool
- [desync](https://github.com/folbricht/desync) — Go implementation of casync protocol
- [ostree](https://ostreedev.github.io/ostree/) — content-addressed OS tree (used by Fedora/RHEL)
- Kyle Mistele thread on [separating tool interface from execution](https://x.com/0xblacklight/status/2036534699582255329)
