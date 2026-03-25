# Database Alternatives to Snapshot Images

**Date**: 2026-03-24
**Trigger**: [Kyle Mistele (@0xblacklight) thread](https://x.com/0xblacklight/status/2036534699582255329) on rethinking sandbox filesystem storage
**Status**: Research / exploration
**Updated**: 2026-03-25 — added Datom/DAG model, prior art survey (Prolly Trees, OSTree, LMDB CoW B-trees), four-way architecture comparison (Irmin vs libmdbx vs Okra vs CAS S-exp), revised 5-phase implementation plan

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

## Prior Art: Merkle DAG / CAS Filesystems

Every system below solves the same problem from a different angle. The shared insight: **structural sharing through content addressing** — if two states share a subtree, they share the same hash, so you store it once and point from both roots.

### Prolly Trees (Dolt) — the closest match

[DoltHub: How to Chunk Your Database into a Merkle Tree](https://www.dolthub.com/blog/2022-06-27-prolly-chunker/) | [Study in Structural Sharing](https://www.dolthub.com/blog/2024-04-12-study-in-structural-sharing/)

Dolt stores tables in **Prolly Trees** — content-addressed B-trees where each node is identified by SHA-256 of its content. Originating from Attic-Labs' Noms project.

- **History independence**: tree structure is a deterministic function of contents, not insertion order
- **Content-defined chunking**: chunk boundaries determined by rolling hash of keys — inserting near position K doesn't shift all boundaries after K
- **O(1) fork**: new branch = new root pointer
- **O(log N) diff**: walk two trees, skip identical subtrees when hashes match
- **Caveat**: structural sharing degrades under scattered mutations (secondary indexes can amplify 66x)

Prolly Trees are the ideal data structure for our tree index — but no standalone Rust/Zig library exists yet. The SQLite flat-table approach is a pragmatic first step that could be upgraded to Prolly Trees later.

### OSTree — Git for OS Binaries (production-hardened)

[OSTree overview](https://ostreedev.github.io/ostree/introduction/) | [Repo anatomy](https://ostreedev.github.io/ostree/repo/)

The most mature production system for content-addressed OS filesystem trees:
- Object types: `commit`, `dirtree`, `dirmeta`, `content` (file)
- SHA-256 hashes identify all objects
- Deployments are **hardlink farms** into the object store — creating a new deployment = O(1) fork
- Dedup is automatic: same content + metadata = same hash = stored once
- Used by Flatpak, rpm-ostree, Fedora Silverblue, Automotive Grade Linux
- `bare` mode = real files with hardlinks; `archive` mode = compressed for HTTP serving

**OSTree is the closest to what we want** — but it uses hardlinks on the host filesystem rather than a portable embedded store, and has no EAV/datom metadata model.

### LMDB — Userspace CoW B-tree

[LMDB docs](http://www.lmdb.tech/doc/) | [How LMDB works](https://xgwang.me/posts/how-lmdb-works/)

LMDB implements the exact persistent CoW B-tree primitive that autopoiesis uses via FSet:
- Copy-on-write per page: modifications copy the modified page and all ancestors to root
- Two oscillating meta-pages: committed root stored atomically, no WAL needed
- **MVCC via page immutability**: readers pin current root, writers create new root
- Zero-copy reads: mmap'd, no malloc in read path
- `sqlightning` project replaced SQLite's B-tree with LMDB — giving SQLite MVCC for free

### OCI / containerd / Nydus — Container-Native CAS

- **OCI Image Spec**: explicitly a Merkle DAG — every blob, manifest, index identified by SHA-256 digest
- **containerd snapshotters**: OverlayFS (file-level CoW), btrfs/ZFS (block-level CoW), Nydus (chunk-based CAS with lazy pull)
- **Nydus snapshotter**: chunk-based CAS using RAFS format — lazy-pulls individual chunks on demand, sub-layer deduplication. This is the container ecosystem's answer to lazy materialization.

### casync / desync — Content-Addressed Sync

[casync (Lennart Poettering)](https://0pointer.net/blog/casync-a-tool-for-distributing-file-system-images.html) | [desync (Go)](https://github.com/folbricht/desync)

Content-defined chunking across serialized byte streams (not tree-structured):
- `.caidx` index: ordered list of `(offset, length, hash)` tuples
- `.castr` chunk store: individually compressed chunks named by hash
- Sync = download only chunks whose hash isn't in local store
- desync adds S3/GCS backends and parallel chunking (up to 10x faster)
- No O(1) forking or sub-directory diffs — it's a sync tool, not a versioned store

### bup — Git + Hashsplitting for Backups

[bup](https://github.com/bup/bup) — literally git's packfile format + content-defined chunking for large files. Backup = git branch. FUSE mount exposes snapshots as filesystem.

### Structural Sharing Granularity Comparison

| System | Sharing unit | Fork cost | Diff cost |
|---|---|---|---|
| **OSTree** | Per-file | O(1) — new commit object | O(changed files) |
| **OCI/Docker** | Per-layer (coarse) | O(layers) — copy manifests | O(layers) |
| **Nydus** | Per-chunk (~64KB) | O(1) | O(changed chunks) |
| **casync/desync** | Per-chunk (~64KB) | N/A | O(changed chunks) |
| **restic** | Per-blob (~1MB) | N/A | O(blobs) |
| **git/bup** | Per-blob + per-tree-node | O(1) — new ref | O(changed) via tree walk |
| **Dolt Prolly Tree** | Per-B-tree-node (~KB) | O(1) — new root pointer | O(diff × log N) |
| **LMDB** | Per-B-tree-page (~4KB) | O(1) — new meta-page | O(changed pages) |
| **Btrfs** | Per-extent (4KB-128MB) | O(1) — new subvol root | O(changed extents) |
| **Proposed sq-sandbox** | Per-file (CAS blobs) | O(1) — new branch row | O(changed files) via SQL join |

### Key Takeaway

The pattern is universal: **fork = new root pointer, diff = skip matching hashes**. Our proposed SQLite CAS store implements the same primitive as all of these, just with SQL as the query layer instead of a custom tree walker. The upgrade path to Prolly Trees (for sub-file chunk-level sharing) is clear if we ever need it.

## 10 Alternative Storage Engines (Expanded Survey)

### Tier 1: CAS-Native / Purpose-Built

**1. Irmin (OCaml)** — The only system that IS a content-addressed Merkle DAG natively.
- BLAKE2B-keyed append-only pack file with bidirectional hash index
- Git-compatible branch/merge semantics built in
- Deduplication automatic (same content = same hash = stored once)
- Used by Tezos blockchain for ledger state
- Notafs (Nov 2024): proves Irmin can be a standalone filesystem on MirageOS block devices
- v3.11.0 stable
- **Constraint**: OCaml ecosystem. [github.com/mirage/irmin](https://github.com/mirage/irmin)

**2. Okra (Zig, on LMDB)** — Prolly tree with Merkle root, written in Zig.
- Content-addressed deterministic B-tree: same entries = same root hash regardless of insertion order
- Built on LMDB for durability, Zig for zero-cost C ABI
- CRDT-compatible `merge` operation for state-based CRDTs
- Leaf hashes: SHA256(key||value); internal: SHA256(concat children hashes)
- **Callable from CL via CFFI** (Zig exports C ABI natively)
- Enables P2P sync via Merkle tree reconciliation
- [github.com/canvasxyz/okra](https://github.com/canvasxyz/okra)

**3. Lurk (Rust)** — Content-addressed S-expression storage, existence proof.
- Turing-complete Lisp where every expression is identified by `hash(sexp)`
- The `Store` is literally `hash → sexp preimage` — a Merkle DAG of S-expressions
- Uses Poseidon hashes (ZK-friendly) but architecture works with SHA256/BLAKE3
- Proves that hash-consed S-expression storage is viable and performant
- **The natural autopoiesis storage primitive** if generalized
- [github.com/lurk-lab/lurk-beta](https://github.com/lurk-lab/lurk-beta)

### Tier 2: High-Performance Embedded (build CAS on top)

**4. libmdbx (C)** — LMDB fork, 10-20% faster, critical fixes.
- `MDB_APPEND` mode for pre-sorted hash keys = maximum write throughput for CAS
- LIFO GC reclamation eliminates long-reader degradation that plagued LMDB
- Automatic geometry management (no manual map sizing)
- Amalgamated single-file C source (trivial to embed)
- Used by Ethereum clients (Erigon, Reth) for hash-keyed blockchain state
- Apache 2.0 since May 2024
- [github.com/erthink/libmdbx](https://github.com/erthink/libmdbx)

**5. Redb (Rust)** — Pure Rust CoW B-tree with first-class Savepoints.
- **Savepoints**: capture entire DB state in ~64KB per GB of data. Restore = atomic rollback.
- Copy-on-write B-tree = every committed state is implicitly a snapshot
- Zero-copy reads via mmap'd MVCC
- Stable file format, v3.1.1 (Mar 2026), production-ready
- `redbx` fork adds AES-256-GCM encryption as drop-in replacement
- [github.com/cberner/redb](https://github.com/cberner/redb)

**6. Pebble (Go)** — CockroachDB's LSM-tree with value separation.
- **Value separation** (v25.3): stores large values in separate files, reducing write amplification ~47%
- `DeleteSized`: tombstone carries deleted value's size for space-aware compaction
- Bloom filters for fast hash-existence checks (avoid disk I/O for "already stored?" queries)
- Range tombstones for efficient bulk deletion
- Pure Go, no CGO, BSD-3 license
- [github.com/cockroachdb/pebble](https://github.com/cockroachdb/pebble)

### Tier 3: Datom-Native / Autopoiesis-Compatible

**7. Datahike (Clojure)** — Datomic-compatible with hitchhiker trees + structural sharing.
- **Hitchhiker trees**: write-optimized persistent B+-tree with structural sharing across snapshots
- Full time-travel queries (as-of any transaction)
- Pluggable backends via Konserve: LMDB, S3, filesystem, Redis, JDBC
- Database snapshots are immutable Clojure values — pass them around freely
- Datoms are first-class, Datalog queries built in
- **JVM dependency** — best integrated as server or via Konserve's LMDB backend pattern
- [github.com/replikativ/datahike](https://github.com/replikativ/datahike)

**8. Datalevin (Clojure on LMDB)** — Datomic API, LMDB performance.
- LMDB's DUPSORT for nested B+ tree EAV indexing (EAVT, AEVT, AVET, VAET)
- 100K datoms to disk in <0.2 seconds
- Cost-based query optimizer, ~2x faster than PostgreSQL on complex joins
- No history retention (simplification vs Datomic) — but could be extended
- **The reference design** for implementing EAV indexing over LMDB in any language
- [github.com/juji-io/datalevin](https://github.com/juji-io/datalevin)

**9. XTDB v2 (Clojure)** — Bitemporal by default.
- Every record carries system-time AND valid-time automatically
- Query: "what did the filesystem look like at T1, as of what we knew at T2?"
- Columnar Arrow storage, append-only immutable transaction log
- SQL:2011 temporal standard over Postgres wire protocol
- **The time-travel query model** — unmatched for snapshot history analysis
- [docs.xtdb.com](https://docs.xtdb.com/intro/what-is-xtdb.html)

### Tier 4: Inspirational / Novel Architecture

**10. Content-Addressed S-Expression Store (novel, unbuilt)**
- `hash(sexp) → sexp` as the universal storage primitive
- Every cons cell, symbol, integer, list is content-addressed
- Compound expressions reference children by hash → Merkle DAG of S-expressions
- FSet tree nodes, datoms, filesystem tree entries are ALL the same data structure
- Backed by libmdbx or Redb for durability
- **Homoiconic storage**: the storage format IS the computation format
- Lurk proves the concept; Okra provides the Merkle index; autopoiesis provides the runtime
- No existing implementation — this is the design space autopoiesis could pioneer

### Comparison Matrix

| # | System | Language | CAS-Native | Datom-Native | Embeddable | S-exp | Point lookup |
|---|---|---|---|---|---|---|---|
| 1 | Irmin | OCaml | Yes | No | Yes | No | O(1) hash index |
| 2 | Okra | Zig/LMDB | Yes (Prolly) | No | Yes | No | O(log N) |
| 3 | Lurk Store | Rust | Yes | No | Yes | **Yes** | O(1) hash map |
| 4 | libmdbx | C | Build on top | No | Yes | No | ~1μs mmap |
| 5 | Redb | Rust | Savepoints | No | Yes | No | ~few μs |
| 6 | Pebble | Go | Build on top | No | Yes | No | ~few μs |
| 7 | Datahike | Clojure/JVM | Structural | **Yes** | Server | EDN | Datalog |
| 8 | Datalevin | Clojure/JVM | No | **Yes** | Server | EDN | ~few μs (LMDB) |
| 9 | XTDB v2 | Clojure/JVM | Immutable log | Bitemporal | Server | EDN | SQL/Datalog |
| 10 | CAS S-exp | Novel | **Yes** | **Yes** | **Yes** | **Yes** | O(1) hash |

### Eliminated Candidates

| System | Why not |
|---|---|
| **DuckDB** | Columnar/OLAP — point lookups ~15x slower than SQLite. Wrong workload. |
| **FoundationDB** | Not embeddable (daemon), 100KB value hard limit, IPC latency overhead. Wrong scale. |
| **ClickHouse** | OLAP engine, multi-file storage, point lookups are worst case. |
| **Sled** | Still beta, unstable on-disk format, no ETA for 1.0. |
| **SurrealDB/KV** | VART is interesting but file format unstable, RocksDB still recommended for production. |

### The Autopoiesis Integration Thesis

The S-expression angle changes the question from "which DB stores blobs" to:

> **What if the storage format IS the computation format?**

Autopoiesis already represents state as datoms and FSet persistent trees. If on-disk storage is content-addressed S-expressions (#10), then:
- No serialization/deserialization boundary — `read`/`print` IS the codec
- The Merkle DAG IS the datom index
- Filesystem trees, agent state, and snapshot history are the same data structure
- `diff` = structural comparison of S-expression trees
- `fork` = new root pointer to the same S-expression DAG
- Hash-consing (classical Lisp, 1970s) becomes durable persistence

**Most viable composition**: Okra (Zig Prolly tree on LMDB, C ABI) + content-addressed S-expression serialization + CL CFFI bindings. This gives autopoiesis a durable, content-addressed, Merkle-rooted datom store callable directly from Common Lisp, with P2P sync capability via tree reconciliation.

## Original Embedded DB Comparison

For reference, the original four-way comparison:

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
| **Single-file portable** | Yes | Yes | No (directory of files) | Yes |

**SQLite** remains the pragmatic first step for sq-sandbox (shell-scriptable, SQL queries, single file, already in use). The 10 alternatives above represent the design space for where to evolve — particularly if autopoiesis integration becomes a priority.

## Four-Way Architecture Comparison: The Final Candidates

After surveying the full design space, four architectures emerge as serious contenders. Each represents a different point on the build-vs-buy and performance-vs-expressiveness spectrum.

### 1. Irmin (OCaml) — The Reference Implementation

**What it is**: The only production-grade system that IS a content-addressed Merkle DAG natively. Used by Tezos blockchain for ledger state (400M+ objects, 8,258 ops/sec during replay, 25 GB store — 10x smaller than equivalent LMDB store).

**Architecture — irmin-pack**:
- Append-only pack file with bidirectional hash index
- Offset-based internal references: once you have a root, traversing children is a direct file seek — no index roundtrip
- Content-addressing preserved via index for external lookups, but internal tree walks are O(1) per hop
- Object types: `contents` (file data), `node` (directory tree), `commit` (snapshot with parent pointers)
- BLAKE2B hashes by default (configurable)
- Irmin 3 achieved 12x latency reduction and 360x index size reduction over Irmin 2

**What you get for free**:
- Git-compatible branch/merge semantics (branches, merges, 3-way merge with custom strategies)
- Automatic deduplication (same content = same hash = stored once)
- Tree diffing: `Irmin.Diff` walks two trees, skips matching subtrees
- O(1) fork: new branch = new ref to same commit
- GC via `irmin-pack`'s garbage collection (rewrite pack, drop unreachable objects)
- `irmin-server`: binary protocol + JSON mode over WebSocket

**CL interop**: irmin-server exposes a well-defined binary/JSON protocol over WebSocket. Practical from CL via usocket/dexador. Not zero-overhead (IPC hop) but well-defined and debuggable. Alternative: OCaml C API via ctypes → CL CFFI (tighter coupling, more work).

**FUSE**: No existing FUSE mount — would be novel work. But the tree model maps perfectly: nodes = directories, contents = files, commits = snapshots.

**The catch**: OCaml ecosystem. Build system (dune/opam), runtime (OCaml GC), deployment (need OCaml runtime or static linking). If you're already invested in OCaml this is a non-issue. If not, it's a significant adoption cost.

### 2. libmdbx (C) — The Blank Canvas

**What it is**: LMDB fork, 10-20% faster, with critical fixes. Ordered key-value pairs on mmap'd B+-tree. The fastest possible embedded read (~1μs zero-copy mmap) and fastest possible sorted write (`MDB_APPEND` mode).

**Key improvements over LMDB**:
- LIFO GC reclamation: eliminates long-reader degradation that plagued LMDB
- Automatic geometry management: no manual map size tuning
- `MDB_APPEND` mode: pre-sorted hash key ingestion at near-sequential-write speed (~500MB/s) — tailor-made for CAS (SHA-256 keys are uniformly distributed)
- Amalgamated single-file C source: trivial to embed
- Used by Ethereum clients (Erigon, Reth) for exactly the hash-keyed CAS workload

**What you must build**: Everything. Hashing, tree structure, serialization format, snapshot logic, diff algorithm, forking, indexing, GC. libmdbx just stores and retrieves bytes. Maximum flexibility, minimum leverage.

**Known issue — hash-key freelist fragmentation**: Reth (Ethereum client) documented multi-second stalls when searching for contiguous free pages with random hash keys ([#5228](https://github.com/paradigmxyz/reth/issues/5228)). LIFO GC significantly reduces this but doesn't fully solve it. For CAS workloads, mitigation is to batch and sort hash keys before bulk insertion via `MDB_APPEND`.

**Distribution**: Since December 2025, libmdbx ships as amalgamated `mdbx.c` + `mdbx.h` only (like SQLite). Internal tests and dev docs are no longer publicly distributed.

**CL interop**: Universal C FFI. CFFI bindings are straightforward — libmdbx's API is small and well-documented.

### 3. Okra (Zig on LMDB) — The Practical Middle Ground

**What it is**: Prolly tree with Merkle root, built on LMDB. Content-addressed deterministic B-tree: same entries = same root hash regardless of insertion order.

**Architecture**: Node promotion via content-defined chunking — `u32(node.hash[0..4]) < (2^32 / Q)` where Q=32 (target fanout). Leaves hashed as `SHA256(key_len || key || value_len || value)`. Internally uses Blake3 16-byte digests. Stored as regular LMDB key/value pairs with `[level: u8][firstLeafKey: bytes]` keys. Empirically ~2.25 LMDB node changes per random leaf update in a 65k-entry tree.

**What you get beyond libmdbx**:
- Deterministic Merkle root hash (same data = same hash, always)
- Tree reconciliation for P2P sync (compare root hashes, walk divergent subtrees)
- CRDT-compatible `merge` operation — supports grow-only union and custom merge functions
- Performance: 489K reads/s (single), 9.9K writes/s (vs LMDB's 397K reads/s, 13.9K writes/s) — similar reads, ~29% slower writes due to Merkle node updates

**Critical caveat — no C ABI today**: Okra currently builds only a CLI executable and test binaries. No `callconv(.C)` exports, no shared library, no `.h` header. CL/C integration would require adding ~50 lines of Zig export wrappers + a `addSharedLibrary` build step. Feasible but not free.

**What you must still build**: C ABI wrapper, serialization (values are opaque bytes), datom model, fork semantics (branch refs), query layer. Okra gives you the Merkle index; you supply the meaning.

**The semantic gap**: Okra sees `key → bytes`, not `entity → attribute → value`. Your datoms get serialized to bytes going in and deserialized coming out. Every read/write crosses a serialization boundary. The Prolly tree diffs byte ranges, not semantic structures.

### 4. CAS S-expression Store (Novel, Unbuilt) — The Autopoiesis Endgame

**What it is**: `hash(sexp) → sexp` as the universal storage primitive. Every cons cell, symbol, integer, list is content-addressed. Compound expressions reference children by hash → Merkle DAG of S-expressions. Lurk (Rust) proves the concept works.

**What makes it unique**: There is no serialization boundary. A datom `(file-123 :content-hash "abc123" :mtime 1711324800 :tx-42 t)` is stored as-is. Its hash commits to all sub-expressions. When you change one attribute, you create a new list S-expression that shares all unchanged atoms with the old one.

**Operations**:
- `diff` = structural comparison of S-expression trees, stop when hashes match
- `fork` = new root pointer to same S-expression DAG
- `read`/`print` IS the codec — no serialization/deserialization boundary
- The Merkle DAG IS the datom index
- Filesystem trees, agent state, and snapshot history are the same data structure

**Hashing overhead**: For 10,000 files → ~50,000 hash operations (files + dir nodes + metadata). At ~500ns per BLAKE3 hash on small inputs → ~25ms. Negligible vs file I/O. For bulk (100K files), batch and sort hashes before commit (what Okra does internally).

**Lurk as existence proof**: Lurk's `ZStore` uses `BTreeMap<ZExprPtr, Option<ZExpr>>` — content-addressed DAG with tagged Poseidon hashes as pointers. Key insight from Lurk: **deferred hashing** — evaluation uses cheap in-memory pointers (~7x faster), Poseidon hashes only computed on persistence/proof. A filesystem CAS S-exp store would use BLAKE3 instead of Poseidon (no ZK constraint budget to minimize) and libmdbx instead of BTreeMap for durability.

**Backed by**: libmdbx or Redb for durability. The S-exp layer is ~500 lines of CL above the KV store.

### Head-to-Head Comparison

| Dimension | Irmin (OCaml) | libmdbx (C) | Okra (Zig/LMDB) | CAS S-exp (unbuilt) |
|---|---|---|---|---|
| **Exists today** | Yes, production (Tezos) | Yes, battle-tested | Yes, working | No |
| **CAS native** | Yes | No (build on top) | Yes (Prolly tree) | Yes (hash-consing) |
| **Point lookup** | O(1) via index | ~1μs (mmap) | ~few μs (LMDB + tree) | ~1μs (backed by libmdbx) |
| **Root hash** | Yes (commit hash) | None | Yes (Merkle root) | Yes (root S-exp hash) |
| **Fork** | O(1) — new branch ref | Manual | O(1) — new root pointer | O(1) — new root pointer |
| **Diff** | O(changed) — tree walk | Manual — iterate both | O(changes × log N) — Prolly walk | O(changes) — DAG walk |
| **Structural sharing** | Per tree node | None | Per Prolly node (~KB) | Per S-expression (finest) |
| **Serialization** | OCaml types (Repr lib) | Your problem | Your problem (bytes) | None — IS the format |
| **Query model** | Tree API + watches | Range scan on sorted keys | Range scan + Merkle skip | Datalog (if built) / tree walk |
| **Autopoiesis compat** | Good (tree model maps) | Low (raw bytes) | Medium (Merkle + CFFI) | **Native** (datoms = S-exps) |
| **CL interop** | irmin-server (IPC) | CFFI (direct) | CFFI (needs C ABI wrapper) | Native CL |
| **FUSE ecosystem** | None (novel work) | None (build) | None (build) | None (build) |
| **Build effort** | ~1 week (bindings + server) | ~3 weeks (everything) | ~1 week (CFFI + serialization) | ~2-3 months (full store) |
| **Language** | OCaml | C | Zig (C ABI) | CL (or Zig/C backing) |
| **Production scale** | 400M+ objects (Tezos) | Billions (Ethereum) | Experimental | Unproven |

### Where Each Wins

**Choose Irmin if**: You want the most complete solution today. Everything — CAS, branches, merges, tree diffing, GC — is built and battle-tested at Tezos scale. The cost is the OCaml ecosystem and an IPC hop for CL integration.

**Choose libmdbx if**: You want maximum raw performance and full control. Best for a team that wants to own the entire stack and optimize every layer. Ethereum-proven at billions of hash-keyed records.

**Choose Okra if**: You want CAS with Merkle diffing as a library, callable from CL via CFFI, without building the tree structure yourself. The practical middle ground — get Prolly tree semantics, defer the S-expression layer until needed.

**Choose CAS S-exp if**: Autopoiesis integration is the primary goal. The store speaks the same language as the runtime. No serialization boundary. But it requires 2-3 months of novel implementation.

### The Composition That Wins

These aren't mutually exclusive. The ideal architecture layers them:

```
┌─────────────────────────────────────┐
│  Autopoiesis (Common Lisp)          │
│  datoms, FSet, S-expressions        │
├─────────────────────────────────────┤
│  CAS S-exp layer (CL)              │  ← Phase 3: hash-cons + read/print
│  hash(sexp) → sexp                  │
├─────────────────────────────────────┤
│  Okra Prolly tree (Zig, C ABI)     │  ← Phase 2: Merkle index + diff
│  deterministic tree, root hash      │
├─────────────────────────────────────┤
│  libmdbx (C)                       │  ← Phase 1: fast durable KV
│  mmap'd B+-tree, zero-copy reads    │
└─────────────────────────────────────┘
```

**Or**, replace the bottom two layers with Irmin (if OCaml is acceptable):

```
┌─────────────────────────────────────┐
│  Autopoiesis (Common Lisp)          │
│  datoms, FSet, S-expressions        │
├─────────────────────────────────────┤
│  CAS S-exp layer (CL)              │  ← Phase 2: hash-cons + read/print
│  hash(sexp) → sexp                  │
├─────────────────────────────────────┤
│  Irmin (OCaml, via irmin-server)   │  ← Phase 1: CAS + branches + diff
│  pack file, Merkle tree, GC         │
└─────────────────────────────────────┘
```

Irmin collapses Phase 1 and Phase 2 into a single dependency — you get CAS, Merkle tree, branching, diffing, and GC out of the box. The trade-off is the OCaml runtime and IPC overhead.

## Revised Implementation Plan

### Phase 1: Foundation — CAS Blob Store + Metadata (2-3 weeks)

**Goal**: Replace squashfs snapshots with content-addressed storage. Keep overlayfs for execution.

**Option A: libmdbx path** (if building from scratch)
1. Embed libmdbx (amalgamated C source) into sq-sandbox build
2. Three LMDB databases (sub-databases in one file):
   - `blobs`: SHA-256 hash → zstd-compressed file content
   - `tree`: `(snapshot_id, path)` → `(hash, mode, size, mtime, link_target)`
   - `refs`: `branch_name` → `snapshot_id`
3. Snapshot creation: walk `upper/data`, BLAKE3-hash each file, store new blobs, record tree
4. Restore: read tree entries, materialize files to `upper/data`
5. Replace `snapshots.jsonl` with snapshot metadata in LMDB

**Option B: Irmin path** (if OCaml is acceptable)
1. Deploy `irmin-server` alongside sandbox runtime
2. Map filesystem tree to Irmin tree: `path → (hash, mode, size, mtime)` as Irmin contents
3. Snapshot = Irmin commit. Fork = Irmin branch. Diff = Irmin tree diff.
4. Store file blobs as Irmin contents (automatic dedup + compression)
5. CL integration via irmin-server's WebSocket/JSON protocol

**Deliverables**:
- `sq-snap create` — hash + store files, create snapshot in DAG
- `sq-snap restore <label>` — materialize snapshot to upper/
- `sq-snap list` — show snapshot DAG with labels
- `sq-snap diff <a> <b>` — show changed files between two snapshots
- S3 sync pushes/pulls individual blobs by hash (content-addressed, dedup across sandboxes)

### Phase 2: Merkle Index + O(1) Fork (1-2 weeks on top of Phase 1)

**Goal**: Add Prolly tree indexing and branch-based forking.

**If Phase 1 was Option A (libmdbx)**:
1. Integrate Okra (Zig Prolly tree) via CFFI for the tree index
2. Replace flat `(snapshot_id, path)` keys with Prolly tree nodes
3. Get deterministic Merkle root hash for each snapshot (same files = same hash, always)
4. Implement O(changes × log N) diff via Prolly tree walk
5. O(1) fork: `INSERT INTO refs (name, snapshot_id)` pointing at source's current snapshot

**If Phase 1 was Option B (Irmin)**:
1. Already have Merkle tree and branch semantics — this phase is mostly wiring
2. `POST /api/sandboxes/:id/fork` → create Irmin branch pointing at source's HEAD
3. Expose Irmin diff as `sq-snap diff`
4. Add P2P sync capability via Irmin's tree reconciliation

**Deliverables**:
- `sq-snap fork <source> <new-name>` — O(1) sandbox cloning
- `sq-snap verify` — recompute Merkle root, check integrity
- `POST /api/sandboxes/:id/fork` — API endpoint for parallel agent exploration
- Deterministic snapshot hashes (same content = same hash across machines)

### Phase 3: S-expression Layer + Autopoiesis Integration (2-3 weeks)

**Goal**: Eliminate the serialization boundary. Storage format = computation format.

1. Implement hash-consing in CL: `(hash-cons sexp) → content-address`
2. Store datoms as S-expressions: `(file-123 :content-hash #x... :mode 33188 :mtime 1711324800)`
3. FSet tree nodes stored by hash, sharing sub-expressions automatically
4. Autopoiesis can read/write its native data structures directly to the store
5. Filesystem trees, agent state, and snapshot history become the same data structure
6. `diff` = structural comparison of S-expression trees (walk DAG, skip matching hashes)

**Deliverables**:
- CL library: `cas-store` with `put-sexp`, `get-sexp`, `root-hash`, `diff-roots`
- Autopoiesis integration: datom store backed by CAS S-expressions
- Unified storage: filesystem snapshots + agent state in one Merkle DAG
- REPL-friendly: `(inspect-snapshot "sandbox-abc/snap-3")` → browse tree interactively

### Phase 4: Lazy FUSE Materialization (2-3 weeks)

**Goal**: Eliminate the restore materialization step entirely.

1. FUSE mount backed by CAS store (libmdbx or Irmin)
2. `open(path)` → lookup tree entry, decompress blob on demand
3. `readdir(path)` → query tree entries with path prefix
4. Combined with overlayfs for writes (copy-up on first write)
5. `sq-snap restore` becomes O(1): just point FUSE at new snapshot_id

**Deliverables**:
- `sq-fuse mount <snapshot> <mountpoint>` — lazy CAS-backed filesystem
- Restore penalty eliminated: O(1) instead of O(total_files)
- Only decompress files actually accessed (O(accessed) not O(total))
- Full POSIX compat: `exec` works (binaries materialized to page cache on demand)

### Phase 5: Virtual Filesystem for Agent Harness (optional, 1-2 weeks)

**Goal**: For agents that don't need `exec`, bypass FUSE entirely (Kyle's insight).

1. Tool interface: `read_file`, `write_file`, `list_dir`, `search` backed directly by CAS
2. Only materialize to disk when `exec` is requested
3. Agent sees no difference — same tool API, different backend
4. Enables running sandboxes without mount privileges, overlayfs, or kernel modules

### Decision Matrix: Which Path to Take

| Factor | libmdbx + Okra path | Irmin path |
|---|---|---|
| **Time to Phase 1** | ~3 weeks | ~1 week (Irmin gives more for free) |
| **Time to Phase 2** | +2 weeks (Okra integration) | +1 week (already have branches/diff) |
| **Raw performance** | Faster (in-process, zero-copy) | Slower (IPC to irmin-server) |
| **CL integration** | CFFI (tight) | WebSocket/JSON (loose) |
| **Maintenance burden** | Higher (own the stack) | Lower (Irmin team maintains core) |
| **Autopoiesis alignment** | Medium (need S-exp layer) | Medium (need S-exp layer) |
| **Production track record** | Ethereum (libmdbx), experimental (Okra) | Tezos (Irmin) |
| **OCaml dependency** | No | Yes |

**Recommendation**: Start with **libmdbx + Okra** if CL/Zig is the primary ecosystem and you want tight in-process integration. Start with **Irmin** if you want maximum functionality fastest and don't mind the OCaml dependency. Both paths converge at Phase 3 (S-expression layer) where autopoiesis integration happens.

## References

### Core
- [pyrex41/autopoiesis](https://github.com/pyrex41/autopoiesis) — Datom store, snapshot DAG, O(1) forking, structural diffing
- Kyle Mistele thread on [separating tool interface from execution](https://x.com/0xblacklight/status/2036534699582255329)

### Storage Engines (Final Candidates)
- [Irmin](https://github.com/mirage/irmin) — OCaml content-addressed Merkle DAG store (Tezos)
- [libmdbx](https://github.com/erthink/libmdbx) — LMDB fork, 10-20% faster, used by Erigon/Reth
- [Okra](https://github.com/canvasxyz/okra) — Zig Prolly tree on LMDB, Merkle root, C ABI
- [Lurk](https://github.com/lurk-lab/lurk-beta) — content-addressed S-expression store in Rust

### Storage Engines (Surveyed)
- [Redb](https://github.com/cberner/redb) — Rust CoW B-tree with Savepoints
- [Pebble](https://github.com/cockroachdb/pebble) — Go LSM-tree with value separation
- [Datahike](https://github.com/replikativ/datahike) — Clojure Datomic-compatible with hitchhiker trees
- [Datalevin](https://github.com/juji-io/datalevin) — Clojure Datomic API on LMDB
- [XTDB v2](https://docs.xtdb.com/intro/what-is-xtdb.html) — Clojure bitemporal database

### Prior Art
- [DoltHub: Prolly Trees](https://www.dolthub.com/blog/2022-06-27-prolly-chunker/) — content-addressed B-trees
- [DoltHub: Structural Sharing](https://www.dolthub.com/blog/2024-04-12-study-in-structural-sharing/) — sharing analysis
- [OSTree](https://ostreedev.github.io/ostree/) — content-addressed OS tree (Fedora/RHEL)
- [LMDB internals](http://www.lmdb.tech/doc/) — zero-copy CoW B-tree
- [How LMDB works](https://xgwang.me/posts/how-lmdb-works/) — deep technical overview
- [casync](https://github.com/systemd/casync) — content-addressable sync tool
- [desync](https://github.com/folbricht/desync) — Go implementation of casync protocol
- [Nydus](https://github.com/dragonflyoss/nydus) — container-native CAS with lazy pull

### Filesystem / Snapshot
- [SQLite Archive (sqlar)](https://www.sqlite.org/sqlar.html) — built-in archive format
- [SQLite Faster Than Filesystem](https://sqlite.org/fasterthanfs.html) — blob access benchmarks
- [sqlarfs FUSE](https://github.com/nicmcd/sqlarfs) — mount sqlar as filesystem
- [libsqlfs](https://github.com/guardianproject/libsqlfs) — full POSIX filesystem on SQLite
- [erofs](https://erofs.docs.kernel.org/) — next-gen read-only filesystem (kernel-native)
- [composefs](https://github.com/containers/composefs) — EROFS + overlayfs for containers
- [RocksDB BlobDB](https://rocksdb.org/blog/2021/05/26/integrated-blob-db.html) — key-value separation
- [Alluxio + RocksDB](https://www.alluxio.io/blog/scalable-metadata-service-in-alluxio-storing-billions-of-files) — billion-file metadata
