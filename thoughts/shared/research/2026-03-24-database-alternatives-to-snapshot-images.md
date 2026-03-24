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

## Concrete Next Steps

### Low-hanging fruit (incremental, compatible with current arch)

1. **Add content-addressed blob cache alongside squashfs** — when creating a snapshot, also index file hashes. Enable "fast diff" between snapshots.

2. **Replace `snapshots.jsonl` with SQLite** — already have SQLite for sync queue in `storage.sh`. Unify metadata storage. Enables queries like "list snapshots containing file X".

3. **Implement incremental squashfs creation** — use the hash index to `mksquashfs` only changed files (via `-pf` pseudo file definitions or `sqfstar` with filtered input).

### Medium-term (new snapshot format)

4. **SQLite-backed snapshot format** — a `.sq-snap` file that is a SQLite database containing the file tree + compressed blobs. Replaces `.squashfs` files. Can be mounted via FUSE for backward compat, or materialized to overlay upper.

5. **CAS blob store with SQLite index** — content-addressable storage for dedup across snapshots. Dramatically reduces S3 sync bandwidth for similar snapshots.

### Longer-term (rethink execution model per Kyle's insight)

6. **Virtual filesystem tools for agent harness** — for agents that only need read/write/search (not exec), bypass the mounted filesystem entirely. Serve file operations from the DB. Only materialize to disk when the agent requests `exec`.

7. **Lazy materialization** — mount a FUSE filesystem backed by the CAS. Files are fetched/decompressed on first access. Combine with overlayfs for writes. Snapshots are just new tree pointers.

## References

- [SQLite Archive (sqlar)](https://www.sqlite.org/sqlar.html) — built-in archive format
- [sqlarfs FUSE](https://github.com/nicmcd/sqlarfs) — mount sqlar as filesystem
- [littlefs](https://github.com/littlefs-project/littlefs) — embedded filesystem in a DB-like format
- [LMDB](http://www.lmdb.tech/doc/) — zero-copy memory-mapped KV store
- [erofs](https://erofs.docs.kernel.org/) — next-gen read-only filesystem (kernel-native)
- [casync](https://github.com/systemd/casync) — content-addressable sync tool (inspiration for CAS approach)
- [desync](https://github.com/folbricht/desync) — Go implementation of casync protocol
- Kyle Mistele thread on [separating tool interface from execution](https://x.com/0xblacklight/status/2036534699582255329)
