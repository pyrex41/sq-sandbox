---
date: 2026-02-13T15:26:14-06:00
researcher: reuben
git_commit: faf9bf9c0df80b50976d9b4c83ea0ca5f7021378
branch: main
repository: sq-sandbox
topic: "SquashFS performance characteristics for coding agent sandboxes"
tags: [research, performance, squashfs, overlayfs, latency, coding-agents, gvisor, compression, zstd]
status: complete
last_updated: 2026-02-13
last_updated_by: reuben
last_updated_note: "Added gVisor architectural comparison and compression algorithm analysis"
---

# Research: SquashFS Performance for Coding Agent Sandboxes

**Date**: 2026-02-13T15:26:14-06:00
**Researcher**: reuben
**Git Commit**: faf9bf9c0df80b50976d9b4c83ea0ca5f7021378
**Branch**: main
**Repository**: sq-sandbox

## Research Question

What are the performance characteristics of SquashFS + overlayfs for coding agent sandboxes? Are there latency concerns for agents that are already network-bound on inference time?

## Summary

**Short answer: No meaningful latency issues for coding agents.** The architecture (read-only squashfs lower layers + tmpfs upper via overlayfs) is well-suited for this workload. After initial page cache warm-up (first access of any given file), reads are served at native memory speeds. Writes go directly to tmpfs (RAM). The dominant latency in a coding agent loop is inference time (seconds to tens of seconds per turn), which dwarfs any filesystem overhead by 3-4 orders of magnitude.

The one operation that introduces measurable wall-clock time is **snapshotting** (`mksquashfs`), which compresses the upper layer. For a typical code project (10-500MB), this takes 1-5 seconds with gzip on modern multi-core hardware — still small relative to a single LLM inference call.

## Detailed Findings

### 1. SquashFS Read Performance

SquashFS splits compressed files into fixed-size blocks (256KB in sq-sandbox's configuration). On first access, the kernel decompresses the relevant block into the **page cache**. Subsequent reads of that file come directly from cache at native memory speed.

**Benchmark data:**
- SquashFS (gzip) sequential reads: ~92 MB/s (cold cache)
- ext4 sequential reads: ~242 MB/s (for comparison)
- After cache warm-up: effectively equivalent — both served from page cache
- Modern kernels decompress directly into page cache, improving from ~13 MB/s to ~67 MB/s single-threaded (LWN patch analysis)

**For source code files (1-100KB):** A single 256KB block decompression serves the entire file. At 67+ MB/s decompression rate, a 100KB file decompresses in ~1.5ms. After first access, it's a page cache hit (~microseconds).

**Relevant code:** `cgi-bin/common.sh:420` — modules are loop-mounted read-only:
```sh
mount -o loop,ro -t squashfs "$(mod_path "$mod")" "$mp"
```

### 2. Overlayfs Overhead

Overlayfs adds negligible overhead for the steady-state read/write pattern of coding agents:

- **SSD benchmarks:** overlayfs adds ~17% throughput overhead (486 MB/s vs 588 MB/s)
- **After file open:** operations go directly to the underlying filesystem (no union overhead per I/O op)
- **gVisor real-world:** switching to overlayfs with tmpfs upper **halved sandboxing overhead** for Abseil-cpp Bazel builds

**Copy-up behavior:** The first write to a file from a lower (squashfs) layer triggers a full file copy to the upper (tmpfs) layer. For source code files this is negligible (copying a 50KB file to RAM takes microseconds). Subsequent reads and writes of that file go directly to tmpfs.

**Relevant code:** `cgi-bin/common.sh:211-218` — overlay mount with tmpfs upper:
```sh
mount -t overlay overlay \
    -o "lowerdir=$lower,upperdir=$s/upper,workdir=$s/work" \
    "$s/merged"
```

### 3. Write Path: tmpfs Upper Layer

All writes land in the tmpfs upper layer, which is RAM-backed. This means:

- **Write latency:** ~nanoseconds (memory copy, no disk I/O)
- **git clone into sandbox:** Files written to tmpfs at RAM speed (~10-20 GB/s)
- **File edits by agent:** Direct tmpfs writes, no squashfs involvement
- **Test execution output:** All generated files (`.pyc`, build artifacts, logs) go to tmpfs

The upper layer is the "hot" working set for a coding agent. After initial sandbox creation (base OS + runtime from squashfs), the agent's workflow is predominantly tmpfs reads/writes.

### 4. Snapshot Performance (mksquashfs)

Snapshotting compresses the upper layer to a squashfs image. This is the most expensive operation in the system.

**Relevant code:** `cgi-bin/common.sh:572`:
```sh
mksquashfs "$s/upper" "$snapfile" -comp gzip -b 256K -noappend -quiet
```

**Benchmark data for gzip compression:**
- Single-threaded: ~15 MB/s
- 8 threads (AMD Ryzen 7): ~73 MB/s (6.8x speedup)
- 16 threads: ~127 MB/s (11.6x speedup)
- lz4 alternative: ~1.3 GB/s (if latency is critical, could switch compressor)

**Estimated times for coding agent workloads:**

| Upper layer size | gzip (8 threads) | lz4 (8 threads) |
|-----------------|-------------------|------------------|
| 10 MB           | ~0.1s             | ~0.01s           |
| 50 MB           | ~0.7s             | ~0.04s           |
| 200 MB          | ~2.7s             | ~0.15s           |
| 500 MB          | ~6.8s             | ~0.38s           |

For context, a single Claude API call with tool use takes 3-30+ seconds. A snapshot of a typical code project upper layer fits within a single inference round-trip.

### 5. Loop Mount Overhead

The system uses kernel-native loop mounts (`mount -t squashfs`), not FUSE. This is the right choice:

- **Kernel loop mount:** negligible overhead over direct block device access
- **FUSE (squashfuse):** ~57% overhead measured in LLVM compilation benchmark (12m49s vs 8m10s)

**Relevant code:** `cgi-bin/common.sh:420` uses native mount, not squashfuse.

### 6. Layer Stacking Depth

sq-sandbox stacks layers via overlayfs `lowerdir=` with colon-separated paths (`cgi-bin/common.sh:196-209`). Typical depth: 2-4 layers (base + runtime + optional snapshot + optional activated module).

**Performance impact:** Overlayfs does a top-down search through layers for file lookup. With 2-4 layers, this adds microseconds per uncached lookup. After initial dentry cache population, lookups are O(1). This is not a concern at any realistic layer count.

### 7. Real-World Analogues

This exact pattern (squashfs + overlayfs) is used in production at scale:

- **Snap packages:** Every snap is a loop-mounted squashfs with overlayfs. Billions of installations.
- **PorteuX Linux:** Entire OS built from stacked squashfs modules + overlayfs. Boots in <15s.
- **Docker layers:** Conceptually identical (though Docker uses tar, not squashfs). overlayfs2 is the default storage driver.
- **gVisor sandboxes:** Google switched to overlayfs + tmpfs upper for container sandboxing, halving overhead.
- **HPC (Spack, conda-pack):** SquashFS used for distributing software environments on clusters.

## Performance Model for Coding Agent Workload

A coding agent session looks like:

1. **Create sandbox** (~1-3s): Mount 2-3 squashfs layers, set up overlayfs, network namespace
2. **Agent loop** (repeated):
   - LLM inference: **3-30s** (dominant latency)
   - File read: **<1ms** (page cache hit after first access) or **~1-5ms** (cold cache, decompression)
   - File write: **<0.1ms** (tmpfs)
   - Command exec (`python`, `npm test`, etc.): **100ms-60s** (CPU-bound, not filesystem-bound)
3. **Snapshot** (occasional): **1-5s** for typical code project
4. **Destroy**: **<1s**

**The filesystem is never the bottleneck.** The agent spends >95% of its wall-clock time waiting for inference or running user-space programs. SquashFS read latency (milliseconds on cold cache, microseconds on warm) is invisible against these timescales.

### Where You Might Notice Latency

- **First `exec` in a new sandbox:** Cold cache means decompressing base OS files (libc, shell, Python interpreter). Expect ~100-500ms of decompression overhead on first command. Second command is fast.
- **Large `git clone` into sandbox:** Writing to tmpfs is fast, but if the repo is large (>1GB), tmpfs memory pressure could trigger swapping. Monitor with `memory_mb` limit.
- **Snapshot of large upper layer:** If the agent has installed many packages (npm, pip), upper layer could be hundreds of MB. Snapshot takes proportionally longer.
- **Many concurrent sandboxes:** Each sandbox's squashfs layers share the page cache (same underlying files), which is a *benefit* — base Alpine and Python runtime blocks are decompressed once, shared across all sandboxes.

### What Would NOT Work Well

- **Database workload** (random 4KB writes with fsync): overlayfs copy-up + tmpfs has no durable fsync. Not the use case here.
- **Massive file I/O** (video processing, ML training data): squashfs decompression throughput could be limiting. Not the use case here.
- **Thousands of stacked layers:** overlayfs lookup degrades. Not the use case (2-4 layers typical).

## Code References

- `cgi-bin/common.sh:420` — squashfs loop mount during sandbox creation
- `cgi-bin/common.sh:211-218` — overlayfs mount with lowerdir/upperdir/workdir
- `cgi-bin/common.sh:196-209` — lowerdir string construction from stacked layers
- `cgi-bin/common.sh:572` — mksquashfs snapshot with gzip, 256K blocks
- `bin/sq-mkmod:27` — module build: `mksquashfs ... -comp gzip -b 256K -noappend -quiet -no-xattrs -no-exports`
- `cgi-bin/common.sh:486-513` — exec path: unshare + chroot into merged overlayfs

## Architecture Documentation

The filesystem stack for each sandbox:

```
┌─────────────────────────────┐
│ upper/  (writable, tmpfs)   │  ← all agent writes land here (RAM speed)
├─────────────────────────────┤
│ _snapshot/ (squashfs, r/o)  │  ← optional: restored checkpoint
├─────────────────────────────┤
│ 100-python312.squashfs (r/o)│  ← page-cached after first access
├─────────────────────────────┤
│ 000-base-alpine.squashfs(r/o)│ ← shared page cache across sandboxes
└─────────────────────────────┘
         ↓ overlayfs merge ↓
┌─────────────────────────────┐
│ merged/  (chroot target)    │  ← agent sees unified filesystem
└─────────────────────────────┘
```

Key properties:
- Read-only layers are shared via page cache across all sandboxes using the same modules
- Writes never touch squashfs — all modifications go to tmpfs upper
- Copy-up is per-file, on first write (not on read)
- Layer count is small (2-4 typical), keeping overlayfs lookup fast

## Historical Context (from thoughts/)

No dedicated performance analysis documents exist in the thoughts/ directory. Existing documents focus on security architecture and implementation plans. Relevant architectural context:

- `thoughts/shared/plans/plan-firecracker-and-security.md` — Implementation details for Firecracker guest init that mounts squashfs layers from virtio block devices, same overlayfs pattern
- `thoughts/shared/research/2026-02-13-security-architecture-and-testability.md` — Documents the overlayfs stack architecture

## Sources

- [Squashfs Performance Testing – Jonathan Carter](https://jonathancarter.org/2015/04/06/squashfs-performance-testing/)
- [Squashfs: Directly decompress into the page cache (LWN)](https://lwn.net/Articles/570325/)
- [Linux Kernel SquashFS Documentation](https://docs.kernel.org/filesystems/squashfs.html)
- [EROFS vs. SquashFS: A Gentle Benchmark](https://sigma-star.at/blog/2022/07/squashfs-erofs/)
- [gVisor improves performance with root filesystem overlay](https://opensource.googleblog.com/2023/04/gvisor-improves-performance-with-root-filesystem-overlay.html)
- [squashfs-tools-ng benchmarks](https://github.com/AgentD/squashfs-tools-ng/blob/master/doc/benchmark.txt)
- [squashfs compression tests (baryluk)](https://gist.github.com/baryluk/70a99b5f26df4671378dd05afef97fce)
- [The curious case of stalled squashfs reads (Chris Down)](https://chrisdown.name/2018/04/17/kernel-adventures-the-curious-case-of-squashfs-stalls.html)
- [OverlayFS storage driver (Docker docs)](https://docs.docker.com/engine/storage/drivers/overlayfs-driver/)

## Open Questions

- **Memory pressure monitoring:** With tmpfs upper layers, memory is the constraint. No data yet on typical upper layer sizes for coding agent sessions. Worth instrumenting `upper_bytes` over real sessions.
- **Page cache sharing validation:** The theory that multiple sandboxes sharing the same base module share decompressed page cache blocks should be validated with `/proc/meminfo` measurements under concurrent load.

---

## Follow-up Research: gVisor Comparison & Compression Algorithm Selection

### gVisor Architectural Comparison

#### How gVisor Works

gVisor interposes a **user-space kernel** (the Sentry) between the application and the host kernel. Every syscall from the sandboxed process is intercepted and re-implemented in Go:

- **Sentry**: Reimplements ~70-80% of Linux syscalls in user-space. Only needs 53 host syscalls itself (without networking), drastically reducing kernel attack surface.
- **Gofer**: Separate process handling filesystem I/O. Communicates with Sentry via LISAFS protocol over Unix domain sockets. Even if compromised, can only access files (not network).
- **Systrap** (current default platform): Uses `SECCOMP_RET_TRAP` to intercept syscalls. Kernel sends `SIGSYS` signal to the triggering thread. ~800ns per syscall vs ~70ns native.

#### Performance: sq-sandbox vs gVisor

| Aspect | sq-sandbox (chroot+overlayfs) | gVisor |
|--------|-------------------------------|--------|
| **Syscall overhead** | ~70ns (native) | ~800ns (11x slower) |
| **File open** | native | 11x slower (Sentry interception) |
| **File read (cached)** | page cache direct | page cache via Sentry |
| **File write (tmpfs)** | native tmpfs | Sentry-managed tmpfs |
| **Network throughput** | native | 2.5-5.4x slower (userspace TCP) |
| **CPU-bound work** | native | native (no instruction emulation) |
| **Process fork/exec** | native | overhead from Sentry process mgmt |
| **Cold start** | ~1-3s (mount layers) | <100ms (warm), varies (cold) |

**Concrete gVisor benchmark data (USENIX HotCloud 2019):**

| Operation | gVisor vs native container |
|-----------|---------------------------|
| Simple syscalls | 2.2x slower |
| Open/close files (external tmpfs) | 216x slower |
| Read small files | 11x slower |
| Large file download | 2.8x slower |

**gVisor's 2023 rootfs overlay optimization** moved overlayfs inside the sandbox, eliminating Gofer RPCs for upper-layer writes. This brought fsstress from 262s to 3.2s (98.8% improvement) and halved sandboxing overhead for real compilation workloads. But even after this optimization, file operations remain significantly slower than native due to Sentry syscall interception.

#### Security Tradeoff

| Aspect | sq-sandbox (chroot) | sq-sandbox (Firecracker) | gVisor |
|--------|---------------------|--------------------------|--------|
| **Kernel attack surface** | Full (~350 syscalls) | Full (but isolated VM) | 53-211 filtered syscalls |
| **Kernel exploit** | Container escape | Contained by VMM | Must compromise Sentry first |
| **Defense layers** | 1 (namespaces) | 2 (VM + namespaces) | 2 (Sentry + seccomp) |
| **Hardware requirement** | None (privileged) | `/dev/kvm` | None |

**Key insight:** gVisor trades ~10x syscall overhead for a dramatically reduced kernel attack surface. sq-sandbox's chroot mode trades security for native performance; its Firecracker mode provides comparable isolation to gVisor with near-native performance (since Firecracker uses hardware virtualization, not syscall interposition).

#### What the Industry Uses for AI Sandboxes

| Provider | Isolation | Why |
|----------|-----------|-----|
| **E2B** | Firecracker microVMs | Strongest isolation, ~150ms cold start |
| **Modal** | gVisor (Systrap) | OCI compatibility, existing container workflows |
| **AWS Lambda** | Firecracker | Untrusted code at massive scale |
| **Google Cloud Run** | gVisor | Serverless containers, 10-30% I/O overhead acceptable |
| **sq-sandbox** | chroot or Firecracker | User chooses: max performance or VM isolation |

The hyperscaler signal is clear: nobody uses bare containers for untrusted AI-generated code. The choice is between gVisor (software isolation, moderate overhead) and Firecracker (hardware isolation, minimal overhead but needs KVM).

#### Relevance to sq-sandbox

For coding agents specifically, the performance comparison favors sq-sandbox's architecture:

1. **File I/O is the primary workload** — reading source code, writing edits, running builds. gVisor's 2-11x file overhead is the worst possible tax for this workload. sq-sandbox's native overlayfs pays no such tax.
2. **Inference time dominates anyway** — even gVisor's overhead would be invisible against 3-30s inference calls. But it matters for command execution (running tests, installing packages), where gVisor overhead compounds across thousands of syscalls.
3. **Firecracker mode exists** — for users who need gVisor-class isolation, sq-sandbox offers Firecracker with near-native file I/O performance inside the VM.

---

### Compression Algorithm Analysis

#### Current State

All three `mksquashfs` invocations in the codebase use identical hardcoded settings:

- `bin/sq-mkbase:33` — `-comp gzip -b 256K`
- `bin/sq-mkmod:27` — `-comp gzip -b 256K`
- `cgi-bin/common.sh:572` — `-comp gzip -b 256K`

No environment variables or config options exist for compression settings.

#### Algorithm Comparison (128KB blocks, representative datasets)

| Algorithm | Compress MB/s | Decompress MB/s | Ratio | CPU on decompress |
|-----------|--------------|-----------------|-------|-------------------|
| **lz4** | 94 | 218 | 2.1x | Very low (I/O bound) |
| **lz4hc** | 50-60 | 210-218 | 2.5x | Very low |
| **lzo** | 9.5 | 217 | 2.6x | Very low |
| **zstd -1** | 96 | 210 | 2.7x | Low |
| **zstd -3** | ~75 | 200-210 | 2.85x | Low |
| **zstd -5** | 69 | 198 | 2.9x | Low-moderate |
| **gzip** (current) | 15 | 128 | 2.9x | Moderate |
| **zstd -15** | 11.4 | 224 | 3.1x | Moderate |
| **xz** | 5.5 | 35 | 3.4x | Very high |

**Key observations:**
- **gzip is dominated by zstd on every axis.** zstd -3 compresses 5x faster, decompresses 1.6x faster, and achieves a comparable ratio.
- **lz4 decompresses 1.7x faster than gzip** but images are ~40% larger.
- **xz compresses 17% smaller than gzip** but decompresses 3.7x slower — terrible for sandbox cold starts.
- **zstd has a unique property**: decompression speed is nearly constant across compression levels (200-224 MB/s), while compression speed varies 10x. This means you can crank up compression for module builds (one-time cost) without affecting mount-time performance.

#### Block Size Analysis

| Block size | Compression ratio | Small-file cold read latency | Best for |
|-----------|-------------------|------------------------------|----------|
| 4K | worst (~3% penalty) | lowest | random access only |
| 64K | good | low | interactive development |
| **128K** | **good** | **reasonable** | **general purpose, industry standard** |
| 256K (current) | better | higher (decompresses 256K for 1-byte read) | sequential/archive |
| 1M | best | highest | distribution images |

SquashFS has a **fragment block** feature that packs files smaller than the block size into shared blocks (default cache: 3 fragment blocks). This mitigates the large-block-small-file problem somewhat, but reducing block size from 256K to 128K would still reduce read amplification for cold cache access of small source files.

**Industry defaults:** Ubuntu live media uses 128K. Snap packages use 128K. Fedora is standardizing on 128K.

#### Recommendation: Two-Tier Compression Strategy

**For modules and base images** (built once, mounted many times):

```sh
mksquashfs "$src" "$out" -comp zstd -Xcompression-level 15 -b 128K -noappend -quiet -no-xattrs -no-exports
```

- zstd -15 gives near-xz compression (3.1x) with 6x faster decompression than xz
- Compression is slow (~11 MB/s) but this is a one-time build cost
- 128K blocks reduce cold-cache latency for small source files
- Decompression speed (224 MB/s) is actually faster than lower zstd levels

**For snapshots** (created frequently during sandbox lifecycle):

```sh
mksquashfs "$s/upper" "$snapfile" -comp zstd -Xcompression-level 3 -b 128K -noappend -quiet
```

- zstd -3 compresses at ~75 MB/s (5x faster than gzip)
- Decompression: ~200 MB/s (1.6x faster than gzip)
- Compression ratio: ~2.85x (comparable to gzip's 2.9x)
- Low CPU overhead — won't compete with running sandboxes

**Estimated snapshot times with zstd -3 (8 threads):**

| Upper layer size | gzip (current) | zstd -3 | Improvement |
|-----------------|-----------------|---------|-------------|
| 10 MB | ~0.1s | ~0.02s | 5x |
| 50 MB | ~0.7s | ~0.14s | 5x |
| 200 MB | ~2.7s | ~0.54s | 5x |
| 500 MB | ~6.8s | ~1.36s | 5x |

#### Kernel Support

All algorithms are built into the kernel's squashfs driver since Linux 4.14 (2017). No additional modules needed. The Firecracker guest kernel (6.1) supports all of them.

Ensure `CONFIG_SQUASHFS_DECOMP_MULTI_PERCPU=y` is enabled in the kernel for parallel decompression (default in most distros). This allows `num_online_cpus() * 2` concurrent decompressors.

#### Compression Settings in Codebase

The three mksquashfs call sites are:

- `bin/sq-mkbase:33-34` — base image creation
- `bin/sq-mkmod:27` — module creation (called by `squash_it()`)
- `cgi-bin/common.sh:572` — snapshot creation

All are hardcoded. To make this configurable, the simplest approach would be environment variables:

```sh
SQUASH_COMP="${SQUASH_COMP:-zstd}"
SQUASH_COMP_LEVEL="${SQUASH_COMP_LEVEL:-3}"
SQUASH_BLOCK_SIZE="${SQUASH_BLOCK_SIZE:-128K}"
```

## Additional Sources

- [The True Cost of Containing: A gVisor Case Study (USENIX HotCloud 2019)](https://www.usenix.org/system/files/hotcloud19-paper-young.pdf)
- [gVisor Security Basics](https://gvisor.dev/blog/2019/11/18/gvisor-security-basics-part-1/)
- [gVisor rootfs overlay optimization](https://gvisor.dev/blog/2023/05/08/rootfs-overlay/)
- [Optimizing gVisor filesystems with Directfs](https://gvisor.dev/blog/2023/06/27/directfs/)
- [Releasing Systrap platform](https://gvisor.dev/blog/2023/04/28/systrap-release/)
- [Firecracker vs gVisor comparison](https://northflank.com/blog/firecracker-vs-gvisor)
- [AI agent sandboxing guide (2026)](https://manveerc.substack.com/p/ai-agent-sandboxing-guide)
- [Snap speed improvements with LZO](https://snapcraft.io/blog/snap-speed-improvements-with-new-compression-algorithm)
- [Why LZO was chosen for Snap](https://snapcraft.io/blog/why-lzo-was-chosen-as-the-new-compression-method)
- [AppImage zstd evaluation (Issue #478)](https://github.com/AppImage/AppImageKit/issues/478)
- [PorteuX migrating to zstd](https://forum.porteus.org/viewtopic.php?t=8515)
- [Fedora OptimizeSquashFS](https://fedoraproject.org/wiki/Changes/OptimizeSquashFS)
