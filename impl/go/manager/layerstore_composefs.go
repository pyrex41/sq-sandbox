package manager

import (
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"squashd/config"
)

// composefsLayerStore is the opt-in LayerStore that assembles RO layers as
// per-layer EROFS metadata images stacked over a shared, content-addressed
// objects store (the composefs model).
//
// It implements the SAME LayerStore interface as squashfsLayerStore, so the
// manager's create/activate/snapshot/restore/destroy paths are unchanged — only
// the primitive operations differ:
//
//   - MountLayer:  mount one EROFS image read-only at its per-layer mountpoint
//     (`mount -t erofs -o ro`). On kernels >= 6.12 this is file-backed and needs
//     no loop device. Because each layer is its own EROFS mount, Activate and
//     Restore add a single layer + remount the overlay WITHOUT rebuilding or
//     repacking the rest of the stack (see BuildLowerDirs reuse below).
//   - MountOverlay: the same ordered layer mountpoints become overlay lowerdirs,
//     but a data-only ('::') lower carrying the shared objects store is appended
//     and redirect_dir=on,metacopy=on are set so the EROFS metadata can redirect
//     file data into the content-addressed objects store.
//   - Snapshot:    freeze the writable upper into a new EROFS image via
//     `mkcomposefs --digest-store=<objects> --use-epoch`, writing file data into
//     the same shared objects store every layer references.
//
// The objects store is daemon-global (DataDir/cfs/objects), not per-sandbox, so
// identical content is stored once across all sandboxes/layers. Destroy only
// tears down this sandbox's overlay + per-layer EROFS mounts; the shared objects
// store is intentionally left intact (it is referenced by other sandboxes; GC of
// unreferenced objects is a separate store-level concern, not per-sandbox).
//
// This store is gated at runtime: every mount/pack operation first verifies the
// kernel actually supports EROFS + overlayfs redirect_dir/metacopy. If support
// is missing it returns a CLEAR error instructing the operator to fall back to
// SQUASH_LAYER_BACKEND=squashfs — it never silently produces a broken stack.
//
// fs-verity is opt-in (SQUASH_COMPOSEFS_VERITY) and fail-open AT THE MOUNT
// BOUNDARY: when enabled, Snapshot best-effort seals the objects it writes and
// MountOverlay ATTEMPTS verity=require, but on ANY mount error (older overlayfs
// without the verity= option, unsealed objects, non-verity-capable store FS) it
// retries the identical mount unauthenticated and logs a warning — it never hard-
// fails. verity is absent on stock Firecracker, Docker overlay2, and tmpfs, so it
// must be treated as a best-effort integrity enhancement, not a hard requirement.
type composefsLayerStore struct {
	cfg *config.Config
}

// composefsFallbackHint is appended to every composefs error that an operator can
// resolve by switching layer stores. File-backed EROFS (kernel >=6.12) and
// data-only ('::') overlay lowers (kernel ~6.7) are NOT version-gated up front —
// distros backport unevenly, so a version number is a poor predictor. Instead the
// probe catches obviously-incapable kernels fast, and the actual mount calls wrap
// any residual EINVAL with this hint so an in-between kernel (5.x / early 6.x)
// surfaces an actionable message instead of a raw mount error.
const composefsFallbackHint = "; set SQUASH_LAYER_BACKEND=squashfs to use the portable squashfuse layer store"

// objectsDir returns the daemon-global content-addressed objects store path.
// Shared across every sandbox and layer so identical file data is stored once.
func (c *composefsLayerStore) objectsDir() string {
	return filepath.Join(c.cfg.DataDir, "cfs", "objects")
}

func (c *composefsLayerStore) MountLayer(image, mountpoint string) error {
	if err := composefsSupported(); err != nil {
		return err
	}
	img := resolveComposefsImage(image)
	// Read-only EROFS metadata mount (loop-free on file-backed EROFS, kernel >=6.12).
	if out, err := exec.Command("mount", "-t", "erofs", "-o", "ro", img, mountpoint).CombinedOutput(); err != nil {
		return fmt.Errorf("composefs mount erofs %s -> %s: %w: %s (kernel may lack file-backed EROFS, requires >=6.12)%s",
			img, mountpoint, err, strings.TrimSpace(string(out)), composefsFallbackHint)
	}
	return nil
}

func (c *composefsLayerStore) UnmountLayer(mountpoint string) error {
	if out, err := exec.Command("umount", mountpoint).CombinedOutput(); err != nil {
		return fmt.Errorf("composefs umount %s: %w: %s",
			mountpoint, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// BuildLowerDirs / BuildLowerDirsExcluding reuse the shared package helpers: the
// enumeration semantics (one mountpoint dir per RO layer under imagesDir, sorted
// highest numeric prefix first) are identical for composefs. This is what lets
// Activate/Restore add a single EROFS layer and remount without recomposing the
// full stack — the layer set is just the directory listing, not a repacked image.
func (c *composefsLayerStore) BuildLowerDirs(imagesDir string) ([]string, error) {
	return buildLowerDirs(imagesDir)
}

func (c *composefsLayerStore) BuildLowerDirsExcluding(imagesDir, exclude string) ([]string, error) {
	return buildLowerDirsExcluding(imagesDir, exclude)
}

func (c *composefsLayerStore) MountOverlay(lowerDirs []string, upper, work, merged string) error {
	if err := composefsSupported(); err != nil {
		return err
	}
	objects := c.objectsDir()
	if err := os.MkdirAll(objects, 0755); err != nil {
		return fmt.Errorf("composefs objects dir: %w", err)
	}

	// Normal lowers (highest-priority leftmost) ':' separated; the shared objects
	// store is appended as a data-only ('::') lower. Data-only layers are invisible
	// in the merged tree — they only satisfy redirect_dir/metacopy data references.
	lower := strings.Join(lowerDirs, ":")
	if lower == "" {
		// Degenerate (no RO layers): use objects as a plain lower so the kernel
		// still has >=1 normal lower.
		lower = objects
	} else {
		lower = lower + "::" + objects
	}
	base := "lowerdir=" + lower +
		",upperdir=" + upper +
		",workdir=" + work +
		",redirect_dir=on,metacopy=on"

	// fs-verity: opt-in (SQUASH_COMPOSEFS_VERITY) and GENUINELY fail-open AT THE
	// MOUNT BOUNDARY. The probe can only ever be a hint — it cannot prove the
	// running overlayfs accepts the `verity=` option (kernel >=6.6) nor that every
	// referenced object is actually sealed. So we ATTEMPT verity=require and, on ANY
	// error, retry the IDENTICAL mount WITHOUT verity and warn — we never let a
	// requested-but-unsatisfiable verity take a sandbox down. (verity is absent on
	// stock Firecracker, Docker overlay2, and tmpfs.)
	wantVerity := c.cfg != nil && c.cfg.ComposefsVerity && composefsVeritySupported(objects)
	if wantVerity {
		if out, err := exec.Command("mount", "-t", "overlay", "overlay", "-o", base+",verity=require", merged).CombinedOutput(); err != nil {
			slog.Warn("composefs: overlay mount with verity=require failed; retrying UNAUTHENTICATED (fail-open)",
				"merged", merged, "err", err, "out", strings.TrimSpace(string(out)))
		} else {
			return nil
		}
	} else if c.cfg != nil && c.cfg.ComposefsVerity {
		slog.Warn("composefs: fs-verity requested (SQUASH_COMPOSEFS_VERITY) but unsupported on this kernel/objects-store filesystem; mounting UNAUTHENTICATED (fail-open)",
			"objects", objects)
	}

	if out, err := exec.Command("mount", "-t", "overlay", "overlay", "-o", base, merged).CombinedOutput(); err != nil {
		return fmt.Errorf("composefs mount overlay -> %s: %w: %s (kernel may lack data-only overlay lowers or redirect_dir/metacopy, requires ~6.7+)%s",
			merged, err, strings.TrimSpace(string(out)), composefsFallbackHint)
	}
	return nil
}

func (c *composefsLayerStore) UnmountOverlay(merged string) error {
	if out, err := exec.Command("umount", merged).CombinedOutput(); err != nil {
		return fmt.Errorf("composefs umount overlay %s: %w: %s",
			merged, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// Snapshot freezes upperDir into a new EROFS image at outPath, writing file data
// into the shared content-addressed objects store. The resulting image is a valid
// RO layer that MountLayer can later mount (Restore re-mounts it as the top lower).
// --use-epoch zeroes mtimes for deterministic, content-addressable output.
func (c *composefsLayerStore) Snapshot(upperDir, outPath string) error {
	if err := composefsSupported(); err != nil {
		return err
	}
	objects := c.objectsDir()
	if err := os.MkdirAll(objects, 0755); err != nil {
		return fmt.Errorf("composefs objects dir: %w", err)
	}
	args := []string{"mkcomposefs", "--digest-store=" + objects, "--use-epoch", upperDir, outPath}
	if out, err := exec.Command(args[0], args[1:]...).CombinedOutput(); err != nil {
		return fmt.Errorf("mkcomposefs: %w: %s", err, strings.TrimSpace(string(out)))
	}
	// mkcomposefs records the EXPECTED fs-verity digest into the EROFS metacopy
	// xattrs but does NOT seal the data objects themselves. When verity enforcement
	// is opted-in, seal the just-written objects so overlay verity=require can
	// actually authenticate them. Best-effort + fail-open: any failure is ignored
	// (MountOverlay falls back to an unauthenticated mount), so this can never break
	// snapshotting on a filesystem/kernel without fs-verity.
	if c.cfg != nil && c.cfg.ComposefsVerity {
		sealObjectsBestEffort(objects)
	}
	return nil
}

// sealObjectsBestEffort runs `fsverity enable` on every file in the shared objects
// store so overlay verity=require can authenticate it. It is an integrity
// ENHANCEMENT, never a correctness requirement: every error (already-sealed,
// unsupported FS, missing tool) is intentionally ignored.
func sealObjectsBestEffort(objectsDir string) {
	fsverity, err := exec.LookPath("fsverity")
	if err != nil {
		return
	}
	_ = filepath.WalkDir(objectsDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return nil
		}
		_ = exec.Command(fsverity, "enable", path).Run()
		return nil
	})
}

// resolveComposefsImage bridges the manager's hard-coded ".squashfs" image-path
// convention to composefs's ".cfs" EROFS artifacts: if a sibling ".cfs" file
// exists it is preferred, otherwise the given path is used as-is (snapshots that
// this store itself produced are EROFS regardless of their filename, so mounting
// them via -t erofs is correct).
func resolveComposefsImage(image string) string {
	if strings.HasSuffix(image, ".squashfs") {
		cfs := strings.TrimSuffix(image, ".squashfs") + ".cfs"
		if _, err := os.Stat(cfs); err == nil {
			return cfs
		}
	}
	return image
}

// ── Runtime kernel support probing ───────────────────────────────────────────

var (
	composefsProbeOnce sync.Once
	composefsProbeErr  error
)

// composefsSupported verifies (once, cached) that the running kernel supports the
// composefs assembly path: EROFS + overlayfs with redirect_dir and metacopy. On
// any missing prerequisite it returns a clear, actionable error pointing the
// operator at SQUASH_LAYER_BACKEND=squashfs. Never silently falls through.
func composefsSupported() error {
	composefsProbeOnce.Do(func() { composefsProbeErr = probeComposefsSupport() })
	return composefsProbeErr
}

func probeComposefsSupport() error {
	if runtime.GOOS != "linux" {
		return fmt.Errorf("composefs LayerStore requires Linux (EROFS + overlayfs), host is %s%s", runtime.GOOS, composefsFallbackHint)
	}

	if !fsRegistered("erofs") {
		_ = exec.Command("modprobe", "erofs").Run()
		if !fsRegistered("erofs") {
			return fmt.Errorf("composefs LayerStore requires the erofs filesystem (absent from /proc/filesystems and modprobe erofs failed)%s", composefsFallbackHint)
		}
	}
	if !fsRegistered("overlay") {
		_ = exec.Command("modprobe", "overlay").Run()
		if !fsRegistered("overlay") {
			return fmt.Errorf("composefs LayerStore requires overlayfs (absent from /proc/filesystems and modprobe overlay failed)%s", composefsFallbackHint)
		}
	}
	// redirect_dir / metacopy are gated on the PRESENCE of the module-parameter
	// file, not its value. The module default ships "N" on stock kernels (a
	// security default) and the params accept non-boolean values ("follow"/
	// "nofollow"), but the per-mount `redirect_dir=on,metacopy=on` options we pass
	// in MountOverlay override the module default. So a present param file proves
	// the running overlayfs was compiled with the feature; the actual mount (whose
	// error is wrapped with composefsFallbackHint) is the real capability gate.
	if !overlayParamPresent("redirect_dir") {
		return fmt.Errorf("composefs LayerStore requires overlayfs redirect_dir support (/sys/module/overlay/parameters/redirect_dir absent; kernel overlayfs lacks redirect_dir)%s", composefsFallbackHint)
	}
	if !overlayParamPresent("metacopy") {
		return fmt.Errorf("composefs LayerStore requires overlayfs metacopy support (/sys/module/overlay/parameters/metacopy absent; kernel overlayfs lacks metacopy)%s", composefsFallbackHint)
	}
	return nil
}

// fsRegistered reports whether name appears as a registered filesystem in
// /proc/filesystems.
func fsRegistered(name string) bool {
	b, err := os.ReadFile("/proc/filesystems")
	if err != nil {
		return false
	}
	for _, line := range strings.Split(string(b), "\n") {
		for _, field := range strings.Fields(line) {
			if field == name {
				return true
			}
		}
	}
	return false
}

// overlayParamPresent reports whether the overlay module exposes the named
// parameter file at all. Presence means the running overlayfs was compiled with
// the feature; the per-mount option (redirect_dir=on / metacopy=on) then enables
// it regardless of the module-default value. We deliberately do NOT check the
// value: the default is "N" on stock kernels and the param is not strictly
// boolean (redirect_dir accepts follow/nofollow), so a value check is a
// false-negative on otherwise-capable kernels.
func overlayParamPresent(name string) bool {
	_, err := os.Stat(filepath.Join("/sys/module/overlay/parameters", name))
	return err == nil
}

// composefsVeritySupported is a conservative, fail-open probe for fs-verity
// enforcement capability. It requires the fsverity tool AND that the filesystem
// actually backing objectsDir is one that supports fs-verity (ext4/btrfs/f2fs)
// AND that the kernel advertises the verity feature for that filesystem type.
// Crucially it inspects the FS under objectsDir specifically — not generic sysfs
// feature files — so a store on tmpfs/overlay/Docker overlay2 returns false. When
// in doubt it returns false so MountOverlay mounts unauthenticated; even a false
// positive is harmless because MountOverlay retries without verity on any error.
func composefsVeritySupported(objectsDir string) bool {
	if runtime.GOOS != "linux" {
		return false
	}
	if _, err := exec.LookPath("fsverity"); err != nil {
		return false
	}
	fstype := backingFSType(objectsDir)
	switch fstype {
	case "ext4", "btrfs", "f2fs":
		if _, err := os.Stat(filepath.Join("/sys/fs", fstype, "features", "verity")); err == nil {
			return true
		}
	}
	return false
}

// backingFSType returns the filesystem type that backs path by finding the
// longest mount-point prefix in /proc/self/mountinfo. Returns "" if it cannot be
// determined (e.g. no /proc on non-Linux hosts).
func backingFSType(path string) string {
	abs, err := filepath.Abs(path)
	if err != nil {
		abs = path
	}
	b, err := os.ReadFile("/proc/self/mountinfo")
	if err != nil {
		return ""
	}
	bestLen := -1
	bestType := ""
	for _, line := range strings.Split(string(b), "\n") {
		// Format: ... <mountpoint(field5)> ... " - " <fstype> <source> <opts>
		sep := strings.SplitN(line, " - ", 2)
		if len(sep) != 2 {
			continue
		}
		left := strings.Fields(sep[0])
		right := strings.Fields(sep[1])
		if len(left) < 5 || len(right) < 1 {
			continue
		}
		mp := left[4]
		if !mountContains(mp, abs) {
			continue
		}
		if len(mp) > bestLen {
			bestLen = len(mp)
			bestType = right[0]
		}
	}
	return bestType
}

// mountContains reports whether mount point mp contains the absolute path abs.
func mountContains(mp, abs string) bool {
	if abs == mp {
		return true
	}
	if mp == "/" {
		return strings.HasPrefix(abs, "/")
	}
	return strings.HasPrefix(abs, mp+"/")
}
