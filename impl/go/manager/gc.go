package manager

import (
	"fmt"
	"io/fs"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"squashd/config"
)

// ── composefs objects-store garbage collection ───────────────────────────────
//
// The composefs LayerStore writes file DATA into a single, daemon-global,
// content-addressed objects store (DataDir/cfs/objects) shared by every layer
// and every sandbox. Each EROFS (.cfs) image holds only metadata + redirect
// xattrs pointing into that store, so an object is "live" iff at least one
// reachable .cfs image references it. Because the store is shared, objects must
// NOT be deleted per-sandbox — that would corrupt other sandboxes referencing
// the same content. GCObjects performs a conservative mark-and-sweep instead.
//
// Safety invariants (in order of importance):
//  1. Never delete an object that any reachable .cfs image references. The live
//     set is the UNION of referenced objects across all module images and all
//     per-sandbox snapshot images, enumerated from disk so both active and idle
//     sandboxes are covered.
//  2. Never sweep while a sandbox is mid-create/destroy: it may be writing new
//     objects (via mkcomposefs) whose owning image is not yet persisted and thus
//     not yet in the live set.
//  3. If ANY image's referenced-object set cannot be determined, abort the whole
//     sweep rather than risk deleting a live object (fail closed).
//  4. S3 object deletion is GATED behind a fully successful local sweep.

// GCOptions configures an objects-store mark-and-sweep.
type GCOptions struct {
	// DryRun computes and reports the sweep plan without deleting anything.
	DryRun bool
	// IncludeS3, when set and an S3 bucket is configured, also deletes the swept
	// objects from S3 — but only after a fully successful local sweep (gated).
	IncludeS3 bool
	// AssumeSingleHost asserts that this daemon owns the S3 bucket exclusively and
	// holds a COMPLETE local mirror of every module/snapshot image referenced in
	// the bucket. It is REQUIRED to enable IncludeS3 (see GCObjects): the live set
	// is computed from local disk only, so deleting from the shared, multi-host S3
	// store on the basis of a partial local view would orphan objects referenced
	// by images that exist only in the bucket or on another host. Setting this on
	// a multi-host deployment is unsafe.
	AssumeSingleHost bool
}

// GCResult summarizes a mark-and-sweep run.
type GCResult struct {
	StoredObjects int      `json:"stored_objects"`
	LiveObjects   int      `json:"live_objects"`
	SweptObjects  int      `json:"swept_objects"`
	BytesFreed    int64    `json:"bytes_freed"`
	DryRun        bool     `json:"dry_run"`
	Swept         []string `json:"swept,omitempty"`
}

// GCObjects runs a conservative mark-and-sweep over the shared composefs objects
// store. It is a no-op (empty result, no error) when no objects store exists,
// e.g. on squashfs-only deployments.
func (m *Manager) GCObjects(opts GCOptions) (GCResult, error) {
	objectsDir := filepath.Join(m.cfg.DataDir, "cfs", "objects")
	if fi, err := os.Stat(objectsDir); err != nil || !fi.IsDir() {
		// No composefs objects store → nothing to collect.
		return GCResult{DryRun: opts.DryRun}, nil
	}

	// Invariant (4+): S3 deletion is unsafe from a partial local view. The live set
	// below is computed from THIS host's local mirror; S3 is a shared, multi-host
	// store with lazy pulls, so a module/snapshot that lives only in the bucket (or
	// on another host) is invisible here and its objects would look dead. Refuse S3
	// GC unless the operator explicitly asserts a single-host, complete-mirror
	// deployment. (A proper fix builds the live set from the S3 inventory itself.)
	if opts.IncludeS3 && m.cfg.S3Bucket != "" && !opts.AssumeSingleHost {
		return GCResult{}, fmt.Errorf("refusing S3 object GC: the live set is computed from this host's local mirror only, but the S3 bucket is a shared multi-host store with lazy pulls; deleting from it on a partial local view can orphan objects still referenced by images that exist only in the bucket or on another host. Re-run with the single-host assertion (assume_single_host) only if this deployment is provably single-host with a complete local mirror")
	}

	// Serialize against all shared-store WRITERS (Snapshot's mkcomposefs) for the
	// ENTIRE mark+sweep, not just an initial check: this closes the race where an
	// object written mid-sweep — whose owning image is not yet persisted/enumerable
	// — is observed as dead and deleted (invariant 2). Snapshot holds this for read.
	m.objWriteMu.Lock()
	defer m.objWriteMu.Unlock()

	// Invariant (2): refuse to sweep while any sandbox is in a transitional state.
	m.mu.Lock()
	for id, s := range m.sandboxes {
		st := sandboxState(s)
		if st == "creating" || st == "destroying" {
			m.mu.Unlock()
			return GCResult{}, fmt.Errorf("refusing to GC: sandbox %q is %s; retry when all sandboxes are idle", id, st)
		}
	}
	m.mu.Unlock()

	// Completeness cross-check (invariant 3, registry side): every image referenced
	// by a live composefs sandbox must be locally enumerable, else its objects look
	// dead. Fail closed rather than sweep on an incomplete view.
	if err := m.validateComposefsReferencesLocal(); err != nil {
		return GCResult{}, err
	}

	// MARK (invariant 1): union of referenced objects across every reachable image.
	images, err := m.composefsImages()
	if err != nil {
		return GCResult{}, err
	}
	perImage := make([][]string, 0, len(images))
	for _, img := range images {
		refs, err := composefsImageObjects(img)
		if err != nil {
			// Invariant (3): cannot prove what this image references → fail closed.
			return GCResult{}, fmt.Errorf("enumerate referenced objects for %s: %w", img, err)
		}
		perImage = append(perImage, refs)
	}
	live := computeLiveSet(perImage)

	// Enumerate stored objects.
	stored, sizes, err := walkObjectsStore(objectsDir)
	if err != nil {
		return GCResult{}, err
	}

	// SWEEP candidates = stored − live.
	sweep := unreferencedObjects(stored, live)

	res := GCResult{
		StoredObjects: len(stored),
		LiveObjects:   len(live),
		SweptObjects:  len(sweep),
		DryRun:        opts.DryRun,
		Swept:         sweep,
	}
	for _, k := range sweep {
		res.BytesFreed += sizes[k]
	}
	if opts.DryRun {
		return res, nil
	}

	// Delete locally. Track whether every removal succeeded so the S3 delete stays
	// gated on a clean local sweep (invariant 4).
	localOK := true
	for _, k := range sweep {
		if err := os.Remove(filepath.Join(objectsDir, k)); err != nil && !os.IsNotExist(err) {
			slog.Warn("gc: failed to remove object", "key", k, "err", err)
			localOK = false
		}
	}
	pruneEmptyObjectDirs(objectsDir)

	if opts.IncludeS3 && m.cfg.S3Bucket != "" {
		if !localOK {
			slog.Warn("gc: skipping S3 object deletion because the local sweep had errors")
		} else {
			for _, k := range sweep {
				s3DeleteObject(m.cfg, "cfs/objects/"+k)
			}
		}
	}
	return res, nil
}

// sandboxState returns a sandbox's state, treating a nil (reserved/creating)
// registry slot as "creating".
func sandboxState(s *Sandbox) string {
	if s == nil {
		return "creating"
	}
	return s.State
}

// composefsImages lists every EROFS image that can reference objects in the
// shared store: module layer images plus every per-sandbox snapshot image.
// Enumerated from disk (not the in-memory registry) so it is robust to drift and
// covers idle sandboxes.
//
// CRITICAL: module images are the content-addressed `.cfs` artifacts, but the
// manager writes per-sandbox SNAPSHOTS — including composefs EROFS ones — to
// "<label>.squashfs" (Manager.Snapshot), NOT "<label>.cfs". Filtering snapshots by
// a ".cfs" extension therefore misses every composefs snapshot and computes a live
// set with zero snapshot references, sweeping objects pinned only by a snapshot.
// We instead include EVERY snapshot file for sandboxes whose LayerStore is
// composefs (their snapshots are EROFS referencing the shared store), and skip
// non-composefs sandboxes entirely (their squashfs snapshots are self-contained
// and would make composefs-info fail-closed, aborting an otherwise valid GC).
func (m *Manager) composefsImages() ([]string, error) {
	var images []string

	modDir := m.cfg.ModulesDir()
	modEntries, err := os.ReadDir(modDir)
	if err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("read modules dir: %w", err)
	}
	for _, e := range modEntries {
		if !e.IsDir() && strings.HasSuffix(e.Name(), ".cfs") {
			images = append(images, filepath.Join(modDir, e.Name()))
		}
	}

	sbDir := m.cfg.SandboxesDir()
	sbEntries, err := os.ReadDir(sbDir)
	if err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("read sandboxes dir: %w", err)
	}
	for _, sb := range sbEntries {
		if !sb.IsDir() {
			continue
		}
		if sandboxLayerStoreOnDisk(filepath.Join(sbDir, sb.Name())) != "composefs" {
			continue
		}
		snapDir := filepath.Join(sbDir, sb.Name(), "snapshots")
		snaps, err := os.ReadDir(snapDir)
		if err != nil {
			continue
		}
		for _, sn := range snaps {
			if sn.IsDir() {
				continue
			}
			// Snapshot images are EROFS regardless of their ".squashfs" name; only
			// sidecar manifests are not images.
			if strings.HasSuffix(sn.Name(), ".manifest.json") {
				continue
			}
			images = append(images, filepath.Join(snapDir, sn.Name()))
		}
	}
	return images, nil
}

// sandboxLayerStoreOnDisk reads a sandbox's persisted LayerStore selection from
// its .meta/layer_store file, defaulting to "squashfs" (mirrors readMeta).
func sandboxLayerStoreOnDisk(sbDir string) string {
	b, _ := os.ReadFile(filepath.Join(sbDir, ".meta", "layer_store"))
	if ls := strings.TrimSpace(string(b)); ls != "" {
		return ls
	}
	return "squashfs"
}

// validateComposefsReferencesLocal fails closed if any image referenced by a live
// composefs sandbox is not locally enumerable. The manager only ever pulls
// "<layer>.squashfs" from S3 (never the "<layer>.cfs"), so a sandbox can be
// mounted/referenced while its content-addressed EROFS image is absent locally;
// such an image's objects would be missing from the live set and wrongly swept.
// Requiring the "<layer>.cfs" to be present both proves enumerability and matches
// exactly what composefsImages() marks.
func (m *Manager) validateComposefsReferencesLocal() error {
	type ref struct {
		id     string
		layers []string
	}
	var refs []ref
	m.mu.Lock()
	for id, s := range m.sandboxes {
		if s == nil || s.LayerStore != "composefs" {
			continue
		}
		s.mu.Lock()
		refs = append(refs, ref{id: id, layers: append([]string(nil), s.Layers...)})
		s.mu.Unlock()
	}
	m.mu.Unlock()

	for _, r := range refs {
		for _, layer := range r.layers {
			cfs := filepath.Join(m.cfg.ModulesDir(), layer+".cfs")
			if _, err := os.Stat(cfs); err != nil {
				return fmt.Errorf("refusing to GC: composefs sandbox %q references module %q but its EROFS image %s is missing/unreadable (%v); its referenced objects cannot be enumerated, so sweeping could delete live objects", r.id, layer, cfs, err)
			}
		}
	}
	return nil
}

// composefsImageObjects returns the relative object keys an EROFS image
// references in the shared objects store, via `composefs-info objects`.
func composefsImageObjects(image string) ([]string, error) {
	out, err := exec.Command("composefs-info", "objects", image).Output()
	if err != nil {
		return nil, fmt.Errorf("composefs-info objects %s: %w", image, err)
	}
	return parseComposefsObjects(string(out)), nil
}

// ── Pure helpers (no I/O — table-tested without mounts) ───────────────────────

// parseComposefsObjects parses the output of `composefs-info objects`, one
// relative object key per line (e.g. "ab/cdef…"). Leading "./" or "/" is
// trimmed and blank lines are skipped.
func parseComposefsObjects(out string) []string {
	var keys []string
	for _, line := range strings.Split(out, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		line = strings.TrimPrefix(line, "./")
		line = strings.TrimPrefix(line, "/")
		if line != "" {
			keys = append(keys, line)
		}
	}
	return keys
}

// computeLiveSet unions per-image referenced-object lists into the live set.
func computeLiveSet(perImage [][]string) map[string]struct{} {
	live := make(map[string]struct{})
	for _, refs := range perImage {
		for _, k := range refs {
			if k != "" {
				live[k] = struct{}{}
			}
		}
	}
	return live
}

// unreferencedObjects returns the stored keys absent from the live set, sorted.
func unreferencedObjects(stored []string, live map[string]struct{}) []string {
	var out []string
	for _, k := range stored {
		if _, ok := live[k]; !ok {
			out = append(out, k)
		}
	}
	sort.Strings(out)
	return out
}

// ── Object-store I/O ──────────────────────────────────────────────────────────

// walkObjectsStore returns the relative keys of all stored objects plus their
// sizes. Keys use forward slashes regardless of host OS.
func walkObjectsStore(objectsDir string) ([]string, map[string]int64, error) {
	var keys []string
	sizes := make(map[string]int64)
	err := filepath.WalkDir(objectsDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(objectsDir, path)
		if err != nil {
			return err
		}
		rel = filepath.ToSlash(rel)
		keys = append(keys, rel)
		if info, err := d.Info(); err == nil {
			sizes[rel] = info.Size()
		}
		return nil
	})
	if err != nil {
		return nil, nil, fmt.Errorf("walk objects store: %w", err)
	}
	return keys, sizes, nil
}

// pruneEmptyObjectDirs removes now-empty fan-out subdirectories (the store uses a
// two-level XX/rest layout). The root is always preserved.
func pruneEmptyObjectDirs(root string) {
	entries, err := os.ReadDir(root)
	if err != nil {
		return
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		sub := filepath.Join(root, e.Name())
		if inner, err := os.ReadDir(sub); err == nil && len(inner) == 0 {
			_ = os.Remove(sub)
		}
	}
}

// s3DeleteObject removes a single object key from S3 (best-effort). Only ever
// called after a successful local sweep.
func s3DeleteObject(cfg *config.Config, s3Key string) {
	if cfg.S3Bucket == "" {
		return
	}
	env := append(os.Environ(),
		"SQUASH_S3_BUCKET="+cfg.S3Bucket,
		"SQUASH_S3_REGION="+cfg.S3Region,
	)
	if cfg.S3Endpoint != "" {
		env = append(env, "SQUASH_S3_ENDPOINT="+cfg.S3Endpoint)
	}
	if cfg.S3Prefix != "" {
		env = append(env, "SQUASH_S3_PREFIX="+cfg.S3Prefix)
	}
	cmd := exec.Command("sq-s3", "rm", s3Key)
	cmd.Env = env
	if out, err := cmd.CombinedOutput(); err != nil {
		slog.Warn("gc: sq-s3 rm failed", "key", s3Key, "err", err, "out", strings.TrimSpace(string(out)))
	}
}
