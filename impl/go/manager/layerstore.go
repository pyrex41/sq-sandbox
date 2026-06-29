package manager

// LayerStore abstracts read-only (RO) layer assembly: how module/snapshot
// images are mounted, how the overlay lowerdir stack is assembled from them,
// and how a writable upper is frozen back into a new RO layer image.
//
// This seam is ORTHOGONAL to Backend (backend.go). Backend abstracts how a
// sandbox's payload is *executed* and memory-checkpointed (chroot/gvisor);
// LayerStore abstracts how its read-only filesystem layers are *assembled*
// (squashfs/squashfuse today, composefs/EROFS in a future stage). The two axes
// are selected independently — do not conflate s.Backend with s.LayerStore.
//
// The default implementation (squashfsLayerStore) is a behavior-identical
// pass-through to the existing sqexec.* mount helpers + mksquashfs, so existing
// sandboxes keep the exact current code path.
type LayerStore interface {
	// MountLayer mounts a single RO layer image at mountpoint.
	MountLayer(image, mountpoint string) error

	// UnmountLayer unmounts a single RO layer mountpoint.
	UnmountLayer(mountpoint string) error

	// BuildLowerDirs returns the overlay lowerdirs assembled from imagesDir,
	// ordered highest-priority first.
	BuildLowerDirs(imagesDir string) ([]string, error)

	// BuildLowerDirsExcluding is like BuildLowerDirs but skips a named entry
	// (used by Restore to place the snapshot layer on top explicitly).
	BuildLowerDirsExcluding(imagesDir, exclude string) ([]string, error)

	// MountOverlay mounts the writable overlay (lowerDirs RO + upper/work) at
	// merged. lowerDirs are listed highest-priority first.
	MountOverlay(lowerDirs []string, upper, work, merged string) error

	// UnmountOverlay unmounts the overlay at merged.
	UnmountOverlay(merged string) error

	// Snapshot freezes a writable upper directory into a new RO layer image at
	// outPath that MountLayer can later mount.
	Snapshot(upperDir, outPath string) error
}

// layerStoreFor returns the LayerStore implementation for a sandbox, keyed by
// the per-sandbox LayerStore field (stamped at create from SQUASH_LAYER_BACKEND,
// persisted in .meta/layer_store, defaulting to "squashfs"). Mirrors backendFor
// so the choice is per-sandbox, not global — existing squashfs sandboxes keep
// the exact current code path.
func (m *Manager) layerStoreFor(s *Sandbox) LayerStore {
	switch s.LayerStore {
	case "composefs":
		return &composefsLayerStore{cfg: m.cfg}
	default:
		return squashfsLayerStore{cfg: m.cfg}
	}
}
