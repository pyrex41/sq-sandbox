package manager

import (
	"fmt"
	"os/exec"
	"strings"

	"squashd/config"
	sqexec "squashd/exec"
)

// squashfsLayerStore is the default LayerStore: a behavior-identical extraction
// of the existing squashfs/squashfuse RO-layer code paths. Every method is a
// thin pass-through to today's sqexec.* helpers, the buildLowerDirs logic, and
// the exact mksquashfs invocation — zero behavioral change for existing
// sandboxes. squashfuse (FUSE, zero loop devices) remains the portable default.
type squashfsLayerStore struct {
	cfg *config.Config
}

func (squashfsLayerStore) MountLayer(image, mountpoint string) error {
	return sqexec.MountLayer(image, mountpoint)
}

func (squashfsLayerStore) UnmountLayer(mountpoint string) error {
	return sqexec.UnmountLayer(mountpoint)
}

func (squashfsLayerStore) BuildLowerDirs(imagesDir string) ([]string, error) {
	return buildLowerDirs(imagesDir)
}

func (squashfsLayerStore) BuildLowerDirsExcluding(imagesDir, exclude string) ([]string, error) {
	return buildLowerDirsExcluding(imagesDir, exclude)
}

func (squashfsLayerStore) MountOverlay(lowerDirs []string, upper, work, merged string) error {
	return sqexec.MountOverlay(lowerDirs, upper, work, merged)
}

func (squashfsLayerStore) UnmountOverlay(merged string) error {
	return sqexec.UnmountOverlay(merged)
}

// Snapshot freezes upperDir into a squashfs image at outPath using the exact
// mksquashfs args the manager used inline (-comp gzip -b 256K -noappend -quiet).
// The per-sandbox lock + existence check stay in the manager (concurrency
// policy); this is a pure pack operation.
func (squashfsLayerStore) Snapshot(upperDir, outPath string) error {
	args := []string{"mksquashfs", upperDir, outPath,
		"-comp", "gzip", "-b", "256K", "-noappend", "-quiet"}
	if out, err := exec.Command(args[0], args[1:]...).CombinedOutput(); err != nil {
		return fmt.Errorf("mksquashfs: %w: %s", err, strings.TrimSpace(string(out)))
	}
	return nil
}
