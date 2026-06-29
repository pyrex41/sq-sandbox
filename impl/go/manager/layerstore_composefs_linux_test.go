//go:build linux

package manager

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"squashd/config"
)

// TestProbeComposefsSupportFailClosed asserts that when the running kernel does
// NOT support the composefs assembly path, the probe returns an actionable error
// pointing the operator at the squashfs fallback (never a silent success).
func TestProbeComposefsSupportFailClosed(t *testing.T) {
	err := probeComposefsSupport()
	if err == nil {
		t.Skip("kernel supports composefs (erofs + overlay redirect_dir/metacopy); nothing to assert")
	}
	if !strings.Contains(err.Error(), "SQUASH_LAYER_BACKEND=squashfs") {
		t.Fatalf("probe error must steer to the squashfs fallback, got: %v", err)
	}
}

// TestComposefsMountLayerGatedOnSupport asserts that on a kernel lacking support,
// MountLayer refuses (returns the support error) rather than attempting a mount.
func TestComposefsMountLayerGatedOnSupport(t *testing.T) {
	if composefsSupported() == nil {
		t.Skip("kernel supports composefs; cannot assert the gated-refusal path here")
	}
	c := &composefsLayerStore{cfg: &config.Config{DataDir: t.TempDir()}}
	err := c.MountLayer(filepath.Join(t.TempDir(), "nope.cfs"), t.TempDir())
	if err == nil {
		t.Fatal("expected MountLayer to refuse on an unsupported kernel")
	}
}

// TestResolveComposefsImagePrefersCfsSibling verifies the .squashfs→.cfs bridge.
func TestResolveComposefsImagePrefersCfsSibling(t *testing.T) {
	dir := t.TempDir()
	sq := filepath.Join(dir, "100-python.squashfs")
	cfs := filepath.Join(dir, "100-python.cfs")
	if err := os.WriteFile(cfs, []byte("erofs"), 0644); err != nil {
		t.Fatalf("write cfs: %v", err)
	}
	if got := resolveComposefsImage(sq); got != cfs {
		t.Fatalf("resolveComposefsImage = %q, want %q", got, cfs)
	}
	// No sibling → path returned as-is.
	other := filepath.Join(dir, "200-node.squashfs")
	if got := resolveComposefsImage(other); got != other {
		t.Fatalf("resolveComposefsImage without sibling = %q, want %q", got, other)
	}
}

// TestComposefsMountRoundTripPrivileged exercises the one path the other tests
// cannot reach without a real kernel: EROFS layer mount -> overlay assembly with
// the shared objects store as a data-only lower -> write-through to the upper ->
// mkcomposefs snapshot of the upper. It SKIPS only on clearly-environmental
// conditions (not root, kernel without composefs support, tools absent); once it
// commits to mounting, any failure is a real FAIL — we never skip past a broken
// mount, which would silently mask a regression.
//
// Run it via the validation path: test/composefs-validate.sh (or the
// test/Dockerfile.composefs image) on a kernel >= 6.12.
func TestComposefsMountRoundTripPrivileged(t *testing.T) {
	if os.Geteuid() != 0 {
		t.Skip("requires root to mount erofs + overlay")
	}
	if err := composefsSupported(); err != nil {
		t.Skipf("kernel lacks composefs support: %v", err)
	}
	mkcomposefs, err := exec.LookPath("mkcomposefs")
	if err != nil {
		t.Skipf("mkcomposefs unavailable: %v", err)
	}
	if _, err := exec.LookPath("composefs-info"); err != nil {
		t.Skipf("composefs-info unavailable: %v", err)
	}

	const payload = "composefs round trip\n"
	dataDir := t.TempDir()
	c := &composefsLayerStore{cfg: &config.Config{DataDir: dataDir}}
	objects := filepath.Join(dataDir, "cfs", "objects")
	if err := os.MkdirAll(objects, 0755); err != nil {
		t.Fatalf("mkdir objects: %v", err)
	}

	// Pack a source tree into a per-layer EROFS image, writing file data into the
	// shared content-addressed objects store.
	src := t.TempDir()
	if err := os.WriteFile(filepath.Join(src, "hello.txt"), []byte(payload), 0644); err != nil {
		t.Fatalf("write src: %v", err)
	}
	img := filepath.Join(dataDir, "100-layer.cfs")
	if out, err := exec.Command(mkcomposefs, "--digest-store="+objects, "--use-epoch", src, img).CombinedOutput(); err != nil {
		t.Fatalf("mkcomposefs build: %v: %s", err, strings.TrimSpace(string(out)))
	}

	// Mount the layer as read-only EROFS.
	layerMP := filepath.Join(dataDir, "images", "100-layer")
	if err := os.MkdirAll(layerMP, 0755); err != nil {
		t.Fatalf("mkdir layer mp: %v", err)
	}
	if err := c.MountLayer(img, layerMP); err != nil {
		t.Fatalf("MountLayer: %v", err)
	}
	t.Cleanup(func() { _ = exec.Command("umount", "-l", layerMP).Run() })

	// File data must resolve through the EROFS metadata into the objects store.
	if b, err := os.ReadFile(filepath.Join(layerMP, "hello.txt")); err != nil || string(b) != payload {
		t.Fatalf("read through erofs layer: got %q err=%v", string(b), err)
	}

	// Assemble the overlay: the layer mountpoint as a normal lower, the objects
	// store appended as a data-only ('::') lower, redirect_dir/metacopy on.
	//
	// The upperdir/workdir MUST live on a filesystem overlayfs accepts as an upper
	// (ext4/xfs/tmpfs/btrfs) — NOT on an overlayfs itself. On a container rootfs
	// that is already overlayfs (Docker, Fly), t.TempDir() would put the upper on
	// overlayfs and the mount fails with "not supported as upperdir". The real
	// daemon sidesteps this via SQUASH_UPPER_BACKEND (tmpfs default); we mirror
	// that here by mounting a dedicated tmpfs for the upper+work.
	upperBase := filepath.Join(dataDir, "upperfs")
	if err := os.MkdirAll(upperBase, 0755); err != nil {
		t.Fatalf("mkdir upperfs: %v", err)
	}
	if out, err := exec.Command("mount", "-t", "tmpfs", "tmpfs", upperBase).CombinedOutput(); err != nil {
		t.Skipf("cannot mount tmpfs for overlay upper (need it on a non-overlayfs): %v: %s", err, strings.TrimSpace(string(out)))
	}
	t.Cleanup(func() { _ = exec.Command("umount", "-l", upperBase).Run() })
	upper := filepath.Join(upperBase, "upper")
	work := filepath.Join(upperBase, "work")
	merged := filepath.Join(dataDir, "merged")
	for _, d := range []string{upper, work, merged} {
		if err := os.MkdirAll(d, 0755); err != nil {
			t.Fatalf("mkdir %s: %v", d, err)
		}
	}
	if err := c.MountOverlay([]string{layerMP}, upper, work, merged); err != nil {
		t.Fatalf("MountOverlay: %v", err)
	}
	t.Cleanup(func() { _ = exec.Command("umount", "-l", merged).Run() })

	// The layer file is visible in the merged view, and writes land in the upper.
	if b, err := os.ReadFile(filepath.Join(merged, "hello.txt")); err != nil || string(b) != payload {
		t.Fatalf("layer file not visible in overlay merged: got %q err=%v", string(b), err)
	}
	if err := os.WriteFile(filepath.Join(merged, "new.txt"), []byte("written"), 0644); err != nil {
		t.Fatalf("write through overlay: %v", err)
	}
	if _, err := os.Stat(filepath.Join(upper, "new.txt")); err != nil {
		t.Fatalf("upper write did not land in upperdir: %v", err)
	}

	// Snapshot the upper into a new EROFS image + objects, and prove it is a valid
	// composefs image (composefs-info parses it and lists referenced objects).
	snap := filepath.Join(dataDir, "900-snap.cfs")
	if err := c.Snapshot(upper, snap); err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	if out, err := exec.Command("composefs-info", "objects", snap).CombinedOutput(); err != nil {
		t.Fatalf("snapshot is not a valid composefs image: %v: %s", err, strings.TrimSpace(string(out)))
	}
}

// TestGCObjectsLiveSetFromImages is a Linux-gated mark-sweep test that runs only
// when composefs-info + mkcomposefs are installed: it builds a real EROFS image
// from a tree, then asserts GC keeps the objects that image references and sweeps
// an injected orphan.
func TestGCObjectsLiveSetFromImages(t *testing.T) {
	if _, err := os.Stat("/proc/filesystems"); err != nil {
		t.Skip("no /proc")
	}
	mkcomposefs, err := exec.LookPath("mkcomposefs")
	if err != nil {
		t.Skipf("mkcomposefs unavailable: %v", err)
	}
	if _, err := exec.LookPath("composefs-info"); err != nil {
		t.Skipf("composefs-info unavailable: %v", err)
	}

	dataDir := t.TempDir()
	objects := filepath.Join(dataDir, "cfs", "objects")
	if err := os.MkdirAll(objects, 0755); err != nil {
		t.Fatalf("mkdir objects: %v", err)
	}
	modules := filepath.Join(dataDir, "modules")
	if err := os.MkdirAll(modules, 0755); err != nil {
		t.Fatalf("mkdir modules: %v", err)
	}

	// Build a small rootfs and pack it into an EROFS image, writing data into the
	// shared objects store.
	src := t.TempDir()
	if err := os.WriteFile(filepath.Join(src, "hello.txt"), []byte(strings.Repeat("x", 4096)), 0644); err != nil {
		t.Fatalf("write src file: %v", err)
	}
	out := filepath.Join(modules, "100-test.cfs")
	if buildOut, err := exec.Command(mkcomposefs, "--digest-store="+objects, "--use-epoch", src, out).CombinedOutput(); err != nil {
		t.Skipf("mkcomposefs build failed (environment lacks support): %v: %s", err, strings.TrimSpace(string(buildOut)))
	}

	// Inject an orphan object that no image references.
	mustWriteObject(t, objects, "ff/orphan", "dead")

	m := New(&config.Config{DataDir: dataDir})
	res, err := m.GCObjects(GCOptions{})
	if err != nil {
		t.Fatalf("GCObjects: %v", err)
	}
	if res.SweptObjects < 1 {
		t.Fatalf("expected at least the orphan to be swept, got %+v", res)
	}
	if _, err := os.Stat(filepath.Join(objects, "ff", "orphan")); !os.IsNotExist(err) {
		t.Fatalf("orphan object should have been swept")
	}
	// The image's referenced objects must survive.
	if res.LiveObjects < 1 {
		t.Fatalf("expected the EROFS image to reference >=1 live object, got %+v", res)
	}
}
