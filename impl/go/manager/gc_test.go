package manager

import (
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"squashd/config"
)

func TestParseComposefsObjects(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want []string
	}{
		{"empty", "", nil},
		{"blank lines", "\n\n  \n", nil},
		{"simple", "ab/cdef\n12/3456\n", []string{"ab/cdef", "12/3456"}},
		{"trim whitespace", "  ab/cdef  \n\t12/3456\n", []string{"ab/cdef", "12/3456"}},
		{"strip leading slash", "/ab/cdef\n./12/3456\n", []string{"ab/cdef", "12/3456"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := parseComposefsObjects(tc.in)
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("parseComposefsObjects(%q) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

func TestComputeLiveSet(t *testing.T) {
	cases := []struct {
		name     string
		perImage [][]string
		want     []string // sorted keys expected present
	}{
		{"none", nil, nil},
		{"single image", [][]string{{"a", "b"}}, []string{"a", "b"}},
		{
			"union dedups overlap",
			[][]string{{"a", "b"}, {"b", "c"}, {"c", "d"}},
			[]string{"a", "b", "c", "d"},
		},
		{"skips empty keys", [][]string{{"a", "", "b"}}, []string{"a", "b"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			live := computeLiveSet(tc.perImage)
			if len(live) != len(tc.want) {
				t.Fatalf("live set size = %d, want %d (%v)", len(live), len(tc.want), live)
			}
			for _, k := range tc.want {
				if _, ok := live[k]; !ok {
					t.Fatalf("live set missing %q (%v)", k, live)
				}
			}
		})
	}
}

func TestUnreferencedObjects(t *testing.T) {
	cases := []struct {
		name   string
		stored []string
		live   []string
		want   []string
	}{
		{"all live", []string{"a", "b"}, []string{"a", "b"}, nil},
		{"all dead", []string{"a", "b"}, nil, []string{"a", "b"}},
		{"mixed sorted", []string{"c", "a", "b"}, []string{"b"}, []string{"a", "c"}},
		{"live superset", []string{"a"}, []string{"a", "b", "c"}, nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			live := make(map[string]struct{}, len(tc.live))
			for _, k := range tc.live {
				live[k] = struct{}{}
			}
			got := unreferencedObjects(tc.stored, live)
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("unreferencedObjects = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestWalkObjectsStore(t *testing.T) {
	dir := t.TempDir()
	mustWriteObject(t, dir, "ab/cdef", "hello")
	mustWriteObject(t, dir, "12/3456", "world!!")

	keys, sizes, err := walkObjectsStore(dir)
	if err != nil {
		t.Fatalf("walkObjectsStore: %v", err)
	}
	if len(keys) != 2 {
		t.Fatalf("keys = %v, want 2 entries", keys)
	}
	if sizes["ab/cdef"] != 5 || sizes["12/3456"] != 7 {
		t.Fatalf("sizes = %v", sizes)
	}
}

func TestPruneEmptyObjectDirs(t *testing.T) {
	dir := t.TempDir()
	mustWriteObject(t, dir, "ab/cdef", "x")
	// Remove the file, leaving an empty fan-out dir.
	if err := os.Remove(filepath.Join(dir, "ab", "cdef")); err != nil {
		t.Fatalf("remove: %v", err)
	}
	pruneEmptyObjectDirs(dir)
	if _, err := os.Stat(filepath.Join(dir, "ab")); !os.IsNotExist(err) {
		t.Fatalf("expected empty fan-out dir to be pruned, stat err = %v", err)
	}
}

func TestGCObjectsNoStore(t *testing.T) {
	m := New(&config.Config{DataDir: t.TempDir()})
	res, err := m.GCObjects(GCOptions{})
	if err != nil {
		t.Fatalf("GCObjects with no store: %v", err)
	}
	if res.StoredObjects != 0 || res.SweptObjects != 0 {
		t.Fatalf("expected empty result, got %+v", res)
	}
}

func TestGCObjectsRefusesTransitionalSandbox(t *testing.T) {
	dir := t.TempDir()
	// Objects store must exist for the guard to be reached.
	if err := os.MkdirAll(filepath.Join(dir, "cfs", "objects"), 0755); err != nil {
		t.Fatalf("mkdir objects: %v", err)
	}
	m := New(&config.Config{DataDir: dir})
	m.sandboxes["pending"] = nil // nil slot = creating sentinel

	if _, err := m.GCObjects(GCOptions{}); err == nil {
		t.Fatal("expected GCObjects to refuse while a sandbox is creating")
	}
}

// TestGCObjectsDryRunSweepsUnreferenced exercises the full mark-and-sweep on
// darwin: with no .cfs images present the live set is empty, so every stored
// object is unreferenced. DryRun must report them without deleting.
func TestGCObjectsDryRunSweepsUnreferenced(t *testing.T) {
	dir := t.TempDir()
	objects := filepath.Join(dir, "cfs", "objects")
	mustWriteObject(t, objects, "ab/cdef", "abcde")
	mustWriteObject(t, objects, "12/3456", "xyz")

	m := New(&config.Config{DataDir: dir})
	res, err := m.GCObjects(GCOptions{DryRun: true})
	if err != nil {
		t.Fatalf("GCObjects dry-run: %v", err)
	}
	if res.StoredObjects != 2 || res.LiveObjects != 0 || res.SweptObjects != 2 {
		t.Fatalf("unexpected dry-run result: %+v", res)
	}
	if res.BytesFreed != 8 {
		t.Fatalf("BytesFreed = %d, want 8", res.BytesFreed)
	}
	// Dry run must not delete anything.
	if _, err := os.Stat(filepath.Join(objects, "ab", "cdef")); err != nil {
		t.Fatalf("dry-run deleted an object: %v", err)
	}
}

func TestGCObjectsDeletesUnreferenced(t *testing.T) {
	dir := t.TempDir()
	objects := filepath.Join(dir, "cfs", "objects")
	mustWriteObject(t, objects, "ab/cdef", "abcde")

	m := New(&config.Config{DataDir: dir})
	res, err := m.GCObjects(GCOptions{})
	if err != nil {
		t.Fatalf("GCObjects: %v", err)
	}
	if res.SweptObjects != 1 {
		t.Fatalf("SweptObjects = %d, want 1", res.SweptObjects)
	}
	if _, err := os.Stat(filepath.Join(objects, "ab", "cdef")); !os.IsNotExist(err) {
		t.Fatalf("expected object to be deleted, stat err = %v", err)
	}
}

// TestComposefsImagesEnumeratesSquashfsNamedSnapshots is the regression guard for
// the live-set miss: the manager writes composefs snapshots to "<label>.squashfs"
// (EROFS content), so enumeration MUST include them despite the extension, or GC
// would compute a live set with zero snapshot references and sweep pinned objects.
func TestComposefsImagesEnumeratesSquashfsNamedSnapshots(t *testing.T) {
	dir := t.TempDir()
	cfg := &config.Config{DataDir: dir}
	m := New(cfg)

	// A composefs sandbox with a snapshot written under the ".squashfs" name.
	sbDir := filepath.Join(cfg.SandboxesDir(), "sb-cfs")
	if err := os.MkdirAll(filepath.Join(sbDir, ".meta"), 0755); err != nil {
		t.Fatalf("mkdir sb meta: %v", err)
	}
	if err := os.WriteFile(filepath.Join(sbDir, ".meta", "layer_store"), []byte("composefs"), 0644); err != nil {
		t.Fatalf("write layer_store: %v", err)
	}
	snapDir := filepath.Join(sbDir, "snapshots")
	if err := os.MkdirAll(snapDir, 0755); err != nil {
		t.Fatalf("mkdir snapshots: %v", err)
	}
	cfsSnap := filepath.Join(snapDir, "v1.squashfs")
	if err := os.WriteFile(cfsSnap, []byte("erofs"), 0644); err != nil {
		t.Fatalf("write snap: %v", err)
	}
	// A manifest sidecar must be ignored.
	if err := os.WriteFile(filepath.Join(snapDir, "v1.squashfs.manifest.json"), []byte("{}"), 0644); err != nil {
		t.Fatalf("write manifest: %v", err)
	}

	// A squashfs sandbox's snapshot must NOT be enumerated (it would make
	// composefs-info fail-closed and abort an otherwise-valid GC).
	sqDir := filepath.Join(cfg.SandboxesDir(), "sb-sq")
	if err := os.MkdirAll(filepath.Join(sqDir, ".meta"), 0755); err != nil {
		t.Fatalf("mkdir sq meta: %v", err)
	}
	if err := os.WriteFile(filepath.Join(sqDir, ".meta", "layer_store"), []byte("squashfs"), 0644); err != nil {
		t.Fatalf("write sq layer_store: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(sqDir, "snapshots"), 0755); err != nil {
		t.Fatalf("mkdir sq snapshots: %v", err)
	}
	if err := os.WriteFile(filepath.Join(sqDir, "snapshots", "v1.squashfs"), []byte("squashfs"), 0644); err != nil {
		t.Fatalf("write sq snap: %v", err)
	}

	images, err := m.composefsImages()
	if err != nil {
		t.Fatalf("composefsImages: %v", err)
	}
	var sawCfsSnap, sawSqSnap, sawManifest bool
	for _, img := range images {
		switch img {
		case cfsSnap:
			sawCfsSnap = true
		case filepath.Join(sqDir, "snapshots", "v1.squashfs"):
			sawSqSnap = true
		case filepath.Join(snapDir, "v1.squashfs.manifest.json"):
			sawManifest = true
		}
	}
	if !sawCfsSnap {
		t.Fatalf("composefs snapshot (.squashfs-named) was NOT enumerated; live set would miss its objects: %v", images)
	}
	if sawSqSnap {
		t.Fatalf("squashfs sandbox snapshot must not be enumerated as a composefs image: %v", images)
	}
	if sawManifest {
		t.Fatalf("manifest sidecar must not be enumerated as an image: %v", images)
	}
}

// TestGCObjectsRefusesS3WithoutSingleHost asserts IncludeS3 is gated behind the
// single-host assertion (the live set is computed from local disk only).
func TestGCObjectsRefusesS3WithoutSingleHost(t *testing.T) {
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "cfs", "objects"), 0755); err != nil {
		t.Fatalf("mkdir objects: %v", err)
	}
	m := New(&config.Config{DataDir: dir, S3Bucket: "bucket"})
	if _, err := m.GCObjects(GCOptions{IncludeS3: true}); err == nil {
		t.Fatal("expected GCObjects to refuse IncludeS3 without AssumeSingleHost")
	}
	// With the assertion it must get past the S3 gate (and succeed: no images).
	if _, err := m.GCObjects(GCOptions{IncludeS3: true, AssumeSingleHost: true, DryRun: true}); err != nil {
		t.Fatalf("GCObjects with AssumeSingleHost: %v", err)
	}
}

func mustWriteObject(t *testing.T, objectsDir, key, content string) {
	t.Helper()
	p := filepath.Join(objectsDir, filepath.FromSlash(key))
	if err := os.MkdirAll(filepath.Dir(p), 0755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(p, []byte(content), 0644); err != nil {
		t.Fatalf("write object: %v", err)
	}
}
