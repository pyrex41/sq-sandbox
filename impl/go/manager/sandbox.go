package manager

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"time"
)

// Sandbox represents a running sandbox instance.
type Sandbox struct {
	mu sync.Mutex // protects per-sandbox mutable state (LastActiveAt, ExecCount)

	ID           string
	State        string // "creating", "ready", "destroying", "destroyed"
	Owner        string
	Task         string
	Layers       []string
	Backend      string // "chroot", "firecracker", "gvisor"
	CPU          float64
	MemoryMB     int
	MaxLifetimeS int
	AllowNet     []string
	Features     []string
	GUI          *GUIState
	CreatedAt    time.Time
	LastActiveAt time.Time
	Dir          string // abs path to sandbox directory
	ExecCount    int
	CID          int    // Firecracker vsock CID (0 if unused)

	// snapshotMounted tracks whether a snapshot squashfs is currently
	// loop-mounted at images/_snapshot (chroot backend only).
	snapshotMounted bool
}

// ToInfo converts a Sandbox to its JSON-serializable representation.
func (s *Sandbox) ToInfo() SandboxInfo {
	s.mu.Lock()
	defer s.mu.Unlock()
	var gui *GUIState
	if s.GUI != nil {
		copy := *s.GUI
		gui = &copy
	}
	features := append([]string(nil), s.Features...)
	return SandboxInfo{
		ID:           s.ID,
		State:        s.State,
		Owner:        s.Owner,
		Task:         s.Task,
		Layers:       s.Layers,
		Backend:      s.Backend,
		CreatedAt:    s.CreatedAt.UTC().Format(time.RFC3339),
		LastActiveAt: s.LastActiveAt.UTC().Format(time.RFC3339),
		CPULimit:     s.CPU,
		MemoryMB:     s.MemoryMB,
		MaxLifetimeS: s.MaxLifetimeS,
		AllowNet:     s.AllowNet,
		Features:     features,
		GUI:          gui,
	}
}

// mergedDir returns the overlay-merged path (root of the sandbox filesystem).
func (s *Sandbox) mergedDir() string { return filepath.Join(s.Dir, "merged") }

// upperDataDir returns the writable upper layer path.
func (s *Sandbox) upperDataDir() string { return filepath.Join(s.Dir, "upper", "data") }

// upperWorkDir returns the overlayfs work directory.
func (s *Sandbox) upperWorkDir() string { return filepath.Join(s.Dir, "upper", "work") }

// imagesDir returns the directory where squashfs layers are loop-mounted.
func (s *Sandbox) imagesDir() string { return filepath.Join(s.Dir, "images") }

// snapshotsDir returns the directory where squashfs snapshots are stored.
func (s *Sandbox) snapshotsDir() string { return filepath.Join(s.Dir, "snapshots") }

// metaDir returns the metadata directory.
func (s *Sandbox) metaDir() string { return filepath.Join(s.Dir, ".meta") }

// logDir returns the exec log directory.
func (s *Sandbox) logDir() string { return filepath.Join(s.Dir, ".meta", "log") }

// ── Disk I/O ──────────────────────────────────────────────────────────────────

// writeMeta writes all sandbox metadata to the .meta/ directory.
func writeMeta(sdir string, s *Sandbox) error {
	m := s.metaDir()
	writes := map[string]string{
		"owner":          s.Owner,
		"task":           s.Task,
		"layers":         strings.Join(s.Layers, "\n"),
		"created":        s.CreatedAt.UTC().Format(time.RFC3339),
		"last_active":    s.LastActiveAt.UTC().Format(time.RFC3339),
		"cpu":            strconv.FormatFloat(s.CPU, 'f', -1, 64),
		"memory_mb":      strconv.Itoa(s.MemoryMB),
		"max_lifetime_s": strconv.Itoa(s.MaxLifetimeS),
		"backend":        s.Backend,
	}
	for name, val := range writes {
		if err := os.WriteFile(filepath.Join(m, name), []byte(val), 0644); err != nil {
			return fmt.Errorf("write meta %s: %w", name, err)
		}
	}
	// allow_net as JSON array (or empty)
	if len(s.AllowNet) > 0 {
		b, _ := json.Marshal(s.AllowNet)
		if err := os.WriteFile(filepath.Join(m, "allow_net"), b, 0644); err != nil {
			return fmt.Errorf("write meta allow_net: %w", err)
		}
	}
	// features as JSON array
	if len(s.Features) > 0 {
		b, _ := json.Marshal(s.Features)
		if err := os.WriteFile(filepath.Join(m, "features"), b, 0644); err != nil {
			return fmt.Errorf("write meta features: %w", err)
		}
	} else {
		_ = os.Remove(filepath.Join(m, "features"))
	}
	// gui state as JSON object
	if s.GUI != nil {
		b, _ := json.Marshal(s.GUI)
		if err := os.WriteFile(filepath.Join(m, "gui"), b, 0644); err != nil {
			return fmt.Errorf("write meta gui: %w", err)
		}
	} else {
		_ = os.Remove(filepath.Join(m, "gui"))
	}
	return nil
}

// readMeta reconstructs a Sandbox from .meta/ files on disk.
// Used during startup recovery.
func readMeta(id, sdir string) (*Sandbox, error) {
	m := filepath.Join(sdir, ".meta")

	readFile := func(name string) string {
		b, _ := os.ReadFile(filepath.Join(m, name))
		return strings.TrimSpace(string(b))
	}

	s := &Sandbox{
		ID:      id,
		State:   "ready",
		Dir:     sdir,
		Owner:   readFile("owner"),
		Task:    readFile("task"),
		Backend: readFile("backend"),
	}
	if s.Backend == "" {
		s.Backend = "chroot"
	}

	// Layers: newline-separated
	layersRaw := readFile("layers")
	if layersRaw != "" {
		for _, l := range strings.Split(layersRaw, "\n") {
			l = strings.TrimSpace(l)
			if l != "" {
				s.Layers = append(s.Layers, l)
			}
		}
	}

	// Numeric fields
	if v := readFile("cpu"); v != "" {
		s.CPU, _ = strconv.ParseFloat(v, 64)
	}
	if s.CPU == 0 {
		s.CPU = 2
	}
	if v := readFile("memory_mb"); v != "" {
		s.MemoryMB, _ = strconv.Atoi(v)
	}
	if s.MemoryMB == 0 {
		s.MemoryMB = 1024
	}
	if v := readFile("max_lifetime_s"); v != "" {
		s.MaxLifetimeS, _ = strconv.Atoi(v)
	}
	if v := readFile("cid"); v != "" {
		s.CID, _ = strconv.Atoi(v)
	}

	// Timestamps
	if v := readFile("created"); v != "" {
		s.CreatedAt, _ = time.Parse(time.RFC3339, v)
	}
	if s.CreatedAt.IsZero() {
		s.CreatedAt = time.Now()
	}
	if v := readFile("last_active"); v != "" {
		s.LastActiveAt, _ = time.Parse(time.RFC3339, v)
	}
	if s.LastActiveAt.IsZero() {
		s.LastActiveAt = s.CreatedAt
	}

	// allow_net: JSON array
	if b, err := os.ReadFile(filepath.Join(m, "allow_net")); err == nil {
		json.Unmarshal(b, &s.AllowNet)
	}

	// features: JSON array
	if b, err := os.ReadFile(filepath.Join(m, "features")); err == nil {
		json.Unmarshal(b, &s.Features)
	}

	// gui: JSON object
	if b, err := os.ReadFile(filepath.Join(m, "gui")); err == nil {
		gs := &GUIState{}
		if json.Unmarshal(b, gs) == nil {
			s.GUI = gs
		}
	}

	// ExecCount: count log files
	entries, _ := os.ReadDir(filepath.Join(m, "log"))
	s.ExecCount = len(entries)

	return s, nil
}

// updateLastActive writes the current timestamp to .meta/last_active.
func (s *Sandbox) updateLastActive() {
	s.mu.Lock()
	s.LastActiveAt = time.Now()
	t := s.LastActiveAt
	s.mu.Unlock()
	_ = os.WriteFile(
		filepath.Join(s.metaDir(), "last_active"),
		[]byte(t.UTC().Format(time.RFC3339)),
		0644,
	)
}

// nextSeq increments and returns the next exec sequence number.
func (s *Sandbox) nextSeq() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.ExecCount++
	return s.ExecCount
}

// writeExecLog appends an exec result to .meta/log/NNNN.json.
func writeExecLog(logDir string, result *ExecResult) {
	if err := os.MkdirAll(logDir, 0755); err != nil {
		return
	}
	b, err := json.Marshal(result)
	if err != nil {
		return
	}
	path := filepath.Join(logDir, fmt.Sprintf("%04d.json", result.Seq))
	_ = os.WriteFile(path, b, 0644)
}

// readExecLogs returns all exec log entries sorted by sequence number.
func readExecLogs(logDir string) ([]ExecResult, error) {
	entries, err := os.ReadDir(logDir)
	if os.IsNotExist(err) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	var results []ExecResult
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".json") {
			continue
		}
		b, err := os.ReadFile(filepath.Join(logDir, e.Name()))
		if err != nil {
			continue
		}
		var r ExecResult
		if err := json.Unmarshal(b, &r); err == nil {
			results = append(results, r)
		}
	}
	return results, nil
}
