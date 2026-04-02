package manager

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"time"

	sqexec "squashd/exec"
	"squashd/config"
	"squashd/store"
)

// SandboxInfo is the JSON-serializable view of a sandbox returned by the API.
type SandboxInfo struct {
	ID           string   `json:"id"`
	State        string   `json:"state"`
	Owner        string   `json:"owner"`
	Task         string   `json:"task,omitempty"`
	Layers       []string `json:"layers"`
	Backend      string   `json:"backend"`
	CreatedAt    string   `json:"created"`
	LastActiveAt string   `json:"last_active"`
	CPULimit     float64  `json:"cpu"`
	MemoryMB     int      `json:"memory_mb"`
	MaxLifetimeS int      `json:"max_lifetime_s"`
	AllowNet     []string `json:"allow_net,omitempty"`
}

// CreateOpts holds the parameters for creating a new sandbox.
type CreateOpts struct {
	Owner        string
	Task         string
	Layers       []string
	CPU          float64
	MemoryMB     int
	MaxLifetimeS int
	AllowNet     []string
}

// ExecOpts holds the parameters for executing a command in a sandbox.
type ExecOpts struct {
	WorkDir  string
	TimeoutS int
}

// ExecResult is the stored record of a command execution.
type ExecResult struct {
	Seq       int    `json:"seq"`
	Cmd       string `json:"cmd"`
	WorkDir   string `json:"workdir"`
	ExitCode  int    `json:"exit_code"`
	Started   int64  `json:"started"`   // Unix timestamp (matches Janet/shell format)
	Finished  int64  `json:"finished"`  // Unix timestamp
	DurMS     int64  `json:"duration_ms"`
	Stdout    string `json:"stdout"`
	Stderr    string `json:"stderr"`
	TimedOut  bool   `json:"timed_out"`
}

// SnapshotResult is returned after a snapshot operation.
type SnapshotResult struct {
	Snapshot string `json:"snapshot"`
	Size     int64  `json:"size"`
}

// Manager is the thread-safe sandbox registry.
type Manager struct {
	mu        sync.Mutex
	sandboxes map[string]*Sandbox // nil entry = slot reserved (creating)
	cfg       *config.Config

	jobMu  sync.Mutex
	jobSeq int
	jobs   map[string]map[int]*Job // sandboxID → jobID → Job
}

// New returns a new empty Manager.
func New(cfg *config.Config) *Manager {
	return &Manager{
		sandboxes: make(map[string]*Sandbox),
		cfg:       cfg,
	}
}

// SandboxCount returns the number of ready sandboxes.
func (m *Manager) SandboxCount() int {
	m.mu.Lock()
	defer m.mu.Unlock()
	n := 0
	for _, s := range m.sandboxes {
		if s != nil && s.State == "ready" {
			n++
		}
	}
	return n
}

// List returns info for all ready sandboxes.
func (m *Manager) List() []SandboxInfo {
	m.mu.Lock()
	sbs := make([]*Sandbox, 0, len(m.sandboxes))
	for _, s := range m.sandboxes {
		if s != nil && s.State == "ready" {
			sbs = append(sbs, s)
		}
	}
	m.mu.Unlock()

	infos := make([]SandboxInfo, 0, len(sbs))
	for _, s := range sbs {
		infos = append(infos, s.ToInfo())
	}
	return infos
}

// Get returns info for a single sandbox, or an error if not found.
func (m *Manager) Get(id string) (SandboxInfo, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil || s.State == "creating" {
		return SandboxInfo{}, fmt.Errorf("sandbox not found: %s", id)
	}
	return s.ToInfo(), nil
}

// Create creates a new sandbox. Returns SandboxInfo or error.
func (m *Manager) Create(id string, opts CreateOpts) (SandboxInfo, error) {
	// Reserve slot under lock.
	m.mu.Lock()
	if _, exists := m.sandboxes[id]; exists {
		m.mu.Unlock()
		return SandboxInfo{}, fmt.Errorf("sandbox already exists: %s", id)
	}
	count := 0
	for _, s := range m.sandboxes {
		if s != nil {
			count++
		}
	}
	if count >= m.cfg.MaxSandboxes {
		m.mu.Unlock()
		return SandboxInfo{}, fmt.Errorf("sandbox limit reached: %d", m.cfg.MaxSandboxes)
	}
	m.sandboxes[id] = nil // nil = creating sentinel
	m.mu.Unlock()

	// Do the heavy work outside the lock.
	sb, err := m.createSandbox(id, opts)
	if err != nil {
		m.mu.Lock()
		delete(m.sandboxes, id) // remove sentinel on failure
		m.mu.Unlock()
		return SandboxInfo{}, err
	}

	m.mu.Lock()
	m.sandboxes[id] = sb
	m.mu.Unlock()

	return sb.ToInfo(), nil
}

// Destroy tears down a sandbox and removes it from the registry.
func (m *Manager) Destroy(id string) error {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	if !exists {
		m.mu.Unlock()
		return fmt.Errorf("sandbox not found: %s", id)
	}
	if s == nil {
		m.mu.Unlock()
		return fmt.Errorf("sandbox is still being created: %s", id)
	}
	delete(m.sandboxes, id)
	m.mu.Unlock()

	// Teardown outside lock.
	s.State = "destroying"
	m.destroySandbox(s)
	return nil
}

// Exec runs a command in a sandbox and returns the result.
func (m *Manager) Exec(id, cmd string, opts ExecOpts) (ExecResult, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return ExecResult{}, fmt.Errorf("sandbox not found: %s", id)
	}

	if opts.WorkDir == "" {
		opts.WorkDir = "/"
	}
	if opts.TimeoutS <= 0 {
		opts.TimeoutS = 300
	}

	seq := s.nextSeq()
	s.updateLastActive()

	res, err := sqexec.Run(s.mergedDir(), cmd, opts.WorkDir, opts.TimeoutS)
	if err != nil {
		return ExecResult{}, fmt.Errorf("exec: %w", err)
	}

	result := ExecResult{
		Seq:      seq,
		Cmd:      cmd,
		WorkDir:  opts.WorkDir,
		ExitCode: res.ExitCode,
		Started:  res.StartedAt.Unix(),
		Finished: res.FinishedAt.Unix(),
		DurMS:    res.FinishedAt.Sub(res.StartedAt).Milliseconds(),
		Stdout:   res.Stdout,
		Stderr:   res.Stderr,
		TimedOut: res.TimedOut,
	}
	writeExecLog(s.logDir(), &result)
	return result, nil
}

// Logs returns all exec log entries for a sandbox.
func (m *Manager) Logs(id string) ([]ExecResult, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return nil, fmt.Errorf("sandbox not found: %s", id)
	}
	return readExecLogs(s.logDir())
}

// Activate adds a module layer to a sandbox and remounts the overlay.
func (m *Manager) Activate(id, moduleName string) (SandboxInfo, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return SandboxInfo{}, fmt.Errorf("sandbox not found: %s", id)
	}

	modPath := filepath.Join(m.cfg.ModulesDir(), moduleName+".squashfs")
	if _, err := os.Stat(modPath); os.IsNotExist(err) {
		// Try S3 pull
		if err2 := s3Pull(m.cfg, "modules/"+moduleName+".squashfs", modPath); err2 != nil {
			return SandboxInfo{}, fmt.Errorf("module not found: %s", moduleName)
		}
	}

	mp := filepath.Join(s.imagesDir(), moduleName+".squashfs")
	if _, err := os.Stat(mp); err == nil {
		return SandboxInfo{}, fmt.Errorf("already active: %s", moduleName)
	}
	if err := os.MkdirAll(mp, 0755); err != nil {
		return SandboxInfo{}, fmt.Errorf("mkdir %s: %w", mp, err)
	}
	if err := sqexec.MountLayer(modPath, mp); err != nil {
		os.Remove(mp)
		return SandboxInfo{}, err
	}

	// Remount overlay with the new layer.
	lower, err := buildLowerDirs(s.imagesDir())
	if err != nil {
		_ = sqexec.UnmountLayer(mp)
		return SandboxInfo{}, err
	}
	if err := sqexec.UnmountOverlay(s.mergedDir()); err != nil {
		_ = sqexec.UnmountLayer(mp)
		return SandboxInfo{}, err
	}
	if err := sqexec.MountOverlay(lower, s.upperDataDir(), s.upperWorkDir(), s.mergedDir()); err != nil {
		return SandboxInfo{}, err
	}

	// Update metadata
	s.mu.Lock()
	s.Layers = append(s.Layers, moduleName)
	s.mu.Unlock()
	_ = writeMeta(s.Dir, s)

	return s.ToInfo(), nil
}

// Snapshot creates a snapshot of the sandbox's upper layer.
// Returns the label and size of the snapshot.
func (m *Manager) Snapshot(id, label string) (SnapshotResult, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return SnapshotResult{}, fmt.Errorf("sandbox not found: %s", id)
	}

	if label == "" {
		t := time.Now().UTC()
		label = fmt.Sprintf("%04d%02d%02d-%02d%02d%02d",
			t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	}
	if !validLabel(label) {
		return SnapshotResult{}, fmt.Errorf("label: alphanumeric/dash/underscore/dot only")
	}

	// Irmin backend: delegate to store sidecar.
	if m.cfg.SnapshotBackend == "irmin" {
		c := store.New(m.cfg.StoreSockPath)
		res, err := c.Snapshot(id, label, s.upperDataDir())
		if err != nil {
			return SnapshotResult{}, err
		}
		return SnapshotResult{Snapshot: res.Label, Size: res.Size}, nil
	}

	// squashfs backend: run mksquashfs on the upper layer.
	snapDir := s.snapshotsDir()
	if err := os.MkdirAll(snapDir, 0755); err != nil {
		return SnapshotResult{}, fmt.Errorf("mkdir snapshots: %w", err)
	}
	snapFile := filepath.Join(snapDir, label+".squashfs")
	if _, err := os.Stat(snapFile); err == nil {
		return SnapshotResult{}, fmt.Errorf("snapshot exists: %s", label)
	}

	args := []string{"mksquashfs", s.upperDataDir(), snapFile,
		"-comp", "gzip", "-b", "256K", "-noappend", "-quiet"}
	if out, err := exec.Command(args[0], args[1:]...).CombinedOutput(); err != nil {
		return SnapshotResult{}, fmt.Errorf("mksquashfs: %w: %s", err, strings.TrimSpace(string(out)))
	}

	fi, _ := os.Stat(snapFile)
	size := int64(0)
	if fi != nil {
		size = fi.Size()
	}

	// Notify sq-sync sidecar (best-effort) or do a background S3 push.
	s3Key := "snapshots/" + id + "/" + label + ".squashfs"
	go s3PushBg(m.cfg, snapFile, s3Key)

	return SnapshotResult{Snapshot: label, Size: size}, nil
}

// Restore restores a sandbox from a previously taken snapshot.
func (m *Manager) Restore(id, label string) (SandboxInfo, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return SandboxInfo{}, fmt.Errorf("sandbox not found: %s", id)
	}
	if !validLabel(label) {
		return SandboxInfo{}, fmt.Errorf("label: alphanumeric/dash/underscore/dot only")
	}

	snapFile := filepath.Join(s.snapshotsDir(), label+".squashfs")
	if _, err := os.Stat(snapFile); os.IsNotExist(err) {
		// Try S3 pull.
		s3Key := "snapshots/" + id + "/" + label + ".squashfs"
		_ = os.MkdirAll(s.snapshotsDir(), 0755)
		if err2 := s3Pull(m.cfg, s3Key, snapFile); err2 != nil {
			return SandboxInfo{}, fmt.Errorf("snapshot not found: %s", label)
		}
	}

	// Irmin backend: restore via sidecar (materialises files into upper_data).
	if m.cfg.SnapshotBackend == "irmin" {
		c := store.New(m.cfg.StoreSockPath)
		if _, err := c.Restore(id, label, s.upperDataDir()); err != nil {
			return SandboxInfo{}, err
		}
		return s.ToInfo(), nil
	}

	// squashfs backend: unmount overlay, clear upper, mount snapshot, remount overlay.
	snapMP := filepath.Join(s.imagesDir(), "_snapshot")

	// Unmount overlay.
	_ = sqexec.UnmountOverlay(s.mergedDir())

	// Unmount previous snapshot if any.
	s.mu.Lock()
	wasMounted := s.snapshotMounted
	s.snapshotMounted = false
	s.mu.Unlock()
	if wasMounted {
		_ = sqexec.UnmountLayer(snapMP)
	}

	// Clear upper layer.
	_ = os.RemoveAll(s.upperDataDir())
	_ = os.RemoveAll(s.upperWorkDir())
	_ = os.MkdirAll(s.upperDataDir(), 0755)
	_ = os.MkdirAll(s.upperWorkDir(), 0755)

	// Mount the snapshot as the top read-only layer.
	_ = os.MkdirAll(snapMP, 0755)
	if err := sqexec.MountLayer(snapFile, snapMP); err != nil {
		return SandboxInfo{}, fmt.Errorf("mount snapshot: %w", err)
	}
	s.mu.Lock()
	s.snapshotMounted = true
	s.mu.Unlock()

	// Rebuild lower dirs with snapshot first.
	moduleLowers, err := buildLowerDirsExcluding(s.imagesDir(), "_snapshot")
	if err != nil {
		return SandboxInfo{}, err
	}
	lower := append([]string{snapMP}, moduleLowers...)

	// Remount overlay.
	if err := sqexec.MountOverlay(lower, s.upperDataDir(), s.upperWorkDir(), s.mergedDir()); err != nil {
		// Degraded recovery: remount without snapshot so sandbox remains usable.
		_ = sqexec.MountOverlay(moduleLowers, s.upperDataDir(), s.upperWorkDir(), s.mergedDir())
		return SandboxInfo{}, fmt.Errorf("remount overlay: %w", err)
	}

	return s.ToInfo(), nil
}

// Fork creates a new sandbox by copying a snapshot (irmin-only).
func (m *Manager) Fork(sourceID, sourceLabel, targetID string) error {
	if m.cfg.SnapshotBackend != "irmin" {
		return fmt.Errorf("fork requires irmin snapshot backend")
	}
	c := store.New(m.cfg.StoreSockPath)
	return c.Fork(sourceID, sourceLabel, targetID)
}

// Diff returns the diff between two snapshots (irmin-only).
func (m *Manager) Diff(sandboxID, fromLabel, toLabel string) (store.DiffResult, error) {
	if m.cfg.SnapshotBackend != "irmin" {
		return store.DiffResult{}, fmt.Errorf("diff requires irmin snapshot backend")
	}
	c := store.New(m.cfg.StoreSockPath)
	return c.Diff(sandboxID, fromLabel, toLabel)
}

// ExpiredSandboxes returns the IDs of sandboxes that have exceeded their
// max_lifetime_s (non-zero) measured from CreatedAt.
// Satisfies the reaper.Destroyer interface.
func (m *Manager) ExpiredSandboxes() []string {
	m.mu.Lock()
	defer m.mu.Unlock()
	now := time.Now()
	var ids []string
	for id, s := range m.sandboxes {
		if s == nil || s.State != "ready" {
			continue
		}
		if s.MaxLifetimeS > 0 && now.Sub(s.CreatedAt) > time.Duration(s.MaxLifetimeS)*time.Second {
			ids = append(ids, id)
		}
	}
	return ids
}

// SetupWireGuard configures WireGuard for a sandbox, adding the provided peers.
// Delegates to sq-wg shell script; returns public key and listen port.
func (m *Manager) SetupWireGuard(id string, peers []map[string]any) (map[string]any, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return nil, fmt.Errorf("sandbox not found: %s", id)
	}

	portFile := filepath.Join(s.metaDir(), "wg_listen_port")
	pubKeyFile := filepath.Join(s.metaDir(), "wg_public_key")

	// Generate keys if wg0 not yet set up.
	if _, err := os.Stat(portFile); os.IsNotExist(err) {
		out, err := exec.Command("sq-wg", "genkey").Output()
		if err != nil {
			return nil, fmt.Errorf("sq-wg genkey: %w", err)
		}
		lines := strings.Split(strings.TrimSpace(string(out)), "\n")
		if len(lines) < 2 {
			return nil, fmt.Errorf("sq-wg genkey: unexpected output")
		}
		privKey := strings.TrimSpace(lines[0])
		pubKey := strings.TrimSpace(lines[1])
		if out2, err := exec.Command("sq-wg", "setup", id, privKey, "51820").CombinedOutput(); err != nil {
			return nil, fmt.Errorf("sq-wg setup: %w: %s", err, strings.TrimSpace(string(out2)))
		}
		_ = os.WriteFile(pubKeyFile, []byte(pubKey), 0600)
		_ = os.WriteFile(portFile, []byte("51820"), 0644)
	}

	// Add each peer.
	for _, peer := range peers {
		pk, _ := peer["publicKey"].(string)
		if pk == "" {
			continue
		}
		ep, _ := peer["endpoint"].(string)
		ips, _ := peer["allowedIPs"].(string)
		if ips == "" {
			ips = "0.0.0.0/0"
		}
		psk, _ := peer["presharedKey"].(string)
		if out, err := exec.Command("sq-wg", "add-peer", id, pk, ep, ips, psk).CombinedOutput(); err != nil {
			return nil, fmt.Errorf("sq-wg add-peer: %w: %s", err, strings.TrimSpace(string(out)))
		}
	}

	pubKeyBytes, _ := os.ReadFile(pubKeyFile)
	pubKey := strings.TrimSpace(string(pubKeyBytes))

	return map[string]any{
		"status":      "ok",
		"publicKey":   pubKey,
		"listenPort":  51820,
		"peersAdded":  len(peers),
	}, nil
}

// Register adds a pre-existing sandbox (from recovery) to the registry.
func (m *Manager) Register(s *Sandbox) {
	m.mu.Lock()
	m.sandboxes[s.ID] = s
	m.mu.Unlock()
}

// ── Internal creation / destruction ─────────────────────────────────────────

func (m *Manager) createSandbox(id string, opts CreateOpts) (*Sandbox, error) {
	sdir := filepath.Join(m.cfg.SandboxesDir(), id)
	if opts.CPU == 0 {
		opts.CPU = 2
	}
	if opts.MemoryMB == 0 {
		opts.MemoryMB = 1024
	}
	if opts.Owner == "" {
		opts.Owner = "anon"
	}
	if len(opts.Layers) == 0 {
		opts.Layers = []string{"000-base-alpine"}
	}

	// Create directory tree.
	for _, d := range []string{
		filepath.Join(sdir, "images"),
		filepath.Join(sdir, "upper", "data"),
		filepath.Join(sdir, "upper", "work"),
		filepath.Join(sdir, "merged"),
		filepath.Join(sdir, ".meta", "log"),
		filepath.Join(sdir, "snapshots"),
	} {
		if err := os.MkdirAll(d, 0755); err != nil {
			return nil, fmt.Errorf("mkdir %s: %w", d, err)
		}
	}

	s := &Sandbox{
		ID:           id,
		State:        "creating",
		Owner:        opts.Owner,
		Task:         opts.Task,
		Layers:       opts.Layers,
		Backend:      m.cfg.Backend,
		CPU:          opts.CPU,
		MemoryMB:     opts.MemoryMB,
		MaxLifetimeS: opts.MaxLifetimeS,
		AllowNet:     opts.AllowNet,
		Dir:          sdir,
		CreatedAt:    time.Now(),
		LastActiveAt: time.Now(),
	}

	// Track mounted layers for rollback.
	var mountedLayers []string
	rollback := func() {
		_ = sqexec.UnmountOverlay(s.mergedDir())
		for i := len(mountedLayers) - 1; i >= 0; i-- {
			_ = sqexec.UnmountLayer(mountedLayers[i])
		}
	}

	// Ensure modules exist (pull from S3 if needed) and mount them.
	for _, layer := range opts.Layers {
		modPath := filepath.Join(m.cfg.ModulesDir(), layer+".squashfs")
		if _, err := os.Stat(modPath); os.IsNotExist(err) {
			if err2 := s3Pull(m.cfg, "modules/"+layer+".squashfs", modPath); err2 != nil {
				rollback()
				return nil, fmt.Errorf("module not found: %s", layer)
			}
		}
		mp := filepath.Join(sdir, "images", layer+".squashfs")
		if err := os.MkdirAll(mp, 0755); err != nil {
			rollback()
			return nil, fmt.Errorf("mkdir layer mp: %w", err)
		}
		if err := sqexec.MountLayer(modPath, mp); err != nil {
			rollback()
			return nil, err
		}
		mountedLayers = append(mountedLayers, mp)
	}

	// Build lower dirs (highest numeric prefix = highest priority).
	lower, err := buildLowerDirs(s.imagesDir())
	if err != nil {
		rollback()
		return nil, err
	}

	// Mount overlay.
	if err := sqexec.MountOverlay(lower, s.upperDataDir(), s.upperWorkDir(), s.mergedDir()); err != nil {
		rollback()
		return nil, err
	}

	// Inject secrets into upper layer.
	_ = injectSecrets(m.cfg, s)

	// Write metadata.
	if err := writeMeta(sdir, s); err != nil {
		rollback()
		return nil, err
	}

	s.State = "ready"
	return s, nil
}

func (m *Manager) destroySandbox(s *Sandbox) {
	// Unmount overlay, then each squashfs layer.
	_ = sqexec.UnmountOverlay(s.mergedDir())
	entries, _ := os.ReadDir(s.imagesDir())
	for i := len(entries) - 1; i >= 0; i-- {
		mp := filepath.Join(s.imagesDir(), entries[i].Name())
		_ = sqexec.UnmountLayer(mp)
	}
	// Remove the sandbox directory.
	_ = os.RemoveAll(s.Dir)
}

// ── Helpers ──────────────────────────────────────────────────────────────────

// buildLowerDirs returns mounted squashfs directories sorted highest-prefix-first.
func buildLowerDirs(imagesDir string) ([]string, error) {
	entries, err := os.ReadDir(imagesDir)
	if err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("read images dir: %w", err)
	}
	var dirs []string
	for _, e := range entries {
		if e.IsDir() {
			dirs = append(dirs, filepath.Join(imagesDir, e.Name()))
		}
	}
	// Sort: highest numeric prefix first (200- before 100- before 000-)
	sort.Slice(dirs, func(i, j int) bool {
		return layerPrefix(dirs[i]) > layerPrefix(dirs[j])
	})
	return dirs, nil
}

func layerPrefix(path string) int {
	name := filepath.Base(path)
	if len(name) >= 3 {
		n := 0
		for _, c := range name[:3] {
			if c < '0' || c > '9' {
				return 0
			}
			n = n*10 + int(c-'0')
		}
		return n
	}
	return 0
}

// buildLowerDirsExcluding is like buildLowerDirs but skips entries named exclude.
func buildLowerDirsExcluding(imagesDir, exclude string) ([]string, error) {
	entries, err := os.ReadDir(imagesDir)
	if err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("read images dir: %w", err)
	}
	var dirs []string
	for _, e := range entries {
		if e.IsDir() && e.Name() != exclude {
			dirs = append(dirs, filepath.Join(imagesDir, e.Name()))
		}
	}
	sort.Slice(dirs, func(i, j int) bool {
		return layerPrefix(dirs[i]) > layerPrefix(dirs[j])
	})
	return dirs, nil
}

// validLabel checks that a snapshot label contains only safe characters.
func validLabel(label string) bool {
	if label == "" || len(label) > 128 {
		return false
	}
	for _, c := range label {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.') {
			return false
		}
	}
	return true
}

// s3PushBg runs sq-s3 push in the background (non-blocking).
func s3PushBg(cfg *config.Config, localPath, s3Key string) {
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
	cmd := exec.Command("sq-s3", "push", localPath, s3Key)
	cmd.Env = env
	_ = cmd.Run()
}

// s3Pull delegates to the sq-s3 shell script to download a module.
// Returns nil on success, error if the pull failed.
func s3Pull(cfg *config.Config, s3Key, localPath string) error {
	if cfg.S3Bucket == "" {
		return fmt.Errorf("no S3 bucket configured")
	}
	env := os.Environ()
	if cfg.S3Bucket != "" {
		env = append(env, "SQUASH_S3_BUCKET="+cfg.S3Bucket)
	}
	if cfg.S3Endpoint != "" {
		env = append(env, "SQUASH_S3_ENDPOINT="+cfg.S3Endpoint)
	}
	if cfg.S3Region != "" {
		env = append(env, "SQUASH_S3_REGION="+cfg.S3Region)
	}
	if cfg.S3Prefix != "" {
		env = append(env, "SQUASH_S3_PREFIX="+cfg.S3Prefix)
	}
	cmd := exec.Command("sq-s3", "pull", s3Key, localPath)
	cmd.Env = env
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("sq-s3 pull %s: %w: %s", s3Key, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// injectSecrets writes placeholder env vars to the sandbox upper layer.
// Matches the pattern in Janet/CL secrets.janet / secrets.lisp.
func injectSecrets(cfg *config.Config, s *Sandbox) error {
	secretsPath := cfg.SecretsFile()
	b, err := os.ReadFile(secretsPath)
	if os.IsNotExist(err) {
		return nil // no secrets configured — fine
	}
	if err != nil {
		return nil
	}

	var secretsFile struct {
		Secrets map[string]struct {
			Placeholder string `json:"placeholder"`
		} `json:"secrets"`
	}
	if err := json.Unmarshal(b, &secretsFile); err != nil {
		return nil
	}

	profileDir := filepath.Join(s.upperDataDir(), "etc", "profile.d")
	if err := os.MkdirAll(profileDir, 0755); err != nil {
		return nil
	}

	var lines []string
	lines = append(lines, "#!/bin/sh")
	for _, secret := range secretsFile.Secrets {
		if secret.Placeholder != "" {
			lines = append(lines, fmt.Sprintf("export %s", secret.Placeholder))
		}
	}

	// Proxy env vars — sandboxes route outbound traffic through the MITM proxy.
	// Using 127.0.0.1 (host loopback); sandbox netns shares host network in chroot mode.
	lines = append(lines,
		"export http_proxy=http://127.0.0.1:8888",
		"export https_proxy=http://127.0.0.1:8888",
		"export HTTP_PROXY=http://127.0.0.1:8888",
		"export HTTPS_PROXY=http://127.0.0.1:8888",
	)

	if cfg.ProxyHTTPS {
		caPath := filepath.Join(cfg.ProxyCADir(), "ca.crt")
		if _, err := os.Stat(caPath); err == nil {
			destCADir := filepath.Join(s.upperDataDir(), "usr", "local", "share", "ca-certificates")
			_ = os.MkdirAll(destCADir, 0755)
			if caData, err := os.ReadFile(caPath); err == nil {
				_ = os.WriteFile(filepath.Join(destCADir, "sq-proxy-ca.crt"), caData, 0644)
			}
			lines = append(lines,
				"export NODE_EXTRA_CA_CERTS=/usr/local/share/ca-certificates/sq-proxy-ca.crt",
				"export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt",
				"export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt",
			)
		}
	}

	script := strings.Join(lines, "\n") + "\n"
	return os.WriteFile(filepath.Join(profileDir, "squash-secrets.sh"), []byte(script), 0644)
}
