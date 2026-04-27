package manager

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"squashd/config"
	sqexec "squashd/exec"
)

// Default GUI parameters used when the caller does not override them.
const (
	defaultGUIDesktop    = "xfce"
	defaultGUIResolution = "1920x1080"
	defaultGUINoVNCPort  = 6080
	defaultGUIVNCPort    = 5900
)

// EnableGUI ensures the GUI layer is mounted and starts the in-sandbox GUI
// stack (Xvfb + window manager + x11vnc + websockify). Idempotent: a second
// call returns the current state without restarting the stack.
func (m *Manager) EnableGUI(id string, opts *GUIOpts) (*GUIState, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return nil, fmt.Errorf("sandbox not found: %s", id)
	}

	s.mu.Lock()
	if s.GUI != nil && s.GUI.Enabled {
		state := *s.GUI
		s.mu.Unlock()
		return &state, nil
	}
	s.mu.Unlock()

	if err := m.startGUI(s, opts); err != nil {
		return nil, err
	}

	s.mu.Lock()
	defer s.mu.Unlock()
	if s.GUI == nil {
		return nil, fmt.Errorf("gui state missing after start")
	}
	state := *s.GUI
	return &state, nil
}

// DisableGUI stops the running GUI stack. Idempotent: returns success if GUI
// was not running. The GUI layer remains mounted so re-enabling is fast.
func (m *Manager) DisableGUI(id string) error {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return fmt.Errorf("sandbox not found: %s", id)
	}

	s.mu.Lock()
	gui := s.GUI
	s.mu.Unlock()
	if gui == nil || !gui.Enabled {
		return nil
	}

	// Best-effort kill of the background job that started the stack.
	if gui.JobID > 0 {
		_ = m.KillJob(id, gui.JobID)
	}

	// Best-effort cleanup of stragglers (Xvfb / x11vnc / websockify) inside
	// the sandbox. Non-fatal on failure.
	cleanup := "pkill -TERM -f 'Xvfb|x11vnc|websockify|startxfce4|sq-gui-start' 2>/dev/null; sleep 1; pkill -KILL -f 'Xvfb|x11vnc|websockify|startxfce4|sq-gui-start' 2>/dev/null; true"
	_, _ = sqexec.Run(s.mergedDir(), cleanup, "/", 10)

	s.mu.Lock()
	s.GUI = &GUIState{Enabled: false, Module: gui.Module}
	s.mu.Unlock()
	_ = writeMeta(s.Dir, s)

	return nil
}

// GUIStatus returns the current GUI state for a sandbox, or a disabled state
// if GUI was never enabled.
func (m *Manager) GUIStatus(id string) (*GUIState, error) {
	m.mu.Lock()
	s, exists := m.sandboxes[id]
	m.mu.Unlock()
	if !exists || s == nil {
		return nil, fmt.Errorf("sandbox not found: %s", id)
	}

	s.mu.Lock()
	defer s.mu.Unlock()
	if s.GUI == nil {
		return &GUIState{Enabled: false}, nil
	}
	state := *s.GUI
	return &state, nil
}

// startGUI mounts the GUI module if needed, writes /etc/sq-gui.conf into the
// upper layer, and launches sq-gui-start as a background job. Caller must NOT
// hold s.mu.
func (m *Manager) startGUI(s *Sandbox, opts *GUIOpts) error {
	if opts == nil {
		opts = &GUIOpts{}
	}
	desktop := opts.Desktop
	if desktop == "" {
		desktop = defaultGUIDesktop
	}
	resolution := opts.Resolution
	if resolution == "" {
		resolution = defaultGUIResolution
	}
	module := opts.Module
	if module == "" {
		module = guiModule(m.cfg, opts)
	}

	if !validGUIDesktop(desktop) {
		return fmt.Errorf("invalid desktop: %s", desktop)
	}
	if !validGUIResolution(resolution) {
		return fmt.Errorf("invalid resolution: %s (want WIDTHxHEIGHT)", resolution)
	}
	if !validModuleName(module) {
		return fmt.Errorf("invalid module: %s", module)
	}

	// Ensure the GUI module is part of the active layer set. If not, activate it
	// so it shows up in the overlay merged view. Activate's "already active"
	// error is treated as success here (idempotent behavior).
	if !contains(s.Layers, module) {
		if _, err := m.Activate(s.ID, module); err != nil &&
			!strings.Contains(err.Error(), "already active") {
			return fmt.Errorf("activate gui module: %w", err)
		}
	}

	// Write a small config file the in-sandbox sq-gui-start script reads so
	// runtime parameters are available without quoting tricks on the cmdline.
	if err := writeGUIConfig(s, desktop, resolution, opts.VNCPassword); err != nil {
		return fmt.Errorf("write gui config: %w", err)
	}

	// Launch the GUI stack as a background job. The script lives inside the
	// GUI module rootfs at /usr/local/bin/sq-gui-start.
	jobID, err := m.ExecBg(s.ID, "sq-gui-start", "/", 0)
	if err != nil {
		return fmt.Errorf("exec sq-gui-start: %w", err)
	}

	state := &GUIState{
		Enabled:    true,
		Desktop:    desktop,
		Resolution: resolution,
		Module:     module,
		JobID:      jobID,
		NoVNCPort:  defaultGUINoVNCPort,
		VNCPort:    defaultGUIVNCPort,
		StartedAt:  time.Now().UTC().Format(time.RFC3339),
	}
	s.mu.Lock()
	s.GUI = state
	if !contains(s.Features, "gui") {
		s.Features = append(s.Features, "gui")
	}
	s.mu.Unlock()
	_ = writeMeta(s.Dir, s)

	return nil
}

// writeGUIConfig writes /etc/sq-gui.conf into the sandbox upper layer. The
// in-sandbox sq-gui-start script sources this file to pick up parameters.
func writeGUIConfig(s *Sandbox, desktop, resolution, vncPassword string) error {
	dir := filepath.Join(s.upperDataDir(), "etc")
	if err := os.MkdirAll(dir, 0755); err != nil {
		return err
	}
	var b strings.Builder
	b.WriteString("# generated by squashd; safe to overwrite\n")
	fmt.Fprintf(&b, "DESKTOP=%s\n", desktop)
	fmt.Fprintf(&b, "RESOLUTION=%s\n", resolution)
	fmt.Fprintf(&b, "NOVNC_PORT=%d\n", defaultGUINoVNCPort)
	fmt.Fprintf(&b, "VNC_PORT=%d\n", defaultGUIVNCPort)
	if vncPassword != "" {
		fmt.Fprintf(&b, "VNC_PASSWORD=%s\n", vncPassword)
	}
	mode := os.FileMode(0644)
	if vncPassword != "" {
		mode = 0600
	}
	return os.WriteFile(filepath.Join(dir, "sq-gui.conf"), []byte(b.String()), mode)
}

// guiModule returns the module name to use for the GUI layer, falling back
// through opts → config → built-in default.
func guiModule(cfg *config.Config, opts *GUIOpts) string {
	if opts != nil && opts.Module != "" {
		return opts.Module
	}
	if cfg != nil && cfg.GUIModule != "" {
		return cfg.GUIModule
	}
	return "500-gui-base"
}

// hasFeature returns true if name is in the features slice (case-insensitive).
func hasFeature(features []string, name string) bool {
	for _, f := range features {
		if strings.EqualFold(strings.TrimSpace(f), name) {
			return true
		}
	}
	return false
}

// mergeFeatures returns base with any items from extra appended that are not
// already present (case-insensitive). Order is preserved.
func mergeFeatures(base, extra []string) []string {
	out := append([]string(nil), base...)
	for _, e := range extra {
		if !hasFeature(out, e) {
			out = append(out, e)
		}
	}
	return out
}

// contains is a small string-slice helper.
func contains(xs []string, target string) bool {
	for _, x := range xs {
		if x == target {
			return true
		}
	}
	return false
}

// validGUIDesktop accepts short alphanumeric/dash identifiers ("xfce",
// "minimal", "kde-plasma"). Rejects anything that could break shell parsing.
func validGUIDesktop(s string) bool {
	if s == "" || len(s) > 32 {
		return false
	}
	for _, c := range s {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c >= '0' && c <= '9') || c == '-' || c == '_') {
			return false
		}
	}
	return true
}

// validGUIResolution accepts "WIDTHxHEIGHT" with sane bounds.
func validGUIResolution(s string) bool {
	if len(s) > 16 {
		return false
	}
	parts := strings.SplitN(s, "x", 2)
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return false
	}
	for _, p := range parts {
		if len(p) > 5 {
			return false
		}
		for _, c := range p {
			if c < '0' || c > '9' {
				return false
			}
		}
	}
	return true
}

// validModuleName mirrors the API-side validation but is local to manager so
// the package has no dependency on api.
func validModuleName(name string) bool {
	if name == "" || len(name) > 128 {
		return false
	}
	for _, c := range name {
		if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.') {
			return false
		}
	}
	return true
}
