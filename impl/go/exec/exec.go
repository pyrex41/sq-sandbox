package exec

import (
	"fmt"
	"io"
	"os/exec"
	"strings"
	"time"
)

const (
	maxOutput       = 65536 // 64 KiB per stream — matches Janet/Zig/shell
	timeoutExitCode = 124   // matches timeout(1) exit code
)

// Result is the outcome of running a command in a sandbox.
type Result struct {
	ExitCode   int    `json:"exit_code"`
	Stdout     string `json:"stdout"`
	Stderr     string `json:"stderr"`
	TimedOut   bool   `json:"timed_out"`
	StartedAt  time.Time
	FinishedAt time.Time
}

// Run executes cmd inside the merged rootfs via sq-exec.
//
// sq-exec handles isolation (bubblewrap/unshare+chroot), timeout enforcement,
// network namespace lookup, and seccomp — the daemon just orchestrates.
//
// argv: sq-exec <merged> <cmd> <workdir> <timeout_s> [net]
//
// Run is a thin wrapper over RunTool (see tool.go); the gvisor backend uses the
// same launcher with a different argv.
func Run(merged, cmd, workdir string, timeoutS int) (*Result, error) {
	if timeoutS <= 0 {
		timeoutS = 300
	}
	argv := []string{
		"sq-exec",
		merged,
		cmd,
		workdir,
		fmt.Sprintf("%d", timeoutS),
		"1", // net=1: enable slirp4netns so sandbox can reach MITM proxy
	}
	return RunTool(argv, timeoutS, 5)
}

// RunBg starts a background execution of cmd inside the merged rootfs.
// onLine is called for each line of stdout or stderr (merged).
// extraEnv is appended to os.Environ() for the spawned sq-exec process; pass
// nil for default environment.
// Returns a cancel func (kills the process) and a channel that receives the
// exit code when the process completes.
func RunBg(merged, cmd, workdir string, timeoutS int, extraEnv []string, onLine func(string)) (cancel func(), exitCh <-chan int) {
	if timeoutS <= 0 {
		timeoutS = 7200
	}
	argv := []string{
		"sq-exec",
		merged,
		cmd,
		workdir,
		fmt.Sprintf("%d", timeoutS),
		"1", // net=1: enable slirp4netns so sandbox can reach MITM proxy
	}
	return RunBgTool(argv, timeoutS, extraEnv, onLine)
}

// readCapped reads up to maxBytes from r, then drains the remainder so the
// child process is not blocked on a full pipe buffer.
func readCapped(r io.Reader, maxBytes int) string {
	buf := make([]byte, 0, min(maxBytes, 4096))
	tmp := make([]byte, 4096)

	for len(buf) < maxBytes {
		n, err := r.Read(tmp[:min(4096, maxBytes-len(buf))])
		if n > 0 {
			buf = append(buf, tmp[:n]...)
		}
		if err != nil {
			return string(buf)
		}
	}
	// Drain excess so the child can write past the cap without blocking.
	drain := make([]byte, 4096)
	for {
		_, err := r.Read(drain)
		if err != nil {
			break
		}
	}
	return string(buf)
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// MountLayer mounts a squashfs file at mountPoint using sq-mount-layer.
func MountLayer(sqfsPath, mountPoint string) error {
	out, err := exec.Command("sq-mount-layer", sqfsPath, mountPoint).CombinedOutput()
	if err != nil {
		return fmt.Errorf("sq-mount-layer %s -> %s: %w: %s",
			sqfsPath, mountPoint, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// UnmountLayer unmounts a squashfs mount point using sq-mount-layer --unmount.
func UnmountLayer(mountPoint string) error {
	out, err := exec.Command("sq-mount-layer", "--unmount", mountPoint).CombinedOutput()
	if err != nil {
		return fmt.Errorf("sq-mount-layer --unmount %s: %w: %s",
			mountPoint, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// MountOverlay mounts an overlayfs at merged using sq-mount-overlay.
// lowerDirs are listed highest-priority first.
func MountOverlay(lowerDirs []string, upper, work, merged string) error {
	lower := strings.Join(lowerDirs, ":")
	out, err := exec.Command("sq-mount-overlay", lower, upper, work, merged).CombinedOutput()
	if err != nil {
		return fmt.Errorf("sq-mount-overlay: %w: %s", err, strings.TrimSpace(string(out)))
	}
	return nil
}

// UnmountOverlay unmounts the overlay at merged.
func UnmountOverlay(merged string) error {
	out, err := exec.Command("sq-mount-overlay", "--unmount", merged).CombinedOutput()
	if err != nil {
		return fmt.Errorf("sq-mount-overlay --unmount %s: %w: %s",
			merged, err, strings.TrimSpace(string(out)))
	}
	return nil
}
