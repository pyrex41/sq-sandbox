package exec

import (
	"bufio"
	"fmt"
	"io"
	"os/exec"
	"strings"
	"sync"
	"time"
)

const (
	maxOutput       = 65536 // 64 KiB per stream — matches Janet/Zig/shell
	timeoutExitCode = 124   // matches timeout(1) exit code
)

// Result is the outcome of running a command in a sandbox.
type Result struct {
	ExitCode  int    `json:"exit_code"`
	Stdout    string `json:"stdout"`
	Stderr    string `json:"stderr"`
	TimedOut  bool   `json:"timed_out"`
	StartedAt time.Time
	FinishedAt time.Time
}

// Run executes cmd inside the merged rootfs via sq-exec.
//
// sq-exec handles isolation (bubblewrap/unshare+chroot), timeout enforcement,
// network namespace lookup, and seccomp — the daemon just orchestrates.
//
// argv: sq-exec <merged> <cmd> <workdir> <timeout_s> [net]
func Run(merged, cmd, workdir string, timeoutS int) (*Result, error) {
	if timeoutS <= 0 {
		timeoutS = 300
	}

	args := []string{
		"sq-exec",
		merged,
		cmd,
		workdir,
		fmt.Sprintf("%d", timeoutS),
		"1", // net=1: enable slirp4netns so sandbox can reach MITM proxy
	}

	c := exec.Command(args[0], args[1:]...)

	stdoutPipe, err := c.StdoutPipe()
	if err != nil {
		return &Result{ExitCode: 126, Stderr: "pipe: " + err.Error()}, nil
	}
	stderrPipe, err := c.StderrPipe()
	if err != nil {
		return &Result{ExitCode: 126, Stderr: "pipe: " + err.Error()}, nil
	}

	startedAt := time.Now()
	if err := c.Start(); err != nil {
		return &Result{
			ExitCode:   126,
			Stderr:     "spawn: " + err.Error(),
			StartedAt:  startedAt,
			FinishedAt: time.Now(),
		}, nil
	}

	// Read stdout and stderr concurrently to prevent pipe buffer deadlock.
	type readResult struct{ data string }
	outCh := make(chan readResult, 1)
	errCh := make(chan readResult, 1)

	go func() { outCh <- readResult{readCapped(stdoutPipe, maxOutput)} }()
	go func() { errCh <- readResult{readCapped(stderrPipe, maxOutput)} }()

	// Safety-net: kill if sq-exec's own timeout doesn't fire within grace period.
	gracePeriod := time.Duration(timeoutS+5) * time.Second
	timer := time.AfterFunc(gracePeriod, func() {
		_ = c.Process.Kill()
	})

	stdout := (<-outCh).data
	stderr := (<-errCh).data

	exitErr := c.Wait()
	timer.Stop()
	finishedAt := time.Now()

	exitCode := 0
	timedOut := false
	if exitErr != nil {
		if ee, ok := exitErr.(*exec.ExitError); ok {
			exitCode = ee.ExitCode()
			if exitCode == timeoutExitCode {
				timedOut = true
			}
		} else {
			exitCode = 1
		}
	}

	return &Result{
		ExitCode:   exitCode,
		Stdout:     stdout,
		Stderr:     stderr,
		TimedOut:   timedOut,
		StartedAt:  startedAt,
		FinishedAt: finishedAt,
	}, nil
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

// RunBg starts a background execution of cmd inside the merged rootfs.
// onLine is called for each line of stdout or stderr (merged).
// Returns a cancel func (kills the process) and a channel that receives the
// exit code when the process completes.
func RunBg(merged, cmd, workdir string, timeoutS int, onLine func(string)) (cancel func(), exitCh <-chan int) {
	if timeoutS <= 0 {
		timeoutS = 7200
	}

	args := []string{
		"sq-exec",
		merged,
		cmd,
		workdir,
		fmt.Sprintf("%d", timeoutS),
		"1", // net=1: enable slirp4netns so sandbox can reach MITM proxy
	}

	c := exec.Command(args[0], args[1:]...)
	ch := make(chan int, 1)

	stdoutPipe, err := c.StdoutPipe()
	if err != nil {
		go func() { ch <- 126 }()
		return func() {}, ch
	}
	stderrPipe, err := c.StderrPipe()
	if err != nil {
		go func() { ch <- 126 }()
		return func() {}, ch
	}

	if err := c.Start(); err != nil {
		stdoutPipe.Close()
		stderrPipe.Close()
		onLine("spawn error: " + err.Error())
		go func() { ch <- 126 }()
		return func() {}, ch
	}

	var wg sync.WaitGroup
	wg.Add(2)

	scanPipe := func(r io.Reader) {
		defer wg.Done()
		sc := bufio.NewScanner(r)
		sc.Buffer(make([]byte, 64*1024), 1024*1024)
		for sc.Scan() {
			onLine(sc.Text())
		}
	}
	go scanPipe(stdoutPipe)
	go scanPipe(stderrPipe)

	// Safety-net kill after timeout + grace.
	timer := time.AfterFunc(time.Duration(timeoutS+10)*time.Second, func() {
		_ = c.Process.Kill()
	})

	go func() {
		wg.Wait()
		exitErr := c.Wait()
		timer.Stop()
		exitCode := 0
		if exitErr != nil {
			if ee, ok := exitErr.(*exec.ExitError); ok {
				exitCode = ee.ExitCode()
			} else {
				exitCode = 1
			}
		}
		ch <- exitCode
	}()

	return func() { _ = c.Process.Kill() }, ch
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
