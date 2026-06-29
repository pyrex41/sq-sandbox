package exec

import (
	"bufio"
	"io"
	"os"
	"os/exec"
	"sync"
	"time"
)

// RunTool runs an arbitrary argv synchronously, capturing capped stdout/stderr.
// It is the generic core behind Run (sq-exec) and the gvisor backend (sq-gvisor):
// the launcher is identical; only the argv differs. graceS is the extra grace
// period (seconds) before the safety-net kill fires past timeoutS.
func RunTool(argv []string, timeoutS, graceS int) (*Result, error) {
	c := exec.Command(argv[0], argv[1:]...)

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

	outCh := make(chan string, 1)
	errCh := make(chan string, 1)
	go func() { outCh <- readCapped(stdoutPipe, maxOutput) }()
	go func() { errCh <- readCapped(stderrPipe, maxOutput) }()

	timer := time.AfterFunc(time.Duration(timeoutS+graceS)*time.Second, func() {
		_ = c.Process.Kill()
	})

	stdout := <-outCh
	stderr := <-errCh

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

// RunBgTool starts an arbitrary argv in the background, streaming each line of
// merged stdout/stderr to onLine. It is the generic core behind RunBg.
// Returns a cancel func (kills the process) and a channel that receives the exit
// code when the process completes.
func RunBgTool(argv []string, timeoutS int, extraEnv []string, onLine func(string)) (cancel func(), exitCh <-chan int) {
	c := exec.Command(argv[0], argv[1:]...)
	if len(extraEnv) > 0 {
		c.Env = append(os.Environ(), extraEnv...)
	}
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
