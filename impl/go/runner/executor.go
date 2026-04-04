package runner

// ExecResult is the result of running a command inside the sandbox.
type ExecResult struct {
	ExitCode int
	Stdout   string
	Stderr   string
	TimedOut bool
}

// Executor runs commands inside the sandbox environment (chroot/bwrap).
// This is provided by the manager package to avoid direct os/exec from the runner.
type Executor interface {
	// Exec runs a command synchronously inside the sandbox.
	Exec(cmd string, workdir string, timeoutS int) (*ExecResult, error)

	// ExecBg runs a command in the background inside the sandbox.
	// Returns a job ID, a channel of stdout lines, and a done channel with exit code.
	ExecBg(cmd string, workdir string, timeoutS int) (jobID int, lines <-chan string, done <-chan int, cancel func(), err error)
}
