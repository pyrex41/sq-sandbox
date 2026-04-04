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

// Snapshotter takes snapshots of the sandbox's upper layer.
// Provided by the manager package.
type Snapshotter interface {
	// Snapshot takes a named snapshot. Returns the label and size.
	Snapshot(label string) (label_ string, size int64, err error)
}

// SnapshotPolicy controls automatic snapshots during task execution.
type SnapshotPolicy struct {
	// Every N batches, take a snapshot. 0 = disabled.
	EveryNBatches int `json:"every_n_batches"`
	// Max snapshots to retain. Older ones are not deleted (managed by caller).
	MaxSnapshots int `json:"max_snapshots"`
}
