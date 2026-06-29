package manager

import (
	"fmt"
	"os/exec"
	"strings"

	"squashd/config"
	sqexec "squashd/exec"
)

// gvisorBackend runs the sandbox payload under runsc (systrap platform — no
// /dev/kvm, so it nests inside Firecracker/Fly guests) and implements memory
// checkpoint/restore via `runsc checkpoint` / `runsc restore`.
//
// The heavy, Linux-specific runsc orchestration (OCI bundle generation from the
// existing merged/ overlay, slirp4netns networking + MITM-proxy env, a
// long-lived `runsc run` holder + `runsc exec` per command, and the
// checkpoint/restore image handling) lives in the shared/bin/sq-gvisor helper —
// mirroring how the chroot backend delegates isolation to sq-exec. Keeping the
// orchestration in the helper keeps this Go layer thin and portable.
//
// Run model (load-bearing for C/R): gVisor checkpoint/restore resumes only the
// container INIT process tree — `runsc exec` children are not restored. So the
// long-running ExecBg workload (the agent loop) is launched as the container
// INIT via `runsc run` (sq-gvisor run-init), and checkpoint/restore resumes it.
// Synchronous Exec runs as `runsc exec` into the live container (sq-gvisor exec).
type gvisorBackend struct{ cfg *config.Config }

func (g *gvisorBackend) Run(s *Sandbox, cmd, workdir string, timeoutS int) (*sqexec.Result, error) {
	if timeoutS <= 0 {
		timeoutS = 300
	}
	argv := []string{"sq-gvisor", "exec", s.gvisorRoot(), s.mergedDir(), cmd, workdir, fmt.Sprintf("%d", timeoutS), s.ID}
	return sqexec.RunTool(argv, timeoutS, 5)
}

func (g *gvisorBackend) RunBg(s *Sandbox, cmd, workdir string, timeoutS int, extraEnv []string, onLine func(string)) (func(), <-chan int) {
	if timeoutS <= 0 {
		timeoutS = 7200
	}
	// run-init: the command becomes the sandbox's PID 1 so C/R can resume it.
	argv := []string{"sq-gvisor", "run-init", s.gvisorRoot(), s.mergedDir(), cmd, workdir, fmt.Sprintf("%d", timeoutS), s.ID}
	return sqexec.RunBgTool(argv, timeoutS, extraEnv, onLine)
}

func (g *gvisorBackend) Checkpoint(s *Sandbox, imgPath string) error {
	if out, err := gvisorRun("checkpoint", s.gvisorRoot(), s.ID, imgPath); err != nil {
		return fmt.Errorf("gvisor checkpoint: %w: %s", err, out)
	}
	return nil
}

func (g *gvisorBackend) Restore(s *Sandbox, imgPath string) error {
	if out, err := gvisorRun("restore", s.gvisorRoot(), s.ID, imgPath, s.mergedDir()); err != nil {
		return fmt.Errorf("gvisor restore: %w: %s", err, out)
	}
	return nil
}

func (g *gvisorBackend) Destroy(s *Sandbox) error {
	// Best-effort: tear down the runsc sandbox + its state dir + slirp.
	_, _ = gvisorRun("destroy", s.gvisorRoot(), s.ID)
	return nil
}

// gvisorRun invokes the sq-gvisor helper and returns trimmed combined output.
func gvisorRun(args ...string) (string, error) {
	out, err := exec.Command("sq-gvisor", args...).CombinedOutput()
	return strings.TrimSpace(string(out)), err
}
