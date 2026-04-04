package runner

import (
	"strings"
)

// WorkspaceState captures the current state of the workspace at a turn boundary.
type WorkspaceState struct {
	FilesChanged []string `json:"files_changed"`
	Commits      []Commit `json:"commits"`
	Branch       string   `json:"branch"`
	Dirty        bool     `json:"dirty"`
}

type Commit struct {
	SHA     string `json:"sha"`
	Message string `json:"message"`
}

// InspectWorkspace checks the git state of the workspace via Executor.
func InspectWorkspace(exec Executor, workdir string) (*WorkspaceState, error) {
	ws := &WorkspaceState{}

	// Current branch
	result, err := exec.Exec("git branch --show-current", workdir, 5)
	if err == nil && result.ExitCode == 0 {
		ws.Branch = strings.TrimSpace(result.Stdout)
	}

	// Uncommitted changes
	result, err = exec.Exec("git status --porcelain", workdir, 5)
	if err == nil && result.ExitCode == 0 && strings.TrimSpace(result.Stdout) != "" {
		ws.Dirty = true
		for _, line := range strings.Split(result.Stdout, "\n") {
			line = strings.TrimSpace(line)
			if line != "" && len(line) > 3 {
				ws.FilesChanged = append(ws.FilesChanged, line[3:])
			}
		}
	}

	// Commits on branch vs main
	result, err = exec.Exec("git log --oneline main..HEAD", workdir, 5)
	if err == nil && result.ExitCode == 0 {
		for _, line := range strings.Split(result.Stdout, "\n") {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			parts := strings.SplitN(line, " ", 2)
			msg := ""
			if len(parts) > 1 {
				msg = parts[1]
			}
			ws.Commits = append(ws.Commits, Commit{SHA: parts[0], Message: msg})
		}
	}

	return ws, nil
}

// DetectWorkdir finds which repo under /workspace has changes or is the right target.
func DetectWorkdir(exec Executor, workspaceRoot string, branch string) string {
	candidates := []string{"fg-backend", "fg-frontend", "fg-cloud", "fg-auth"}
	for _, repo := range candidates {
		dir := workspaceRoot + "/" + repo
		result, err := exec.Exec("git branch --show-current", dir, 5)
		if err == nil && result.ExitCode == 0 && strings.TrimSpace(result.Stdout) == branch {
			return dir
		}
	}
	// Fallback: first repo that exists
	for _, repo := range candidates {
		dir := workspaceRoot + "/" + repo
		result, err := exec.Exec("git rev-parse --git-dir", dir, 5)
		if err == nil && result.ExitCode == 0 {
			return dir
		}
	}
	return workspaceRoot
}
