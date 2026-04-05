package runner

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"bytes"
	"io"
)

// GitOps handles git operations inside the sandbox via Executor.
type GitOps struct {
	exec      Executor
	workdir   string
	remote    string // full URL with token
	branch    string
	userEmail string
	userName  string
}

func NewGitOps(exec Executor, workdir, remote, branch string) *GitOps {
	return &GitOps{
		exec:      exec,
		workdir:   workdir,
		remote:    remote,
		branch:    branch,
		userEmail: "agent@facilitygrid.com",
		userName:  "FG Agent",
	}
}

// SetupBranch configures git identity and creates the agent branch.
func (g *GitOps) SetupBranch() error {
	cmds := []string{
		fmt.Sprintf("git config user.email %q", g.userEmail),
		fmt.Sprintf("git config user.name %q", g.userName),
		fmt.Sprintf("git checkout -b %s", g.branch),
	}
	for _, cmd := range cmds {
		result, err := g.exec.Exec(cmd, g.workdir, 30)
		if err != nil {
			return fmt.Errorf("git setup: %w", err)
		}
		if result.ExitCode != 0 {
			return fmt.Errorf("git setup: exit %d: %s", result.ExitCode, result.Stderr)
		}
	}
	return nil
}

// Pull does a fast-forward pull.
func (g *GitOps) Pull() error {
	result, err := g.exec.Exec("GIT_SSL_NO_VERIFY=1 git pull --ff-only", g.workdir, 60)
	if err != nil {
		return err
	}
	if result.ExitCode != 0 {
		return fmt.Errorf("git pull: exit %d", result.ExitCode)
	}
	return nil
}

// AutoCommit stages all changes and commits with a standard message.
func (g *GitOps) AutoCommit(issueKey string) (string, error) {
	// Check if there's anything to commit
	result, err := g.exec.Exec("git status --porcelain", g.workdir, 10)
	if err != nil {
		return "", err
	}
	if strings.TrimSpace(result.Stdout) == "" {
		return "", nil // nothing to commit
	}

	// Stage and commit
	msg := fmt.Sprintf("fix(%s): agent implementation", issueKey)
	cmd := fmt.Sprintf("git add -A && git commit -m %q", msg)
	result, err = g.exec.Exec(cmd, g.workdir, 30)
	if err != nil {
		return "", fmt.Errorf("git commit: %w", err)
	}
	if result.ExitCode != 0 {
		return "", fmt.Errorf("git commit: exit %d: %s", result.ExitCode, result.Stderr)
	}

	// Get SHA
	result, err = g.exec.Exec("git rev-parse --short HEAD", g.workdir, 5)
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(result.Stdout), nil
}

// Push pushes the agent branch to the remote.
func (g *GitOps) Push() error {
	cmd := fmt.Sprintf("git remote set-url origin %q && GIT_SSL_NO_VERIFY=1 git push -u origin %s",
		g.remote, g.branch)
	result, err := g.exec.Exec(cmd, g.workdir, 600)
	if err != nil {
		return fmt.Errorf("git push: %w", err)
	}
	if result.ExitCode != 0 {
		return fmt.Errorf("git push: exit %d: %s", result.ExitCode, result.Stderr)
	}
	return nil
}

// HasCommits returns true if there are commits on the branch vs main.
func (g *GitOps) HasCommits() bool {
	result, err := g.exec.Exec("git log --oneline main..HEAD", g.workdir, 10)
	return err == nil && strings.TrimSpace(result.Stdout) != ""
}

// CreateDraftMR creates a draft merge request on GitLab.
// This runs from the host (not inside sandbox) since it's an API call.
func (g *GitOps) CreateDraftMR(gitlabURL, project, title, description string) (string, error) {
	encodedProject := url.PathEscape(project)
	apiURL := fmt.Sprintf("%s/api/v4/projects/%s/merge_requests", gitlabURL, encodedProject)

	body := map[string]any{
		"source_branch":        g.branch,
		"target_branch":        "main",
		"title":                "Draft: " + title,
		"description":          description,
		"labels":               "agent-generated",
		"remove_source_branch": true,
	}
	bodyJSON, _ := json.Marshal(body)

	token := extractTokenFromRemote(g.remote)

	req, err := http.NewRequest("POST", apiURL, bytes.NewReader(bodyJSON))
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("PRIVATE-TOKEN", token)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("MR API: %w", err)
	}
	defer resp.Body.Close()

	respBody, _ := io.ReadAll(resp.Body)
	if resp.StatusCode >= 300 {
		return "", fmt.Errorf("MR API %d: %s", resp.StatusCode, string(respBody))
	}

	var mr struct {
		WebURL string `json:"web_url"`
	}
	if err := json.Unmarshal(respBody, &mr); err != nil {
		return "", err
	}
	return mr.WebURL, nil
}

func extractTokenFromRemote(remote string) string {
	u, err := url.Parse(remote)
	if err != nil {
		return ""
	}
	if u.User != nil {
		p, _ := u.User.Password()
		return p
	}
	return ""
}

// ProjectFromRemote extracts "group/repo" from the remote URL.
func ProjectFromRemote(remote string) string {
	u, err := url.Parse(remote)
	if err != nil {
		return ""
	}
	path := strings.TrimPrefix(u.Path, "/")
	path = strings.TrimSuffix(path, ".git")
	return path
}

// GitLabURLFromRemote extracts "https://host" from the remote URL.
func GitLabURLFromRemote(remote string) string {
	u, err := url.Parse(remote)
	if err != nil {
		return ""
	}
	return fmt.Sprintf("%s://%s", u.Scheme, u.Host)
}
