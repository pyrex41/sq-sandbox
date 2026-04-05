// Package store implements a client for the sq-store Unix socket sidecar.
//
// Protocol: newline-delimited JSON, single request → single response.
// The sidecar (store/bin/main.ml) handles content-addressed snapshot storage
// using Irmin-pack. The daemon uses this when SQUASH_SNAPSHOT_BACKEND=irmin.
package store

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net"
	"time"
)

const (
	dialTimeout = 5 * time.Second
	readTimeout = 5 * time.Minute // max time to wait for sidecar response
)

// SnapshotResult is the response from an "snapshot" op.
type SnapshotResult struct {
	Label string `json:"label"`
	Size  int64  `json:"size"`
	Stats struct {
		Files      int `json:"files"`
		BlobsNew   int `json:"blobs_new"`
		BlobsReused int `json:"blobs_reused"`
	} `json:"stats"`
}

// RestoreResult is the response from a "restore" op.
type RestoreResult struct {
	Label        string `json:"label"`
	FilesWritten int    `json:"files_written"`
	FilesDeleted int    `json:"files_deleted"`
}

// DiffResult is the response from a "diff" op.
type DiffResult struct {
	Added    []string `json:"added"`
	Modified []string `json:"modified"`
	Deleted  []string `json:"deleted"`
}

// Client is a handle to the sq-store sidecar socket.
type Client struct {
	sockPath string
}

// New returns a new Client for the given Unix socket path.
func New(sockPath string) *Client {
	return &Client{sockPath: sockPath}
}

// Snapshot asks the sidecar to snapshot the upper_data directory.
func (c *Client) Snapshot(sandboxID, label, upperData string) (SnapshotResult, error) {
	req := map[string]string{
		"op":         "snapshot",
		"sandbox_id": sandboxID,
		"label":      label,
		"upper_data": upperData,
	}
	var resp struct {
		Ok    bool           `json:"ok"`
		Error string         `json:"error"`
		SnapshotResult
	}
	if err := c.do(req, &resp); err != nil {
		return SnapshotResult{}, err
	}
	if !resp.Ok {
		return SnapshotResult{}, fmt.Errorf("sq-store: %s", resp.Error)
	}
	return resp.SnapshotResult, nil
}

// Restore asks the sidecar to restore a snapshot into upper_data.
func (c *Client) Restore(sandboxID, label, upperData string) (RestoreResult, error) {
	req := map[string]string{
		"op":         "restore",
		"sandbox_id": sandboxID,
		"label":      label,
		"upper_data": upperData,
	}
	var resp struct {
		Ok    bool          `json:"ok"`
		Error string        `json:"error"`
		RestoreResult
	}
	if err := c.do(req, &resp); err != nil {
		return RestoreResult{}, err
	}
	if !resp.Ok {
		return RestoreResult{}, fmt.Errorf("sq-store: %s", resp.Error)
	}
	return resp.RestoreResult, nil
}

// Fork copies a snapshot from sourceSandbox/sourceLabel to targetSandbox.
func (c *Client) Fork(sourceID, sourceLabel, targetID string) error {
	req := map[string]string{
		"op":           "fork",
		"source_id":    sourceID,
		"source_label": sourceLabel,
		"target_id":    targetID,
	}
	var resp struct {
		Ok    bool   `json:"ok"`
		Error string `json:"error"`
	}
	if err := c.do(req, &resp); err != nil {
		return err
	}
	if !resp.Ok {
		return fmt.Errorf("sq-store fork: %s", resp.Error)
	}
	return nil
}

// Diff returns the diff between two snapshots.
func (c *Client) Diff(sandboxID, fromLabel, toLabel string) (DiffResult, error) {
	req := map[string]string{
		"op":         "diff",
		"sandbox_id": sandboxID,
		"from":       fromLabel,
		"to":         toLabel,
	}
	var resp struct {
		Ok    bool       `json:"ok"`
		Error string     `json:"error"`
		DiffResult
	}
	if err := c.do(req, &resp); err != nil {
		return DiffResult{}, err
	}
	if !resp.Ok {
		return DiffResult{}, fmt.Errorf("sq-store diff: %s", resp.Error)
	}
	return resp.DiffResult, nil
}

// do sends req as JSON and decodes the response JSON into out.
func (c *Client) do(req any, out any) error {
	conn, err := net.DialTimeout("unix", c.sockPath, dialTimeout)
	if err != nil {
		return fmt.Errorf("sq-store connect: %w", err)
	}
	defer conn.Close()

	if err := json.NewEncoder(conn).Encode(req); err != nil {
		return fmt.Errorf("sq-store send: %w", err)
	}

	// Set read deadline so we don't hang forever if the sidecar stalls.
	conn.SetDeadline(time.Now().Add(readTimeout))

	// Read one line response. Use a larger buffer for diff responses with many paths.
	scanner := bufio.NewScanner(conn)
	scanner.Buffer(make([]byte, 64*1024), 1024*1024)
	if !scanner.Scan() {
		if err := scanner.Err(); err != nil {
			return fmt.Errorf("sq-store read: %w", err)
		}
		return fmt.Errorf("sq-store: empty response")
	}
	if err := json.Unmarshal(scanner.Bytes(), out); err != nil {
		return fmt.Errorf("sq-store parse: %w", err)
	}
	return nil
}
