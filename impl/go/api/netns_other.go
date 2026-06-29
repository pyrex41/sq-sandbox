//go:build !linux

package api

import (
	"context"
	"fmt"
	"net"
)

// Stub for non-Linux builds (developer machines). The daemon only ships on
// Linux; this stub exists so `go build ./...` and `go test ./...` work on
// macOS dev boxes.
func dialInNetns(_ context.Context, _ int, _, _ string) (net.Conn, error) {
	return nil, fmt.Errorf("dialInNetns: setns not supported on this platform")
}
