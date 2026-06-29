//go:build linux

package api

import (
	"context"
	"fmt"
	"net"
	"runtime"
	"time"

	"golang.org/x/sys/unix"
)

// dialInNetns opens a TCP connection to addr from inside the network
// namespace owned by pid (looked up via /proc/<pid>/ns/net).
//
// It pins the calling goroutine to its OS thread, swaps the thread's netns,
// dials, and restores the original netns before unlocking. The returned
// connection is a regular socket FD — once established, byte transfer no
// longer depends on the thread's current netns, so the caller can copy
// freely from any goroutine.
//
// Critical: if Setns to the original namespace fails on the way out, the
// thread is left in the wrong netns. We deliberately leave the thread
// LOCKED in that case so the Go runtime retires it (LockOSThread without
// matching UnlockOSThread terminates the thread when the goroutine exits).
// That trades one OS thread for correctness — far cheaper than serving
// future requests from the wrong netns.
func dialInNetns(ctx context.Context, pid int, network, addr string) (net.Conn, error) {
	if pid <= 0 {
		return nil, fmt.Errorf("dialInNetns: invalid pid %d", pid)
	}

	type result struct {
		conn net.Conn
		err  error
	}
	ch := make(chan result, 1)

	go func() {
		runtime.LockOSThread()
		// Default: unlock when we leave this goroutine. We *only* override
		// this on a setns-restore failure (see below).
		unlock := true
		defer func() {
			if unlock {
				runtime.UnlockOSThread()
			}
		}()

		origFd, err := unix.Open("/proc/self/ns/net", unix.O_RDONLY|unix.O_CLOEXEC, 0)
		if err != nil {
			ch <- result{nil, fmt.Errorf("open self netns: %w", err)}
			return
		}
		defer unix.Close(origFd)

		targetPath := fmt.Sprintf("/proc/%d/ns/net", pid)
		targetFd, err := unix.Open(targetPath, unix.O_RDONLY|unix.O_CLOEXEC, 0)
		if err != nil {
			ch <- result{nil, fmt.Errorf("open %s: %w", targetPath, err)}
			return
		}
		defer unix.Close(targetFd)

		if err := unix.Setns(targetFd, unix.CLONE_NEWNET); err != nil {
			ch <- result{nil, fmt.Errorf("setns into pid %d: %w", pid, err)}
			return
		}

		// Dial inside the target netns. Use a short fixed timeout — the
		// destination is in-process, on loopback. Anything slower than
		// 5s means websockify is dead.
		dialer := &net.Dialer{Timeout: 5 * time.Second}
		conn, dialErr := dialer.DialContext(ctx, network, addr)

		// Restore original netns. If this fails the thread is poisoned;
		// retire it instead of returning it to the pool.
		if rerr := unix.Setns(origFd, unix.CLONE_NEWNET); rerr != nil {
			unlock = false
			if dialErr == nil {
				_ = conn.Close()
			}
			ch <- result{nil, fmt.Errorf("restore netns: %w (thread retired)", rerr)}
			return
		}

		if dialErr != nil {
			ch <- result{nil, dialErr}
			return
		}
		ch <- result{conn, nil}
	}()

	select {
	case r := <-ch:
		return r.conn, r.err
	case <-ctx.Done():
		return nil, ctx.Err()
	}
}
