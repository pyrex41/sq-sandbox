package squashd

// Syscall wrappers not available in core:sys/linux.
// Centralized here so a single `foreign import libc` is shared across the package.
//
// c_mount / c_umount2 REMOVED — unprivileged mode uses shared helper scripts
// (sq-mount-layer, sq-mount-overlay) via run_cmd().

import "core:c"

when ODIN_OS == .Linux {
	foreign import libc "system:c"
} else when ODIN_OS == .Darwin {
	foreign import libc "system:c"
}

foreign libc {
	@(link_name = "close")
	c_close :: proc(fd: c.int) -> c.int ---
	@(link_name = "flock")
	c_flock :: proc(fd: c.int, operation: c.int) -> c.int ---
}

// flock operations
LOCK_EX :: 2
LOCK_UN :: 8
