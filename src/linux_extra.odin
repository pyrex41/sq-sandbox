package squashd

// Syscall wrappers not available in core:sys/linux.
// Centralized here so a single `foreign import libc` is shared across the package.

import "core:c"

foreign import libc "system:c"

foreign libc {
	@(link_name = "mount")
	c_mount :: proc(source: cstring, target: cstring, fstype: cstring, flags: c.ulong, data: rawptr) -> c.int ---
	@(link_name = "umount2")
	c_umount2 :: proc(target: cstring, flags: c.int) -> c.int ---
	@(link_name = "system")
	c_system :: proc(cmd: cstring) -> c.int ---
	@(link_name = "open")
	c_open :: proc(path: cstring, flags: c.int, #c_vararg args: ..u16) -> c.int ---
	@(link_name = "close")
	c_close :: proc(fd: c.int) -> c.int ---
	@(link_name = "flock")
	c_flock :: proc(fd: c.int, operation: c.int) -> c.int ---
}

// flock operations
LOCK_EX :: 2
LOCK_UN :: 8

// open flags
O_RDWR   :: 2
O_CREAT  :: 64
O_CLOEXEC :: 0o2000000
