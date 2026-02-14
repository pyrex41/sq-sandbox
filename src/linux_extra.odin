package squashd

// Syscall wrappers not available in core:sys/linux.
// Centralized here so a single `foreign import libc` is shared across the package.
//
// Note: We use syscall numbers directly via c.int to avoid redeclaration
// conflicts with core:sys/posix (which declares open/openat).

import "core:c"

when ODIN_OS == .Linux {
	foreign import libc "system:c"
} else when ODIN_OS == .Darwin {
	foreign import libc "system:c"
}

foreign libc {
	@(link_name = "mount")
	c_mount :: proc(source: cstring, target: cstring, fstype: cstring, flags: c.ulong, data: rawptr) -> c.int ---
	@(link_name = "umount2")
	c_umount2 :: proc(target: cstring, flags: c.int) -> c.int ---
	@(link_name = "system")
	c_system :: proc(cmd: cstring) -> c.int ---
	@(link_name = "close")
	c_close :: proc(fd: c.int) -> c.int ---
	@(link_name = "flock")
	c_flock :: proc(fd: c.int, operation: c.int) -> c.int ---
	@(link_name = "syscall")
	c_syscall :: proc(number: c.long, #c_vararg args: ..c.ulong) -> c.long ---
}

// Use syscall() to call open directly
// Linux syscall numbers: open=2, close=3
when ODIN_OS == .Linux {
	SYS_OPEN :: 2
} else when ODIN_OS == .Darwin {
	SYS_OPEN :: 5
}

c_open :: proc(path: cstring, flags: c.int, mode: c.uint = 0) -> c.int {
	path_ptr := rawptr(path)
	return c.int(c_syscall(SYS_OPEN, c.ulong(uintptr(path_ptr)), c.ulong(flags), c.ulong(mode)))
}

// flock operations
LOCK_EX :: 2
LOCK_UN :: 8

// open flags
O_RDWR   :: 2
O_CREAT  :: 64
O_CLOEXEC :: 0o2000000
