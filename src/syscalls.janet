# Syscalls — FFI bindings for mount, umount2
# Linux-only. Requires root for actual mount operations.

# MS_RDONLY = 1, MNT_DETACH = 2 (from sys/mount.h)
(def MS_RDONLY 1)
(def MNT_DETACH 2)

# FFI bindings — only on Linux (FFI unavailable on macOS)
(var c-mount nil)
(var c-umount2 nil)
(try
  (when (= (os/which) :linux)
    (def lib (ffi/native))
    (def mount-sig (ffi/signature :default :int :string :string :string :ulong :ptr))
    (def umount-sig (ffi/signature :default :int :string :int))
    (set c-mount (ffi/jitfn (ffi/lookup lib "mount") mount-sig))
    (set c-umount2 (ffi/jitfn (ffi/lookup lib "umount2") umount-sig)))
  ([e] nil))

(defn mount-syscall [source target fstype flags data]
  "Call mount(2). Returns 0 on success, -1 on error."
  (if c-mount
    (c-mount source target fstype flags (or data 0))
    -1))

(defn umount-syscall [target flags]
  "Call umount2(2). Returns 0 on success, -1 on error."
  (if c-umount2
    (c-umount2 target flags)
    -1))
