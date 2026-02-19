# Syscalls — FFI bindings for mount, umount2, flock
# Linux-only. Requires root for actual mount operations.

# MS_RDONLY = 1, MNT_DETACH = 2 (from sys/mount.h)
(def MS_RDONLY 1)
(def MNT_DETACH 2)

# LOCK_EX = 2, LOCK_UN = 8 (from fcntl.h)
(def LOCK_EX 2)
(def LOCK_UN 8)

# O_RDWR = 2, O_CREAT = 64, O_CLOEXEC = 524288 (from fcntl.h)
(def O_RDWR 2)
(def O_CREAT 64)
(def O_CLOEXEC 524288)

# FFI bindings — only on Linux (FFI unavailable on macOS)
(var c-mount nil)
(var c-umount2 nil)
(var c-open nil)
(var c-close nil)
(var c-flock nil)
(try
  (when (= (os/which) :linux)
    (def lib (ffi/native))
    (def mount-sig (ffi/signature :default :int :string :string :string :ulong :ptr))
    (def umount-sig (ffi/signature :default :int :string :int))
    (set c-mount (ffi/jitfn (ffi/lookup lib "mount") mount-sig))
    (set c-umount2 (ffi/jitfn (ffi/lookup lib "umount2") umount-sig))
    (def open-sig (ffi/signature :default :int :string :int :int))
    (def close-sig (ffi/signature :default :int :int))
    (def flock-sig (ffi/signature :default :int :int :int))
    (set c-open (ffi/jitfn (ffi/lookup lib "open") open-sig))
    (set c-close (ffi/jitfn (ffi/lookup lib "close") close-sig))
    (set c-flock (ffi/jitfn (ffi/lookup lib "flock") flock-sig)))
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

(defn flock-allocate-counter
  "Atomically read-increment-write a counter file using flock(2).
   Returns the allocated value. Throws on failure.
   No shell — pure FFI. Linux only."
  [lock-path counter-path initial]
  (when (not (and c-open c-close c-flock))
    (error "flock not available (non-Linux?)"))
  (def fd (c-open lock-path (+ O_RDWR O_CREAT O_CLOEXEC) 384))  # 384 = 0600
  (when (< fd 0)
    (error (string/format "flock open %s failed" lock-path)))
  (var locked false)
  (def result
    (try
      (do
        (def rc (c-flock fd LOCK_EX))
        (when (not= rc 0)
          (error (string/format "flock LOCK_EX failed: %s" lock-path)))
        (set locked true)
        (var current initial)
        (def content (try (slurp counter-path) ([e] nil)))
        (when (and content (> (length (string/trim content)) 0))
          (set current (or (scan-number (string/trim content)) initial)))
        (when (< current initial)
          (set current initial))
        (def next-val (string (inc current)))
        (spit counter-path next-val)
        current)
      ([e]
        (when locked
          (try (c-flock fd LOCK_UN) ([e2] nil)))
        (try (c-close fd) ([e2] nil))
        (error (string e)))))
  (when locked
    (try (c-flock fd LOCK_UN) ([e] nil)))
  (try (c-close fd) ([e] nil))
  result)
