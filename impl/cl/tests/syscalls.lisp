(in-package #:squashd-syscalls)

(declaim (optimize (speed 3) (safety 1)))

;;; ── Mount flags ──────────────────────────────────────────────────────

(defconstant +ms-rdonly+     #x0001)
(defconstant +ms-nosuid+     #x0002)
(defconstant +ms-nodev+      #x0004)
(defconstant +ms-noexec+     #x0008)
(defconstant +ms-remount+    #x0020)
(defconstant +ms-bind+       #x1000)

;;; ── Umount flags ─────────────────────────────────────────────────────

(defconstant +mnt-detach+    #x0002)
(defconstant +mnt-force+     #x0001)

;;; ── CFFI Definitions ─────────────────────────────────────────────────

(cffi:defcfun ("mount" %mount) :int
  (source :string) (target :string) (fstype :string)
  (flags :unsigned-long) (data :pointer))

(cffi:defcfun ("umount2" %umount2) :int
  (target :string) (flags :int))

(cffi:defcfun ("fork" %fork) :int)

(cffi:defcfun ("waitpid" %waitpid) :int
  (pid :int) (status :pointer) (options :int))

(cffi:defcfun ("pipe" %pipe) :int
  (fds :pointer))

(cffi:defcfun ("close" %close) :int
  (fd :int))

(cffi:defcfun ("read" %read) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("write" %sys-write) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("getpid" %getpid) :int)

(cffi:defcfun ("_exit" %exit) :void
  (status :int))

;;; ── Errno ────────────────────────────────────────────────────────────

(cffi:defcvar ("errno" %errno) :int)

(declaim (inline get-errno))
(defun get-errno ()
  (declare (optimize (speed 3) (safety 0)))
  %errno)
