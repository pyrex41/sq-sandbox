(in-package #:squashd)

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

;;; ── Clone/unshare flags ──────────────────────────────────────────────

(defconstant +clone-newns+   #x00020000)
(defconstant +clone-newpid+  #x20000000)
(defconstant +clone-newipc+  #x08000000)
(defconstant +clone-newuts+  #x04000000)
(defconstant +clone-newnet+  #x40000000)

;;; ── Signals ──────────────────────────────────────────────────────────

(defconstant +sigkill+ 9)
(defconstant +sigterm+ 15)

;;; ── Open flags ───────────────────────────────────────────────────────

(defconstant +o-rdonly+  #x0000)
(defconstant +o-wronly+  #x0001)
(defconstant +o-rdwr+    #x0002)
(defconstant +o-creat+   #x0040)
(defconstant +o-trunc+   #x0200)

;;; ── Flock operations ─────────────────────────────────────────────────

(defconstant +lock-sh+ 1)
(defconstant +lock-ex+ 2)
(defconstant +lock-un+ 8)
(defconstant +lock-nb+ 4)

;;; ── Waitpid options ──────────────────────────────────────────────────

(defconstant +wnohang+ 1)

;;; ── Poll events ──────────────────────────────────────────────────────

(defconstant +pollin+  #x0001)
(defconstant +pollhup+ #x0010)

;;; ── CFFI Definitions ─────────────────────────────────────────────────

(cffi:defcfun ("mount" %mount) :int
  (source :string) (target :string) (fstype :string)
  (flags :unsigned-long) (data :pointer))

(cffi:defcfun ("umount2" %umount2) :int
  (target :string) (flags :int))

(cffi:defcfun ("fork" %fork) :int)

(cffi:defcfun ("execve" %execve) :int
  (path :string) (argv :pointer) (envp :pointer))

(cffi:defcfun ("waitpid" %waitpid) :int
  (pid :int) (status :pointer) (options :int))

(cffi:defcfun ("pipe" %pipe) :int
  (fds :pointer))

(cffi:defcfun ("poll" %poll) :int
  (fds :pointer) (nfds :unsigned-long) (timeout :int))

(cffi:defcfun ("close" %close) :int
  (fd :int))

(cffi:defcfun ("read" %read) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("write" %sys-write) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("dup2" %dup2) :int
  (oldfd :int) (newfd :int))

(cffi:defcfun ("flock" %flock) :int
  (fd :int) (operation :int))

(cffi:defcfun ("open" %sys-open) :int
  (path :string) (flags :int) (mode :int))

(cffi:defcfun ("getpid" %getpid) :int)

(cffi:defcfun ("_exit" %exit) :void
  (status :int))

(cffi:defcfun ("unshare" %unshare) :int
  (flags :int))

(cffi:defcfun ("chroot" %chroot) :int
  (path :string))

(cffi:defcfun ("chdir" %chdir) :int
  (path :string))

(cffi:defcfun ("setns" %setns) :int
  (fd :int) (nstype :int))

(cffi:defcfun ("kill" %kill) :int
  (pid :int) (sig :int))

;;; ── Errno ────────────────────────────────────────────────────────────

(cffi:defcvar ("errno" %errno) :int)

(declaim (inline get-errno))
(defun get-errno ()
  (declare (optimize (speed 3) (safety 0)))
  %errno)

;;; ── Shell-out helpers ────────────────────────────────────────────────

(defun run-command (program &rest args)
  "Run a command, return (values exit-code stdout-string stderr-string).
   For iptables/ip commands where we don't care about output."
  (declare (optimize (speed 2) (safety 2)))
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (cons program args)
                         :output :string :error-output :string
                         :ignore-error-status t)
    (values exit-code stdout stderr)))

(defun run-command-ok (program &rest args)
  "Run a command, return T if exit code 0."
  (zerop (apply #'run-command program args)))
