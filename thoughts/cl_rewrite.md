# sq-sandbox Common Lisp Rewrite — SBCL, Fully Optimized

## Project Summary

Rewrite the `sq-sandbox` container runtime into a **single SBCL image** plus a **Go sidecar**:

1. **`squashd`** — a saved SBCL core image (`sb-ext:save-lisp-and-die`). The main daemon: HTTP API, sandbox lifecycle, reaper, S3 sync, init/recovery. Approximately 4000-6000 lines of Common Lisp.

2. **`sq-secret-proxy-https`** — the existing Go binary, kept as a child process. Same rationale as all other prompts.

3. **`sq-guest-agent`** — written in C, not Lisp. The guest agent is PID 1 in a Firecracker microVM where the entire rootfs should be a few MB. An SBCL image is 20-50MB. This is disqualifying for the guest agent, full stop.

Shell scripts (`sq-mkbase`, `sq-mkmod`, `sq-mkvm`, `sq-ctl`) remain as-is.

The existing repo is at https://github.com/pyrex41/sq-sandbox. Read every file before starting.

---

## Why Common Lisp / SBCL

SBCL's native compiler produces machine code competitive with C for numerical work and close to Go for systems work. The condition/restart system allows error recovery without unwinding — when mount step 4 of 6 fails, you can fix the problem and resume from step 4 rather than tearing everything down and starting over. CLOS with method combinations gives clean extensibility for the backend abstraction (chroot vs Firecracker). And the image-based deployment model means startup is instant — no JIT warmup, no interpreter overhead, just `mmap` the core and jump.

**The honest tradeoffs:**
- **Binary size**: 20-50MB compressed image. Disqualifying for Firecracker guest (hence C guest agent). Fine for the host daemon — the Docker image with Alpine + tools is already 200MB+.
- **GC pauses**: SBCL's generational GC can pause 1-10ms. For a container management daemon doing maybe 10 requests/second, this is invisible. Not suitable for microsecond-latency systems, but we're calling `fork()` and `mount()` which take milliseconds each anyway.
- **Ecosystem perception**: "Lisp for a container runtime" will raise eyebrows. The code needs to be clean enough to justify it.

---

## Performance Strategy

### Compilation Policy

Three tiers of optimization:

```lisp
;;; HOT PATH — syscall wrappers, mount operations, exec, HTTP request handling
;;; These run on every API call. Maximize speed, minimize safety checks.
(declaim (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0)))

;;; WARM PATH — sandbox management, state transitions, JSON serialization
;;; These run frequently but aren't the bottleneck.
(declaim (optimize (speed 2) (safety 2) (debug 2)))

;;; COLD PATH — init, recovery, config loading, reaper
;;; Run once or every 10 seconds. Maximize safety for correctness.
(declaim (optimize (speed 1) (safety 3) (debug 3)))
```

Per-function override where needed:
```lisp
(defun mount-squashfs (source target)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-string source target))
  ...)
```

### Type Declarations Everywhere on Hot Paths

SBCL's type inference is good but not omniscient. On hot paths, declare everything:

```lisp
(defstruct (sandbox (:constructor %make-sandbox))
  (id          "" :type simple-string)
  (state       :creating :type sandbox-state)
  (mounts      nil :type (or null sandbox-mounts))
  (netns       nil :type (or null netns-handle))
  (cgroup      nil :type (or null cgroup-handle))
  (created     0 :type (unsigned-byte 64))
  (last-active 0 :type (unsigned-byte 64))
  (exec-count  0 :type fixnum)
  (max-lifetime-s 0 :type fixnum)
  (lock        (bt:make-lock) :type bt:lock))
```

Use `defstruct`, not `defclass`, for performance-critical types. SBCL generates direct slot access for structs (single instruction) vs method dispatch for CLOS slots. Reserve `defclass` for the backend abstraction where polymorphism is needed.

### Stack Allocation

For short-lived objects on hot paths, use `dynamic-extent`:

```lisp
(defun build-overlay-opts (lower-components upper-dir work-dir)
  (declare (optimize (speed 3) (safety 0)))
  (let ((parts (list "lowerdir=" (join-strings lower-components ":")
                     ",upperdir=" upper-dir
                     ",workdir=" work-dir)))
    (declare (dynamic-extent parts))
    (apply #'concatenate 'string parts)))
```

### CFFI with :convention :cdecl and Inlining

```lisp
(cffi:defcfun ("mount" %mount) :int
  (source :string) (target :string) (fstype :string)
  (flags :unsigned-long) (data :pointer))

;;; Inline the wrapper
(declaim (inline mount-fs))
(defun mount-fs (source target fstype &key (flags 0) data)
  (declare (optimize (speed 3) (safety 0))
           (type simple-string source target fstype))
  (let ((rc (%mount source target fstype flags (or data (cffi:null-pointer)))))
    (declare (type fixnum rc))
    (unless (zerop rc)
      (error 'mount-error :source source :target target :errno (get-errno)))))
```

### Avoid Consing on Hot Paths

The #1 performance killer in Lisp is unnecessary allocation (consing) that triggers GC. On hot paths:

- Use `with-output-to-string` + `write-string` instead of `format` for string construction
- Use `simple-string` and `simple-base-string` types
- Pre-allocate buffers for exec stdout/stderr (stack-allocated `(unsigned-byte 8)` arrays)
- Use `sb-ext:atomic-incf` for counters instead of lock+increment
- Use `the` for type assertions that help the compiler eliminate boxing

```lisp
;;; BAD — conses a new string every time
(format nil "~A/sandboxes/~A" data-dir id)

;;; GOOD — pre-allocated, zero cons on hot path
(defun sandbox-path (data-dir id buf)
  (declare (optimize (speed 3) (safety 0))
           (type simple-string data-dir id)
           (type (simple-array character (*)) buf))
  (setf (fill-pointer buf) 0)
  (write-string data-dir buf)
  (write-string "/sandboxes/" buf)
  (write-string id buf)
  buf)
```

### GC Tuning

At startup:

```lisp
;;; Increase nursery to reduce minor GC frequency
(setf (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))  ; 64MB nursery

;;; Pre-allocate enough heap for expected sandbox count
;;; Each sandbox ~100KB of Lisp objects, 100 sandboxes = 10MB
;;; Give ourselves 256MB before the GC gets aggressive
(sb-ext:gc :full t)
```

### Thread Pool Instead of Thread-Per-Request

Use a fixed thread pool (matching Go's goroutine model conceptually):

```lisp
(defvar *thread-pool* nil)
(defvar *work-queue* (sb-concurrency:make-queue))

(defun init-thread-pool (n)
  (setf *thread-pool*
        (loop repeat n
              collect (bt:make-thread #'worker-loop :name "squashd-worker"))))

(defun worker-loop ()
  (loop
    (let ((work (sb-concurrency:dequeue *work-queue*)))
      (when work (funcall work)))))
```

Or simply use **Woo** as the Clack HTTP backend — it uses `libev` and a worker thread pool internally, and benchmarks at 20,000+ req/s on a single core. For 10 req/s of container management, this is absurd overkill, but it means zero HTTP performance concerns. Started via `clack:clackup` with `:server :woo`, so swapping to Hunchentoot for local development is a one-keyword change.

---

## Dependencies (Quicklisp)

```lisp
;;; System definition: squashd.asd
(defsystem "squashd"
  :depends-on ("cffi"              ; C FFI for syscalls
               "clack"             ; HTTP server abstraction (start/stop any backend)
               "woo"               ; HTTP server backend (libev-based, fastest CL server)
               "lack"              ; Application builder: middleware composition, env/response protocol
               "ningle"            ; Lightweight Sinatra-style routing (builds on Lack)
               "jonathan"          ; Fast JSON (SAX-style, faster than cl-json)
               "ironclad"          ; Crypto: SHA256, HMAC for S3 SigV4
               "dexador"           ; HTTP client for S3
               "bordeaux-threads"  ; Threading
               "alexandria"        ; Utility library
               "local-time"        ; Time formatting (ISO 8601)
               "cl-ppcre"          ; Regex (for parsing, validation)
               "trivial-mimes"     ; MIME types (optional)
               "log4cl")           ; Logging
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "validate")
               (:file "conditions")
               (:file "syscalls")
               (:file "mounts")
               (:file "cgroup")
               (:file "netns")
               (:file "exec")
               (:file "sandbox")
               (:file "manager")
               (:file "meta")
               (:file "modules")
               (:file "snapshot")
               (:file "secrets")
               (:file "s3")
               (:file "reaper")
               (:file "init")
               (:file "api")
               (:file "main")))
```

**Why these choices:**
- **Clack + Lack** (Fukamachi stack): Clack is the HTTP server abstraction layer — like Rack (Ruby) or WSGI (Python). It decouples the app from the server, so switching between Woo, Hunchentoot, etc. is a one-keyword change. Lack defines the application protocol: apps are functions `(lambda (env) ...)` returning `(status headers body)`. Middleware composes via `lack:builder`. Ningle adds Sinatra-style routing on top. This is the standard CL web stack. See [How to build a web app with Clack/Lack](https://fukamachi.hashnode.dev/how-to-build-a-web-app-with-clack-and-lack-1).
- **Woo** as the Clack backend: Woo uses libev + epoll, handles 20k req/s. Hunchentoot is threaded, ~3k req/s. Both are massive overkill for this API, but Woo is fire-and-forget with no tuning needed. Started via `clack:clackup` with `:server :woo`.
- **Jonathan** over cl-json: Jonathan is 3-5x faster for JSON encoding. Uses SAX parsing and pre-compiled serializers.
- **Dexador** over Drakma: Dexador has connection pooling, automatic decompression, and a cleaner API for the S3 HTTP calls.
- **CFFI** over sb-alien: CFFI is portable and well-maintained. sb-alien works but CFFI has better string handling and memory management.

---

## Package Structure

```lisp
;;; packages.lisp
(defpackage #:squashd
  (:use #:cl #:alexandria)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:jojo #:jonathan)
                    (#:lt #:local-time))
  (:export #:main))
```

Single package. This is a self-contained daemon, not a library. No need for package-per-module complexity.

---

## Condition System: The Unique Advantage

Common Lisp's condition/restart system is the single feature no other language in this comparison offers. It allows **non-local error recovery without stack unwinding**.

### The Problem It Solves

Sandbox creation is a 6-step process: tmpfs → squashfs layers → overlay → cgroup → netns → metadata. In every other language, if step 4 fails, you either:
1. Unwind and clean up everything (Rust Drop, Zig errdefer, Go defers)
2. Ignore the error and continue with partial state (shell `|| true`)

With conditions and restarts, you can:
3. **Fix the problem and resume from step 4** without tearing down steps 1-3.

```lisp
(define-condition mount-error (error)
  ((source :initarg :source :reader mount-error-source)
   (target :initarg :target :reader mount-error-target)
   (errno  :initarg :errno  :reader mount-error-errno))
  (:report (lambda (c s)
             (format s "mount failed: ~A on ~A (errno ~D)"
                     (mount-error-source c)
                     (mount-error-target c)
                     (mount-error-errno c)))))

(define-condition module-not-found (error)
  ((name :initarg :name :reader module-not-found-name))
  (:report (lambda (c s)
             (format s "module not found: ~A" (module-not-found-name c)))))

(define-condition sandbox-error (error)
  ((id :initarg :id :reader sandbox-error-id)
   (message :initarg :message :reader sandbox-error-message))
  (:report (lambda (c s)
             (format s "sandbox ~A: ~A" (sandbox-error-id c) (sandbox-error-message c)))))

(define-condition cgroup-setup-failed (warning)
  ((id :initarg :id :reader cgroup-error-id))
  (:report (lambda (c s)
             (format s "cgroup setup failed for ~A (continuing without limits)"
                     (cgroup-error-id c)))))
```

### Restarts for Sandbox Creation

```lisp
(defun create-sandbox (manager id &key owner layers task cpu memory-mb
                                       max-lifetime-s allow-net)
  (restart-case
      (%create-sandbox-inner manager id
                             :owner owner :layers layers :task task
                             :cpu cpu :memory-mb memory-mb
                             :max-lifetime-s max-lifetime-s
                             :allow-net allow-net)
    (retry-create ()
      :report "Retry sandbox creation from the beginning"
      (create-sandbox manager id :owner owner :layers layers :task task
                      :cpu cpu :memory-mb memory-mb
                      :max-lifetime-s max-lifetime-s :allow-net allow-net))
    (skip-sandbox ()
      :report "Skip this sandbox creation"
      (values nil :skipped))))

(defun mount-squashfs-layers (sandbox layers)
  "Mount each squashfs module. If a module is missing, offer to pull from S3."
  (loop for layer in layers
        for mount-point = (squashfs-mount-point sandbox layer)
        collect
        (restart-case
            (mount-squashfs (module-path layer) mount-point)
          (pull-from-s3-and-retry ()
            :report (lambda (s) (format s "Pull ~A from S3 and retry" layer))
            :test (lambda (c) (declare (ignore c)) *s3-client*)
            (s3-pull-module *s3-client* layer)
            (mount-squashfs (module-path layer) mount-point))
          (skip-layer ()
            :report (lambda (s) (format s "Skip layer ~A" layer))
            nil))))
```

### How This Works in Practice

When running interactively (REPL connected to running daemon via Swank/SLIME):

```
;; Module isn't on disk, but it's in S3
ERROR: module not found: 100-python312

Available restarts:
  0: [PULL-FROM-S3-AND-RETRY] Pull 100-python312 from S3 and retry
  1: [SKIP-LAYER] Skip layer 100-python312
  2: [RETRY-CREATE] Retry sandbox creation from the beginning
  3: [SKIP-SANDBOX] Skip this sandbox creation
  4: [ABORT] Abort

;; Choose restart 0 — pulls from S3, mounts, continues
;; Steps 1-3 (tmpfs, earlier modules, etc.) are still intact
```

When running as a daemon (no interactive debugger), install a handler that automatically invokes the right restart:

```lisp
(defun production-handler (condition)
  "Automatic restart selection for production (non-interactive) mode."
  (typecase condition
    (module-not-found
     (when *s3-client*
       (invoke-restart 'pull-from-s3-and-retry)))
    (cgroup-setup-failed
     ;; Cgroups are optional — just warn and continue
     (invoke-restart 'muffle-warning))
    (mount-error
     ;; Can't recover from mount failures in production
     ;; Let it unwind to the cleanup handler
     nil)))

(defun run-production ()
  (handler-bind ((module-not-found #'production-handler)
                 (cgroup-setup-failed #'production-handler))
    (main-loop)))
```

**This is genuinely powerful.** In Rust/Zig/Go/Odin, a missing module during sandbox creation means: unwind, clean up 3 mounts, re-pull module, retry from scratch. In CL, you pull the module and continue from exactly where you left off. The 3 existing mounts stay up. Zero wasted work.

The flip side: this pattern requires discipline. If you signal a condition after acquiring resources but before setting up cleanup, a restart that resumes can leak. The rule is: **always establish cleanup before signaling recoverable conditions.**

---

## Syscall Layer (`syscalls.lisp`)

```lisp
(in-package #:squashd)

(declaim (optimize (speed 3) (safety 1)))

;;; ── Constants ─────────────────────────────────────────────────────────

(defconstant +ms-rdonly+    #x0001)
(defconstant +ms-nosuid+    #x0002)
(defconstant +ms-nodev+     #x0004)
(defconstant +ms-noexec+    #x0008)
(defconstant +mnt-detach+   #x0002)
(defconstant +clone-newns+  #x00020000)
(defconstant +clone-newpid+ #x20000000)
(defconstant +clone-newipc+ #x08000000)
(defconstant +clone-newuts+ #x04000000)
(defconstant +clone-newnet+ #x40000000)
(defconstant +sigkill+ 9)
(defconstant +sigterm+ 15)

;;; ── CFFI Definitions ──────────────────────────────────────────────────

(cffi:defcfun ("mount" %mount) :int
  (source :string) (target :string) (fstype :string)
  (flags :unsigned-long) (data :string))

(cffi:defcfun ("umount2" %umount2) :int
  (target :string) (flags :int))

(cffi:defcfun ("unshare" %unshare) :int
  (flags :int))

(cffi:defcfun ("chroot" %chroot) :int
  (path :string))

(cffi:defcfun ("chdir" %chdir) :int
  (path :string))

(cffi:defcfun ("setns" %setns) :int
  (fd :int) (nstype :int))

(cffi:defcfun ("fork" %fork) :int)

(cffi:defcfun ("execve" %execve) :int
  (path :string) (argv :pointer) (envp :pointer))

(cffi:defcfun ("waitpid" %waitpid) :int
  (pid :int) (status :pointer) (options :int))

(cffi:defcfun ("kill" %kill) :int
  (pid :int) (sig :int))

(cffi:defcfun ("pipe" %pipe) :int
  (fds :pointer))

(cffi:defcfun ("close" %close) :int
  (fd :int))

(cffi:defcfun ("read" %read) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("write" %sys-write) :long
  (fd :int) (buf :pointer) (count :unsigned-long))

(cffi:defcfun ("poll" %poll) :int
  (fds :pointer) (nfds :unsigned-long) (timeout :int))

(cffi:defcfun ("dup2" %dup2) :int
  (oldfd :int) (newfd :int))

(cffi:defcfun ("flock" %flock) :int
  (fd :int) (operation :int))

(cffi:defcfun ("open" %sys-open) :int
  (path :string) (flags :int) (mode :int))

(cffi:defcfun ("getpid" %getpid) :int)

(cffi:defcfun ("_exit" %exit) :void
  (status :int))

;;; ── Errno ─────────────────────────────────────────────────────────────

(cffi:defcvar ("errno" %errno) :int)

(declaim (inline get-errno))
(defun get-errno ()
  (declare (optimize (speed 3) (safety 0)))
  %errno)

;;; ── Helpers ───────────────────────────────────────────────────────────

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
```

### Mount Operations (`mounts.lisp`)

```lisp
(in-package #:squashd)

;;; Use defstruct for performance — direct slot access, no CLOS dispatch

(defstruct squashfs-mount
  (mount-point "" :type simple-string)
  (active-p t :type boolean))

(defstruct tmpfs-mount
  (mount-point "" :type simple-string)
  (active-p t :type boolean))

(defstruct overlay-mount
  (merged-path "" :type simple-string)
  (active-p t :type boolean))

(defstruct sandbox-mounts
  (squashfs-mounts #() :type simple-vector)  ; vector of squashfs-mount
  (snapshot-mount nil :type (or null squashfs-mount))
  (tmpfs nil :type (or null tmpfs-mount))
  (overlay nil :type (or null overlay-mount)))

;;; ── Mount ─────────────────────────────────────────────────────────────

(defun mount-squashfs (source-path mount-point)
  (declare (optimize (speed 3) (safety 1))
           (type simple-string source-path mount-point))
  (ensure-directories-exist (concatenate 'string mount-point "/"))
  (let ((rc (%mount source-path mount-point "squashfs" +ms-rdonly+ (cffi:null-pointer))))
    (unless (zerop rc)
      (error 'mount-error :source source-path :target mount-point :errno (get-errno))))
  (make-squashfs-mount :mount-point mount-point))

(defun unmount-squashfs (mount)
  (declare (optimize (speed 3) (safety 0)))
  (when (and mount (squashfs-mount-active-p mount))
    (%umount2 (squashfs-mount-mount-point mount) +mnt-detach+)
    (setf (squashfs-mount-active-p mount) nil)))

(defun mount-tmpfs (mount-point size-mb)
  (declare (optimize (speed 3) (safety 1))
           (type simple-string mount-point)
           (type fixnum size-mb))
  (ensure-directories-exist (concatenate 'string mount-point "/"))
  (let* ((opts (format nil "size=~Dm" size-mb))
         (rc (%mount "tmpfs" mount-point "tmpfs" 0 opts)))
    (unless (zerop rc)
      (error 'mount-error :source "tmpfs" :target mount-point :errno (get-errno))))
  ;; Create overlay subdirs
  (ensure-directories-exist (concatenate 'string mount-point "/data/"))
  (ensure-directories-exist (concatenate 'string mount-point "/work/"))
  (make-tmpfs-mount :mount-point mount-point))

(defun unmount-tmpfs (mount)
  (declare (optimize (speed 3) (safety 0)))
  (when (and mount (tmpfs-mount-active-p mount))
    (%umount2 (tmpfs-mount-mount-point mount) +mnt-detach+)
    (setf (tmpfs-mount-active-p mount) nil)))

(defun mount-overlay (lower-components upper-data work merged)
  (declare (optimize (speed 3) (safety 1)))
  (ensure-directories-exist (concatenate 'string merged "/"))
  (let* ((lowerdir (format nil "~{~A~^:~}" lower-components))
         (opts (format nil "lowerdir=~A,upperdir=~A,workdir=~A"
                       lowerdir upper-data work))
         (rc (%mount "overlay" merged "overlay" 0 opts)))
    (unless (zerop rc)
      (error 'mount-error :source "overlay" :target merged :errno (get-errno))))
  (make-overlay-mount :merged-path merged))

(defun unmount-overlay (mount)
  (declare (optimize (speed 3) (safety 0)))
  (when (and mount (overlay-mount-active-p mount))
    (%umount2 (overlay-mount-merged-path mount) +mnt-detach+)
    (setf (overlay-mount-active-p mount) nil)))

;;; ── Ordered teardown ──────────────────────────────────────────────────

(defun destroy-sandbox-mounts (mounts)
  "Unmount everything in reverse creation order. Never signals."
  (declare (optimize (speed 2) (safety 3)))  ; safety here — this must not fail
  (when mounts
    ;; 1. Overlay first (references lower layers)
    (ignore-errors (unmount-overlay (sandbox-mounts-overlay mounts)))
    ;; 2. Snapshot layer
    (ignore-errors (unmount-squashfs (sandbox-mounts-snapshot-mount mounts)))
    ;; 3. Tmpfs
    (ignore-errors (unmount-tmpfs (sandbox-mounts-tmpfs mounts)))
    ;; 4. Squashfs layers in reverse
    (let ((sqfs (sandbox-mounts-squashfs-mounts mounts)))
      (loop for i from (1- (length sqfs)) downto 0
            do (ignore-errors (unmount-squashfs (aref sqfs i)))))))
```

### Sandbox Creation with Cleanup Macros

```lisp
;;; WITH-CLEANUP: macro that guarantees cleanup on non-local exit
;;; but does NOT clean up on normal return (ownership transfers out)

(defmacro with-rollback-on-error ((var init-form cleanup-form) &body body)
  "Evaluate INIT-FORM, bind to VAR. If BODY signals, run CLEANUP-FORM.
   If BODY returns normally, VAR's ownership transfers to caller."
  (let ((ok (gensym "OK")))
    `(let ((,var ,init-form)
           (,ok nil))
       (unwind-protect
            (prog1 (progn ,@body)
              (setf ,ok t))
         (unless ,ok
           (ignore-errors ,cleanup-form))))))

(defun %create-sandbox-inner (manager id &key owner layers task cpu memory-mb
                                             max-lifetime-s allow-net)
  (declare (optimize (speed 2) (safety 2)))

  ;; Validate
  (unless (valid-id-p id) (error 'sandbox-error :id id :message "invalid id"))
  (when (gethash id (manager-sandboxes manager))
    (error 'sandbox-error :id id :message "already exists"))
  (when (>= (hash-table-count (manager-sandboxes manager))
            (config-max-sandboxes (manager-config manager)))
    (error 'sandbox-error :id id :message "sandbox limit reached"))

  (let* ((config (manager-config manager))
         (layers (or layers '("000-base-alpine")))
         (sandbox-dir (format nil "~A/sandboxes/~A" (config-data-dir config) id))
         (upper-path (format nil "~A/upper" sandbox-dir))
         (merged-path (format nil "~A/merged" sandbox-dir))
         (meta-dir (format nil "~A/.meta/log" sandbox-dir)))

    ;; Create directory tree
    (ensure-directories-exist (format nil "~A/" meta-dir))
    (ensure-directories-exist (format nil "~A/images/" sandbox-dir))
    (ensure-directories-exist (format nil "~A/snapshots/" sandbox-dir))

    ;; 1. Mount tmpfs
    (with-rollback-on-error (tmpfs
                             (mount-tmpfs upper-path (or (config-upper-limit-mb config) 512))
                             (unmount-tmpfs tmpfs))

      ;; 2. Mount squashfs layers
      (let ((sqfs-mounts (make-array (length layers))))
        ;; On error, unmount all mounted layers
        (unwind-protect
             (progn
               (loop for layer in layers
                     for i from 0
                     for mod-path = (format nil "~A/~A.squashfs"
                                            (modules-dir config) layer)
                     for mp = (format nil "~A/images/~A.squashfs"
                                      sandbox-dir layer)
                     do (setf (aref sqfs-mounts i)
                              (restart-case
                                  (mount-squashfs mod-path mp)
                                (pull-from-s3-and-retry ()
                                  :report (lambda (s) (format s "Pull ~A from S3" layer))
                                  :test (lambda (c) (declare (ignore c)) *s3-client*)
                                  (s3-pull-module *s3-client* layer)
                                  (mount-squashfs mod-path mp)))))

               ;; 3. Overlay
               (let ((lower-components (build-lower-components sqfs-mounts nil layers)))
                 (with-rollback-on-error (overlay
                                          (mount-overlay lower-components
                                                         (format nil "~A/data" upper-path)
                                                         (format nil "~A/work" upper-path)
                                                         merged-path)
                                          (unmount-overlay overlay))

                   ;; 4. Cgroup (may fail — that's ok)
                   (let ((cgroup (handler-case
                                     (create-cgroup id (or cpu 2.0) (or memory-mb 1024))
                                   (error (e)
                                     (warn 'cgroup-setup-failed :id id)
                                     nil))))

                     ;; 5. Network namespace
                     (with-rollback-on-error (netns
                                              (setup-netns config id allow-net)
                                              (teardown-netns netns))

                       ;; 6. Seed resolv.conf, inject secrets, write metadata
                       (seed-resolv-conf config sandbox-dir netns)
                       (inject-secret-placeholders config sandbox-dir netns)
                       (write-sandbox-meta config id
                                           :owner (or owner "anon")
                                           :layers layers
                                           :task (or task "")
                                           :cpu (or cpu 2.0)
                                           :memory-mb (or memory-mb 1024)
                                           :max-lifetime-s (or max-lifetime-s 0)
                                           :allow-net allow-net)

                       ;; Success — build and return the sandbox
                       (let ((sandbox
                               (%make-sandbox
                                :id id
                                :state :ready
                                :mounts (make-sandbox-mounts
                                         :squashfs-mounts sqfs-mounts
                                         :tmpfs tmpfs
                                         :overlay overlay)
                                :netns netns
                                :cgroup cgroup
                                :created (get-unix-time)
                                :last-active (get-unix-time)
                                :max-lifetime-s (or max-lifetime-s 0))))
                         sandbox))))))

          ;; Cleanup squashfs on error
          (loop for m across sqfs-mounts
                when m do (ignore-errors (unmount-squashfs m))))))))
```

The nesting is deep. This is the tradeoff for manual cleanup without destructors. An alternative style uses a flat `prog1` / `unwind-protect` chain — either works.

### Network Namespace (`netns.lisp`)

Shell out to `ip` and `iptables`. Same pragmatic choice as Zig and Odin prompts.

```lisp
(defstruct netns-handle
  (name     "" :type simple-string)
  (index    0  :type (unsigned-byte 8))
  (veth-host "" :type simple-string)
  (chain-name nil :type (or null simple-string))
  (host-dns nil :type (or null simple-string)))

(defun setup-netns (config id allow-net)
  "Set up network namespace with veth pair, NAT, DNS forwarding, optional egress filtering."
  (let* ((index (allocate-netns-index config))
         (name (format nil "squash-~A" id))
         (veth-host (format nil "sq-~A-h" id))
         (veth-sandbox (format nil "sq-~A-s" id))
         (subnet (format nil "10.200.~D.0/30" index))
         (host-addr (format nil "10.200.~D.1/30" index))
         (sandbox-addr (format nil "10.200.~D.2/30" index))
         (gateway (format nil "10.200.~D.1" index)))

    ;; Create netns
    (unless (run-command-ok "ip" "netns" "add" name)
      (error 'sandbox-error :id id :message "failed to create netns"))

    ;; Create veth pair
    (unless (run-command-ok "ip" "link" "add" veth-host "type" "veth"
                            "peer" "name" veth-sandbox)
      (run-command "ip" "netns" "delete" name)
      (error 'sandbox-error :id id :message "failed to create veth"))

    ;; Move sandbox end into netns
    (run-command-ok "ip" "link" "set" veth-sandbox "netns" name)

    ;; Configure host side
    (run-command-ok "ip" "addr" "add" host-addr "dev" veth-host)
    (run-command-ok "ip" "link" "set" veth-host "up")

    ;; Configure sandbox side
    (run-command-ok "ip" "netns" "exec" name "ip" "addr" "add" sandbox-addr "dev" veth-sandbox)
    (run-command-ok "ip" "netns" "exec" name "ip" "link" "set" veth-sandbox "up")
    (run-command-ok "ip" "netns" "exec" name "ip" "link" "set" "lo" "up")
    (run-command-ok "ip" "netns" "exec" name "ip" "route" "add" "default" "via" gateway)

    ;; IP forwarding + NAT + DNS DNAT
    (write-string-to-file "/proc/sys/net/ipv4/ip_forward" "1")
    (run-command-ok "iptables" "-t" "nat" "-A" "POSTROUTING" "-s" subnet "-j" "MASQUERADE")
    (let ((host-dns (parse-first-nameserver "/etc/resolv.conf")))
      (when host-dns
        (run-command-ok "iptables" "-t" "nat" "-A" "PREROUTING"
                        "-s" subnet "-d" gateway "-p" "udp" "--dport" "53"
                        "-j" "DNAT" "--to-destination" host-dns)
        (run-command-ok "iptables" "-t" "nat" "-A" "PREROUTING"
                        "-s" subnet "-d" gateway "-p" "tcp" "--dport" "53"
                        "-j" "DNAT" "--to-destination" host-dns))

      ;; Egress filtering
      (let ((chain-name (when (and allow-net (not (null allow-net)))
                          (apply-egress-rules id veth-host allow-net))))

        (make-netns-handle :name name :index index :veth-host veth-host
                           :chain-name chain-name :host-dns host-dns)))))

(defun teardown-netns (handle)
  "Teardown network namespace. Ignores all errors."
  (when handle
    (let ((subnet (format nil "10.200.~D.0/30" (netns-handle-index handle))))
      (when (netns-handle-chain-name handle)
        (run-command "iptables" "-D" "FORWARD" "-i" (netns-handle-veth-host handle)
                     "-j" (netns-handle-chain-name handle))
        (run-command "iptables" "-F" (netns-handle-chain-name handle))
        (run-command "iptables" "-X" (netns-handle-chain-name handle)))
      (run-command "iptables" "-t" "nat" "-D" "POSTROUTING" "-s" subnet "-j" "MASQUERADE")
      ;; DNS DNAT cleanup...
      (run-command "ip" "link" "delete" (netns-handle-veth-host handle))
      (run-command "ip" "netns" "delete" (netns-handle-name handle)))))
```

### Sandbox Execution (`exec.lisp`)

Uses raw `fork`/`execve` via CFFI. Pre-allocates pipe buffers to avoid consing.

```lisp
(defconstant +stdout-fileno+ 1)
(defconstant +stderr-fileno+ 2)
(defconstant +max-output+ 65536)
(defconstant +pollin+  #x0001)
(defconstant +pollhup+ #x0010)
(defconstant +wnohang+ 1)

(defstruct exec-result
  (exit-code 0 :type fixnum)
  (stdout "" :type simple-string)
  (stderr "" :type simple-string)
  (started 0 :type (unsigned-byte 64))
  (finished 0 :type (unsigned-byte 64))
  (seq 0 :type fixnum))

(defun exec-in-sandbox (sandbox cmd &key (workdir "/") (timeout 300))
  (declare (optimize (speed 3) (safety 1))
           (type simple-string cmd workdir)
           (type fixnum timeout))

  (let ((started (get-unix-time)))
    ;; Create pipes
    (cffi:with-foreign-objects ((stdout-fds :int 2)
                                (stderr-fds :int 2))
      (%pipe stdout-fds)
      (%pipe stderr-fds)

      (let* ((stdout-r (cffi:mem-aref stdout-fds :int 0))
             (stdout-w (cffi:mem-aref stdout-fds :int 1))
             (stderr-r (cffi:mem-aref stderr-fds :int 0))
             (stderr-w (cffi:mem-aref stderr-fds :int 1))
             ;; Open netns fd before fork
             (netns-fd (when (sandbox-netns sandbox)
                         (%sys-open (format nil "/var/run/netns/~A"
                                            (netns-handle-name (sandbox-netns sandbox)))
                                    0 0)))  ; O_RDONLY
             (pid (%fork)))

        (cond
          ;; ═══ CHILD ═══
          ((zerop pid)
           (%close stdout-r) (%close stderr-r)
           (%dup2 stdout-w +stdout-fileno+)
           (%dup2 stderr-w +stderr-fileno+)
           (%close stdout-w) (%close stderr-w)

           ;; Enter cgroup
           (when (sandbox-cgroup sandbox)
             (write-string-to-file
              (format nil "~A/cgroup.procs" (cgroup-handle-path (sandbox-cgroup sandbox)))
              (format nil "~D" (%getpid))))

           ;; Enter network namespace
           (when netns-fd
             (%setns netns-fd +clone-newnet+)
             (%close netns-fd))

           ;; New namespaces
           (%unshare (logior +clone-newns+ +clone-newpid+ +clone-newipc+ +clone-newuts+))

           ;; chroot + chdir
           (%chroot (sandbox-mounts-merged-path (sandbox-mounts sandbox)))
           (%chdir workdir)

           ;; exec /bin/sh -c "cmd"
           (cffi:with-foreign-objects ((argv :pointer 4)
                                       (envp :pointer 1))
             (setf (cffi:mem-aref argv :pointer 0) (cffi:foreign-string-alloc "/bin/sh"))
             (setf (cffi:mem-aref argv :pointer 1) (cffi:foreign-string-alloc "-c"))
             (setf (cffi:mem-aref argv :pointer 2) (cffi:foreign-string-alloc cmd))
             (setf (cffi:mem-aref argv :pointer 3) (cffi:null-pointer))
             (setf (cffi:mem-aref envp :pointer 0) (cffi:null-pointer))
             (%execve "/bin/sh" argv envp))
           (%exit 127))

          ;; ═══ PARENT ═══
          (t
           (%close stdout-w) (%close stderr-w)
           (when netns-fd (%close netns-fd))

           ;; Read stdout/stderr with timeout via poll()
           (cffi:with-foreign-objects ((stdout-buf :unsigned-char +max-output+)
                                       (stderr-buf :unsigned-char +max-output+)
                                       (poll-fds :int 6)  ; 2 * (fd + events + revents)
                                       (status :int 1))
             (let ((stdout-len 0) (stderr-len 0)
                   (deadline (+ (get-monotonic-ms) (* timeout 1000)))
                   (timed-out nil))
               (declare (type fixnum stdout-len stderr-len))

               ;; Poll loop
               (loop
                 (let ((remaining (- deadline (get-monotonic-ms))))
                   (when (<= remaining 0)
                     (setf timed-out t)
                     (%kill pid +sigkill+)
                     (return))

                   ;; Set up pollfd structs
                   (setf (cffi:mem-aref poll-fds :int 0) stdout-r
                         (cffi:mem-aref poll-fds :short 2) +pollin+
                         (cffi:mem-aref poll-fds :short 3) 0
                         (cffi:mem-aref poll-fds :int 2) stderr-r
                         (cffi:mem-aref poll-fds :short 6) +pollin+
                         (cffi:mem-aref poll-fds :short 7) 0)

                   (%poll poll-fds 2 (min remaining 30000))

                   ;; Read stdout
                   (let ((revents (cffi:mem-aref poll-fds :short 3)))
                     (when (and (plusp (logand revents +pollin+))
                                (< stdout-len +max-output+))
                       (let ((n (%read stdout-r
                                       (cffi:inc-pointer stdout-buf stdout-len)
                                       (- +max-output+ stdout-len))))
                         (when (plusp n) (incf stdout-len n)))))

                   ;; Read stderr
                   (let ((revents (cffi:mem-aref poll-fds :short 7)))
                     (when (and (plusp (logand revents +pollin+))
                                (< stderr-len +max-output+))
                       (let ((n (%read stderr-r
                                       (cffi:inc-pointer stderr-buf stderr-len)
                                       (- +max-output+ stderr-len))))
                         (when (plusp n) (incf stderr-len n)))))

                   ;; Check for both pipes closed
                   (when (and (plusp (logand (cffi:mem-aref poll-fds :short 3) +pollhup+))
                              (plusp (logand (cffi:mem-aref poll-fds :short 7) +pollhup+)))
                     (return))))

               ;; Wait for child
               (%waitpid pid status 0)
               (%close stdout-r)
               (%close stderr-r)

               (let* ((raw-status (cffi:mem-aref status :int 0))
                      (exit-code (cond
                                   (timed-out 124)
                                   ((zerop (logand raw-status #x7f))  ; WIFEXITED
                                    (ash (logand raw-status #xff00) -8))
                                   (t (+ 128 (logand raw-status #x7f)))))
                      (finished (get-unix-time))
                      (seq (sb-ext:atomic-incf (sandbox-exec-count sandbox))))

                 ;; Write log entry
                 (write-exec-log sandbox seq cmd workdir exit-code started finished
                                 stdout-buf stdout-len stderr-buf stderr-len)

                 (make-exec-result
                  :exit-code exit-code
                  :stdout (cffi:foreign-string-to-lisp stdout-buf :count stdout-len
                                                                   :encoding :utf-8)
                  :stderr (cffi:foreign-string-to-lisp stderr-buf :count stderr-len
                                                                   :encoding :utf-8)
                  :started started
                  :finished finished
                  :seq seq))))))))))
```

### HTTP API (`api.lisp`)

Uses the Clack/Lack/Ningle stack. Clack abstracts the HTTP server, Lack defines the
app-as-function protocol and middleware composition, Ningle adds routing on top.

**Architecture** (see [Fukamachi's tutorial](https://fukamachi.hashnode.dev/how-to-build-a-web-app-with-clack-and-lack-1)):
- A Lack application is a function `(lambda (env) ...)` returning `(status headers body)`.
- The `env` is a property list from the server: `:request-method`, `:path-info`, `:headers`, `:raw-body`, etc.
- Middleware is a function that takes an app and returns a wrapped app: `(lambda (app) (lambda (env) ...))`.
- `lack:builder` composes middleware declaratively. Keywords resolve to `lack/middleware/*` packages.
- Clack starts/stops the server: `(clack:clackup app :server :woo)`.

```lisp
(defvar *app* (make-instance 'ningle:app))

;;; ── Route definitions ─────────────────────────────────────────────────

(setf (ningle:route *app* "/cgi-bin/health")
      (lambda (params)
        (declare (ignore params))
        (json-response 200 '(("status" . "ok")))))

(setf (ningle:route *app* "/cgi-bin/api/sandboxes" :method :get)
      (lambda (params)
        (declare (ignore params))
        (json-response 200 (list-sandbox-infos *manager*))))

(setf (ningle:route *app* "/cgi-bin/api/sandboxes" :method :post)
      (lambda (params)
        (declare (ignore params))
        (let* ((body (lack.request:request-body-parameters ningle:*request*))
               (id (cdr (assoc "id" body :test #'string=))))
          (handler-case
              (let ((sandbox (create-sandbox *manager* id
                              :owner (cdr (assoc "owner" body :test #'string=))
                              :layers (parse-layers (cdr (assoc "layers" body :test #'string=)))
                              :task (cdr (assoc "task" body :test #'string=))
                              :cpu (cdr (assoc "cpu" body :test #'string=))
                              :memory-mb (cdr (assoc "memory_mb" body :test #'string=))
                              :max-lifetime-s (cdr (assoc "max_lifetime_s" body :test #'string=))
                              :allow-net (cdr (assoc "allow_net" body :test #'string=)))))
                (json-response 201 (sandbox-to-info sandbox)))
            (sandbox-error (e)
              (json-response 400 `(("error" . ,(format nil "~A" e)))))))))

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/exec" :method :post)
      (lambda (params)
        (let* ((id (cdr (assoc :id params)))
               (body (lack.request:request-body-parameters ningle:*request*))
               (cmd (cdr (assoc "cmd" body :test #'string=)))
               (workdir (or (cdr (assoc "workdir" body :test #'string=)) "/"))
               (timeout (or (cdr (assoc "timeout" body :test #'string=)) 300)))
          (handler-case
              (let ((result (manager-exec *manager* id cmd
                                          :workdir workdir :timeout timeout)))
                (json-response 200 (exec-result-to-alist result)))
            (sandbox-error (e)
              (json-response 404 `(("error" . ,(format nil "~A" e)))))))))

;; ... remaining routes follow same pattern ...

;;; ── JSON helpers ──────────────────────────────────────────────────────

(defun json-response (status body)
  (setf (lack.response:response-status ningle:*response*) status)
  (setf (lack.response:response-headers ningle:*response*)
        (list :content-type "application/json"))
  (jojo:to-json body))

;;; ── Auth middleware (Lack pattern) ───────────────────────────────────
;;; A Lack middleware is (lambda (app) (lambda (env) ...)).
;;; This integrates with lack:builder below.

(defun make-auth-middleware (token)
  "Returns a Lack middleware function. Takes app, returns wrapped app."
  (lambda (app)
    (lambda (env)
      (if (and token
               (search "/cgi-bin/api/" (getf env :path-info))
               (not (string= (format nil "Bearer ~A" token)
                              (gethash "authorization"
                                       (getf env :headers) ""))))
          '(401 (:content-type "application/json") ("{\"error\":\"unauthorized\"}"))
          (funcall app env)))))

;;; ── App composition with lack:builder ────────────────────────────────
;;; Middleware stacks declaratively. Keywords resolve to lack/middleware/* packages.

(defun build-app (config)
  "Compose the full Lack application with middleware."
  (lack:builder
    (:accesslog)                                    ; HTTP request logging
    (if (config-auth-token config)
        (make-auth-middleware (config-auth-token config))
        nil)                                        ; nil = skip this middleware
    *app*))
```

### S3 Client (`s3.lisp`)

Use Dexador for HTTP. Use Ironclad for SHA256/HMAC.

```lisp
(defun s3-sign-request (method path query payload-hash
                        access-key secret-key region datetime datestamp)
  "AWS Signature V4 signing. Returns authorization header value."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((canonical-request
           (format nil "~A~%~A~%~A~%host:~A~%~%host~%~A"
                   method path query *s3-host* payload-hash))
         (credential-scope
           (format nil "~A/~A/s3/aws4_request" datestamp region))
         (string-to-sign
           (format nil "AWS4-HMAC-SHA256~%~A~%~A~%~A"
                   datetime credential-scope
                   (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence :sha256
                     (babel:string-to-octets canonical-request)))))
         (signing-key
           (reduce (lambda (key data)
                     (ironclad:produce-mac
                      (ironclad:update-hmac
                       (ironclad:make-hmac key :sha256)
                       (babel:string-to-octets data))))
                   (list datestamp region "s3" "aws4_request")
                   :initial-value (babel:string-to-octets
                                   (concatenate 'string "AWS4" secret-key))))
         (signature
           (ironclad:byte-array-to-hex-string
            (ironclad:produce-mac
             (ironclad:update-hmac
              (ironclad:make-hmac signing-key :sha256)
              (babel:string-to-octets string-to-sign))))))
    (format nil "AWS4-HMAC-SHA256 Credential=~A/~A,SignedHeaders=host,Signature=~A"
            access-key credential-scope signature)))

(defun s3-push (client local-path key)
  (let* ((data (read-file-into-byte-vector local-path))
         (hash (ironclad:byte-array-to-hex-string
                (ironclad:digest-sequence :sha256 data)))
         (datetime (format-datetime-sigv4))
         (datestamp (subseq datetime 0 8))
         (auth (s3-sign-request "PUT" (format nil "/~A/~A" (s3-client-bucket client) key)
                                "" hash
                                (s3-client-access-key client)
                                (s3-client-secret-key client)
                                (s3-client-region client)
                                datetime datestamp)))
    (dex:put (s3-url client key)
             :content data
             :headers `(("Authorization" . ,auth)
                        ("x-amz-content-sha256" . ,hash)
                        ("x-amz-date" . ,datetime)))))

(defun s3-pull (client key local-path)
  "Pull from S3. Atomic: write to .s3tmp then rename."
  (let ((tmp-path (concatenate 'string local-path ".s3tmp")))
    ;; ... sign and GET ...
    (multiple-value-bind (body status)
        (dex:get (s3-url client key)
                 :headers `(("Authorization" . ,auth)
                             ("x-amz-content-sha256" . ,hash)
                             ("x-amz-date" . ,datetime))
                 :want-stream nil)
      (when (= status 200)
        (write-byte-vector-to-file body tmp-path)
        (rename-file tmp-path local-path)
        t))))

(defun s3-push-bg (client local-path key)
  "Push to S3 in background thread."
  (bt:make-thread
   (lambda ()
     (handler-case (s3-push client local-path key)
       (error (e) (log:warn "S3 push failed: ~A" e))))
   :name (format nil "s3-push-~A" key)))
```

### Reaper, Init, Snapshot

Follow the same logic as other prompts. Key difference: the reaper uses `handler-case` around each destroy to prevent a single failure from stopping the reaper loop.

```lisp
(defun reaper-loop (manager)
  (loop
    (sleep 10)
    (let ((expired nil))
      (bt:with-lock-held ((manager-lock manager))
        (maphash (lambda (id ms)
                   (let* ((sb (managed-sandbox-sandbox ms))
                          (max-life (sandbox-max-lifetime-s sb)))
                     (when (and (plusp max-life)
                                (> (- (get-unix-time) (sandbox-created sb)) max-life))
                       (push id expired))))
                 (manager-sandboxes manager)))
      (dolist (id expired)
        (handler-case
            (progn
              (log:info "reaper: destroying ~A" id)
              (manager-destroy-sandbox manager id))
          (error (e)
            (log:warn "reaper: failed to destroy ~A: ~A" id e)))))))
```

---

## Startup and Image Building (`main.lisp`)

```lisp
(defvar *manager* nil)
(defvar *s3-client* nil)
(defvar *config* nil)

(defun main ()
  (setf *config* (config-from-env))

  ;; GC tuning
  (setf (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))
  (sb-ext:gc :full t)

  (log:info "squash v4 (sbcl)")

  ;; 1. Init / recovery
  (init-run *config*)

  ;; 2. S3 client
  (when (config-s3-bucket *config*)
    (setf *s3-client* (make-s3-client-from-config *config*)))

  ;; 3. Start Go secret proxy
  (when (secrets-exist-p *config*)
    (if (config-proxy-https *config*)
        (progn (ensure-proxy-ca *config*)
               (start-go-proxy *config*))
        (start-shell-proxy *config*)))

  ;; 4. Sandbox manager
  (setf *manager* (make-manager *config*))

  ;; 5. Reaper thread
  (bt:make-thread (lambda () (reaper-loop *manager*))
                  :name "squashd-reaper")

  ;; 6. HTTP server (via Clack → Woo backend)
  ;; build-app composes middleware via lack:builder (see api.lisp)
  (let ((app (build-app *config*)))
    (let ((mod-count (count-modules *config*))
          (sb-count (hash-table-count (manager-sandboxes *manager*))))
      (log:info "ready — modules: ~D, sandboxes: ~D" mod-count sb-count))

    ;; clack:clackup abstracts the server — switch :woo to :hunchentoot
    ;; for development, or any other Clack handler, with no app changes.
    (clack:clackup app
                   :server :woo
                   :port (config-port *config*)
                   :worker-num 4
                   :debug nil)))

;;; ── Build the image ───────────────────────────────────────────────────

(defun build-image ()
  "Save a standalone executable."
  (ql:quickload "squashd")
  (sb-ext:save-lisp-and-die "squashd"
    :toplevel #'squashd:main
    :executable t
    :compression t       ; zstd compression — shrinks image from 50MB to ~20MB
    :purify t            ; move as much as possible to read-only space
    :save-runtime-options t))
```

**Image size optimization:**
- `:compression t` — SBCL compresses the core with zstd, typically 2-3x reduction
- `:purify t` — moves immutable data to read-only pages (also helps with fork COW)
- Strip debug info in production: `(declaim (optimize (debug 0)))` globally before building
- Consider `(sb-ext:gc :full t)` before save to compact heap

Expected image size: **~20-30MB compressed**. Startup time: **~50-100ms** (mmap + relocate).

---

## Dockerfile

```dockerfile
# Build stage: load Quicklisp deps and save image
FROM ubuntu:24.04 AS build
RUN apt-get update && apt-get install -y sbcl curl build-essential libev-dev
# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
WORKDIR /build
COPY *.asd *.lisp ./
RUN sbcl --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload "squashd")' \
         --eval '(squashd::build-image)' \
         --quit

# Build Go proxy
FROM golang:1.22-alpine AS proxy-build
COPY proxy/ /build/
RUN cd /build && CGO_ENABLED=0 go build -ldflags='-s -w' -o /sq-secret-proxy-https .

# Build C guest agent
FROM alpine:3.21 AS guest-build
RUN apk add --no-cache gcc musl-dev
COPY guest/ /build/
RUN cd /build && gcc -Os -static -o sq-guest-agent main.c overlay.c vsock.c -s

# Runtime
FROM alpine:3.21
RUN apk add --no-cache \
    jq squashfs-tools util-linux curl wget coreutils \
    socat openssl busybox-extras iproute2 iptables libev \
    && apk add --no-cache aws-cli 2>/dev/null || true

COPY --from=build /build/squashd /app/bin/squashd
COPY --from=proxy-build /sq-secret-proxy-https /app/bin/sq-secret-proxy-https
COPY --from=guest-build /build/sq-guest-agent /app/bin/sq-guest-agent
COPY bin/sq-mkbase bin/sq-mkmod bin/sq-mkvm bin/sq-ctl bin/sq-secret-proxy bin/setup-tailscale /app/bin/
COPY static/ /app/static/

RUN chmod +x /app/bin/*

VOLUME /data
EXPOSE 8080

WORKDIR /app
ENTRYPOINT ["/app/bin/squashd"]
```

Note: `libev` is needed at runtime because Woo uses it for its event loop.

---

## REPL-Driven Development: The Killer Workflow

One thing no other language in this comparison offers: you can connect a REPL to the running production daemon and inspect/modify it live.

**Setup**: Include Swank (SLIME server) in the image, listening on a Unix socket:

```lisp
(defun start-swank ()
  "Start Swank for remote REPL. Only if SQUASH_SWANK=1."
  (when (string= "1" (uiop:getenv "SQUASH_SWANK"))
    (ql:quickload "swank")
    (swank:create-server :port 4005 :dont-close t)
    (log:info "Swank server on port 4005")))
```

**What this enables:**
- Inspect the `*manager*` hash table live: see all sandboxes, their states, mount points
- Call `(create-sandbox *manager* "test" :layers '("000-base-alpine"))` directly
- Redefine a function and it takes effect immediately — no restart, no rebuild
- Use `(trace mount-squashfs)` to see every mount call and its arguments
- Use `(inspect (gethash "dev" (manager-sandboxes *manager*)))` to browse a sandbox's state
- If a sandbox is stuck, manually call `(teardown-netns (sandbox-netns sb))` to clean it up
- Hot-patch a bug fix: redefine the function, and the next request uses the new code

This is genuinely useful for a system managing kernel state. When something is wedged — leaked mount, orphaned iptables rule, stuck process — you can inspect the exact state and fix it without restarting the daemon or losing the other running sandboxes.

**Security**: only enable Swank in development or behind a Tailscale tunnel. Never expose port 4005 publicly.

---

## Comparison Table

| Concern | Rust | Zig | Odin | Common Lisp (SBCL) |
|---|---|---|---|---|
| Cleanup | `Drop` (automatic) | `errdefer` + manual | `defer` + manual | `unwind-protect` + manual |
| Error recovery | Unwind only | Unwind only | Unwind only | **Condition restarts (resume)** |
| Syscalls | `nix` crate | `@cImport` | `foreign import` | CFFI |
| HTTP server | axum (async) | hand-rolled | hand-rolled | **Clack/Lack + Woo (libev, 20k rps)** |
| JSON | serde_json | std.json | core:encoding/json | Jonathan |
| Binary size | 3-5MB | 500KB-1.5MB | 1-3MB | **20-30MB** (compressed image) |
| Startup | Instant | Instant | Instant | ~50-100ms (mmap image) |
| GC | None | None | None | **Generational (1-10ms pauses)** |
| Build time | 30-60s | 3-5s | 2-5s | **~15s** (compile+save) |
| Live debugging | No | No | No | **REPL into running process** |
| MITM proxy | Rewrite in Rust | Keep Go | Keep Go | Keep Go |
| Guest agent | Rust (~1MB) | Zig (~200KB) | Odin (~500KB) | **C (~50KB)** (Lisp too large) |
| Hot code reload | No | No | No | **Yes (redefine functions live)** |

---

## Implementation Order

1. **Phase 1: Project setup + Config + CFFI syscalls** — Get SBCL building an image. Verify `mount`/`umount2` work via CFFI in a privileged container.

2. **Phase 2: Mounts + Conditions** — squashfs, tmpfs, overlay mount/unmount. Define condition types. Test cleanup with `unwind-protect`.

3. **Phase 3: Sandbox create + destroy + exec** — Full lifecycle. Woo HTTP server with health endpoint and create/exec/destroy routes. This is the proof-of-concept milestone.

4. **Phase 4: Cgroups + Netns** — Shell out to `ip`/`iptables`. Test isolation.

5. **Phase 5: Snapshot + Restore + Activate** — mksquashfs shell-out, overlay remount.

6. **Phase 6: S3 sync** — Ironclad SigV4, Dexador HTTP client.

7. **Phase 7: Go proxy integration** — Child process management via `uiop:launch-program`.

8. **Phase 8: Init/recovery + Reaper** — Startup remount, background thread.

9. **Phase 9: Performance pass** — Profile with `sb-sprof`, add type declarations on hot paths, tune GC.

10. **Phase 10: Wire compatibility** — Run `sq-test` against the SBCL image.

---

## Non-Goals

- **Guest agent in Lisp** — image size disqualifies it. C guest agent.
- **Rewriting Go proxy** — same rationale as all prompts.
- **Zero-GC operation** — the GC pauses are fine for this workload. Don't fight it.
- **Portable CL** — this targets SBCL specifically. Uses `sb-ext:atomic-incf`, `sb-ext:save-lisp-and-die`, `sb-concurrency`, SBCL compiler declarations. Not intended to run on CCL/ECL/ABCL.
- **Rewriting shell setup scripts** — same as all prompts.
