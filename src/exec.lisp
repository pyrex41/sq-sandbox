(in-package #:squashd)

(declaim (optimize (speed 3) (safety 1)))

;;; ── Constants ──────────────────────────────────────────────────────

(defconstant +stdout-fileno+ 1)
(defconstant +stderr-fileno+ 2)
(defconstant +max-output+ 65536)
(defconstant +poll-interval-ms+ 100)

;;; ── pollfd struct ──────────────────────────────────────────────────

(cffi:defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

;;; ── Helpers ────────────────────────────────────────────────────────

(declaim (inline get-unix-time))
(defun get-unix-time ()
  "Current Unix timestamp as (unsigned-byte 64)."
  (declare (optimize (speed 3) (safety 0)))
  (the (unsigned-byte 64) (- (get-universal-time) 2208988800)))

(declaim (inline get-monotonic-ms))
(defun get-monotonic-ms ()
  "Monotonic clock in milliseconds, for timeout tracking."
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum (values (floor (get-internal-real-time)
                             (/ internal-time-units-per-second 1000)))))

;;; ── exec-result ────────────────────────────────────────────────────

(defstruct exec-result
  (exit-code 0 :type fixnum)
  (stdout    "" :type simple-string)
  (stderr    "" :type simple-string)
  (started   0  :type (unsigned-byte 64))
  (finished  0  :type (unsigned-byte 64))
  (seq       0  :type fixnum))

;;; ── Exec log ───────────────────────────────────────────────────────

(defun sandbox-root-path (sandbox)
  "Best-effort sandbox root path derived from merged mount path."
  (let* ((merged (sandbox-merged-path sandbox))
         (suffix "/merged")
         (mlen (length merged))
         (slen (length suffix)))
    (if (and (> mlen slen)
             (string= suffix merged :start2 (- mlen slen)))
        (subseq merged 0 (- mlen slen))
        merged)))

(defun write-exec-log (sandbox seq cmd workdir exit-code started finished
                       stdout-str stderr-str &key sandbox-dir)
  "Write a JSON log entry to .meta/log/exec-<seq>.json for the sandbox.
   Uses SANDBOX-DIR when provided, otherwise derives from merged path."
  (declare (type fixnum seq exit-code)
           (type (unsigned-byte 64) started finished)
           (type simple-string cmd workdir stdout-str stderr-str))
  (let* ((root (or sandbox-dir (sandbox-root-path sandbox)))
         (log-dir (concatenate 'string root "/.meta/log"))
         (log-path (format nil "~A/exec-~D.json" log-dir seq)))
    (ensure-directories-exist (concatenate 'string log-dir "/"))
    (with-open-file (s log-path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (write-string
       (jojo:to-json
        (list :|seq| seq
              :|cmd| cmd
              :|workdir| workdir
              :|exit_code| exit-code
              :|started| started
              :|finished| finished
              :|duration_ms| (the fixnum (* (- finished started) 1000))
              :|stdout| stdout-str
              :|stderr| stderr-str))
       s))))

;;; ── Sandbox accessors (forward references) ─────────────────────────
;;; These are defined in sandbox.lisp (task 5.1). We declare their
;;; types here to suppress SBCL style-warnings and enable optimization.
;;; At compile time these are forward references; at runtime the actual
;;; struct accessors exist.

(declaim (ftype (function (t) (or null t)) sandbox-netns))
(declaim (ftype (function (t) (or null t)) sandbox-cgroup))
(declaim (ftype (function (t) t) sandbox-mounts))
(declaim (ftype (function (t) fixnum) sandbox-exec-count))

;;; Helper to extract the merged path from a sandbox's mounts.
;;; This isolates exec.lisp from the internal structure of sandbox/mounts.

(defun sandbox-merged-path (sandbox)
  "Get the overlay merged path for a sandbox."
  (overlay-mount-merged-path
   (sandbox-mounts-overlay (sandbox-mounts sandbox))))

;;; ── Pipe helpers ───────────────────────────────────────────────────

(defun make-pipe ()
  "Create a pipe. Returns (values read-fd write-fd)."
  (cffi:with-foreign-object (fds :int 2)
    (let ((rc (%pipe fds)))
      (unless (zerop rc)
        (error "pipe() failed: errno ~D" (get-errno)))
      (values (cffi:mem-aref fds :int 0)
              (cffi:mem-aref fds :int 1)))))

(defun close-fd-safe (fd)
  "Close a file descriptor, ignoring errors."
  (declare (type fixnum fd))
  (when (>= fd 0)
    (%close fd)))

;;; ── Child process setup ────────────────────────────────────────────

(defun child-setup-and-exec (sandbox cmd workdir netns-fd stdout-w stderr-w)
  "Called in the child process after fork.
   Sets up namespaces, chroot, redirects stdio, then execve's /bin/sh -c cmd.
   This function never returns on success — it calls %exit on failure."
  (declare (type simple-string cmd workdir))
  ;; Redirect stdout/stderr to pipes
  (%dup2 stdout-w +stdout-fileno+)
  (%dup2 stderr-w +stderr-fileno+)
  (%close stdout-w)
  (%close stderr-w)

  ;; Enter cgroup (write our PID to cgroup.procs)
  (when (sandbox-cgroup sandbox)
    (let ((cgroup-path (cgroup-handle-path (sandbox-cgroup sandbox))))
      (write-string-to-file
       (concatenate 'string cgroup-path "/cgroup.procs")
       (format nil "~D" (%getpid)))))

  ;; Enter network namespace via setns
  (when netns-fd
    (when (minusp (%setns netns-fd +clone-newnet+))
      (%exit 125))
    (%close netns-fd))

  ;; New mount, PID, IPC, UTS namespaces
  (when (minusp (%unshare (logior +clone-newns+ +clone-newpid+
                                   +clone-newipc+ +clone-newuts+)))
    (%exit 125))

  ;; After unshare(CLONE_NEWPID), this process is still in the old PID ns.
  ;; Fork once more so the inner child executes in the new PID namespace.
  (let ((inner-pid (%fork)))
    (cond
      ((minusp inner-pid) (%exit 126))
      ((plusp inner-pid)
       (cffi:with-foreign-object (status :int)
         (%waitpid inner-pid status 0)
         (let ((raw-status (cffi:mem-aref status :int 0)))
           (cond
             ((zerop (logand raw-status #x7f))
              (%exit (ash (logand raw-status #xff00) -8)))
             (t
              (%exit (+ 128 (logand raw-status #x7f))))))))
      (t nil)))

  ;; chroot into the overlay merged directory, then chdir to workdir
  (let ((merged (sandbox-merged-path sandbox)))
    (when (minusp (%chroot merged))
      (%exit 125)))
  (when (minusp (%chdir workdir))
    (%exit 125))

  ;; execve /bin/sh -c "<cmd>"
  (cffi:with-foreign-objects ((argv :pointer 4)
                              (envp :pointer 1))
    (let ((sh  (cffi:foreign-string-alloc "/bin/sh"))
          (c   (cffi:foreign-string-alloc "-c"))
          (arg (cffi:foreign-string-alloc cmd)))
      (setf (cffi:mem-aref argv :pointer 0) sh
            (cffi:mem-aref argv :pointer 1) c
            (cffi:mem-aref argv :pointer 2) arg
            (cffi:mem-aref argv :pointer 3) (cffi:null-pointer))
      (setf (cffi:mem-aref envp :pointer 0) (cffi:null-pointer))
      (%execve "/bin/sh" argv envp)))

  ;; If execve returns, it failed
  (%exit 127))

;;; ── Parent: poll-based output reader ───────────────────────────────

(defun parent-read-output (pid stdout-r stderr-r timeout-s)
  "Read stdout/stderr from child via poll() with timeout.
   Returns (values exit-code stdout-str stderr-str timed-out-p)."
  (declare (type fixnum pid stdout-r stderr-r timeout-s)
           (optimize (speed 3) (safety 1)))

  (cffi:with-foreign-objects ((stdout-buf :unsigned-char +max-output+)
                              (stderr-buf :unsigned-char +max-output+)
                              (poll-fds '(:struct pollfd) 2)
                              (status :int 1))
    (let ((stdout-len 0)
          (stderr-len 0)
          (deadline (+ (get-monotonic-ms) (* timeout-s 1000)))
          (timed-out nil)
          (stdout-open t)
          (stderr-open t))
      (declare (type fixnum stdout-len stderr-len))

      ;; Poll loop — read from both pipes until both close or timeout
      (loop while (or stdout-open stderr-open)
            do
            (let ((remaining (- deadline (get-monotonic-ms))))
              (when (<= remaining 0)
                (setf timed-out t)
                (%kill pid +sigkill+)
                (return))

              ;; Set up pollfd array
              (cffi:with-foreign-slots ((fd events revents)
                                        (cffi:mem-aptr poll-fds '(:struct pollfd) 0)
                                        (:struct pollfd))
                (setf fd (if stdout-open stdout-r -1)
                      events +pollin+
                      revents 0))
              (cffi:with-foreign-slots ((fd events revents)
                                        (cffi:mem-aptr poll-fds '(:struct pollfd) 1)
                                        (:struct pollfd))
                (setf fd (if stderr-open stderr-r -1)
                      events +pollin+
                      revents 0))

              (%poll poll-fds 2 (min (the fixnum remaining) +poll-interval-ms+))

              ;; Read stdout
              (let ((revents (cffi:foreign-slot-value
                              (cffi:mem-aptr poll-fds '(:struct pollfd) 0)
                              '(:struct pollfd) 'revents)))
                (when (plusp (logand revents +pollin+))
                  (when (< stdout-len +max-output+)
                    (let ((n (%read stdout-r
                                    (cffi:inc-pointer stdout-buf stdout-len)
                                    (- +max-output+ stdout-len))))
                      (when (plusp n) (incf stdout-len (the fixnum n))))))
                (when (plusp (logand revents +pollhup+))
                  ;; Drain remaining data after HUP
                  (loop for n = (%read stdout-r
                                       (cffi:inc-pointer stdout-buf stdout-len)
                                       (- +max-output+ stdout-len))
                        while (and (plusp n) (< stdout-len +max-output+))
                        do (incf stdout-len (the fixnum n)))
                  (setf stdout-open nil)))

              ;; Read stderr
              (let ((revents (cffi:foreign-slot-value
                              (cffi:mem-aptr poll-fds '(:struct pollfd) 1)
                              '(:struct pollfd) 'revents)))
                (when (plusp (logand revents +pollin+))
                  (when (< stderr-len +max-output+)
                    (let ((n (%read stderr-r
                                    (cffi:inc-pointer stderr-buf stderr-len)
                                    (- +max-output+ stderr-len))))
                      (when (plusp n) (incf stderr-len (the fixnum n))))))
                (when (plusp (logand revents +pollhup+))
                  (loop for n = (%read stderr-r
                                       (cffi:inc-pointer stderr-buf stderr-len)
                                       (- +max-output+ stderr-len))
                        while (and (plusp n) (< stderr-len +max-output+))
                        do (incf stderr-len (the fixnum n)))
                  (setf stderr-open nil)))))

      ;; Reap child process
      (%waitpid pid status 0)
      (%close stdout-r)
      (%close stderr-r)

      (let* ((raw-status (cffi:mem-aref status :int 0))
             (exit-code (cond
                          (timed-out 124)
                          ;; WIFEXITED: low 7 bits zero means normal exit
                          ((zerop (logand raw-status #x7f))
                           (ash (logand raw-status #xff00) -8))
                          ;; Killed by signal: 128 + signal number
                          (t (+ 128 (logand raw-status #x7f))))))

        (values exit-code
                (if (plusp stdout-len)
                    (cffi:foreign-string-to-lisp stdout-buf
                      :count stdout-len :encoding :utf-8)
                    "")
                (if (plusp stderr-len)
                    (cffi:foreign-string-to-lisp stderr-buf
                      :count stderr-len :encoding :utf-8)
                    "")
                timed-out)))))

;;; ── Main entry point ───────────────────────────────────────────────

(defun exec-in-sandbox (sandbox cmd &key (workdir "/") (timeout 300)
                                        sandbox-dir)
  "Execute CMD in SANDBOX via fork/execve.
   Child enters cgroup, network namespace, unshares mount/PID/IPC/UTS,
   chroots into the overlay merged dir, then runs /bin/sh -c CMD.
   Parent captures stdout/stderr via poll() with TIMEOUT seconds.
   Returns an exec-result struct.
   Increments exec-count for sequence tracking."
  (declare (type simple-string cmd workdir)
           (type fixnum timeout))

  (let ((started (get-unix-time)))
    ;; Open netns fd before fork (while we have access to /var/run/netns)
    (let ((netns-fd (when (sandbox-netns sandbox)
                      (let ((fd (%sys-open
                                 (format nil "/var/run/netns/~A"
                                         (netns-handle-name
                                          (sandbox-netns sandbox)))
                                 +o-rdonly+ 0)))
                        (when (minusp fd)
                          (error 'sandbox-error
                                 :id "exec"
                                 :message (format nil "open netns fd failed: errno ~D"
                                                  (get-errno))))
                        fd))))
      ;; Create pipes for stdout and stderr
      (multiple-value-bind (stdout-r stdout-w) (make-pipe)
        (multiple-value-bind (stderr-r stderr-w) (make-pipe)
          (let ((pid (%fork)))
            (cond
              ;; Fork failed
              ((minusp pid)
               (close-fd-safe stdout-r)
               (close-fd-safe stdout-w)
               (close-fd-safe stderr-r)
               (close-fd-safe stderr-w)
               (when netns-fd (close-fd-safe netns-fd))
               (error 'sandbox-error
                      :id "exec"
                      :message (format nil "fork failed: errno ~D" (get-errno))))

              ;; ═══ CHILD ═══
              ((zerop pid)
               ;; Close read ends of pipes (parent owns those)
               (%close stdout-r)
               (%close stderr-r)
               ;; Never returns
               (child-setup-and-exec sandbox cmd workdir
                                     netns-fd stdout-w stderr-w))

              ;; ═══ PARENT ═══
              (t
               ;; Close write ends of pipes (child owns those)
               (%close stdout-w)
               (%close stderr-w)
               (when netns-fd (%close netns-fd))

               (multiple-value-bind (exit-code stdout-str stderr-str)
                   (parent-read-output pid stdout-r stderr-r timeout)

                 (let* ((finished (get-unix-time))
                        (seq (incf (sandbox-exec-count sandbox))))

                   ;; Write log entry to .meta/log
                   (ignore-errors
                     (write-exec-log sandbox seq cmd workdir
                                     exit-code started finished
                                     stdout-str stderr-str
                                     :sandbox-dir sandbox-dir))

                   (make-exec-result
                    :exit-code exit-code
                    :stdout stdout-str
                    :stderr stderr-str
                    :started started
                    :finished finished
                    :seq seq)))))))))))
