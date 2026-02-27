(in-package #:squashd)

;;; ── gVisor Backend ──────────────────────────────────────────────────
;;;
;;; Sandbox backend using gVisor's runsc for user-space kernel isolation.
;;; Reuses the same overlayfs infrastructure as the chroot backend
;;; (squashfs layers + tmpfs upper + overlay merge) but runs processes
;;; through runsc instead of fork/chroot/execve.
;;;
;;; Each sandbox runs a `sleep infinity` init process via `runsc create`
;;; + `runsc start`, then commands are executed via `runsc exec`.
;;;
;;; Key flags:
;;;   --root=<state-dir>   — runsc state directory (per-daemon)
;;;   --overlay2=none      — disable runsc's internal overlay so writes
;;;                          go to our overlayfs upper layer (needed for
;;;                          snapshot/restore to work)
;;;
;;; Isolation: runsc Sentry (user-space kernel intercepting syscalls)
;;; Network:   same netns as chroot backend (veth + NAT + DNS DNAT)
;;; Mounts:    same overlayfs + squashfs stack as chroot backend
;;; Exec:      `runsc exec` instead of fork/chroot/execve
;;;
;;; WARM PATH — runs on every sandbox operation when backend is :gvisor.

(declaim (optimize (speed 2) (safety 2) (debug 2)))

;;; ── State Directory ────────────────────────────────────────────────

(defun gvisor-state-dir (config)
  "Return the runsc state directory. Stored under <data-dir>/runsc."
  (format nil "~A/runsc" (config-data-dir config)))

;;; ── Container ID ───────────────────────────────────────────────────

(defun gvisor-container-id (sandbox-id)
  "Generate a runsc container ID from a sandbox ID.
   Prefixed with 'sq-' to avoid collisions with other runsc users."
  (format nil "sq-~A" sandbox-id))

;;; ── OCI Config Generation ──────────────────────────────────────────
;;;
;;; Generates a minimal OCI runtime spec (config.json) suitable for
;;; runsc. The rootfs is our overlayfs merged directory. We disable
;;; runsc's internal overlay (--overlay2=none) so writes go through
;;; to our upper layer.

(defun gvisor-write-oci-config (bundle-dir merged-path netns-name
                                cpu memory-mb)
  "Write an OCI runtime spec to BUNDLE-DIR/config.json.
   MERGED-PATH is the absolute path to the overlayfs merged directory.
   NETNS-NAME is the network namespace name (e.g. 'squash-<id>') or NIL.
   CPU is a float (cores), MEMORY-MB is an integer.
   Returns the path to the generated config.json."
  (declare (type simple-string bundle-dir merged-path)
           (type (or null simple-string) netns-name))
  (ensure-directories-exist (format nil "~A/" bundle-dir))
  (let* ((config-path (format nil "~A/config.json" bundle-dir))
         ;; CPU quota in microseconds (cores * period)
         (cpu-quota (round (* (or cpu 2.0) 100000)))
         (mem-bytes (* (or memory-mb 1024) 1048576))
         ;; Build namespace list
         (namespaces
           (append
            (list (list :|type| "pid")
                  (list :|type| "ipc")
                  (list :|type| "uts")
                  (list :|type| "mount"))
            (when netns-name
              (list (list :|type| "network"
                          :|path| (format nil "/var/run/netns/~A"
                                          netns-name))))))
         ;; OCI spec
         (spec
           (list
            :|ociVersion| "1.0.0"
            :|process|
            (list :|terminal| nil
                  :|user| (list :|uid| 0 :|gid| 0)
                  :|args| '("sleep" "infinity")
                  :|env| '("PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
                            "HOME=/root"
                            "USER=root"
                            "TERM=xterm")
                  :|cwd| "/")
            :|root|
            (list :|path| merged-path
                  :|readonly| nil)
            :|mounts|
            (list (list :|destination| "/proc"
                        :|type| "proc"
                        :|source| "proc")
                  (list :|destination| "/dev"
                        :|type| "tmpfs"
                        :|source| "tmpfs"
                        :|options| '("nosuid" "strictatime" "mode=755"
                                     "size=65536k"))
                  (list :|destination| "/dev/pts"
                        :|type| "devpts"
                        :|source| "devpts"
                        :|options| '("nosuid" "noexec" "newinstance"
                                     "ptmxmode=0666" "mode=0620"))
                  (list :|destination| "/sys"
                        :|type| "sysfs"
                        :|source| "sysfs"
                        :|options| '("nosuid" "noexec" "nodev" "ro")))
            :|linux|
            (list :|namespaces| namespaces
                  :|resources|
                  (list :|memory| (list :|limit| mem-bytes)
                        :|cpu| (list :|quota| cpu-quota
                                     :|period| 100000))))))
    (with-open-file (s config-path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      (write-string (jojo:to-json spec) s))
    config-path))

;;; ── Lifecycle ──────────────────────────────────────────────────────

(defun gvisor-create-and-start (config sandbox-id bundle-dir)
  "Create and start a gVisor container in detached mode.
   The container runs 'sleep infinity' as its init process.
   CONFIG provides the runsc state directory path.
   BUNDLE-DIR contains the OCI config.json."
  (declare (type simple-string sandbox-id bundle-dir))
  (let ((container-id (gvisor-container-id sandbox-id))
        (state-dir (gvisor-state-dir config)))
    (ensure-directories-exist (format nil "~A/" state-dir))
    ;; Create container
    (multiple-value-bind (exit-code stdout stderr)
        (run-command "runsc"
                     (format nil "--root=~A" state-dir)
                     "--overlay2=none"
                     "create"
                     (format nil "--bundle=~A" bundle-dir)
                     container-id)
      (declare (ignore stdout))
      (unless (zerop exit-code)
        (error 'sandbox-error :id sandbox-id
               :message (format nil "runsc create failed: ~A" stderr))))
    ;; Start container
    (multiple-value-bind (exit-code stdout stderr)
        (run-command "runsc"
                     (format nil "--root=~A" state-dir)
                     "start"
                     container-id)
      (declare (ignore stdout))
      (unless (zerop exit-code)
        ;; Clean up the created-but-not-started container
        (ignore-errors
          (run-command "runsc"
                       (format nil "--root=~A" state-dir)
                       "delete" "--force" container-id))
        (error 'sandbox-error :id sandbox-id
               :message (format nil "runsc start failed: ~A" stderr))))
    container-id))

(defun gvisor-stop (config sandbox-id)
  "Kill and delete a gVisor container. Best-effort — ignores errors."
  (declare (type simple-string sandbox-id))
  (let ((container-id (gvisor-container-id sandbox-id))
        (state-dir (gvisor-state-dir config)))
    ;; Kill the container (SIGKILL)
    (ignore-errors
      (run-command "runsc"
                   (format nil "--root=~A" state-dir)
                   "kill" container-id "SIGKILL"))
    ;; Wait briefly for the container to stop
    (sleep 0.1)
    ;; Delete the container
    (ignore-errors
      (run-command "runsc"
                   (format nil "--root=~A" state-dir)
                   "delete" "--force" container-id))))

(defun gvisor-exec (config sandbox-id cmd workdir timeout-s)
  "Execute a command in a running gVisor container.
   Returns an exec-result struct."
  (declare (type simple-string sandbox-id cmd workdir)
           (type fixnum timeout-s))
  (let* ((container-id (gvisor-container-id sandbox-id))
         (state-dir (gvisor-state-dir config))
         (started (get-unix-time))
         (start-ticks (get-internal-real-time))
         ;; Build the command: timeout + runsc exec
         (args (list "timeout" (format nil "~Ds" timeout-s)
                     "runsc"
                     (format nil "--root=~A" state-dir)
                     "exec"
                     "--user=root"
                     (format nil "--cwd=~A" workdir)
                     container-id
                     "/bin/sh" "-c" cmd)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program args
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (let* ((finished (get-unix-time))
             (end-ticks (get-internal-real-time))
             (duration-ms (round (* (- end-ticks start-ticks) 1000)
                                 internal-time-units-per-second))
             ;; Truncate output to match chroot backend limits
             (stdout-str (if (> (length stdout) +max-output+)
                             (subseq stdout 0 +max-output+)
                             stdout))
             (stderr-str (if (> (length stderr) +max-output+)
                             (subseq stderr 0 +max-output+)
                             stderr)))
        (make-exec-result
         :exit-code (cond
                      ((= exit-code 124) 124)  ; timeout
                      (t exit-code))
         :stdout stdout-str
         :stderr stderr-str
         :started started
         :finished finished
         :duration-ms duration-ms)))))

(defun gvisor-running-p (config sandbox-id)
  "Check if a gVisor container is currently running.
   Returns T if the container exists and is in running state."
  (declare (type simple-string sandbox-id))
  (let ((container-id (gvisor-container-id sandbox-id))
        (state-dir (gvisor-state-dir config)))
    (multiple-value-bind (exit-code stdout stderr)
        (run-command "runsc"
                     (format nil "--root=~A" state-dir)
                     "state" container-id)
      (declare (ignore stderr))
      (and (zerop exit-code)
           (search "\"running\"" stdout)
           t))))
