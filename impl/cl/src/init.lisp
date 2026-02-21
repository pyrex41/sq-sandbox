(in-package #:squashd)

;;; ── Startup recovery ──────────────────────────────────────────────────
;;;
;;; On daemon start, scan the data directory for existing sandboxes left
;;; from a previous run. For each, attempt to remount filesystems and
;;; restore state into the manager. Sandboxes that cannot be recovered
;;; are cleaned up (best effort).
;;;
;;; COLD PATH — runs once at startup. Maximize safety for correctness.

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defun scan-sandbox-dirs (data-dir)
  "Return a list of sandbox ID strings found under DATA-DIR/sandboxes/."
  (let ((sandboxes-dir (format nil "~A/sandboxes" data-dir))
        (ids nil))
    (when (probe-file sandboxes-dir)
      (dolist (entry (uiop:subdirectories sandboxes-dir))
        (let ((name (car (last (pathname-directory entry)))))
          (when (and name (stringp name) (plusp (length name)))
            (push name ids)))))
    (nreverse ids)))

(defun read-firecracker-index (sandbox-dir)
  "Read the recovered Firecracker tap index from metadata.
Returns NIL if no valid index is present."
  (flet ((read-int (path)
           (when (probe-file path)
             (handler-case
                 (parse-integer
                  (string-trim '(#\Space #\Newline #\Return #\Tab)
                               (uiop:read-file-string path)))
               (error () nil)))))
    (or (read-int (format nil "~A/.meta/log/index" sandbox-dir))
        (read-int (format nil "~A/.meta/index" sandbox-dir)))))

(defun recover-firecracker-sandbox (manager id sandbox-dir meta)
  "Recover a Firecracker sandbox without attempting chroot remount operations."
  (let* ((owner (or (getf meta :owner) "anon"))
         (task (or (getf meta :task) ""))
         (max-lifetime-s (or (getf meta :max-lifetime-s) 0))
         (created (or (getf meta :created) (get-unix-time)))
         (index (read-firecracker-index sandbox-dir))
         (netns (when (and index (>= index 1) (<= index 254))
                  (reserve-netns-index index)
                  (make-netns-handle
                   :name (format nil "fc-~A" id)
                   :index index)))
         (sandbox (%make-sandbox
                   :id id
                   :state :ready
                   :mounts nil
                   :netns netns
                   :cgroup nil
                   :created created
                   :last-active (get-unix-time)
                   :max-lifetime-s max-lifetime-s)))
    (register-recovered-sandbox manager sandbox :owner owner :task task)
    (log:info "init: recovered firecracker sandbox ~A" id)
    sandbox))

(defun recover-sandbox (manager id data-dir)
  "Attempt to recover a single sandbox by re-reading its metadata and
   remounting its filesystem layers. Returns the sandbox on success, or
   NIL on failure (after best-effort cleanup)."
  (let ((sandbox-dir (format nil "~A/sandboxes/~A" data-dir id)))
    (handler-case
        (let ((meta (read-sandbox-meta sandbox-dir)))
          (when (null meta)
            (log:warn "init: ~A has no metadata, skipping" id)
            (return-from recover-sandbox nil))
          ;; Extract metadata fields
          (let* ((config (manager-config manager))
                 (layers (getf meta :layers))
                 (owner (or (getf meta :owner) "anon"))
                 (task (or (getf meta :task) ""))
                 (cpu (or (getf meta :cpu) 2.0))
                 (memory-mb (or (getf meta :memory-mb) 1024))
                 (allow-net (or (getf meta :allow-net) nil))
                 (max-lifetime-s (or (getf meta :max-lifetime-s) 0))
                 (created (or (getf meta :created) (get-unix-time))))
            (if (eq (config-backend config) :firecracker)
                (recover-firecracker-sandbox manager id sandbox-dir meta)
                ;; Chroot recovery path.
                (let ((mounts (remount-sandbox-filesystems
                               manager sandbox-dir id layers)))
                  (when (null mounts)
                    (log:warn "init: ~A remount failed, cleaning up" id)
                    (ignore-errors (cleanup-sandbox-dir sandbox-dir))
                    (return-from recover-sandbox nil))
                  ;; Recreate cgroup/netns for recovered sandbox so exec + teardown
                  ;; retain isolation/enforcement after daemon restart.
                  (let ((cgroup (handler-case
                                    (create-cgroup id
                                                   (coerce cpu 'single-float)
                                                   (truncate memory-mb))
                                  (error (e)
                                    (log:warn "init: ~A cgroup recovery failed: ~A" id e)
                                    nil)))
                        (netns (handler-case
                                   (setup-netns config id allow-net)
                                 (error (e)
                                   (log:warn "init: ~A netns recovery failed: ~A" id e)
                                   nil))))
                    (ignore-errors
                      (seed-resolv-conf sandbox-dir netns))
                    ;; Build sandbox struct and register with manager
                    (let ((sandbox (%make-sandbox
                                    :id id
                                    :state :ready
                                    :mounts mounts
                                    :netns netns
                                    :cgroup cgroup
                                    :created created
                                    :last-active (get-unix-time)
                                    :max-lifetime-s max-lifetime-s)))
                      (register-recovered-sandbox manager sandbox
                                                  :owner owner :task task)
                      (log:info "init: recovered ~A (~D layers)" id (length layers))
                      sandbox))))))
      (error (e)
        (log:warn "init: failed to recover ~A: ~A" id e)
        (ignore-errors (cleanup-sandbox-dir sandbox-dir))
        nil))))

(defun remount-sandbox-filesystems (manager sandbox-dir id layers)
  "Remount squashfs layers and overlay for a recovered sandbox.
   Returns a sandbox-mounts struct on success, NIL on failure."
  (let ((config (manager-config manager)))
    (handler-case
        (let* ((upper-path (format nil "~A/upper" sandbox-dir))
               (merged-path (format nil "~A/merged" sandbox-dir))
               (sqfs-mounts (make-array (length layers))))
          ;; Ensure upper/data and upper/work dirs exist
          (ensure-directories-exist (format nil "~A/data/" upper-path))
          (ensure-directories-exist (format nil "~A/work/" upper-path))
          ;; 1. Mount squashfs layers
          (loop for layer in layers
                for i from 0
                for mod-path = (format nil "~A/~A.squashfs"
                                       (modules-dir config) layer)
                for mp = (format nil "~A/images/~A.squashfs"
                                 sandbox-dir layer)
                do (setf (aref sqfs-mounts i)
                         (mount-squashfs mod-path mp)))
          ;; 2. Overlay
          (let* ((lower-components
                   (loop for m across sqfs-mounts
                         collect (squashfs-mount-mount-point m)))
                 (overlay (mount-overlay lower-components
                                         (format nil "~A/data" upper-path)
                                         (format nil "~A/work" upper-path)
                                         merged-path)))
            (make-sandbox-mounts
             :squashfs-mounts sqfs-mounts
             :overlay overlay)))
      (error (e)
        (log:warn "init: remount error for ~A: ~A" id e)
        nil))))

(defun cleanup-sandbox-dir (sandbox-dir)
  "Best-effort cleanup of a sandbox directory's mounts and files."
  (let ((merged (format nil "~A/merged" sandbox-dir)))
    ;; Try unmounting overlay
    (ignore-errors
      (uiop:run-program (list "sq-mount-overlay" "--unmount" merged)
                         :output nil :error-output nil :ignore-error-status t))
    ;; Unmount any squashfs layers under images/
    (let ((images-dir (format nil "~A/images" sandbox-dir)))
      (when (probe-file images-dir)
        (dolist (entry (uiop:subdirectories images-dir))
          (ignore-errors
            (uiop:run-program (list "sq-mount-layer" "--unmount"
                                     (namestring entry))
                               :output nil :error-output nil
                               :ignore-error-status t)))))))

(defun read-sandbox-meta (sandbox-dir)
  "Read .meta/sandbox.json from SANDBOX-DIR. Returns a plist or NIL."
  (let ((meta-path (format nil "~A/.meta/sandbox.json" sandbox-dir)))
    (when (probe-file meta-path)
      (handler-case
          (let ((content (uiop:read-file-string meta-path)))
            (when (and content (plusp (length content)))
              (parse-sandbox-meta-json content)))
        (error (e)
          (log:warn "init: failed to read metadata ~A: ~A" meta-path e)
          nil)))))

(defun parse-sandbox-meta-json (json-string)
  "Parse sandbox metadata JSON into a plist with :layers :owner :task
   :max-lifetime-s :created keys."
  (let ((parsed (jojo:parse json-string)))
    (when parsed
      (list :layers (getf parsed :|layers|)
            :owner (getf parsed :|owner|)
            :task (getf parsed :|task|)
            :cpu (or (getf parsed :|cpu|) 2.0)
            :memory-mb (or (getf parsed :|memory_mb|) 1024)
            :allow-net (getf parsed :|allow_net|)
            :max-lifetime-s (or (getf parsed :|max_lifetime_s|) 0)
            :created (or (getf parsed :|created|) 0)))))

(defun register-recovered-sandbox (manager sandbox &key owner task)
  "Register a recovered sandbox with the manager's sandbox table.
   This bypasses the normal creation flow since the sandbox already exists."
  (declare (ignore owner task))
  (bt:with-lock-held ((manager-lock manager))
    (setf (gethash (sandbox-id sandbox) (manager-sandboxes manager))
          sandbox)))

(defun init-recover (config manager)
  "Main init/recovery entry point. Scans data directory for existing
   sandboxes and attempts to recover each one."
  (let* ((data-dir (config-data-dir config))
         (ids (scan-sandbox-dirs data-dir))
         (recovered 0)
         (failed 0))
    (if (null ids)
        (log:info "init: no existing sandboxes found")
        (progn
          (log:info "init: found ~D existing sandbox(es), recovering..."
                    (length ids))
          (dolist (id ids)
            (if (recover-sandbox manager id data-dir)
                (incf recovered)
                (incf failed)))
          (log:info "init: recovery complete — ~D recovered, ~D failed"
                    recovered failed)))
    (values recovered failed)))
