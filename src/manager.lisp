(in-package #:squashd)

;;; ── Thread-Safe Sandbox Manager ─────────────────────────────────────
;;;
;;; The manager is the central registry for all live sandboxes.
;;; It holds a hash-table of sandbox structs protected by a lock,
;;; plus a reference to the daemon config (for max-sandboxes limits
;;; and data-dir paths).
;;;
;;; All public functions (manager-create-sandbox, manager-destroy-sandbox,
;;; manager-exec, list-sandbox-infos, manager-sandbox-info) acquire the
;;; manager lock for registry mutations. Per-sandbox operations (exec)
;;; hold only the per-sandbox lock after lookup.
;;;
;;; WARM PATH — runs on every API call.

(declaim (optimize (speed 2) (safety 2) (debug 2)))

;;; ── Manager struct ──────────────────────────────────────────────────

(defstruct (manager (:constructor %make-manager-internal))
  (sandboxes (make-hash-table :test #'equal) :type hash-table)
  (lock      (bt:make-lock "manager")        :type t)
  (config    nil                              :type (or null config)))

;;; ── Constructor ─────────────────────────────────────────────────────

(defun make-manager (config)
  "Create a new manager with the given CONFIG."
  (%make-manager-internal :config config))

;;; ── Sandbox lookup (internal) ───────────────────────────────────────

(defun %lookup-sandbox (manager id)
  "Look up sandbox by ID under the manager lock. Signals sandbox-error if not found."
  (let ((sandbox (gethash id (manager-sandboxes manager))))
    (unless sandbox
      (error 'sandbox-error :id id :message "not found"))
    sandbox))

;;; ── Create sandbox ──────────────────────────────────────────────────

(defun manager-create-sandbox (manager id &key owner layers task cpu
                                              memory-mb max-lifetime-s
                                              allow-net)
  "Create a new sandbox and register it with the manager.
   Thread-safe: acquires the manager lock to check limits and register.
   Signals sandbox-error on invalid ID, duplicate, or capacity exceeded.

   The actual sandbox creation (mounts, cgroup, netns) happens outside
   the manager lock to avoid holding it during slow I/O. Only the
   registry check and final registration hold the lock."

  ;; Validate ID
  (unless (valid-id-p id)
    (error 'sandbox-error :id (or id "") :message "invalid id"))

  ;; Check limits and reserve slot under lock
  (bt:with-lock-held ((manager-lock manager))
    (when (gethash id (manager-sandboxes manager))
      (error 'sandbox-error :id id :message "already exists"))
    (when (>= (hash-table-count (manager-sandboxes manager))
              (config-max-sandboxes (manager-config manager)))
      (error 'sandbox-error :id id :message "sandbox limit reached"))
    ;; Reserve the slot with a placeholder to prevent races
    (setf (gethash id (manager-sandboxes manager)) :creating))

  ;; Create the sandbox outside the lock (slow I/O: mounts, netns, cgroup)
  (let ((sandbox nil)
        (ok nil))
    (unwind-protect
         (progn
           (setf sandbox
                 (restart-case
                     (%create-sandbox-inner manager id
                                            :owner owner
                                            :layers layers
                                            :task task
                                            :cpu cpu
                                            :memory-mb memory-mb
                                            :max-lifetime-s max-lifetime-s
                                            :allow-net allow-net)
                   (retry-create ()
                     :report "Retry sandbox creation from the beginning"
                     ;; Remove placeholder before recursive call
                     (bt:with-lock-held ((manager-lock manager))
                       (remhash id (manager-sandboxes manager)))
                     (setf ok t)  ; prevent double-cleanup in unwind-protect
                     (manager-create-sandbox manager id
                                             :owner owner :layers layers
                                             :task task :cpu cpu
                                             :memory-mb memory-mb
                                             :max-lifetime-s max-lifetime-s
                                             :allow-net allow-net))
                   (skip-sandbox ()
                     :report "Skip this sandbox creation"
                     nil)))
           ;; Register the real sandbox or clean up placeholder
           (bt:with-lock-held ((manager-lock manager))
             (if sandbox
                 (setf (gethash id (manager-sandboxes manager)) sandbox)
                 (remhash id (manager-sandboxes manager))))
           (setf ok t)
           sandbox)
      ;; On abnormal exit, clean up the placeholder
      (unless ok
        (bt:with-lock-held ((manager-lock manager))
          (remhash id (manager-sandboxes manager)))))))

;;; ── Inner creation logic ────────────────────────────────────────────

(defun %create-sandbox-inner (manager id &key owner layers task cpu
                                             memory-mb max-lifetime-s
                                             allow-net)
  "Inner sandbox creation — mounts, cgroup, netns, metadata.
   Called with the manager slot already reserved."
  (let* ((config (manager-config manager))
         (layers (or layers '("000-base-alpine")))
         (sandbox-dir (format nil "~A/sandboxes/~A"
                              (config-data-dir config) id))
         (upper-path (format nil "~A/upper" sandbox-dir))
         (merged-path (format nil "~A/merged" sandbox-dir))
         (meta-dir (format nil "~A/.meta/log" sandbox-dir)))

    ;; Create directory tree
    (ensure-directories-exist (format nil "~A/" meta-dir))
    (ensure-directories-exist (format nil "~A/images/" sandbox-dir))
    (ensure-directories-exist (format nil "~A/snapshots/" sandbox-dir))

    ;; 1. Mount tmpfs for upper layer
    (with-rollback-on-error (tmpfs
                             (mount-tmpfs upper-path
                                          (or (config-upper-limit-mb config) 512))
                             (unmount-tmpfs tmpfs))

      ;; 2. Mount squashfs layers
      (let ((sqfs-mounts (make-array (length layers))))
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
                                  :report (lambda (s)
                                            (format s "Pull ~A from S3" layer))
                                  :test (lambda (c)
                                          (declare (ignore c))
                                          (not (null *s3-client*)))
                                  (funcall 's3-pull-module *s3-client* layer)
                                  (mount-squashfs mod-path mp)))))

               ;; 3. Overlay
               (let* ((lower-components
                        (loop for m across sqfs-mounts
                              collect (squashfs-mount-mount-point m)))
                      (overlay (mount-overlay lower-components
                                              (format nil "~A/data" upper-path)
                                              (format nil "~A/work" upper-path)
                                              merged-path)))

                 (with-rollback-on-error (overlay-guard overlay
                                          (unmount-overlay overlay-guard))

                   ;; 4. Cgroup (may fail — optional)
                   (let ((cgroup (handler-case
                                     (create-cgroup id
                                                    (or cpu 2.0)
                                                    (or memory-mb 1024))
                                   (error (e)
                                     (declare (ignore e))
                                     (warn 'cgroup-setup-failed :id id)
                                     nil))))

                     ;; 5. Network namespace
                     (with-rollback-on-error (netns
                                              (setup-netns config id allow-net)
                                              (teardown-netns netns))

                       ;; 6. Seed resolv.conf (DNS via netns gateway)
                       (ignore-errors
                         (seed-resolv-conf sandbox-dir netns))

                      ;; 7. Inject secret placeholders + proxy env (best effort)
                      (ignore-errors
                        (funcall 'inject-secret-placeholders config sandbox-dir netns))

                      ;; 8. Write metadata
                       (ignore-errors
                         (write-sandbox-meta config id
                                             :owner (or owner "anon")
                                             :layers layers
                                             :task (or task "")
                                             :cpu (or cpu 2.0)
                                             :memory-mb (or memory-mb 1024)
                                             :max-lifetime-s (or max-lifetime-s 0)
                                             :allow-net allow-net))

                       ;; Build and return the sandbox struct
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

;;; ── Destroy sandbox ─────────────────────────────────────────────────

(defun manager-destroy-sandbox (manager id)
  "Remove and destroy a sandbox by ID. Thread-safe.
   Removes from registry first, then tears down resources outside the lock."
  (let ((sandbox nil))
    ;; Remove from registry under lock
    (bt:with-lock-held ((manager-lock manager))
      (setf sandbox (gethash id (manager-sandboxes manager)))
      (unless sandbox
        (error 'sandbox-error :id id :message "not found"))
      ;; Skip if placeholder from in-progress creation
      (when (eq sandbox :creating)
        (error 'sandbox-error :id id :message "sandbox is still being created"))
      (remhash id (manager-sandboxes manager)))
    ;; Tear down resources outside the lock
    (destroy-sandbox sandbox)
    ;; Clean up sandbox directory (best effort)
    (let* ((config (manager-config manager))
           (sandbox-dir (format nil "~A/sandboxes/~A"
                                (config-data-dir config) id)))
      (ignore-errors (uiop:delete-directory-tree
                      (pathname sandbox-dir) :validate t)))
    id))

;;; ── Execute in sandbox ──────────────────────────────────────────────

(defun manager-exec (manager id cmd &key (workdir "/") (timeout 300))
  "Execute CMD in sandbox ID. Thread-safe.
   Looks up the sandbox under the manager lock, then executes outside it.
   Updates last-active timestamp."
  (let ((sandbox nil))
    ;; Lookup under manager lock
    (bt:with-lock-held ((manager-lock manager))
      (setf sandbox (%lookup-sandbox manager id))
      (when (eq sandbox :creating)
        (error 'sandbox-error :id id :message "sandbox is still being created"))
      (unless (eq (sandbox-state sandbox) :ready)
        (error 'sandbox-error :id id
               :message (format nil "sandbox not ready (state: ~A)"
                                (sandbox-state sandbox)))))
    ;; Execute outside the manager lock
    (setf (sandbox-last-active sandbox) (get-unix-time))
    (let* ((config (manager-config manager))
           (sandbox-dir (format nil "~A/sandboxes/~A"
                                (config-data-dir config) id)))
      (exec-in-sandbox sandbox cmd
                       :workdir workdir
                       :timeout timeout
                       :sandbox-dir sandbox-dir))))

;;; ── Sandbox info ────────────────────────────────────────────────────

(defun sandbox-to-info (sandbox)
  "Convert a sandbox struct to a plist for JSON serialization."
  (when (and sandbox (not (eq sandbox :creating)))
    (list :|id| (sandbox-id sandbox)
          :|state| (string-downcase (symbol-name (sandbox-state sandbox)))
          :|created| (sandbox-created sandbox)
          :|last_active| (sandbox-last-active sandbox)
          :|exec_count| (sandbox-exec-count sandbox)
          :|max_lifetime_s| (sandbox-max-lifetime-s sandbox))))

(defun list-sandbox-infos (manager)
  "Return a list of sandbox info plists for all sandboxes.
   Thread-safe: holds the manager lock for the snapshot."
  (let ((infos nil))
    (bt:with-lock-held ((manager-lock manager))
      (maphash (lambda (id sandbox)
                 (declare (ignore id))
                 (let ((info (sandbox-to-info sandbox)))
                   (when info (push info infos))))
               (manager-sandboxes manager)))
    (nreverse infos)))

(defun manager-sandbox-info (manager id)
  "Return sandbox info plist for a single sandbox by ID.
   Signals sandbox-error if not found."
  (bt:with-lock-held ((manager-lock manager))
    (let ((sandbox (%lookup-sandbox manager id)))
      (when (eq sandbox :creating)
        (error 'sandbox-error :id id :message "sandbox is still being created"))
      (sandbox-to-info sandbox))))

(defun manager-sandbox-count (manager)
  "Return the number of registered sandboxes (excluding placeholders)."
  (bt:with-lock-held ((manager-lock manager))
    (let ((count 0))
      (maphash (lambda (id sandbox)
                 (declare (ignore id))
                 (unless (eq sandbox :creating)
                   (incf count)))
               (manager-sandboxes manager))
      count)))

;;; ── Activate module ─────────────────────────────────────────────────

(defun manager-activate-module (manager id module-name)
  "Add a new module layer to an existing sandbox.
   Remounts the overlay with the new layer included.
   Signals sandbox-error if sandbox not found or module not available."
  (let ((sandbox nil))
    (bt:with-lock-held ((manager-lock manager))
      (setf sandbox (%lookup-sandbox manager id))
      (when (eq sandbox :creating)
        (error 'sandbox-error :id id :message "sandbox is still being created")))
    ;; Verify module exists
    (let* ((config (manager-config manager))
           (mod-path (format nil "~A/~A.squashfs"
                             (modules-dir config) module-name)))
      (unless (probe-file mod-path)
        ;; Try S3 pull if configured
        (if *s3-client*
            (handler-case
                (s3-pull-module *s3-client* module-name)
              (error ()
                (error 'sandbox-error :id id
                       :message (format nil "module not found: ~A" module-name))))
            (error 'sandbox-error :id id
                   :message (format nil "module not found: ~A" module-name))))
      ;; Mount the new squashfs layer
      (let* ((sandbox-dir (format nil "~A/sandboxes/~A"
                                  (config-data-dir config) id))
             (mp (format nil "~A/images/~A.squashfs" sandbox-dir module-name)))
        (when (probe-file mp)
          (error 'sandbox-error :id id
                 :message (format nil "already active: ~A" module-name)))
        (mount-squashfs mod-path mp)
        ;; Remount overlay with the new layer
        (let* ((mounts (sandbox-mounts sandbox))
               (old-overlay (sandbox-mounts-overlay mounts))
               (old-sqfs (sandbox-mounts-squashfs-mounts mounts))
               ;; Build new squashfs array with new mount appended
               (new-sqfs (let ((arr (make-array (1+ (length old-sqfs)))))
                           (loop for i below (length old-sqfs)
                                 do (setf (aref arr i) (aref old-sqfs i)))
                           (setf (aref arr (length old-sqfs))
                                 (make-squashfs-mount :mount-point mp))
                           arr))
               ;; Build new lowerdir
               (lower-components (loop for m across new-sqfs
                                       collect (squashfs-mount-mount-point m)))
               (upper-path (format nil "~A/upper" sandbox-dir))
               (merged-path (overlay-mount-merged-path old-overlay)))
          ;; Unmount old overlay
          (unmount-overlay old-overlay)
          ;; Mount new overlay
          (let ((new-overlay (mount-overlay lower-components
                                            (format nil "~A/data" upper-path)
                                            (format nil "~A/work" upper-path)
                                            merged-path)))
            (setf (sandbox-mounts-squashfs-mounts mounts) new-sqfs
                  (sandbox-mounts-overlay mounts) new-overlay)))))))

;;; ── Snapshot ──────────────────────────────────────────────────────────

(defun manager-snapshot (manager id label)
  "Create a snapshot of sandbox ID with optional LABEL.
   If LABEL is NIL, generates a timestamp-based label.
   Returns (values label size-in-bytes).
   Signals sandbox-error on failure."
  (let ((sandbox nil))
    (bt:with-lock-held ((manager-lock manager))
      (setf sandbox (%lookup-sandbox manager id))
      (when (eq sandbox :creating)
        (error 'sandbox-error :id id :message "sandbox is still being created")))
    (let* ((config (manager-config manager))
           (sandbox-dir (format nil "~A/sandboxes/~A"
                                (config-data-dir config) id))
           (snap-label (or label (format-timestamp-label)))
           (snapdir (format nil "~A/snapshots" sandbox-dir))
           (snapfile (format nil "~A/~A.squashfs" snapdir snap-label)))
      ;; Validate label
      (unless (valid-label-p snap-label)
        (error 'sandbox-error :id id
               :message "label: alphanumeric/dash/underscore/dot only"))
      (when (probe-file snapfile)
        (error 'sandbox-error :id id
               :message (format nil "snapshot exists: ~A" snap-label)))
      ;; Create snapshot directory
      (ensure-directories-exist (format nil "~A/" snapdir))
      ;; Use mksquashfs to create snapshot of upper layer data
      (let* ((upper-data (format nil "~A/upper/data" sandbox-dir)))
        (multiple-value-bind (exit-code stdout stderr)
            (run-command "mksquashfs" upper-data snapfile
                         "-comp" "gzip" "-b" "256K"
                         "-noappend" "-quiet")
          (declare (ignore stdout))
          (unless (zerop exit-code)
            (error 'sandbox-error :id id
                   :message (format nil "mksquashfs failed: ~A" stderr)))))
      ;; Get snapshot size
      (let ((size (handler-case
                      (with-open-file (s snapfile :element-type '(unsigned-byte 8))
                        (file-length s))
                    (error () 0))))
        ;; Background push to S3 if configured
        (when *s3-client*
          (ignore-errors
            (s3-push-bg *s3-client* snapfile
                        (format nil "sandboxes/~A/snapshots/~A.squashfs"
                                id snap-label))))
        (values snap-label size)))))

;;; ── Restore ──────────────────────────────────────────────────────────

(defun manager-restore (manager id label)
  "Restore sandbox ID from snapshot LABEL.
   Unmounts overlay, clears upper layer, mounts snapshot as read-only
   layer, then remounts overlay.
   Signals sandbox-error on failure."
  (let ((sandbox nil))
    (bt:with-lock-held ((manager-lock manager))
      (setf sandbox (%lookup-sandbox manager id))
      (when (eq sandbox :creating)
        (error 'sandbox-error :id id :message "sandbox is still being created")))
    (unless (valid-label-p label)
      (error 'sandbox-error :id id
             :message "label: alphanumeric/dash/underscore/dot only"))
    (let* ((config (manager-config manager))
           (sandbox-dir (format nil "~A/sandboxes/~A"
                                (config-data-dir config) id))
           (snapfile (format nil "~A/snapshots/~A.squashfs"
                             sandbox-dir label)))
      ;; Try to find snapshot locally, or pull from S3
      (unless (probe-file snapfile)
        (if *s3-client*
            (let ((ok (s3-pull *s3-client*
                               (format nil "sandboxes/~A/snapshots/~A.squashfs"
                                       id label)
                               snapfile)))
              (unless ok
                (error 'sandbox-error :id id
                       :message (format nil "snapshot not found: ~A" label))))
            (error 'sandbox-error :id id
                   :message (format nil "snapshot not found: ~A" label))))
      ;; Unmount current overlay
      (let ((mounts (sandbox-mounts sandbox)))
        (unmount-overlay (sandbox-mounts-overlay mounts))
        ;; Unmount previous snapshot if any
        (when (sandbox-mounts-snapshot-mount mounts)
          (ignore-errors
            (unmount-squashfs (sandbox-mounts-snapshot-mount mounts))))
        ;; Clear upper layer contents (preserve tmpfs mount)
        (let ((upper-data (format nil "~A/upper/data" sandbox-dir))
              (upper-work (format nil "~A/upper/work" sandbox-dir)))
          (ignore-errors (uiop:delete-directory-tree
                          (pathname upper-data) :validate t))
          (ignore-errors (uiop:delete-directory-tree
                          (pathname upper-work) :validate t))
          (ensure-directories-exist (format nil "~A/" upper-data))
          (ensure-directories-exist (format nil "~A/" upper-work)))
        ;; Mount snapshot as read-only layer
        (let* ((snap-mp (format nil "~A/images/_snapshot" sandbox-dir))
               (snap-mount (mount-squashfs snapfile snap-mp)))
          (setf (sandbox-mounts-snapshot-mount mounts) snap-mount))
        ;; Rebuild lowerdir: snapshot first (highest priority), then modules
        (let* ((lower-components
                 (append
                  (when (sandbox-mounts-snapshot-mount mounts)
                    (list (squashfs-mount-mount-point
                           (sandbox-mounts-snapshot-mount mounts))))
                  (loop for m across (sandbox-mounts-squashfs-mounts mounts)
                        collect (squashfs-mount-mount-point m))))
               (upper-path (format nil "~A/upper" sandbox-dir))
               (merged-path (overlay-mount-merged-path
                             (sandbox-mounts-overlay mounts)))
               (new-overlay (mount-overlay lower-components
                                           (format nil "~A/data" upper-path)
                                           (format nil "~A/work" upper-path)
                                           merged-path)))
          (setf (sandbox-mounts-overlay mounts) new-overlay))
        ;; Re-seed DNS and re-inject secret placeholders after upper reset.
        (ignore-errors
          (seed-resolv-conf sandbox-dir (sandbox-netns sandbox)))
        (ignore-errors
          (funcall 'inject-secret-placeholders config sandbox-dir (sandbox-netns sandbox)))))))

;;; ── Exec logs ────────────────────────────────────────────────────────

(defun manager-exec-logs (manager id)
  "Return execution log entries for sandbox ID as a list of plists.
   Reads JSON log files from .meta/log/ directory.
   Signals sandbox-error if sandbox not found."
  (bt:with-lock-held ((manager-lock manager))
    (%lookup-sandbox manager id))
  (let* ((config (manager-config manager))
         (log-dir (format nil "~A/sandboxes/~A/.meta/log"
                          (config-data-dir config) id))
         (logs nil))
    (when (probe-file log-dir)
      (dolist (path (sort (directory
                           (make-pathname
                            :directory (pathname-directory (pathname log-dir))
                            :name :wild
                            :type "json"))
                          #'string< :key #'namestring))
        (handler-case
            (let ((content (uiop:read-file-string (namestring path))))
              (when (and content (plusp (length content)))
                (push (jojo:parse content) logs)))
          (error () nil))))
    (nreverse logs)))

;;; ── Helpers ──────────────────────────────────────────────────────────

(defun format-timestamp-label ()
  "Generate a timestamp string suitable for snapshot labels: YYYYMMDD-HHMMSS."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
            year month day hour min sec)))
