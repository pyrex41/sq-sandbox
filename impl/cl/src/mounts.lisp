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