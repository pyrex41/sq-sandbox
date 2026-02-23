(in-package #:squashd)

;;; Use defstruct for performance — direct slot access, no CLOS dispatch

(defstruct squashfs-mount
  (mount-point "" :type simple-string)
  (active-p t :type boolean))

(defstruct overlay-mount
  (merged-path "" :type simple-string)
  (active-p t :type boolean))

(defstruct sandbox-mounts
  (squashfs-mounts #() :type simple-vector)  ; vector of squashfs-mount
  (snapshot-mount nil :type (or null squashfs-mount))
  (overlay nil :type (or null overlay-mount)))

;;; ── Mount ─────────────────────────────────────────────────────────────

(defun mount-squashfs (source-path mount-point)
  (declare (type simple-string source-path mount-point))
  (ensure-directories-exist (concatenate 'string mount-point "/"))
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (list "sq-mount-layer" source-path mount-point)
                         :output nil :error-output :string
                         :ignore-error-status t)
    (declare (ignore stdout))
    (unless (zerop exit-code)
      (error 'mount-error :source source-path :target mount-point
                           :errno exit-code)))
  (make-squashfs-mount :mount-point mount-point))

(defun unmount-squashfs (mount)
  (when (and mount (squashfs-mount-active-p mount))
    (uiop:run-program (list "sq-mount-layer" "--unmount"
                             (squashfs-mount-mount-point mount))
                       :output nil :error-output nil :ignore-error-status t)
    (setf (squashfs-mount-active-p mount) nil)))

(defun mount-overlay (lower-components upper-data work merged)
  "Mount an overlay filesystem. LOWER-COMPONENTS is a list of lower dirs."
  (ensure-directories-exist (concatenate 'string merged "/"))
  ;; sq-mount-overlay accepts colon-separated lowerdir string
  (let ((lowerdir (format nil "~{~A~^:~}" lower-components)))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program (list "sq-mount-overlay" lowerdir upper-data work merged)
                           :output nil :error-output :string
                           :ignore-error-status t)
      (declare (ignore stdout))
      (unless (zerop exit-code)
        (error 'mount-error :source "overlay" :target merged
                             :errno exit-code))))
  (make-overlay-mount :merged-path merged))

(defun unmount-overlay (mount)
  (when (and mount (overlay-mount-active-p mount))
    (uiop:run-program (list "sq-mount-overlay" "--unmount"
                             (overlay-mount-merged-path mount))
                       :output nil :error-output nil :ignore-error-status t)
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
    ;; 3. Squashfs layers in reverse
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
