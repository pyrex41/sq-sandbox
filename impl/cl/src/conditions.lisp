(in-package #:squashd)

;;; ── Condition Types ─────────────────────────────────────────────────

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
  ((id      :initarg :id      :reader sandbox-error-id)
   (message :initarg :message :reader sandbox-error-message))
  (:report (lambda (c s)
             (format s "sandbox ~A: ~A"
                     (sandbox-error-id c)
                     (sandbox-error-message c)))))

(define-condition cgroup-setup-failed (warning)
  ((id :initarg :id :reader cgroup-error-id))
  (:report (lambda (c s)
             (format s "cgroup setup failed for ~A (continuing without limits)"
                     (cgroup-error-id c)))))

;;; ── Restart-Equipped Functions ──────────────────────────────────────
;;;
;;; These functions establish restart cases around operations that may
;;; signal recoverable conditions. The restarts are invoked either
;;; interactively (via SLIME/Swank debugger) or automatically by
;;; production-handler in daemon mode.

(defvar *s3-client* nil
  "Bound to an S3 client instance when S3 is configured.
   Used by pull-from-s3-and-retry restart to fetch missing modules.")

(defun mount-squashfs-layers (sandbox-root layers module-path-fn)
  "Mount each squashfs module. If a module is missing, offer to pull from S3.
   SANDBOX-ROOT is the base path for mount points.
   LAYERS is a list of layer names (strings).
   MODULE-PATH-FN is a function (layer-name) → path to the .sqfs file."
  (loop for layer in layers
        for source-path = (funcall module-path-fn layer)
        for mount-point = (format nil "~A/layers/~A" sandbox-root layer)
        collect
        (restart-case
            (progn
              (unless (probe-file source-path)
                (error 'module-not-found :name layer))
              (mount-squashfs source-path mount-point))
          (pull-from-s3-and-retry ()
            :report (lambda (s) (format s "Pull ~A from S3 and retry" layer))
            :test (lambda (c) (declare (ignore c)) (not (null *s3-client*)))
            ;; s3-pull-module will be defined in s3.lisp (task 8)
            (funcall 's3-pull-module *s3-client* layer)
            (mount-squashfs (funcall module-path-fn layer) mount-point))
          (skip-layer ()
            :report (lambda (s) (format s "Skip layer ~A" layer))
            nil))))

;;; ── Production Handler ──────────────────────────────────────────────
;;;
;;; When running as a daemon (no interactive debugger), this handler
;;; automatically selects the appropriate restart for known conditions.
;;; Unknown conditions propagate normally to the enclosing handler-case
;;; or cause debugger entry (which in non-interactive mode means abort).

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
     ;; Can't recover from mount failures in production.
     ;; Let it unwind to the cleanup handler.
     nil)))

(defun call-with-production-handlers (thunk)
  "Call THUNK with production condition handlers installed.
   Use this to wrap the main server loop in daemon mode."
  (handler-bind ((module-not-found #'production-handler)
                 (cgroup-setup-failed #'production-handler)
                 (mount-error #'production-handler))
    (funcall thunk)))

(defmacro with-production-handlers (&body body)
  "Execute BODY with production condition handlers installed."
  `(call-with-production-handlers (lambda () ,@body)))
