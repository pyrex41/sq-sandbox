(in-package #:squashd)

;;; ── Main entry point ──────────────────────────────────────────────────
;;;
;;; Orchestrates: GC tuning, config loading, init/recovery, S3 client
;;; creation, Go proxy start, optional Swank server, manager creation,
;;; reaper thread, and HTTP server launch via Clack.
;;;
;;; COLD PATH — runs once at startup.

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defvar *manager* nil
  "The global sandbox manager instance.")

(defvar *config* nil
  "The global configuration instance.")

(defvar *go-proxy-process* nil
  "Handle for the Go secret proxy child process.")

(defvar *clack-handler* nil
  "Handle returned by clack:clackup, used for graceful shutdown.")

(defun main ()
  "Main entry point for the squashd daemon.
   Orchestrates full startup sequence and blocks on the HTTP server."

  ;; 1. GC tuning — 64MB nursery to reduce minor GC frequency
  (setf (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))
  (sb-ext:gc :full t)

  ;; 2. Load configuration from environment
  (setf *config* (config-from-env))

  (log:info "squashd v4 (sbcl ~A)" (lisp-implementation-version))

  ;; 3. Ensure data directories exist
  (let ((data-dir (config-data-dir *config*)))
    (ensure-directories-exist (format nil "~A/sandboxes/" data-dir))
    (ensure-directories-exist (format nil "~A/modules/" data-dir)))

  ;; 4. Create sandbox manager (needed by init-recover)
  (setf *manager* (make-manager *config*))

  ;; 5. Init / recovery — scan for existing sandboxes
  (init-recover *config* *manager*)

  ;; 6. S3 client
  (when (config-s3-bucket *config*)
    (setf *s3-client* (make-s3-client-from-config *config*))
    (log:info "s3: configured for bucket ~A" (config-s3-bucket *config*)))

  ;; 7. Start Go secret proxy (via uiop:launch-program)
  (when (secrets-exist-p *config*)
    (start-go-proxy *config*))

  ;; 8. Optional Swank server for live REPL debugging
  (start-swank-if-enabled)

  ;; 9. Reaper thread
  (bt:make-thread (lambda () (reaper-loop *manager*))
                  :name "squashd-reaper")

  ;; 10. HTTP server (via Clack → Woo backend)
  (let ((app (build-app *config*))
        (port (config-port *config*)))
    (let ((mod-count (count-modules *config*))
          (sb-count (hash-table-count (manager-sandboxes *manager*))))
      (log:info "ready — port: ~D, modules: ~D, sandboxes: ~D"
                port mod-count sb-count))
    ;; clack:clackup with Woo starts the event loop in a background thread
    ;; and returns immediately. We block the main thread with a semaphore
    ;; so the process stays alive as PID 1 in Docker.
    (setf *clack-handler*
          (clack:clackup app
                         :server :woo
                         :port port
                         :address "0.0.0.0"
                         :worker-num 4
                         :debug nil
                         :use-default-middlewares nil))
    ;; Block forever — the reaper and HTTP server run in background threads.
    ;; SIGTERM/SIGINT will terminate the process via SBCL's default handlers.
    (bt:wait-on-semaphore (bt:make-semaphore :name "main-block"))))

;;; ── Go proxy management ───────────────────────────────────────────────

(defun start-go-proxy (config)
  "Start the Go secret proxy as a child process via uiop:launch-program."
  (let ((proxy-bin (or (uiop:getenv "SQ_PROXY_BIN")
                       "/app/bin/sq-secret-proxy-https")))
    (when (probe-file proxy-bin)
      (handler-case
          (let ((process (uiop:launch-program
                          (list proxy-bin)
                          :output :interactive
                          :error-output :interactive)))
            (setf *go-proxy-process* process)
            (log:info "proxy: started Go secret proxy (~A)" proxy-bin))
        (error (e)
          (log:warn "proxy: failed to start Go proxy: ~A" e))))))

;;; ── Swank server ──────────────────────────────────────────────────────

(defun start-swank-if-enabled ()
  "Start Swank REPL server if SQUASH_SWANK=1 environment variable is set."
  (when (string= "1" (uiop:getenv "SQUASH_SWANK"))
    (handler-case
        (progn
          (asdf:load-system "swank")
          (let ((create-server (find-symbol "CREATE-SERVER" "SWANK")))
            (when create-server
              (funcall create-server :port 4005 :dont-close t)
              (log:info "swank: REPL server listening on port 4005"))))
      (error (e)
        (log:warn "swank: failed to start: ~A" e)))))

;;; ── Graceful shutdown ─────────────────────────────────────────────────

(defun shutdown ()
  "Gracefully shut down the daemon. Stop HTTP server, kill proxy, cleanup."
  (log:info "shutdown: initiated")
  ;; Stop HTTP server
  (when *clack-handler*
    (ignore-errors (clack:stop *clack-handler*))
    (setf *clack-handler* nil))
  ;; Stop Go proxy
  (when *go-proxy-process*
    (ignore-errors (uiop:terminate-process *go-proxy-process*))
    (setf *go-proxy-process* nil))
  (log:info "shutdown: complete"))

;;; ── Image building ────────────────────────────────────────────────────

(defun build-image (&key (output "squashd"))
  "Save a standalone executable image.
   Call this after loading the squashd system:
     (ql:quickload \"squashd\")
     (squashd::build-image)"
  ;; Full GC before save to compact heap
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die output
    :toplevel #'squashd:main
    :executable t
    :compression t    ; zstd compression — shrinks image from ~50MB to ~20MB
    :purify t         ; move immutable data to read-only space (helps fork COW)
    :save-runtime-options t))
