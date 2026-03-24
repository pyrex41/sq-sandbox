(in-package #:squashd)

;;; ── HTTP API ─────────────────────────────────────────────────────────
;;;
;;; Clack/Lack/Ningle web stack:
;;; - Ningle: Sinatra-style routing (setf (ningle:route app path) handler)
;;; - Lack:   Middleware composition via lack:builder
;;; - Clack:  Server abstraction (clack:clackup → Woo backend)
;;;
;;; Route handlers receive a params alist (query + body merged).
;;; Path params are keywords (:id), body/query params are strings ("key").
;;; ningle:*request* and ningle:*response* are bound per-request.
;;;
;;; WARM PATH — runs on every HTTP request.

(declaim (optimize (speed 2) (safety 2) (debug 2)))

;;; ── Ningle app instance ──────────────────────────────────────────────

(defvar *app* (make-instance 'ningle:app)
  "The Ningle application instance. Routes are attached via setf.")

;;; ── JSON response helper ─────────────────────────────────────────────

(defun json-response (status body)
  "Return a Lack-compatible response triple with JSON body.
   STATUS is an HTTP status code. BODY is a plist or alist for Jonathan."
  (declare (type fixnum status))
  (list status
        '(:content-type "application/json")
        (list (jojo:to-json body))))

(defun no-content-response ()
  "Return a 204 No Content response."
  '(204 () ("")))

;;; ── Request body parsing ─────────────────────────────────────────────

(defconstant +max-request-body-bytes+ (* 1024 1024)
  "Maximum allowed request body size (1 MB).")

(defun parse-json-body (request)
  "Parse JSON from the request body. Returns a plist (Jonathan default).
   Returns NIL on parse failure or empty body.
   Signals a body-too-large condition (caught by callers) if content-length > 1MB."
  (handler-case
      (let ((content-type (lack.request:request-content-type request)))
        (when (and content-type
                   (search "application/json" content-type))
          ;; Check content-length before reading body
          (let ((content-length (lack.request:request-content-length request)))
            (when (and content-length
                       (> content-length +max-request-body-bytes+))
              (return-from parse-json-body :body-too-large)))
          (let ((body-bytes (lack.request:request-content request)))
            (when (and body-bytes (> (length body-bytes) +max-request-body-bytes+))
              (return-from parse-json-body :body-too-large))
            (when (and body-bytes (plusp (length body-bytes)))
              (jojo:parse (babel:octets-to-string body-bytes :encoding :utf-8))))))
    (error () nil)))

(defun parse-layers (layers-value)
  "Normalize a layers value to a list of strings.
   Accepts: a list of strings, a comma-separated string, or NIL (defaults to base)."
  (etypecase layers-value
    (list layers-value)
    (string (cl-ppcre:split "\\s*,\\s*" layers-value))
    (null '("000-base-alpine"))))

;;; ── Auth middleware (Lack pattern) ───────────────────────────────────
;;;
;;; A Lack middleware is: (lambda (app) (lambda (env) ...))
;;; ENV is a plist with :request-method, :path-info, :headers (hash-table), etc.
;;; Response is a list: (status-code headers-plist body-list).

(defun make-auth-middleware (token)
  "Create a Lack middleware that checks Bearer token on /cgi-bin/api/ paths.
   TOKEN is the expected bearer token string, or NIL to skip auth."
  (lambda (app)
    (lambda (env)
      (let ((path (getf env :path-info)))
        (if (and token
                 (search "/cgi-bin/api/" path)
                 (let* ((headers (getf env :headers))
                        (auth-header (gethash "authorization" headers "")))
                   (not (string= (concatenate 'string "Bearer " token)
                                 auth-header))))
            ;; Unauthorized
            '(401 (:content-type "application/json")
              ("{\"error\":\"unauthorized\"}"))
            ;; Proceed
            (funcall app env))))))

;;; ── App composition via lack:builder ─────────────────────────────────

(defun build-app (config)
  "Compose the full Lack application with middleware stack.
   CONFIG provides auth-token for the auth middleware.
   Returns a Lack app function suitable for clack:clackup."
  (let ((token (config-auth-token config)))
    (if token
        (lack:builder
          :accesslog
          (make-auth-middleware token)
          *app*)
        (lack:builder
          :accesslog
          *app*))))

;;; ── Route definitions ────────────────────────────────────────────────

;;; ── Health check — unauthenticated (not under /cgi-bin/api/) ────────

(setf (ningle:route *app* "/cgi-bin/health")
      (lambda (params)
        (declare (ignore params))
        (handler-case
            (let* ((backend (case (config-backend *config*)
                                (:firecracker "firecracker")
                                (:gvisor "gvisor")
                                (otherwise "chroot")))
                   (sb-count (manager-sandbox-count *manager*))
                   (mod-count (count-modules *config*))
                   (base-ready (base-module-exists-p *config*)))
              (json-response 200
                (list :|status| "ok"
                      :|backend| backend
                      :|sandboxes| sb-count
                      :|modules| mod-count
                      :|base_ready| base-ready)))
          (error ()
            (json-response 200 '(:|status| "ok"))))))

;;; ── Sandbox collection routes ───────────────────────────────────────

;;; List all sandboxes

(setf (ningle:route *app* "/cgi-bin/api/sandboxes" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (handler-case
            (json-response 200 (list-sandbox-infos *manager*))
          (error (e)
            (json-response 500
              (list :|error| (format nil "~A" e)))))))

;;; Create a sandbox

(setf (ningle:route *app* "/cgi-bin/api/sandboxes" :method :POST)
      (lambda (params)
        (declare (ignore params))
        (let ((body (parse-json-body ningle:*request*)))
          (if (eq body :body-too-large)
              (json-response 413 '(:|error| "request body too large"))
              (handler-case
                  (let* ((id (getf body :|id|))
                         (layers (parse-layers (getf body :|layers|)))
                         (sandbox
                           (manager-create-sandbox
                            *manager* id
                            :owner (getf body :|owner|)
                            :layers layers
                            :task (getf body :|task|)
                            :cpu (getf body :|cpu|)
                            :memory-mb (getf body :|memory_mb|)
                            :max-lifetime-s (getf body :|max_lifetime_s|)
                            :allow-net (getf body :|allow_net|)
                            :policy (getf body :|policy|))))
                    (json-response 201 (sandbox-to-info sandbox)))
                (sandbox-error (e)
                  (json-response 400
                    (list :|error| (format nil "~A" e)))))))))

;;; ── Sandbox instance routes ─────────────────────────────────────────

;;; Get sandbox info

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id" :method :GET)
      (lambda (params)
        (let ((id (cdr (assoc :id params))))
          (handler-case
              (json-response 200 (manager-sandbox-info *manager* id))
            (sandbox-error (e)
              (json-response 404
                (list :|error| (format nil "~A" e))))))))

;;; Destroy a sandbox

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id" :method :DELETE)
      (lambda (params)
        (let ((id (cdr (assoc :id params))))
          (handler-case
              (progn
                (manager-destroy-sandbox *manager* id)
                (no-content-response))
            (sandbox-error (e)
              (json-response 404
                (list :|error| (format nil "~A" e))))))))

;;; ── Sandbox action routes ───────────────────────────────────────────

;;; Execute command in sandbox

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/exec" :method :POST)
      (lambda (params)
        (let* ((id (cdr (assoc :id params)))
               (body (parse-json-body ningle:*request*)))
          (if (eq body :body-too-large)
              (json-response 413 '(:|error| "request body too large"))
              (let ((cmd (getf body :|cmd|)))
                (if (null cmd)
                    (json-response 400 (list :|error| "cmd required"))
                    (let ((workdir (or (getf body :|workdir|) "/"))
                          (timeout (or (getf body :|timeout|) 300)))
                      (handler-case
                          (let ((result (manager-exec *manager* id cmd
                                                      :workdir workdir
                                                      :timeout timeout)))
                            (json-response 200 (exec-result-to-alist result)))
                        (sandbox-error (e)
                          (json-response 404
                            (list :|error| (format nil "~A" e))))))))))))

;;; Activate module (add layer to running sandbox)

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/activate" :method :POST)
      (lambda (params)
        (let* ((id (cdr (assoc :id params)))
               (body (parse-json-body ningle:*request*)))
          (if (eq body :body-too-large)
              (json-response 413 '(:|error| "request body too large"))
              (let ((module-name (getf body :|module|)))
                (if (null module-name)
                    (json-response 400 (list :|error| "module required"))
                    (handler-case
                        (progn
                          (manager-activate-module *manager* id module-name)
                          (json-response 200 (manager-sandbox-info *manager* id)))
                      (sandbox-error (e)
                        (json-response 400
                          (list :|error| (format nil "~A" e)))))))))))

;;; Create snapshot

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/snapshot" :method :POST)
      (lambda (params)
        (let* ((id (cdr (assoc :id params)))
               (body (parse-json-body ningle:*request*)))
          (if (eq body :body-too-large)
              (json-response 413 '(:|error| "request body too large"))
              (let ((label (getf body :|label|)))
                (handler-case
                    (multiple-value-bind (snap-label snap-size)
                        (manager-snapshot *manager* id label)
                      (json-response 200
                        (list :|snapshot| snap-label
                              :|size| snap-size)))
                  (sandbox-error (e)
                    (json-response 400
                      (list :|error| (format nil "~A" e))))))))))

;;; Restore snapshot

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/restore" :method :POST)
      (lambda (params)
        (let* ((id (cdr (assoc :id params)))
               (body (parse-json-body ningle:*request*)))
          (if (eq body :body-too-large)
              (json-response 413 '(:|error| "request body too large"))
              (let ((label (getf body :|label|)))
                (if (null label)
                    (json-response 400 (list :|error| "label required"))
                    (handler-case
                        (progn
                          (manager-restore *manager* id label)
                          (json-response 200 (manager-sandbox-info *manager* id)))
                      (sandbox-error (e)
                        (json-response 400
                          (list :|error| (format nil "~A" e)))))))))))

;;; Get execution logs

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/logs" :method :GET)
      (lambda (params)
        (let ((id (cdr (assoc :id params))))
          (handler-case
              (json-response 200 (manager-exec-logs *manager* id))
            (sandbox-error (e)
              (json-response 404
                (list :|error| (format nil "~A" e))))))))

;;; ── Module routes ───────────────────────────────────────────────────

;;; List available modules

(setf (ningle:route *app* "/cgi-bin/api/modules" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (handler-case
            (json-response 200 (list-available-modules *config*))
          (error (e)
            (json-response 500
              (list :|error| (format nil "~A" e)))))))

;;; ── WireGuard peers route ────────────────────────────────────────────

(setf (ningle:route *app* "/cgi-bin/api/sandboxes/:id/wg/peers" :method :POST)
      (lambda (params)
        (let ((id (cdr (assoc :id params))))
          (handler-case
              (let* ((body (parse-json-body ningle:*request*))
                     (peers (if (listp body) body nil)))
                ;; Require sandbox to exist with a netns
                (let* ((sandbox (%lookup-sandbox *manager* id))
                       (netns (when sandbox
                                (let ((n (sandbox-netns sandbox)))
                                  (when n (netns-handle-name n))))))
                  (unless netns
                    (return-from nil
                      (json-response 400
                        '(:|error| "sandbox has no network namespace"))))
                  ;; Set up wg0 if not already present
                  (let* ((meta-dir (format nil "~A/sandboxes/~A/.meta"
                                          (config-data-dir *config*) id))
                         (port-file (format nil "~A/wg_listen_port" meta-dir))
                         (wg-exists (probe-file port-file)))
                    (unless wg-exists
                      ;; Generate keypair via sq-wg
                      (let ((genkey-output
                              (with-output-to-string (s)
                                (uiop:run-program '("sq-wg" "genkey")
                                  :output s :ignore-error-status t))))
                        (let* ((lines (cl-ppcre:split "\\n"
                                        (string-trim '(#\Space #\Newline #\Return)
                                                     genkey-output)))
                               (privkey (first lines))
                               (pubkey (second lines)))
                          (unless (and privkey pubkey)
                            (return-from nil
                              (json-response 500
                                '(:|error| "wg genkey failed"))))
                          ;; Setup wg0
                          (uiop:run-program
                            (list "sq-wg" "setup" id privkey "51820")
                            :ignore-error-status t)
                          ;; Store public key
                          (with-open-file (out (format nil "~A/wg_public_key" meta-dir)
                                           :direction :output :if-exists :supersede)
                            (write-string pubkey out)))))
                    ;; Add each peer
                    (dolist (peer peers)
                      (let ((pk (getf peer :|publicKey|))
                            (ep (or (getf peer :|endpoint|) ""))
                            (ips (or (getf peer :|allowedIPs|) "0.0.0.0/0"))
                            (psk (or (getf peer :|presharedKey|) "")))
                        (when pk
                          (uiop:run-program
                            (list "sq-wg" "add-peer" id pk ep ips psk)
                            :ignore-error-status t))))
                    ;; Read back metadata
                    (let ((pubkey (handler-case
                                     (string-trim '(#\Space #\Newline #\Return)
                                       (uiop:read-file-string
                                         (format nil "~A/wg_public_key" meta-dir)))
                                   (error () ""))))
                      (json-response 200
                        (list :|status| "ok"
                              :|publicKey| pubkey
                              :|listenPort| 51820
                              :|peersAdded| (length peers)))))))
            (sandbox-error (e)
              (json-response 404
                (list :|error| (format nil "~A" e))))))))

;;; ── Bus notify helper ──────────────────────────────────────────────

(defun bus-notify-push (config local-path s3-key)
  "Notify sq-sync sidecar to push a file to S3. Falls back to direct S3 push."
  (let ((bus-sock (config-bus-sock-path config)))
    (when (and bus-sock (probe-file bus-sock))
      (let ((msg (format nil "{\"op\":\"push\",\"path\":\"~A\",\"key\":\"~A\"}"
                         local-path s3-key)))
        (ignore-errors
          (uiop:run-program (list "sq-sync" "--notify" msg)
                            :ignore-error-status t))
        (return-from bus-notify-push t))))
  ;; Fallback: direct S3 push
  (when *s3-client*
    (ignore-errors
      (s3-push-bg *s3-client* local-path s3-key)))
  nil)

;;; ── Conversion helpers ───────────────────────────────────────────────

(defun exec-result-to-alist (result)
  "Convert an exec-result struct to a plist for JSON serialization."
  (list :|exit_code| (exec-result-exit-code result)
        :|stdout| (exec-result-stdout result)
        :|stderr| (exec-result-stderr result)
        :|started| (exec-result-started result)
        :|finished| (exec-result-finished result)
        :|duration_ms| (exec-result-duration-ms result)
        :|seq| (exec-result-seq result)))
