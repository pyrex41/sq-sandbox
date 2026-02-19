(in-package #:squashd)

;;; ── Secret placeholder injection ────────────────────────────────────
;;;
;;; Writes /etc/profile.d/squash-secrets.sh into the sandbox upper layer with:
;;;   - placeholder exports from secrets.json (or SQUASH_SECRETS_DIR fallback)
;;;   - proxy env vars pointing at the netns gateway (:8888)
;;;   - optional CA trust env vars when proxy HTTPS mode is enabled

(declaim (optimize (speed 1) (safety 3) (debug 2)))

(defun %plist-get-any (plist keys)
  "Return the first present value from PLIST for KEYS."
  (loop for key in keys
        for marker = (gensym "MISSING")
        for value = (getf plist key marker)
        unless (eq value marker) do (return value)))

(defun %object-pairs (obj)
  "Return OBJ as a list of (key . value) pairs for hash tables or plists."
  (cond
    ((hash-table-p obj)
     (let (pairs)
       (maphash (lambda (k v) (push (cons k v) pairs)) obj)
       (nreverse pairs)))
    ((listp obj)
     (loop for (k v) on obj by #'cddr
           when (or (symbolp k) (stringp k))
             collect (cons k v)))
    (t nil)))

(defun %normalize-secret-name (key)
  "Convert JSON key to an env var-like name."
  (string-upcase
   (cond
     ((symbolp key) (symbol-name key))
     ((stringp key) key)
     (t (princ-to-string key)))))

(defun %collect-secrets-from-json (config)
  "Collect (NAME . PLACEHOLDER) from {data-dir}/secrets.json."
  (let* ((path (format nil "~A/secrets.json" (config-data-dir config))))
    (unless (probe-file path)
      (return-from %collect-secrets-from-json nil))
    (handler-case
        (let* ((content (uiop:read-file-string path))
               (parsed (jojo:parse content))
               (secrets (%plist-get-any parsed '(:|secrets| :secrets)))
               (pairs (%object-pairs secrets))
               (result nil))
          (dolist (entry pairs)
            (let* ((name (%normalize-secret-name (car entry)))
                   (spec (cdr entry))
                   (placeholder
                    (cond
                      ((hash-table-p spec)
                       (or (gethash "placeholder" spec)
                           (gethash "PLACEHOLDER" spec)))
                      ((listp spec)
                       (%plist-get-any spec '(:|placeholder| :placeholder)))
                      (t nil))))
              (when (and name placeholder)
                (push (cons name (princ-to-string placeholder)) result))))
          (nreverse result))
      (error (e)
        (log:warn "secrets: failed to parse ~A: ~A" path e)
        nil))))

(defun %collect-secrets-from-dir (config)
  "Collect fallback placeholders from SQUASH_SECRETS_DIR file names."
  (let ((dir (config-secrets-dir config)))
    (unless (and dir (probe-file dir))
      (return-from %collect-secrets-from-dir nil))
    (let ((result nil))
      (dolist (path (uiop:directory-files dir))
        (let* ((name (string-upcase (pathname-name path)))
               (placeholder (format nil "${~A}" name)))
          (push (cons name placeholder) result)))
      (nreverse result))))

(defun load-secret-placeholders (config)
  "Load placeholder mappings as (NAME . PLACEHOLDER)."
  (or (%collect-secrets-from-json config)
      (%collect-secrets-from-dir config)))

(defun inject-secret-placeholders (config sandbox-dir &optional netns)
  "Write /etc/profile.d/squash-secrets.sh for SANDBOX-DIR.
Returns T when a script was written, NIL when nothing to inject."
  (let ((secrets (load-secret-placeholders config)))
    (unless secrets
      (return-from inject-secret-placeholders nil))

    (let* ((env-dir (format nil "~A/upper/data/etc/profile.d" sandbox-dir))
           (script-path (format nil "~A/squash-secrets.sh" env-dir))
           (proxy-host (if netns
                           (format nil "10.200.~D.1" (netns-handle-index netns))
                           "127.0.0.1"))
           (proxy-url (format nil "http://~A:8888" proxy-host))
           (lines nil))
      (ensure-directories-exist (format nil "~A/" env-dir))

      ;; Secret placeholders
      (dolist (entry secrets)
        (push (format nil "export ~A='~A'" (car entry) (cdr entry)) lines))

      ;; Proxy env
      (dolist (name '("http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY"))
        (push (format nil "export ~A=~A" name proxy-url) lines))

      ;; HTTPS proxy CA trust exports (best effort)
      (when (config-proxy-https config)
        (let* ((ca-src (format nil "~A/proxy-ca/ca.crt" (config-data-dir config)))
               (ca-dst-dir (format nil "~A/upper/data/usr/local/share/ca-certificates" sandbox-dir))
               (ca-dst (format nil "~A/sq-proxy-ca.crt" ca-dst-dir)))
          (when (probe-file ca-src)
            (ensure-directories-exist (format nil "~A/" ca-dst-dir))
            (handler-case
                (write-string-to-file ca-dst (uiop:read-file-string ca-src))
              (error (e)
                (log:warn "secrets: failed to copy CA cert: ~A" e)))
            (push "export NODE_EXTRA_CA_CERTS=/usr/local/share/ca-certificates/sq-proxy-ca.crt" lines)
            (push "export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt" lines)
            (push "export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt" lines))))

      (write-string-to-file
       script-path
       (format nil "~{~A~%~}" (nreverse lines)))
      t)))
