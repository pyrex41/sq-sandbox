(in-package #:squashd)

;;; ── Configuration ────────────────────────────────────────────────────
;;;
;;; Daemon configuration loaded from environment variables at startup.
;;; All fields have sensible defaults so the daemon can start with
;;; zero configuration for local development.
;;;
;;; COLD PATH — loaded once at startup.

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defstruct config
  (data-dir       "/data"  :type simple-string)
  (port           8080     :type fixnum)
  (max-sandboxes  100      :type fixnum)
  (upper-limit-mb 512      :type fixnum)
  (backend        :chroot  :type keyword)
  (auth-token     nil      :type (or null simple-string))
  (s3-bucket      nil      :type (or null simple-string))
  (s3-region      "us-east-2" :type simple-string)
  (s3-prefix      "modules"   :type simple-string)
  (s3-access-key  nil      :type (or null simple-string))
  (s3-secret-key  nil      :type (or null simple-string))
  (s3-endpoint    nil      :type (or null simple-string))
  (proxy-https    nil      :type boolean)
  (secrets-dir    nil      :type (or null simple-string)))

;;; ── Derived paths ──────────────────────────────────────────────────

(defun modules-dir (config)
  "Return the modules directory path for CONFIG."
  (format nil "~A/modules" (config-data-dir config)))

;;; ── Environment loading ────────────────────────────────────────────

(defun env-or (name default)
  "Read environment variable NAME, return DEFAULT if unset or empty."
  (let ((val (uiop:getenv name)))
    (if (and val (plusp (length val)))
        val
        default)))

(defun env-int (name default)
  "Read environment variable NAME as an integer, return DEFAULT if unset."
  (let ((val (uiop:getenv name)))
    (if (and val (plusp (length val)))
        (handler-case (parse-integer val)
          (error () default))
        default)))

(defun env-bool (name)
  "Read environment variable NAME as a boolean (\"1\" or \"true\" = T)."
  (let ((val (uiop:getenv name)))
    (and val (or (string= val "1") (string-equal val "true")))))

(defun parse-backend (val)
  "Parse backend string to keyword. Returns :CHROOT or :FIRECRACKER."
  (let ((s (string-downcase (or val "chroot"))))
    (cond
      ((string= s "firecracker") :firecracker)
      (t :chroot))))

(defun config-from-env ()
  "Create a config struct from environment variables.
   Environment variables:
     SQUASH_DATA_DIR     — data directory (default /data)
     SQUASH_PORT         — HTTP port (default 8080)
     SQUASH_MAX_SANDBOXES — max concurrent sandboxes (default 100)
     SQUASH_UPPER_LIMIT_MB — tmpfs upper layer size in MB (default 512)
     SQUASH_BACKEND      — sandbox backend: chroot or firecracker (default chroot)
     SQUASH_AUTH_TOKEN   — bearer token for API auth (optional)
     SQUASH_S3_BUCKET    — S3 bucket for module sync (optional)
     SQUASH_S3_REGION    — S3 region (default us-east-2)
     SQUASH_S3_PREFIX    — S3 key prefix (default modules)
     SQUASH_S3_ACCESS_KEY — AWS access key
     SQUASH_S3_SECRET_KEY — AWS secret key
     SQUASH_S3_ENDPOINT  — Custom S3 endpoint (for R2/MinIO)
     SQUASH_PROXY_HTTPS  — Enable HTTPS proxy (1/true)
     SQUASH_SECRETS_DIR  — Directory containing secret files"
  (make-config
   :data-dir       (env-or "SQUASH_DATA_DIR" "/data")
   :port           (env-int "SQUASH_PORT" 8080)
   :max-sandboxes  (env-int "SQUASH_MAX_SANDBOXES" 100)
   :upper-limit-mb (env-int "SQUASH_UPPER_LIMIT_MB" 512)
   :backend        (parse-backend (env-or "SQUASH_BACKEND" "chroot"))
   :auth-token     (env-or "SQUASH_AUTH_TOKEN" nil)
   :s3-bucket      (env-or "SQUASH_S3_BUCKET" nil)
   :s3-region      (env-or "SQUASH_S3_REGION" "us-east-2")
   :s3-prefix      (env-or "SQUASH_S3_PREFIX" "modules")
   :s3-access-key  (env-or "SQUASH_S3_ACCESS_KEY" nil)
   :s3-secret-key  (env-or "SQUASH_S3_SECRET_KEY" nil)
   :s3-endpoint    (env-or "SQUASH_S3_ENDPOINT" nil)
   :proxy-https    (env-bool "SQUASH_PROXY_HTTPS")
   :secrets-dir    (env-or "SQUASH_SECRETS_DIR" nil)))

;;; ── Helpers used by main.lisp ──────────────────────────────────────

(defun secrets-exist-p (config)
  "Return T if a secrets directory is configured and contains files."
  (let ((dir (config-secrets-dir config)))
    (and dir
         (probe-file dir)
         (not (null (uiop:directory-files dir))))))

(defun make-s3-client-from-config (config)
  "Create an S3 client from daemon config. Requires s3.lisp loaded."
  (make-s3-client
   :bucket     (or (config-s3-bucket config) "")
   :region     (config-s3-region config)
   :prefix     (config-s3-prefix config)
   :access-key (or (config-s3-access-key config) "")
   :secret-key (or (config-s3-secret-key config) "")
   :endpoint   (or (config-s3-endpoint config) "")
   :data-dir   (config-data-dir config)))
