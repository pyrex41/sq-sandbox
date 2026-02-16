(in-package #:squashd)

;;; ── S3 Client ─────────────────────────────────────────────────────
;;;
;;; AWS S3 client with SigV4 signing. Supports S3-compatible services
;;; (Cloudflare R2, MinIO, Backblaze B2) via custom endpoint.
;;; Uses Ironclad for SHA256/HMAC and Dexador for HTTP.

(defstruct s3-client
  (bucket     "" :type simple-string)
  (region     "" :type simple-string)
  (prefix     "" :type simple-string)
  (access-key "" :type simple-string)
  (secret-key "" :type simple-string)
  (endpoint   "" :type simple-string)   ; empty = AWS, otherwise custom (R2/MinIO)
  (data-dir   "" :type simple-string))  ; for module path resolution

;;; ── Helpers ───────────────────────────────────────────────────────

(defun format-datetime-sigv4 ()
  "Return UTC datetime string in SigV4 format: YYYYMMDDTHHMMSSZ."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
            year month day hour min sec)))

(defun sha256-octets (octets)
  "SHA256 hash of octet vector, returned as lowercase hex string."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 octets)))

(defun sha256-string (string)
  "SHA256 hash of a UTF-8 string, returned as lowercase hex string."
  (sha256-octets (babel:string-to-octets string :encoding :utf-8)))

(defun hmac-sha256 (key data)
  "HMAC-SHA256 of DATA (octets) with KEY (octets). Returns raw octets."
  (declare (type (simple-array (unsigned-byte 8) (*)) key data))
  (let ((hmac (ironclad:make-hmac key :sha256)))
    (ironclad:update-hmac hmac data)
    (ironclad:hmac-digest hmac)))

(defun s3-host (client)
  "Return the S3 hostname for this client."
  (if (string= "" (s3-client-endpoint client))
      (format nil "~A.s3.~A.amazonaws.com"
              (s3-client-bucket client) (s3-client-region client))
      ;; Strip protocol and trailing path from custom endpoint
      (let ((ep (s3-client-endpoint client)))
        (setf ep (cl-ppcre:regex-replace "^https?://" ep ""))
        (setf ep (cl-ppcre:regex-replace "/.*" ep ""))
        (cl-ppcre:regex-replace ":.*" ep ""))))

(defun s3-base-url (client)
  "Return the base URL for S3 requests."
  (if (string= "" (s3-client-endpoint client))
      (format nil "https://~A.s3.~A.amazonaws.com"
              (s3-client-bucket client) (s3-client-region client))
      (format nil "~A/~A"
              (string-right-trim "/" (s3-client-endpoint client))
              (s3-client-bucket client))))

(defun s3-url (client key)
  "Return the full URL for an S3 object."
  (format nil "~A/~A~A" (s3-base-url client) (s3-client-prefix client) key))

(defun s3-url-path (client key)
  "Return the URL path component for signing.
   Virtual-hosted style for AWS, path-style for custom endpoints."
  (if (string= "" (s3-client-endpoint client))
      (format nil "/~A~A" (s3-client-prefix client) key)
      (format nil "/~A/~A~A"
              (s3-client-bucket client) (s3-client-prefix client) key)))

;;; ── SigV4 Signing ────────────────────────────────────────────────

(defun sigv4-signing-key (secret-key datestamp region)
  "Derive the SigV4 signing key via chained HMAC.
   AWS4<secret> → date → region → s3 → aws4_request"
  (let ((k-secret (babel:string-to-octets
                   (concatenate 'string "AWS4" secret-key)
                   :encoding :utf-8)))
    (reduce (lambda (key data)
              (hmac-sha256 key (babel:string-to-octets data :encoding :utf-8)))
            (list datestamp region "s3" "aws4_request")
            :initial-value k-secret)))

(defun sigv4-sign (client method url-path payload-hash datetime)
  "Sign a request with AWS SigV4. Returns the Authorization header value.
   METHOD: \"GET\" or \"PUT\"
   URL-PATH: the path component for signing (from s3-url-path)
   PAYLOAD-HASH: hex SHA256 of the request body
   DATETIME: SigV4 timestamp (YYYYMMDDTHHMMSSZ)"
  (let* ((datestamp (subseq datetime 0 8))
         (host (s3-host client))
         (signed-headers "host;x-amz-content-sha256;x-amz-date")
         (canonical-headers (format nil "host:~A~%x-amz-content-sha256:~A~%x-amz-date:~A~%"
                                    host payload-hash datetime))
         ;; Canonical request
         (canonical-request (format nil "~A~%~A~%~%~A~%~A~%~A"
                                    method url-path canonical-headers
                                    signed-headers payload-hash))
         (credential-scope (format nil "~A/~A/s3/aws4_request"
                                   datestamp (s3-client-region client)))
         ;; String to sign
         (string-to-sign (format nil "AWS4-HMAC-SHA256~%~A~%~A~%~A"
                                 datetime credential-scope
                                 (sha256-string canonical-request)))
         ;; Signing key and signature
         (signing-key (sigv4-signing-key (s3-client-secret-key client)
                                         datestamp
                                         (s3-client-region client)))
         (signature (ironclad:byte-array-to-hex-string
                     (hmac-sha256 signing-key
                                  (babel:string-to-octets string-to-sign
                                                          :encoding :utf-8)))))
    (format nil "AWS4-HMAC-SHA256 Credential=~A/~A, SignedHeaders=~A, Signature=~A"
            (s3-client-access-key client) credential-scope
            signed-headers signature)))

;;; ── S3 Operations ────────────────────────────────────────────────

(defun s3-push (client local-path key)
  "Upload LOCAL-PATH to S3 at KEY."
  (let* ((data (read-file-into-byte-vector local-path))
         (payload-hash (sha256-octets data))
         (datetime (format-datetime-sigv4))
         (url-path (s3-url-path client key))
         (auth (sigv4-sign client "PUT" url-path payload-hash datetime))
         (url (s3-url client key)))
    (dex:put url
             :content data
             :headers `(("Authorization" . ,auth)
                        ("x-amz-content-sha256" . ,payload-hash)
                        ("x-amz-date" . ,datetime)))))

(defun s3-pull (client key local-path)
  "Pull KEY from S3 to LOCAL-PATH. Atomic: writes .s3tmp then renames.
   Returns T on success, NIL on failure.
   Dexador signals on HTTP errors, so we catch to return NIL."
  (let* ((empty-hash (sha256-string ""))
         (datetime (format-datetime-sigv4))
         (url-path (s3-url-path client key))
         (auth (sigv4-sign client "GET" url-path empty-hash datetime))
         (url (s3-url client key))
         (tmp-path (concatenate 'string local-path ".s3tmp")))
    (ensure-directories-exist local-path)
    (handler-case
        (let ((body (dex:get url
                             :headers `(("Authorization" . ,auth)
                                        ("x-amz-content-sha256" . ,empty-hash)
                                        ("x-amz-date" . ,datetime))
                             :want-stream nil
                             :force-binary t)))
          (with-open-file (out tmp-path
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (write-sequence body out))
          (rename-file tmp-path local-path)
          t)
      (dex:http-request-failed ()
        nil))))

(defun s3-push-bg (client local-path key)
  "Push LOCAL-PATH to S3 in a background thread."
  (bt:make-thread
   (lambda ()
     (handler-case (s3-push client local-path key)
       (error (e) (log:warn "S3 background push failed for ~A: ~A" key e))))
   :name (format nil "s3-push-~A" key)))

(defun s3-pull-module (client layer-name)
  "Pull a module from S3 to the local modules directory.
   LAYER-NAME is e.g. \"100-python312\".
   Downloads to <data-dir>/modules/<layer-name>.squashfs."
  (let* ((key (format nil "modules/~A.squashfs" layer-name))
         (local-path (format nil "~A/modules/~A.squashfs"
                             (s3-client-data-dir client) layer-name)))
    (ensure-directories-exist local-path)
    (log:info "Pulling module ~A from S3" layer-name)
    (unless (s3-pull client key local-path)
      (error 'module-not-found :name layer-name))
    local-path))
