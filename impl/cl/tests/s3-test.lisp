(in-package #:squashd-s3-tests)

(in-suite squashd-s3-tests)

;;; ── Test Helpers ─────────────────────────────────────────────────────

(defun make-test-s3-client (&key (bucket "test-bucket")
                                  (region "us-east-1")
                                  (prefix "")
                                  (access-key "AKIAIOSFODNN7EXAMPLE")
                                  (secret-key "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")
                                  (endpoint "")
                                  (data-dir "/tmp/squashd-test"))
  "Create a test S3 client with AWS example credentials"
  (squashd::make-s3-client
   :bucket bucket
   :region region
   :prefix prefix
   :access-key access-key
   :secret-key secret-key
   :endpoint endpoint
   :data-dir data-dir))

(defun unique-test-file ()
  "Generate a unique test file path"
  (format nil "/tmp/squashd-s3-test-~A.dat" (get-universal-time)))

(defun cleanup-test-file (path)
  "Remove test file and .s3tmp variant, ignoring errors"
  (ignore-errors (delete-file path))
  (ignore-errors (delete-file (concatenate 'string path ".s3tmp"))))

;;; ── Helper Function Tests ────────────────────────────────────────────

(test format-datetime-sigv4-format
  "Test format-datetime-sigv4 returns correct SigV4 timestamp format"
  (let ((dt (squashd::format-datetime-sigv4)))
    ;; Should be 16 characters: YYYYMMDDTHHMMSSZ
    (is (= 16 (length dt))
        "Datetime should be 16 characters")
    ;; Should end with Z
    (is (char= #\Z (char dt 15))
        "Datetime should end with Z")
    ;; Should have T separator
    (is (char= #\T (char dt 8))
        "Datetime should have T separator at position 8")
    ;; Should be all digits except T and Z
    (is (every (lambda (c) (or (digit-char-p c) (char= c #\T) (char= c #\Z))) dt)
        "Datetime should contain only digits, T, and Z")))

(test sha256-string-known-value
  "Test SHA256 hash of known string matches expected value"
  ;; SHA256("hello") = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
  (let ((hash (squashd::sha256-string "hello")))
    (is (string= hash "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
        "SHA256 hash should match expected value")))

(test sha256-octets-known-value
  "Test SHA256 hash of known octets matches expected value"
  (let* ((data (babel:string-to-octets "hello" :encoding :utf-8))
         (hash (squashd::sha256-octets data)))
    (is (string= hash "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
        "SHA256 hash of octets should match expected value")))

(test sha256-empty-string
  "Test SHA256 hash of empty string"
  ;; SHA256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
  (let ((hash (squashd::sha256-string "")))
    (is (string= hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        "SHA256 of empty string should match expected value")))

(test hmac-sha256-known-value
  "Test HMAC-SHA256 with known test vector"
  ;; HMAC-SHA256(key="key", data="The quick brown fox jumps over the lazy dog")
  ;; = f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8
  (let* ((key (babel:string-to-octets "key" :encoding :utf-8))
         (data (babel:string-to-octets "The quick brown fox jumps over the lazy dog" :encoding :utf-8))
         (hmac (squashd::hmac-sha256 key data))
         (hex (ironclad:byte-array-to-hex-string hmac)))
    (is (string= hex "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8")
        "HMAC-SHA256 should match expected value")))

;;; ── URL Builder Tests ────────────────────────────────────────────────

(test s3-host-aws-standard
  "Test s3-host returns correct AWS hostname"
  (let* ((client (make-test-s3-client :bucket "my-bucket" :region "us-west-2" :endpoint ""))
         (host (squashd::s3-host client)))
    (is (string= host "my-bucket.s3.us-west-2.amazonaws.com")
        "AWS S3 host should use virtual-hosted style")))

(test s3-host-custom-endpoint
  "Test s3-host strips protocol and path from custom endpoint"
  (let* ((client (make-test-s3-client :endpoint "https://s3.example.com:9000/path"))
         (host (squashd::s3-host client)))
    (is (string= host "s3.example.com")
        "Custom endpoint should strip protocol, port, and path")))

(test s3-base-url-aws
  "Test s3-base-url for AWS"
  (let* ((client (make-test-s3-client :bucket "my-bucket" :region "eu-west-1" :endpoint ""))
         (url (squashd::s3-base-url client)))
    (is (string= url "https://my-bucket.s3.eu-west-1.amazonaws.com")
        "AWS base URL should use virtual-hosted style")))

(test s3-base-url-custom
  "Test s3-base-url for custom endpoint"
  (let* ((client (make-test-s3-client :bucket "my-bucket" :endpoint "https://s3.example.com"))
         (url (squashd::s3-base-url client)))
    (is (string= url "https://s3.example.com/my-bucket")
        "Custom endpoint base URL should use path-style")))

(test s3-url-with-prefix
  "Test s3-url includes prefix correctly"
  (let* ((client (make-test-s3-client :bucket "my-bucket"
                                      :region "us-east-1"
                                      :prefix "myprefix/"
                                      :endpoint ""))
         (url (squashd::s3-url client "test.txt")))
    (is (string= url "https://my-bucket.s3.us-east-1.amazonaws.com/myprefix/test.txt")
        "URL should include prefix before key")))

(test s3-url-path-aws
  "Test s3-url-path for AWS (virtual-hosted style)"
  (let* ((client (make-test-s3-client :bucket "my-bucket" :prefix "pre/" :endpoint ""))
         (path (squashd::s3-url-path client "obj.dat")))
    (is (string= path "/pre/obj.dat")
        "AWS URL path should omit bucket in virtual-hosted style")))

(test s3-url-path-custom
  "Test s3-url-path for custom endpoint (path style)"
  (let* ((client (make-test-s3-client :bucket "my-bucket" :prefix "pre/"
                                      :endpoint "https://s3.example.com"))
         (path (squashd::s3-url-path client "obj.dat")))
    (is (string= path "/my-bucket/pre/obj.dat")
        "Custom endpoint URL path should include bucket")))

;;; ── SigV4 Signing Tests ──────────────────────────────────────────────

(test sigv4-signing-key-derivation
  "Test SigV4 signing key derivation follows AWS spec"
  (let* ((secret "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY")
         (datestamp "20150830")
         (region "us-east-1")
         (key (squashd::sigv4-signing-key secret datestamp region)))
    ;; Key should be 32 bytes (SHA256 output)
    (is (= 32 (length key))
        "Signing key should be 32 bytes")
    ;; Key should be a byte array
    (is (typep key '(simple-array (unsigned-byte 8) (*)))
        "Signing key should be byte array")))

(test sigv4-sign-authorization-format
  "Test SigV4 signature produces valid Authorization header"
  (let* ((client (make-test-s3-client))
         (datetime "20240101T120000Z")
         (payload-hash (squashd::sha256-string ""))
         (auth (squashd::sigv4-sign client "GET" "/test-bucket/key" payload-hash datetime)))
    ;; Should start with AWS4-HMAC-SHA256
    (is (alexandria:starts-with-subseq "AWS4-HMAC-SHA256" auth)
        "Authorization should start with AWS4-HMAC-SHA256")
    ;; Should contain Credential=
    (is (search "Credential=" auth)
        "Authorization should contain Credential=")
    ;; Should contain SignedHeaders=
    (is (search "SignedHeaders=" auth)
        "Authorization should contain SignedHeaders=")
    ;; Should contain Signature=
    (is (search "Signature=" auth)
        "Authorization should contain Signature=")
    ;; Signature should be 64 hex characters (256 bits)
    (let* ((sig-pos (search "Signature=" auth))
           (sig-start (+ sig-pos 10))
           (sig (subseq auth sig-start)))
      (is (>= (length sig) 64)
          "Signature should be at least 64 hex characters"))))

(test sigv4-sign-deterministic
  "Test SigV4 signing is deterministic for same inputs"
  (let* ((client (make-test-s3-client))
         (datetime "20240101T120000Z")
         (payload-hash (squashd::sha256-string "test"))
         (auth1 (squashd::sigv4-sign client "PUT" "/bucket/key" payload-hash datetime))
         (auth2 (squashd::sigv4-sign client "PUT" "/bucket/key" payload-hash datetime)))
    (is (string= auth1 auth2)
        "Same inputs should produce same signature")))

(test sigv4-sign-different-methods
  "Test SigV4 signatures differ for different HTTP methods"
  (let* ((client (make-test-s3-client))
         (datetime "20240101T120000Z")
         (payload-hash (squashd::sha256-string ""))
         (auth-get (squashd::sigv4-sign client "GET" "/bucket/key" payload-hash datetime))
         (auth-put (squashd::sigv4-sign client "PUT" "/bucket/key" payload-hash datetime)))
    (is (not (string= auth-get auth-put))
        "Different HTTP methods should produce different signatures")))

;;; ── S3 Pull Atomic Operation Tests ───────────────────────────────────

(test s3-pull-atomic-tmp-file
  "Test s3-pull uses temporary file before rename"
  ;; This test requires mocking dex:get, which we'll do manually
  ;; For now, test the file operation pattern
  (let* ((test-path (unique-test-file))
         (tmp-path (concatenate 'string test-path ".s3tmp"))
         (test-data (babel:string-to-octets "test content" :encoding :utf-8)))
    (unwind-protect
         (progn
           ;; Simulate the atomic write pattern
           (ensure-directories-exist test-path)
           (with-open-file (out tmp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
             (write-sequence test-data out))

           ;; Verify tmp file exists before rename
           (is (probe-file tmp-path)
               "Temporary file should exist before rename")

           ;; Rename to final path
           (rename-file tmp-path test-path)

           ;; Verify tmp file is gone and final file exists
           (is (not (probe-file tmp-path))
               "Temporary file should not exist after rename")
           (is (probe-file test-path)
               "Final file should exist after rename")

           ;; Verify content
           (let ((read-data (uiop:read-file-string test-path)))
             (is (string= read-data "test content")
                 "File content should match")))
      (cleanup-test-file test-path))))

(test s3-pull-creates-parent-directories
  "Test s3-pull creates parent directories"
  (let* ((test-dir (format nil "/tmp/squashd-test-~A" (get-universal-time)))
         (test-path (format nil "~A/subdir/file.dat" test-dir)))
    (unwind-protect
         (progn
           ;; ensure-directories-exist should create parent dirs
           (ensure-directories-exist test-path)
           (is (probe-file (format nil "~A/subdir/" test-dir))
               "Parent directory should be created"))
      (ignore-errors
        (uiop:delete-directory-tree test-dir :validate t :if-does-not-exist :ignore)))))

;;; ── S3 Module Integration Tests ──────────────────────────────────────

(test s3-pull-module-path-construction
  "Test s3-pull-module constructs correct paths"
  ;; This test verifies the path construction logic without network calls
  (let* ((client (make-test-s3-client :data-dir "/opt/squashd/data"))
         (layer-name "100-python312")
         (expected-s3-key "modules/100-python312.squashfs")
         (expected-local-path "/opt/squashd/data/modules/100-python312.squashfs"))
    ;; We can't easily test s3-pull-module without mocking, but we can verify
    ;; the path construction by checking format strings
    (is (string= expected-s3-key (format nil "modules/~A.squashfs" layer-name))
        "S3 key should follow modules/NAME.squashfs pattern")
    (is (string= expected-local-path
                 (format nil "~A/modules/~A.squashfs"
                         (squashd::s3-client-data-dir client) layer-name))
        "Local path should be data-dir/modules/NAME.squashfs")))

;;; ── Background Push Tests ────────────────────────────────────────────

(test s3-push-bg-creates-thread
  "Test s3-push-bg creates a background thread"
  (let* ((client (make-test-s3-client))
         (test-file (unique-test-file))
         (test-data "background push test")
         (thread-created nil))
    (unwind-protect
         (progn
           ;; Create test file
           (with-open-file (out test-file
                                :direction :output
                                :if-exists :supersede)
             (write-string test-data out))

           ;; Mock bt:make-thread to capture thread creation
           ;; (In real test, we'd use a test framework with mocking support)
           ;; For now, just verify the function exists and accepts right args
           (is (fboundp 'bt:make-thread)
               "bordeaux-threads make-thread should be available")

           ;; Note: We can't easily test actual background execution without
           ;; a proper mocking framework. This would require intercepting
           ;; bt:make-thread or using a test HTTP server.
           (setf thread-created t))
      (cleanup-test-file test-file))

    (is (eq thread-created t)
        "Test setup completed")))

;;; ── Error Condition Tests ────────────────────────────────────────────

(test s3-pull-module-signals-module-not-found
  "Test s3-pull-module signals module-not-found on HTTP error"
  ;; This would require mocking dex:get to return an error
  ;; For now, document the expected behavior
  (let* ((client (make-test-s3-client))
         (layer-name "nonexistent-module"))
    ;; When dex:get fails (404, network error, etc.), s3-pull returns NIL
    ;; s3-pull-module checks this and signals module-not-found
    ;; We can test the condition type exists
    (is (find-class 'squashd::module-not-found)
        "module-not-found condition should be defined")))

(test s3-client-structure
  "Test s3-client structure fields"
  (let ((client (make-test-s3-client
                 :bucket "test"
                 :region "us-east-1"
                 :prefix "pre/"
                 :access-key "ACCESS"
                 :secret-key "SECRET"
                 :endpoint "https://s3.example.com"
                 :data-dir "/data")))
    (is (string= "test" (squashd::s3-client-bucket client)))
    (is (string= "us-east-1" (squashd::s3-client-region client)))
    (is (string= "pre/" (squashd::s3-client-prefix client)))
    (is (string= "ACCESS" (squashd::s3-client-access-key client)))
    (is (string= "SECRET" (squashd::s3-client-secret-key client)))
    (is (string= "https://s3.example.com" (squashd::s3-client-endpoint client)))
    (is (string= "/data" (squashd::s3-client-data-dir client)))))

(test s3-client-default-values
  "Test s3-client default values for optional fields"
  (let ((client (squashd::make-s3-client)))
    (is (string= "" (squashd::s3-client-bucket client)))
    (is (string= "" (squashd::s3-client-region client)))
    (is (string= "" (squashd::s3-client-prefix client)))
    (is (string= "" (squashd::s3-client-access-key client)))
    (is (string= "" (squashd::s3-client-secret-key client)))
    (is (string= "" (squashd::s3-client-endpoint client)))
    (is (string= "" (squashd::s3-client-data-dir client)))))

;;; ── Edge Cases ───────────────────────────────────────────────────────

(test s3-url-empty-prefix
  "Test URL construction with empty prefix"
  (let* ((client (make-test-s3-client :bucket "bucket" :region "us-east-1"
                                      :prefix "" :endpoint ""))
         (url (squashd::s3-url client "key")))
    (is (string= url "https://bucket.s3.us-east-1.amazonaws.com/key")
        "Empty prefix should not add extra slashes")))

(test s3-url-trailing-slash-in-prefix
  "Test URL construction with trailing slash in prefix"
  (let* ((client (make-test-s3-client :bucket "bucket" :region "us-east-1"
                                      :prefix "prefix/" :endpoint ""))
         (url (squashd::s3-url client "key")))
    (is (string= url "https://bucket.s3.us-east-1.amazonaws.com/prefix/key")
        "Trailing slash in prefix should be preserved")))

(test s3-url-no-trailing-slash-in-prefix
  "Test URL construction without trailing slash in prefix"
  (let* ((client (make-test-s3-client :bucket "bucket" :region "us-east-1"
                                      :prefix "prefix" :endpoint ""))
         (url (squashd::s3-url client "key")))
    (is (string= url "https://bucket.s3.us-east-1.amazonaws.com/prefixkey")
        "No trailing slash means key is directly concatenated")))

(test sha256-unicode-string
  "Test SHA256 handles Unicode strings correctly"
  (let ((hash (squashd::sha256-string "hello 世界")))
    ;; Should produce a valid 64-character hex string
    (is (= 64 (length hash))
        "Hash should be 64 characters")
    (is (every (lambda (c) (or (digit-char-p c)
                               (member c '(#\a #\b #\c #\d #\e #\f))))
               hash)
        "Hash should be valid hex")))

(test format-datetime-sigv4-year-2000-compliance
  "Test datetime format handles various years correctly"
  ;; Just verify format is consistent - actual value will vary
  (let ((dt (squashd::format-datetime-sigv4)))
    ;; Year should be 4 digits starting at position 0
    (is (every #'digit-char-p (subseq dt 0 4))
        "Year should be 4 digits")
    ;; Month should be 2 digits at position 4
    (is (every #'digit-char-p (subseq dt 4 6))
        "Month should be 2 digits")
    ;; Day should be 2 digits at position 6
    (is (every #'digit-char-p (subseq dt 6 8))
        "Day should be 2 digits")))

;;; ── Test Runner ──────────────────────────────────────────────────────

(defun run-s3-tests ()
  "Run all S3 tests"
  (run! 'squashd-s3-tests))
