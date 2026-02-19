(in-package #:squashd)

;;; ── Sandbox Metadata ─────────────────────────────────────────────────
;;;
;;; Writes sandbox metadata to .meta/ directory as a JSON file.
;;; This metadata is used for recovery (init.lisp) and info display.
;;;
;;; COLD PATH — written once at creation time.

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defun write-sandbox-meta (config id &key owner layers task cpu
                                          memory-mb max-lifetime-s allow-net)
  "Write sandbox metadata to <data-dir>/sandboxes/<id>/.meta/sandbox.json."
  (let* ((sandbox-dir (format nil "~A/sandboxes/~A"
                              (config-data-dir config) id))
         (meta-path (format nil "~A/.meta/sandbox.json" sandbox-dir)))
    (ensure-directories-exist meta-path)
    (let ((meta (list :|id| id
                      :|owner| (or owner "anon")
                      :|layers| (or layers '("000-base-alpine"))
                      :|task| (or task "")
                      :|cpu| (or cpu 2.0)
                      :|memory_mb| (or memory-mb 1024)
                      :|max_lifetime_s| (or max-lifetime-s 0)
                      :|allow_net| allow-net
                      :|created| (get-unix-time))))
      (with-open-file (s meta-path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-string (jojo:to-json meta) s)))))
