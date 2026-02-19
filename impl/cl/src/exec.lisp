(in-package #:squashd)

(declaim (optimize (speed 3) (safety 1)))

;;; ── Constants ──────────────────────────────────────────────────────

(defconstant +max-output+ 65536)

;;; ── Helpers ────────────────────────────────────────────────────────

(declaim (inline get-unix-time))
(defun get-unix-time ()
  "Current Unix timestamp as (unsigned-byte 64)."
  (declare (optimize (speed 3) (safety 0)))
  (the (unsigned-byte 64) (- (get-universal-time) 2208988800)))

(declaim (inline get-monotonic-ms))
(defun get-monotonic-ms ()
  "Monotonic clock in milliseconds, for timeout tracking."
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum (values (floor (get-internal-real-time)
                             (/ internal-time-units-per-second 1000)))))

;;; ── exec-result ────────────────────────────────────────────────────

(defstruct exec-result
  (exit-code 0 :type fixnum)
  (stdout    "" :type simple-string)
  (stderr    "" :type simple-string)
  (started   0  :type (unsigned-byte 64))
  (finished  0  :type (unsigned-byte 64))
  (duration-ms 0 :type fixnum)
  (seq       0  :type fixnum))

;;; ── Exec log ───────────────────────────────────────────────────────

(defun sandbox-root-path (sandbox)
  "Best-effort sandbox root path derived from merged mount path."
  (let* ((merged (sandbox-merged-path sandbox))
         (suffix "/merged")
         (mlen (length merged))
         (slen (length suffix)))
    (if (and (> mlen slen)
             (string= suffix merged :start2 (- mlen slen)))
        (subseq merged 0 (- mlen slen))
        merged)))

(defun write-exec-log (sandbox seq cmd workdir exit-code started finished
                       duration-ms stdout-str stderr-str &key sandbox-dir)
  "Write a JSON log entry to .meta/log/exec-<seq>.json for the sandbox.
   Uses SANDBOX-DIR when provided, otherwise derives from merged path."
  (declare (type fixnum seq exit-code duration-ms)
           (type (unsigned-byte 64) started finished)
           (type simple-string cmd workdir stdout-str stderr-str))
  (let* ((root (or sandbox-dir (sandbox-root-path sandbox)))
         (log-dir (concatenate 'string root "/.meta/log"))
         (log-path (format nil "~A/exec-~D.json" log-dir seq)))
    (ensure-directories-exist (concatenate 'string log-dir "/"))
    (with-open-file (s log-path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (write-string
       (jojo:to-json
        (list :|seq| seq
              :|cmd| cmd
              :|workdir| workdir
              :|exit_code| exit-code
              :|started| started
              :|finished| finished
              :|duration_ms| duration-ms
              :|stdout| stdout-str
              :|stderr| stderr-str))
       s))))

;;; ── Sandbox accessors (forward references) ─────────────────────────
;;; These are defined in sandbox.lisp. We declare their types here to
;;; suppress SBCL style-warnings and enable optimization.

(declaim (ftype (function (t) (or null t)) sandbox-mounts))
(declaim (ftype (function (t) fixnum) sandbox-exec-count))

;;; Helper to extract the merged path from a sandbox's mounts.

(defun sandbox-merged-path (sandbox)
  "Get the overlay merged path for a sandbox."
  (overlay-mount-merged-path
   (sandbox-mounts-overlay (sandbox-mounts sandbox))))

;;; ── Stream reading helper ──────────────────────────────────────────

(defun read-capped-from-stream (stream max-bytes)
  "Read up to MAX-BYTES characters from STREAM, return as string."
  (declare (type fixnum max-bytes))
  (let ((buf (make-string max-bytes))
        (pos 0))
    (declare (type fixnum pos))
    (handler-case
        (loop while (< pos max-bytes)
              for ch = (read-char stream nil nil)
              while ch
              do (setf (schar buf pos) ch)
                 (incf pos))
      (error () nil))
    (if (zerop pos)
        ""
        (subseq buf 0 pos))))

;;; ── Main entry point ───────────────────────────────────────────────

(defun exec-in-sandbox (sandbox cmd &key (workdir "/") (timeout 300)
                                        sandbox-dir)
  "Execute CMD in SANDBOX via sq-exec helper.
   Spawns sq-exec with the overlay merged dir, captures stdout/stderr.
   Returns an exec-result struct.
   Increments exec-count for sequence tracking."
  (declare (type simple-string cmd workdir)
           (type fixnum timeout))

  (let* ((started (get-unix-time))
         (start-ticks (get-internal-real-time))
         (merged (sandbox-merged-path sandbox))
         (proc (uiop:launch-program
                (list "sq-exec" merged cmd workdir
                      (princ-to-string timeout))
                :output :stream :error-output :stream))
         (stdout-stream (uiop:process-info-output proc))
         (stderr-stream (uiop:process-info-error-output proc)))

    (let* ((stdout-str (read-capped-from-stream stdout-stream +max-output+))
           (stderr-str (read-capped-from-stream stderr-stream +max-output+))
           (exit-code (uiop:wait-process proc))
           (finished (get-unix-time))
           (end-ticks (get-internal-real-time))
           (duration-ms (round (* (- end-ticks start-ticks) 1000)
                               internal-time-units-per-second))
           (seq (incf (sandbox-exec-count sandbox))))

      ;; Write log entry to .meta/log
      (ignore-errors
        (write-exec-log sandbox seq cmd workdir
                        exit-code started finished
                        duration-ms stdout-str stderr-str
                        :sandbox-dir sandbox-dir))

      (make-exec-result
       :exit-code exit-code
       :stdout stdout-str
       :stderr stderr-str
       :started started
       :finished finished
       :duration-ms duration-ms
       :seq seq))))
