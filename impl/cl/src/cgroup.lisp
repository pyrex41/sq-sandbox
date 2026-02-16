(in-package #:squashd)

;;; Cgroup v2 resource limits.
;;; Writes cpu.max and memory.max under /sys/fs/cgroup/squashd-<id>.
;;; All operations are file writes — no CFFI needed.

(declaim (optimize (speed 2) (safety 2)))

;;; ── Constants ──────────────────────────────────────────────────────────

(define-constant +cgroup-root+ "/sys/fs/cgroup"
  :test #'equal
  :documentation "cgroupv2 unified hierarchy root.")

(defconstant +cgroup-period-us+ 100000
  "Default CFS period in microseconds (100ms).")

;;; ── Data Structure ─────────────────────────────────────────────────────

(defstruct cgroup-handle
  (path "" :type simple-string)
  (id   "" :type simple-string))

;;; ── Helpers ────────────────────────────────────────────────────────────

(defun write-string-to-file (path content)
  "Write CONTENT string to PATH, truncating any existing file.
   Used for cgroup knobs and /proc tunables."
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content s)))

;;; ── Public API ─────────────────────────────────────────────────────────

(defun create-cgroup (id cpu-cores memory-mb)
  "Create a cgroupv2 directory and write cpu.max + memory.max.
   CPU-CORES is a float (e.g. 2.0 = 200% of one core).
   MEMORY-MB is an integer (e.g. 1024 = 1 GiB).
   Returns a cgroup-handle or signals an error."
  (declare (type simple-string id)
           (type single-float cpu-cores)
           (type fixnum memory-mb))
  (let* ((cgroup-path (format nil "~A/squashd-~A" +cgroup-root+ id))
         (cpu-max-path (format nil "~A/cpu.max" cgroup-path))
         (mem-max-path (format nil "~A/memory.max" cgroup-path)))

    ;; Create the cgroup directory (mkdir -p equivalent)
    (ensure-directories-exist (format nil "~A/" cgroup-path))

    ;; cpu.max: "<quota> <period>" where quota = cores * period
    (let ((quota (round (* cpu-cores +cgroup-period-us+))))
      (write-string-to-file cpu-max-path
                            (format nil "~D ~D" quota +cgroup-period-us+)))

    ;; memory.max: bytes
    (let ((memory-bytes (* memory-mb 1048576)))
      (write-string-to-file mem-max-path
                            (format nil "~D" memory-bytes)))

    (make-cgroup-handle :path cgroup-path :id id)))

(defun destroy-cgroup (handle)
  "Remove the cgroup directory. Ignores errors (processes may still be inside)."
  (when handle
    (ignore-errors
      ;; Move any remaining processes to parent cgroup before removal
      (let ((procs-path (format nil "~A/cgroup.procs" (cgroup-handle-path handle))))
        (ignore-errors
          (with-open-file (s procs-path :direction :input :if-does-not-exist nil)
            (when s
              (loop for line = (read-line s nil nil)
                    while line
                    do (ignore-errors
                         (write-string-to-file
                          (format nil "~A/cgroup.procs" +cgroup-root+)
                          line)))))))
      ;; rmdir the cgroup (kernel removes it when empty)
      (run-command "rmdir" (cgroup-handle-path handle)))))
