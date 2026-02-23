(in-package #:squashd)

(declaim (optimize (speed 3) (safety 1)))

;;; ── Shell-out helpers ────────────────────────────────────────────────

(defun run-command (program &rest args)
  "Run a command, return (values exit-code stdout-string stderr-string).
   For iptables/ip commands where we don't care about output."
  (declare (optimize (speed 2) (safety 2)))
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (cons program args)
                         :output :string :error-output :string
                         :ignore-error-status t)
    (values exit-code stdout stderr)))

(defun run-command-ok (program &rest args)
  "Run a command, return T if exit code 0."
  (zerop (apply #'run-command program args)))
