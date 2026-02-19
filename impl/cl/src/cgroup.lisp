(in-package #:squashd)

;;; Cgroup v2 — REMOVED in unprivileged mode.

(defstruct cgroup-handle
  (path "" :type simple-string)
  (id   "" :type simple-string))

(defun create-cgroup (id cpu-cores memory-mb)
  (declare (ignore id cpu-cores memory-mb))
  nil)

(defun destroy-cgroup (handle)
  (declare (ignore handle))
  nil)

;; Keep write-string-to-file as it's used elsewhere
(defun write-string-to-file (path content)
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string content s)))
