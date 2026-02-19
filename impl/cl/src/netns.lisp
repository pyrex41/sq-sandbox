(in-package #:squashd)

;;; Network namespace — REMOVED in unprivileged mode.
;;; Sandboxes use bubblewrap for isolation (no veth, no iptables).
;;; The netns-handle struct is kept with minimal slots because the
;;; firecracker backend still creates handles with :name and :index.

(defstruct netns-handle
  (name  "" :type simple-string)
  (index 0  :type (unsigned-byte 8)))

(defun setup-netns (config id allow-net)
  (declare (ignore config id allow-net))
  nil)

(defun teardown-netns (handle)
  (declare (ignore handle))
  nil)
