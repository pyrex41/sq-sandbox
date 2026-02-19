(in-package #:squashd)

;;; ── Validation Helpers ──────────────────────────────────────────────
;;;
;;; Input validation for sandbox IDs and other user-supplied values.
;;; Sandbox IDs must be alphanumeric with hyphens, 1-64 characters.
;;; This prevents path traversal and keeps filesystem operations safe.

(declaim (optimize (speed 2) (safety 2)))

(defun valid-id-p (id)
  "Return T if ID is a valid sandbox identifier.
   Valid IDs: 1-64 characters, alphanumeric plus hyphens and underscores.
   Must not start or end with a hyphen."
  (and (stringp id)
       (plusp (length id))
       (<= (length id) 64)
       (not (char= (char id 0) #\-))
       (not (char= (char id (1- (length id))) #\-))
       (every (lambda (c)
                (or (alphanumericp c) (char= c #\-) (char= c #\_)))
              id)))

(defun valid-label-p (label)
  "Return T if LABEL is a valid snapshot/module label.
   Valid labels: 1-128 characters, alphanumeric plus hyphens, underscores, and dots."
  (and (stringp label)
       (plusp (length label))
       (<= (length label) 128)
       (every (lambda (c)
                (or (alphanumericp c)
                    (char= c #\-)
                    (char= c #\_)
                    (char= c #\.)))
              label)))

(defun valid-module-name-p (name)
  "Return T if NAME is a valid module name.
   Same rules as valid-label-p."
  (valid-label-p name))
