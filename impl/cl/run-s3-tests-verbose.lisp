#!/usr/bin/env sbcl --script
;;;
;;; S3 Test Runner (Verbose) for squashd
;;;

(require :asdf)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Ensure dependencies are available
(handler-case
    (progn
      (ql:quickload '("fiveam" "ironclad" "babel" "cl-ppcre"
                      "alexandria" "bordeaux-threads") :silent t))
  (error (e)
    (format t "Error loading dependencies: ~A~%" e)
    (format t "Please install Quicklisp or dependencies manually.~%")
    (uiop:quit 1)))

;; Add current directory to ASDF source registry
(push (uiop:getcwd) asdf:*central-registry*)

;; Load squashd system
(format t "Loading squashd system...~%")
(handler-case
    (asdf:load-system "squashd" :verbose nil)
  (error (e)
    (format t "Error loading squashd: ~A~%" e)
    (uiop:quit 1)))

;; Load test system
(format t "Loading squashd-tests system...~%")
(handler-case
    (asdf:load-system "squashd-tests" :verbose nil)
  (error (e)
    (format t "Error loading squashd-tests: ~A~%" e)
    (uiop:quit 1)))

;; Run tests with details
(format t "~%═══════════════════════════════════════════════════════════~%")
(format t "  Running S3 Tests (Verbose)~%")
(format t "═══════════════════════════════════════════════════════════~%~%")

;; run! prints detailed output and returns T/NIL
(let ((success (fiveam:run! 'squashd-s3-tests::squashd-s3-tests)))
  (format t "~%~%")
  (if success
      (progn
        (format t "✓ All S3 tests passed!~%~%")
        (uiop:quit 0))
      (progn
        (format t "✗ Some S3 tests failed!~%~%")
        (uiop:quit 1))))
