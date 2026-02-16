#!/usr/bin/env sbcl --script
;;;
;;; S3 Test Runner for squashd
;;;
;;; This script loads the system and runs S3 tests. It exits with code 0
;;; if all tests pass, 1 if any fail.

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

;; Run tests
(format t "~%═══════════════════════════════════════════════════════════~%")
(format t "  Running S3 Tests~%")
(format t "═══════════════════════════════════════════════════════════~%~%")

(let ((results (fiveam:run 'squashd-s3-tests::squashd-s3-tests)))
  (format t "~%~%═══════════════════════════════════════════════════════════~%")
  (format t "  Test Summary~%")
  (format t "═══════════════════════════════════════════════════════════~%~%")

  (if (fiveam:results-status results)
      (progn
        (format t "✓ All S3 tests passed!~%~%")
        (uiop:quit 0))
      (progn
        (format t "✗ Some S3 tests failed!~%~%")
        (uiop:quit 1))))
