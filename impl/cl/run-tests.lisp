#!/usr/bin/env sbcl --script
;;;
;;; Test runner for squashd
;;;
;;; This script loads the system and runs tests. It exits with code 0
;;; if all tests pass, 1 if any fail.

(require :asdf)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Ensure FiveAM is available
(handler-case
    (ql:quickload "fiveam" :silent t)
  (error (e)
    (format t "Error loading fiveam: ~A~%" e)
    (format t "Please install Quicklisp or fiveam manually.~%")
    (uiop:quit 1)))

;; Load squashd system
(format t "Loading squashd system...~%")
(handler-case
    (asdf:load-system "squashd" :verbose nil)
  (error (e)
    (format t "Error loading squashd: ~A~%" e)
    (uiop:quit 1)))

;; Load test system
(format t "Loading squashd-test system...~%")
(handler-case
    (asdf:load-system "squashd-test" :verbose nil)
  (error (e)
    (format t "Error loading squashd-test: ~A~%" e)
    (uiop:quit 1)))

;; Run tests
(format t "~%Running tests...~%~%")
(let ((results (fiveam:run 'squashd-test::exec-tests)))
  (format t "~%~%")
  (if (fiveam:results-status results)
      (progn
        (format t "All tests passed!~%")
        (uiop:quit 0))
      (progn
        (format t "Some tests failed.~%")
        (uiop:quit 1))))
