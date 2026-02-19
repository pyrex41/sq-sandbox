#!/usr/bin/env sbcl --script
;;;
;;; Run exec tests specifically
;;;

(require :asdf)
(load "~/quicklisp/setup.lisp")

;; Add current directory to ASDF search path
(push (uiop:getcwd) asdf:*central-registry*)

(format t "Loading dependencies...~%")
(ql:quickload "fiveam" :silent t)

(format t "Loading squashd...~%")
(handler-case
    (asdf:load-system "squashd" :verbose nil)
  (error (e)
    (format t "Warning: ~A~%" e)
    (format t "Continuing anyway...~%")))

(format t "Loading tests...~%")
(asdf:load-system "squashd-test" :verbose nil)

(format t "~%Running exec tests...~%~%")
(let ((results (fiveam:run 'squashd-test::exec-tests)))
  (fresh-line)
  (format t "~%================================================~%")
  (if (fiveam:results-status results)
      (format t "✓ All tests passed!~%")
      (format t "✗ Some tests failed~%"))
  (format t "================================================~%")
  (uiop:quit (if (fiveam:results-status results) 0 1)))
