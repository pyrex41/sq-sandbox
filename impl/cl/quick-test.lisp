#!/usr/bin/env sbcl --script
;;;
;;; Quick test to verify basic test infrastructure
;;;

(require :asdf)
(load "~/quicklisp/setup.lisp")

(format t "Loading dependencies...~%")
(ql:quickload '("cffi" "jonathan" "alexandria" "fiveam") :silent t)

(format t "Loading squashd system...~%")
(asdf:load-system "squashd" :verbose nil)

(format t "~%Testing basic structures...~%")

;; Test exec-result struct
(let ((result (squashd::make-exec-result
               :exit-code 42
               :stdout "test out"
               :stderr "test err"
               :started 100
               :finished 200
               :seq 1)))
  (format t "  exec-result: ~A~%"
          (if (and (= 42 (squashd::exec-result-exit-code result))
                   (string= "test out" (squashd::exec-result-stdout result))
                   (string= "test err" (squashd::exec-result-stderr result)))
              "PASS"
              "FAIL")))

;; Test get-unix-time
(let ((t1 (squashd::get-unix-time)))
  (format t "  get-unix-time: ~A (timestamp: ~D)~%"
          (if (> t1 1577836800) "PASS" "FAIL")
          t1))

;; Test get-monotonic-ms
(let ((t1 (squashd::get-monotonic-ms))
      (t2 (squashd::get-monotonic-ms)))
  (format t "  get-monotonic-ms: ~A~%"
          (if (>= t2 t1) "PASS" "FAIL")))

;; Test pipe creation
(multiple-value-bind (r w)
    (squashd::make-pipe)
  (squashd::close-fd-safe r)
  (squashd::close-fd-safe w)
  (format t "  make-pipe: PASS~%"))

(format t "~%Basic tests completed!~%")
