(defpackage #:squashd-test
  (:use #:cl #:fiveam #:squashd)
  (:export #:run-tests))

(in-package #:squashd-test)

(def-suite :squashd-test
  :description "Test suite for squashd")

(defun run-tests ()
  "Run all tests and return T if all pass, NIL otherwise."
  (run! :squashd-test))
