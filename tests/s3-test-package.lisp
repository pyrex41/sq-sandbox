(defpackage #:squashd-s3-tests
  (:use #:cl #:fiveam)
  (:export #:run-s3-tests))

(in-package #:squashd-s3-tests)

(def-suite squashd-s3-tests
  :description "Test suite for S3 push/pull and module sync")

(in-suite squashd-s3-tests)
