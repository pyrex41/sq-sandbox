(defsystem "squashd-tests"
  :description "Test suite for squashd syscall wrappers and S3 operations"
  :version "4.0.0"
  :depends-on ("cffi"
               "fiveam"
               "uiop"
               "ironclad"
               "babel"
               "cl-ppcre"
               "alexandria"
               "bordeaux-threads")
  :pathname "tests"
  :serial t
  :components ((:file "syscalls-package")
               (:file "syscalls" :depends-on ("syscalls-package"))
               (:file "package" :depends-on ("syscalls"))
               (:file "syscalls-test" :depends-on ("package"))
               (:file "s3-test-package")
               (:file "s3-test" :depends-on ("s3-test-package")))
  :perform (test-op (o c)
             (symbol-call :fiveam :run!
                          (find-symbol* :squashd-tests :squashd-tests))
             (symbol-call :fiveam :run!
                          (find-symbol* :squashd-s3-tests :squashd-s3-tests))))
