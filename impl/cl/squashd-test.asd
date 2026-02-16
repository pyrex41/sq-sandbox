(defsystem "squashd-test"
  :description "Test suite for squashd"
  :version "4.0.0"
  :depends-on ("squashd"
               "fiveam")  ; Unit testing framework
  :pathname "t"
  :serial t
  :components ((:file "packages")
               (:file "test-exec"))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :squashd-test)))
