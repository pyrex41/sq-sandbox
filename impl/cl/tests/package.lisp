(defpackage #:squashd-tests
  (:use #:cl #:fiveam)
  (:import-from #:squashd-syscalls
                ;; Mount flags
                #:+ms-rdonly+ #:+ms-nosuid+ #:+ms-nodev+ #:+ms-noexec+
                #:+ms-remount+ #:+ms-bind+
                ;; Umount flags
                #:+mnt-detach+ #:+mnt-force+
                ;; Syscalls
                #:%mount #:%umount2 #:%fork #:%waitpid #:%pipe #:%close
                #:%read #:%sys-write #:%getpid #:%exit
                ;; Errno
                #:get-errno)
  (:export #:run-tests))

(in-package #:squashd-tests)

(def-suite squashd-tests
  :description "Test suite for squashd syscall wrappers")

(in-suite squashd-tests)
