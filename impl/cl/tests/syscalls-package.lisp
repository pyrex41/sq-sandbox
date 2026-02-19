(defpackage #:squashd-syscalls
  (:use #:cl)
  (:export
   ;; Mount flags
   #:+ms-rdonly+ #:+ms-nosuid+ #:+ms-nodev+ #:+ms-noexec+
   #:+ms-remount+ #:+ms-bind+
   ;; Umount flags
   #:+mnt-detach+ #:+mnt-force+
   ;; Syscalls
   #:%mount #:%umount2 #:%fork #:%waitpid #:%pipe #:%close
   #:%read #:%sys-write #:%getpid #:%exit
   ;; Errno
   #:get-errno))

(in-package #:squashd-syscalls)
