;;; squashd-core.asd - Library-only system (no HTTP server)
;;;
;;; Loads the squashd container runtime as an in-process library,
;;; excluding the HTTP API layer (api.lisp, main.lisp) and its
;;; web server dependencies (clack, woo, lack, ningle, trivial-mimes).
;;;
;;; Usage: (ql:quickload "squashd-core")

(asdf:defsystem "squashd-core"
  :description "sq-sandbox container runtime (library, no HTTP server)"
  :version "4.0.0"
  :depends-on ("cffi"
               "jonathan"
               "ironclad"
               "dexador"
               "bordeaux-threads"
               "alexandria"
               "local-time"
               "cl-ppcre"
               "log4cl")
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "validate")
               (:file "conditions")
               (:file "syscalls")
               (:file "mounts")
               (:file "cgroup")
               (:file "netns")
               (:file "exec")
               (:file "sandbox")
               (:file "firecracker")
               (:file "manager")
               (:file "meta")
               (:file "modules")
               (:file "secrets")
               (:file "s3")
               (:file "reaper")
               (:file "init")))
