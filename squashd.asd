(defsystem "squashd"
  :description "sq-sandbox container runtime daemon"
  :version "4.0.0"
  :depends-on ("cffi"              ; C FFI for syscalls
               "clack"             ; HTTP server abstraction (start/stop any backend)
               "woo"               ; HTTP server backend (libev-based, fastest CL server)
               "lack"              ; Application builder: middleware composition, env/response protocol
               "lack/middleware/accesslog"
               "ningle"            ; Lightweight Sinatra-style routing (builds on Lack)
               "jonathan"          ; Fast JSON (SAX-style, faster than cl-json)
               "ironclad"          ; Crypto: SHA256, HMAC for S3 SigV4
               "dexador"           ; HTTP client for S3
               "bordeaux-threads"  ; Threading
               "alexandria"        ; Utility library
               "local-time"        ; Time formatting (ISO 8601)
               "cl-ppcre"          ; Regex (for parsing, validation)
               "trivial-mimes"     ; MIME types (optional)
               "log4cl")           ; Logging
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
               (:file "init")
               (:file "api")
               (:file "main")))
