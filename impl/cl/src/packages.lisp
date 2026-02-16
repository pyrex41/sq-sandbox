(defpackage #:squashd
  (:use #:cl #:alexandria)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:jojo #:jonathan)
                    (#:lt #:local-time))
  (:export #:main

           ;; Condition types
           #:mount-error
           #:mount-error-source
           #:mount-error-target
           #:mount-error-errno
           #:module-not-found
           #:module-not-found-name
           #:sandbox-error
           #:sandbox-error-id
           #:sandbox-error-message
           #:cgroup-setup-failed
           #:cgroup-error-id

           ;; Restart names (for invoke-restart)
           #:retry-create
           #:skip-sandbox
           #:pull-from-s3-and-retry
           #:skip-layer

           ;; Production handler
           #:*s3-client*
           #:production-handler
           #:with-production-handlers
           #:call-with-production-handlers
           #:mount-squashfs-layers

           ;; HTTP API
           #:*app*
           #:build-app
           #:json-response
           #:make-auth-middleware))
