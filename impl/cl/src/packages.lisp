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
           #:make-auth-middleware

           ;; Config
           #:config
           #:make-config
           #:config-from-env
           #:config-data-dir
           #:config-max-sandboxes
           #:config-upper-limit-mb
           #:config-backend

           ;; Manager
           #:manager
           #:make-manager
           #:manager-create-sandbox
           #:manager-destroy-sandbox
           #:manager-exec
           #:manager-snapshot
           #:manager-restore
           #:manager-activate-module
           #:manager-sandbox-info
           #:manager-sandbox-count
           #:list-sandbox-infos
           #:manager-exec-logs

           ;; Exec result
           #:exec-result
           #:make-exec-result
           #:exec-result-exit-code
           #:exec-result-stdout
           #:exec-result-stderr
           #:exec-result-started
           #:exec-result-finished
           #:exec-result-duration-ms
           #:exec-result-seq

           ;; Sandbox struct
           #:sandbox
           #:sandbox-id
           #:sandbox-state
           #:sandbox-created
           #:sandbox-last-active
           #:sandbox-exec-count

           ;; Modules
           #:list-available-modules
           #:module-exists-p

           ;; Recovery
           #:init-recover

           ;; Reaper
           #:reaper-loop))
