#!/usr/bin/env janet
# squashd — sq-sandbox daemon (Janet implementation)
#
# Entry point: load config, init/recover, start reaper fiber,
# start HTTP server. Signal handling for graceful shutdown.

(import config)
(import manager)
(import firecracker)
(import api)
(import init)
(import reaper)
(import proxy)
(import s3)

(defn main [&]
  (def cfg (config/config-from-env))
  (config/validate-config! cfg)
  (def mgr (manager/make-manager cfg))

  # Ensure data dirs exist
  (os/execute ["mkdir" "-p" (string (get cfg :data-dir) "/sandboxes")] :p)
  (os/execute ["mkdir" "-p" (string (get cfg :data-dir) "/modules")] :p)

  # S3 sync modules (if configured)
  (when (s3/s3-enabled? cfg)
    (eprintf "init: syncing modules from S3\n")
    (s3/sync-modules cfg))

  # Recovery: remount existing sandboxes from previous run
  (def [recovered failed] (init/init-recover cfg mgr))
  (eprintf "init: recovered %d sandboxes, %d failed\n" recovered failed)

  # Start secret proxy (if secrets.json exists)
  (proxy/start-proxy cfg)

  # Start reaper as background ev fiber
  (reaper/start-reaper mgr)

  # Signal handling: graceful shutdown on SIGTERM/SIGINT
  (defn shutdown [sig]
    (eprintf "\nshutdown: received %s, cleaning up...\n" (string sig))
    (each [id s] (pairs (get mgr :sandboxes))
      (when s
        (eprintf "shutdown: destroying sandbox %s\n" id)
        (try (manager/manager-destroy mgr id) ([e] nil))))
    (eprintf "shutdown: complete\n")
    (os/exit 0))

  (os/sigaction :term shutdown)
  (os/sigaction :int shutdown)

  (def port (get cfg :port))
  (eprintf "squash v4 (janet) — port: %d, data: %s\n" port (get cfg :data-dir))

  (def handler (api/build-handler cfg mgr))
  (net/server "0.0.0.0" (string port) handler)

  (eprintf "ready — listening on port %d\n" port)

  # Keep the main fiber alive (net/server runs in background fibers)
  (ev/sleep math/inf))

(main)
