#!/usr/bin/env janet
# squashd — sq-sandbox daemon (Janet implementation)
# Usage: jpm exec janet main.janet  (from project root)
# Or: JANET_PATH="src:$(jpm path)" janet main.janet

(import config)
(import manager)
(import api)

(defn main []
  (def cfg (config/config-from-env))
  (def mgr (manager/make-manager cfg))

  # Ensure data dirs exist
  (os/shell (string "mkdir -p " (get cfg :data-dir) "/sandboxes " (get cfg :data-dir) "/modules"))

  (print "squash v4 (janet) — port:" (get cfg :port)
         "data:" (get cfg :data-dir))

  (def handler (api/build-handler cfg mgr))
  (def port (get cfg :port))
  (net/server "0.0.0.0" (string port) handler)
  (print "ready — listening on port" port))

(main)
