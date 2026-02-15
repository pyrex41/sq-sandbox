# Proxy — spawn secret proxy (shell or Go binary).
#
# When secrets.json exists:
#   - SQUASH_PROXY_HTTPS=1: spawn sq-secret-proxy-https (Go MITM proxy)
#   - else: spawn sq-secret-proxy (shell HTTP-only proxy)
#
# Both use SQUASH_DATA env; Go binary reads secrets.json and proxy-ca/ from $SQUASH_DATA.

(import config :prefix "config/")
(import secrets :prefix "secrets/")
(import mounts :prefix "mounts/")

(defn ensure-proxy-ca [config]
  "Generate proxy CA if SQUASH_PROXY_HTTPS=1 and ca.crt missing."
  (when (not (get config :proxy-https))
    (return nil))
  (def ca-dir (string (get config :data-dir) "/proxy-ca"))
  (def ca-crt (string ca-dir "/ca.crt"))
  (when (os/stat ca-crt)
    (return nil))
  (mounts/ensure-dir ca-dir)
  (def rc (os/execute (array "openssl" "req" "-new" "-newkey" "ec"
                             "-pkeyopt" "ec_paramgen_curve:prime256v1"
                             "-days" "3650" "-nodes" "-x509"
                             "-subj" "/CN=sq-secret-proxy CA"
                             "-keyout" (string ca-dir "/ca.key")
                             "-out" ca-crt)
               :p true))
  (when (= rc 0)
    (eprintf "proxy: generated CA at %s\n" ca-crt)))

(defn start-proxy [config]
  "Start secret proxy if secrets.json exists. Non-blocking (spawns in background)."
  (when (not (secrets/secrets-exist? config))
    (return nil))
  (ensure-proxy-ca config)
  (def data-dir (get config :data-dir))
  (def env @{"SQUASH_DATA" data-dir})
  (var bin (or (os/getenv "SQ_PROXY_BIN") "sq-secret-proxy-https"))
  (when (not (get config :proxy-https))
    (set bin (or (os/getenv "SQ_PROXY_BIN") "sq-secret-proxy")))
  (when (not (os/which bin))
    (eprintf "proxy: %s not found, skipping\n" bin)
    (return nil))
  (ev/go
    (fn []
      (eprintf "proxy: starting %s on :8888\n" bin)
      (os/execute (array bin) :e env :p true))))
