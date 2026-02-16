# Secrets — load secrets.json, inject placeholders + proxy env into sandbox overlay.
#
# When secrets.json exists:
#   - Write /etc/profile.d/squash-secrets.sh with export KEY=placeholder for each secret
#   - Add http_proxy, https_proxy pointing to gateway:8888
#   - If SQUASH_PROXY_HTTPS=1: copy CA cert, append to ca-certificates, set NODE_EXTRA_CA_CERTS etc.

(import config :prefix "config/")
(import json)
(import mounts :prefix "mounts/")

(defn secrets-path [config]
  (string (get config :data-dir) "/secrets.json"))

(defn secrets-exist? [config]
  (os/stat (secrets-path config)))

(defn load-secrets [config]
  (def path (secrets-path config))
  (when (os/stat path)
    (def content (slurp path))
    (when content (try (json/decode content) ([e] nil)))))

(defn inject-secrets [config sandbox]
  "Inject placeholder env vars and proxy config into sandbox overlay. Idempotent."
  (def data (load-secrets config))
  (when (not data)
    (return nil))
  (def secrets (get data "secrets" @{}))
  (when (= (length (keys secrets)) 0)
    (return nil))
  (def sdir (get sandbox :dir))
  (def env-dir (string sdir "/upper/data/etc/profile.d"))
  (mounts/ensure-dir env-dir)
  # Proxy host: gateway IP if netns, else localhost
  (var proxy-host "127.0.0.1")
  (def ns (get sandbox :netns))
  (when ns
    (set proxy-host (string "10.200." (get ns :index) ".1")))
  (def proxy-url (string "http://" proxy-host ":8888"))
  (def lines @[])
  (each [k v] (pairs secrets)
    (when (table? v)
      (def ph (get v "placeholder"))
      (when ph
        (array/push lines (string "export " (string k) "=" (string ph))))))
  (array/push lines (string "export http_proxy=" proxy-url))
  (array/push lines (string "export https_proxy=" proxy-url))
  (array/push lines (string "export HTTP_PROXY=" proxy-url))
  (array/push lines (string "export HTTPS_PROXY=" proxy-url))
  # HTTPS proxy: copy CA cert into sandbox
  (def ca-cert (string (get config :data-dir) "/proxy-ca/ca.crt"))
  (when (and (get config :proxy-https) (os/stat ca-cert))
    (def ca-dest-dir (string sdir "/upper/data/usr/local/share/ca-certificates"))
    (mounts/ensure-dir ca-dest-dir)
    (spit (string ca-dest-dir "/sq-proxy-ca.crt") (slurp ca-cert))
    (def merged-ca (string sdir "/merged/etc/ssl/certs/ca-certificates.crt"))
    (when (os/stat merged-ca)
      (def etc-ssl (string sdir "/upper/data/etc/ssl/certs"))
      (mounts/ensure-dir etc-ssl)
      (def base-ca (string etc-ssl "/ca-certificates.crt"))
      (spit base-ca (string (slurp merged-ca) (slurp ca-cert))))
    (array/push lines "export NODE_EXTRA_CA_CERTS=/usr/local/share/ca-certificates/sq-proxy-ca.crt")
    (array/push lines "export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt")
    (array/push lines "export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"))
  (spit (string env-dir "/squash-secrets.sh") (string/join "\n" lines)))
