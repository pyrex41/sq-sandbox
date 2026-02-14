# Config — load from environment variables
# Matches the shell/CL/Rust config layout for wire compatibility.

(defn env-or [name default]
  (def val (os/getenv name))
  (if (and val (> (length val) 0)) val default))

(defn env-int [name default]
  (def val (os/getenv name))
  (if (and val (> (length val) 0))
    (try (scan-number val) ([e] default))
    default))

(defn env-bool [name]
  (def val (os/getenv name))
  (and val (or (= val "1") (= (string/ascii-lower val) "true"))))

(defn config-from-env
  "Create config table from environment variables."
  []
  @{
    :data-dir (env-or "SQUASH_DATA" "/data")
    :port (env-int "SQUASH_PORT" 8080)
    :max-sandboxes (env-int "SQUASH_MAX_SANDBOXES" 100)
    :upper-limit-mb (env-int "SQUASH_UPPER_LIMIT_MB" 512)
    :auth-token (env-or "SQUASH_AUTH_TOKEN" nil)
    :s3-bucket (env-or "SQUASH_S3_BUCKET" nil)
    :s3-region (env-or "SQUASH_S3_REGION" "us-east-1")
    :s3-prefix (env-or "SQUASH_S3_PREFIX" "")
    :proxy-https (env-bool "SQUASH_PROXY_HTTPS")
    :tailscale-authkey (env-or "TAILSCALE_AUTHKEY" nil)
    :tailscale-hostname (env-or "TAILSCALE_HOSTNAME" "squash")
  })

(defn modules-dir [config]
  (string (get config :data-dir) "/modules"))

(defn sandboxes-dir [config]
  (string (get config :data-dir) "/sandboxes"))

(defn proxy-ca-dir [config]
  (string (get config :data-dir) "/proxy-ca"))
