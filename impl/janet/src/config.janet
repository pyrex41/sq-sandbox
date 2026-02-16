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

(defn data-dir-from-env []
  "Prefer SQUASH_DATA_DIR, fall back to legacy SQUASH_DATA."
  (def preferred (os/getenv "SQUASH_DATA_DIR"))
  (if (and preferred (> (length preferred) 0))
    preferred
    (env-or "SQUASH_DATA" "/data")))

(defn config-from-env
  "Create config table from environment variables."
  []
  @{
    :data-dir (data-dir-from-env)
    :port (env-int "SQUASH_PORT" 8080)
    :max-sandboxes (env-int "SQUASH_MAX_SANDBOXES" 100)
    :upper-limit-mb (env-int "SQUASH_UPPER_LIMIT_MB" 512)
    :backend (env-or "SQUASH_BACKEND" "chroot")
    :auth-token (env-or "SQUASH_AUTH_TOKEN" nil)
    :s3-bucket (env-or "SQUASH_S3_BUCKET" nil)
    :s3-endpoint (env-or "SQUASH_S3_ENDPOINT" nil)
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

# ── Validation (fail-fast at startup) ───────────────────────────────────────

(defn validate-config! [config]
  "Validate config. Throws on invalid values. Call at startup."
  (def port (get config :port 8080))
  (when (or (< port 1) (> port 65535))
    (error (string/format "SQUASH_PORT must be 1-65535, got %d" port)))
  (def max-sb (get config :max-sandboxes 100))
  (when (or (< max-sb 1) (> max-sb 10000))
    (error (string/format "SQUASH_MAX_SANDBOXES must be 1-10000, got %d" max-sb)))
  (def upper (get config :upper-limit-mb 512))
  (when (or (< upper 16) (> upper 32768))
    (error (string/format "SQUASH_UPPER_LIMIT_MB must be 16-32768, got %d" upper)))
  (def backend (get config :backend "chroot"))
  (when (not (or (= backend "chroot") (= backend "firecracker")))
    (error (string/format "SQUASH_BACKEND must be chroot or firecracker, got %s" backend)))
  (def data-dir (get config :data-dir))
  (when (or (not data-dir) (= (length data-dir) 0))
    (error "SQUASH_DATA/SQUASH_DATA_DIR must be set"))
  (when (string/find ".." data-dir)
    (error "data-dir must not contain .."))
  config)
