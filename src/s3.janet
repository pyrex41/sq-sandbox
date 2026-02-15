# S3 — shell out to sq-s3 for push, pull, list, sync.
#
# Uses the shared bin/sq-s3 script which supports aws CLI or curl+SigV4.
# Env: SQUASH_S3_BUCKET, SQUASH_S3_ENDPOINT, SQUASH_S3_REGION, SQUASH_S3_PREFIX,
#      SQUASH_DATA, AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY

(import config :prefix "config/")

(defn s3-enabled? [config]
  (and (get config :s3-bucket) (os/which "sq-s3")))

(defn- s3-env [config]
  (def data (or (get config :data-dir) (os/getenv "SQUASH_DATA") "/data"))
  (def env @{
    "SQUASH_DATA" data
    "SQUASH_S3_BUCKET" (or (get config :s3-bucket) (os/getenv "SQUASH_S3_BUCKET") "")
    "SQUASH_S3_REGION" (or (get config :s3-region) (os/getenv "SQUASH_S3_REGION") "us-east-1")
    "SQUASH_S3_PREFIX" (or (get config :s3-prefix) (os/getenv "SQUASH_S3_PREFIX") "")
  })
  (when (or (get config :s3-endpoint) (os/getenv "SQUASH_S3_ENDPOINT"))
    (put env "SQUASH_S3_ENDPOINT" (or (get config :s3-endpoint) (os/getenv "SQUASH_S3_ENDPOINT"))))
  env)

(defn- s3-cmd [config & args]
  (def full-argv (array "sq-s3"))
  (each a args (array/push full-argv (string a)))
  (os/execute full-argv :e (s3-env config) :p true))

(defn push [config local-path s3-key]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "push" local-path s3-key))))

(defn pull [config s3-key local-path]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "pull" s3-key local-path))))

(defn exists? [config s3-key]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "exists" s3-key))))

(defn list-keys [config prefix]
  (when (s3-enabled? config)
    (def tmp (string (os/getenv "TMPDIR" "/tmp") "/sq-s3-list-" (string (math/random 100000 999999))))
    (def proc (os/spawn (array "sq-s3" "list" prefix) :e (s3-env config) :out tmp :err tmp :p {}))
    (def exit ((in proc :wait) proc))
    (def out (try (slurp tmp) ([e] "")))
    (try (os/execute (array "rm" "-f" tmp)) ([e] nil))
    (when (= 0 exit)
      (filter |(> (length $) 0) (string/split "\n" (or out ""))))))

(defn push-bg [config local-path s3-key]
  "Background push — spawns sq-s3 push in background, non-blocking."
  (when (s3-enabled? config)
    (def log-file (string (get config :data-dir) "/.s3-push.log"))
    (ev/go
      (fn []
        (os/execute (array "sq-s3" "push" local-path s3-key)
          :e (s3-env config)
          :out log-file
          :err log-file
          :p true)))))

(defn sync-modules [config]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "sync-modules"))))

(defn sync-snapshots [config sandbox-id]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "sync-snapshots" sandbox-id))))
