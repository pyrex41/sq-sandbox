# S3 — shell out to sq-s3 for push, pull, list, sync.
#
# Uses the shared bin/sq-s3 script which supports aws CLI or curl+SigV4.
# Env: SQUASH_S3_BUCKET, SQUASH_S3_ENDPOINT, SQUASH_S3_REGION, SQUASH_S3_PREFIX,
#      SQUASH_DATA, AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY

(import config :prefix "config/")

(defn s3-enabled? [config]
  (and (get config :s3-bucket) (= 0 (os/execute ["which" "sq-s3"] :p))))

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
  (os/execute full-argv :ep (s3-env config)))

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
    (def env (s3-env config))
    (def proc (os/spawn ["sq-s3" "list" prefix] :ep env :out :pipe :err :pipe))
    # Drain stderr in background so child never blocks on full pipe
    (ev/go (fn []
      (try
        (forever
          (def chunk (ev/read (in proc :err) 4096))
          (when (or (nil? chunk) (= (length chunk) 0)) (break)))
        ([e] nil))))
    (def out-buf @"")
    (try
      (forever
        (def chunk (ev/read (in proc :out) 4096))
        (when (or (nil? chunk) (= (length chunk) 0)) (break))
        (buffer/push out-buf chunk))
      ([e] nil))
    (def exit (os/proc-wait proc))
    (os/proc-close proc)
    (when (= 0 exit)
      (filter |(> (length $) 0) (string/split "\n" (string out-buf))))))

(defn push-bg [config local-path s3-key]
  "Background push — spawns sq-s3 push in background, non-blocking."
  (when (s3-enabled? config)
    (def env (s3-env config))
    (ev/go
      (fn []
        (def rc (os/execute ["sq-s3" "push" local-path s3-key] :ep env))
        (when (not= rc 0)
          (eprintf "s3 push-bg failed (rc=%d): %s -> %s\n" rc local-path s3-key))))))

(defn sync-modules [config]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "sync-modules"))))

(defn sync-snapshots [config sandbox-id]
  (when (s3-enabled? config)
    (= 0 (s3-cmd config "sync-snapshots" sandbox-id))))
