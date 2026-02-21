# Sandbox — lifecycle, create, destroy.
#
# Creation uses unwind-protect for rollback: if overlay mount fails,
# squashfs mounts are cleaned up. Each resource is tracked so
# destroy-sandbox can tear down in reverse order.
#
# Unprivileged mode: no tmpfs, no cgroups, no netns.
# Isolation is handled by sq-exec (bubblewrap/unshare).

(import config :prefix "config/")
(import validate :prefix "validate/")
(import mounts :prefix "mounts/")
(import meta)
(import secrets :prefix "secrets/")
(import s3)

(defn sandbox-dir [config id]
  (string (config/sandboxes-dir config) "/" id))

(defn create-sandbox
  "Create a sandbox with rollback on failure. Returns sandbox table or throws."
  [config id opts]
  (def sdir (sandbox-dir config id))
  (def mods (config/modules-dir config))

  # Create directory tree
  (mounts/ensure-dir (string sdir "/images"))
  (mounts/ensure-dir (string sdir "/upper/data"))
  (mounts/ensure-dir (string sdir "/upper/work"))
  (mounts/ensure-dir (string sdir "/merged"))
  (mounts/ensure-dir (string sdir "/.meta/log"))

  (var layers (get opts :layers @["000-base-alpine"]))
  (when (string? layers)
    (set layers (string/split "," (string/trim layers))))

  # Track all resources for rollback
  (def sqfs-mounts @[])
  (def lower-components @[])
  (var overlay nil)

  (defn rollback []
    "Tear down all resources created so far."
    (when overlay (try (mounts/unmount-overlay overlay) ([e] nil)))
    (each m (reverse sqfs-mounts)
      (try (mounts/unmount-squashfs m) ([e] nil))))

  (try
    (do
      # Mount squashfs layers
      (each layer layers
        (def sqfs-path (string mods "/" layer ".squashfs"))
        (def mp (string sdir "/images/" layer ".squashfs"))
        (when (not (os/stat sqfs-path))
          # Try S3 pull
          (def s3-key (string "modules/" layer ".squashfs"))
          (when (not (s3/pull config s3-key sqfs-path))
            (error (string/format "module not found: %s" layer))))
        (def m (mounts/mount-squashfs sqfs-path mp))
        (array/push sqfs-mounts m)
        (array/push lower-components mp))

      # Sort lower: highest numeric prefix first (highest priority)
      (defn numeric-prefix [path]
        (def name (last (string/split "/" path)))
        (or (scan-number (string/slice name 0 3)) 0))
      (sort lower-components
        (fn [a b] (> (numeric-prefix a) (numeric-prefix b))))

      # Mount overlay (upper/data and upper/work are regular directories)
      (set overlay (mounts/mount-overlay
                     lower-components
                     (string sdir "/upper/data")
                     (string sdir "/upper/work")
                     (string sdir "/merged")))

      # Secret injection
      (secrets/inject-secrets config @{:dir sdir})

      # Write metadata
      (meta/write-sandbox-meta sdir @{
        :id id
        :owner (get opts :owner "anon")
        :layers layers
        :task (get opts :task "")
        :cpu (or (get opts :cpu) 2)
        :memory_mb (or (get opts :memory-mb) 1024)
        :max_lifetime_s (get opts :max-lifetime-s 0)
        :allow_net (get opts :allow-net)
        :created (os/time)
      })

      # Return sandbox table
      @{
        :id id
        :dir sdir
        :state :ready
        :mounts @{
          :squashfs-mounts sqfs-mounts
          :snapshot-mount nil
          :overlay overlay
        }
        :created (os/time)
        :last-active (os/time)
        :exec-count 0
        :owner (get opts :owner "anon")
        :task (get opts :task "")
        :max-lifetime-s (get opts :max-lifetime-s 0)
      })

    ([e]
      (eprintf "sandbox create failed for %s: %s — rolling back\n" id (string e))
      (rollback)
      (error e))))

(defn destroy-sandbox
  "Tear down sandbox resources. Idempotent, never throws."
  [sandbox]
  (put sandbox :state :destroying)
  (when (get sandbox :mounts)
    (try (mounts/destroy-sandbox-mounts (get sandbox :mounts)) ([e] nil)))
  (put sandbox :state :destroyed)
  sandbox)
