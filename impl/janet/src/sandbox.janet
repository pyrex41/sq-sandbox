# Sandbox — lifecycle, create, destroy.
#
# Creation uses unwind-protect for rollback: if overlay mount fails,
# squashfs and tmpfs are cleaned up. Each resource is tracked so
# destroy-sandbox can tear down in reverse order.

(import config :prefix "config/")
(import validate :prefix "validate/")
(import mounts :prefix "mounts/")
(import cgroup)
(import netns)
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
  (mounts/ensure-dir (string sdir "/upper"))
  (mounts/ensure-dir (string sdir "/merged"))
  (mounts/ensure-dir (string sdir "/.meta/log"))

  (var layers (get opts :layers @["000-base-alpine"]))
  (when (string? layers)
    (set layers (string/split "," (string/trim layers))))

  # Track all resources for rollback
  (def sqfs-mounts @[])
  (def lower-components @[])
  (var tmpfs nil)
  (var overlay nil)
  (var cg nil)
  (var ns nil)

  (defn rollback []
    "Tear down all resources created so far."
    (when overlay (try (mounts/unmount-overlay overlay) ([e] nil)))
    (when tmpfs (try (mounts/unmount-tmpfs tmpfs) ([e] nil)))
    (each m (reverse sqfs-mounts)
      (try (mounts/unmount-squashfs m) ([e] nil)))
    (when ns (try (netns/teardown-netns ns) ([e] nil)))
    (when cg (try (cgroup/destroy-cgroup cg) ([e] nil))))

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

      # Mount tmpfs for upper layer
      (set tmpfs (mounts/mount-tmpfs (string sdir "/upper")
                   (or (get opts :upper-limit-mb) (get config :upper-limit-mb) 512)))

      # Mount overlay
      (set overlay (mounts/mount-overlay
                     lower-components
                     (string sdir "/upper/data")
                     (string sdir "/upper/work")
                     (string sdir "/merged")))

      # Cgroup (optional — may fail on non-Linux or without privileges)
      (set cg
        (try (cgroup/create-cgroup id
               (or (get opts :cpu) 2)
               (or (get opts :memory-mb) 1024))
          ([e] nil)))

      # Network namespace (optional)
      (set ns
        (try (netns/setup-netns id (get opts :allow-net))
          ([e] nil)))

      # Seed resolv.conf (DNS via netns gateway)
      (when ns
        (def etc-dir (string sdir "/upper/data/etc"))
        (mounts/ensure-dir etc-dir)
        (spit (string etc-dir "/resolv.conf")
              (string "nameserver " (string/format "10.200.%d.1" (get ns :index)) "\n")))

      # Secret injection (placeholders + proxy env)
      (secrets/inject-secrets config @{:dir sdir :netns ns})

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
          :tmpfs tmpfs
          :overlay overlay
        }
        :cgroup cg
        :netns ns
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
  (when (get sandbox :netns)
    (try (netns/teardown-netns (get sandbox :netns)) ([e] nil)))
  (when (get sandbox :cgroup)
    (try (cgroup/destroy-cgroup (get sandbox :cgroup)) ([e] nil)))
  (put sandbox :state :destroyed)
  sandbox)
