# Sandbox — lifecycle, create, destroy
# Uses with-rollback pattern: on failure, tear down what we've mounted.

(import config :prefix "config/")
(import validate :prefix "validate/")
(import mounts :prefix "mounts/")

(defn sandbox-dir [config id]
  (string (config/sandboxes-dir config) "/" id))

(defn create-sandbox [config id opts]
  "Create a sandbox. opts: :owner :task :layers :cpu :memory-mb :max-lifetime-s :allow-net
   Returns sandbox table or throws."
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

  (def sqfs-mounts @[])
  (def lower-components @[])

  # Mount squashfs layers
  (each layer layers
    (def sqfs-path (string mods "/" layer ".squashfs"))
    (def mp (string sdir "/images/" layer ".squashfs"))
    (when (not (os/stat sqfs-path))
      (error (string/format "module not found: %s" layer)))
    (def m (mounts/mount-squashfs sqfs-path mp))
    (array/push sqfs-mounts m)
    (array/push lower-components mp))

  # Sort lower: descending numeric (highest priority first)
  (defn numeric-prefix [path]
    (def name (last (string/split "/" path)))
    (scan-number (string/slice name 0 3)))
  (sort lower-components
    (fn [a b] (> (numeric-prefix a) (numeric-prefix b))))

  # Mount tmpfs
  (def tmpfs (mounts/mount-tmpfs (string sdir "/upper")
                                (or (get opts :upper-limit-mb) (get config :upper-limit-mb) 512)))

  # Mount overlay
  (def upper-data (string sdir "/upper/data"))
  (def work (string sdir "/upper/work"))
  (def merged (string sdir "/merged"))
  (def overlay (mounts/mount-overlay lower-components upper-data work merged))

  # TODO: cgroup, netns, resolv.conf, secrets
  # For now, minimal sandbox

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
    :created (os/time)
    :last-active (os/time)
    :exec-count 0
    :owner (get opts :owner "anon")
    :task (get opts :task "")
    :max-lifetime-s (get opts :max-lifetime-s 0)
  })

(defn destroy-sandbox [sandbox]
  "Tear down sandbox resources. Idempotent."
  (put sandbox :state :destroying)
  (when (get sandbox :mounts)
    (mounts/destroy-sandbox-mounts (get sandbox :mounts)))
  (put sandbox :state :destroyed)
  sandbox)
