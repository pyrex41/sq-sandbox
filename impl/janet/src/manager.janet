# Manager — sandbox registry with fiber-safe locking.
#
# Uses channel-based lock from lock.janet for fiber-friendly
# mutual exclusion inside net/server handlers.

(import sandbox :prefix "sandbox/")
(import exec)
(import mounts :prefix "mounts/")
(import config :prefix "config/")
(import validate :prefix "validate/")
(import modules :prefix "modules/")
(import firecracker :prefix "fc/")
(import meta)
(import json)
(import lock)
(import s3)

(defn- fc-backend? [config]
  (= (get config :backend) "firecracker"))

(defn- fc-create-sandbox
  "Create a Firecracker-backed sandbox: allocate CID, setup tap, start VM."
  [config id opts]
  (def sdir (string (config/sandboxes-dir config) "/" id))
  (def mods (config/modules-dir config))
  (def meta-dir (string sdir "/.meta"))
  (mounts/ensure-dir (string sdir "/.meta/log"))

  (var layers (get opts :layers @["000-base-alpine"]))
  (when (string? layers)
    (set layers (string/split "," (string/trim layers))))

  # Collect squashfs paths (ensure modules exist)
  (def squashfs-paths @[])
  (each layer layers
    (def sqfs-path (string mods "/" layer ".squashfs"))
    (when (not (os/stat sqfs-path))
      (def s3-key (string "modules/" layer ".squashfs"))
      (when (not (s3/pull config s3-key sqfs-path))
        (error (string/format "module not found: %s" layer))))
    (array/push squashfs-paths sqfs-path))

  # Allocate CID and network index
  (def cid (fc/allocate-cid config))
  (def index (% cid 255))

  # Setup tap network
  (def net-info (fc/fc-setup-network id index (get opts :allow-net)))

  (try
    (do
      # Start VM
      (fc/fc-start-vm id
        (or (get opts :cpu) 2)
        (or (get opts :memory-mb) 1024)
        squashfs-paths cid meta-dir)

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
        :backend "firecracker"
        :cid cid
      })

      @{
        :id id
        :dir sdir
        :state :ready
        :backend :firecracker
        :cid cid
        :net-info net-info
        :layers layers
        :cpu (or (get opts :cpu) 2)
        :memory-mb (or (get opts :memory-mb) 1024)
        :created (os/time)
        :last-active (os/time)
        :exec-count 0
        :owner (get opts :owner "anon")
        :task (get opts :task "")
        :max-lifetime-s (get opts :max-lifetime-s 0)
      })
    ([e]
      # Rollback: stop VM and teardown network
      (try (fc/fc-stop-vm id meta-dir) ([e2] nil))
      (try (fc/fc-teardown-network id index) ([e2] nil))
      (error e))))

(defn- fc-destroy-sandbox
  "Tear down a Firecracker-backed sandbox: stop VM, remove tap."
  [config sb]
  (put sb :state :destroying)
  (def id (get sb :id))
  (def meta-dir (string (get sb :dir) "/.meta"))
  (try (fc/fc-stop-vm id meta-dir) ([e] nil))
  (def net-info (get sb :net-info))
  (when net-info
    (try (fc/fc-teardown-network id (get net-info :index)) ([e] nil)))
  (put sb :state :destroyed))

(defn- fc-exec-in-sandbox
  "Execute a command in a Firecracker sandbox via vsock."
  [sb cmd opts sdir]
  (def cid (get sb :cid))
  (def workdir (get opts :workdir "/"))
  (def timeout-s (get opts :timeout 300))
  (def started (os/time))
  (def seq (+ 1 (get sb :exec-count 0)))
  (put sb :exec-count seq)
  (put sb :last-active started)

  (def result (fc/fc-exec cid cmd workdir timeout-s))
  (def finished (os/time))

  # Write exec log (best-effort)
  (when sdir
    (def log-dir (string sdir "/.meta/log"))
    (try
      (do
        (os/execute ["mkdir" "-p" log-dir] :p)
        (spit (string log-dir "/" (string/format "%04d" seq) ".json")
          (json/encode @{
            :seq seq :cmd cmd :workdir workdir
            :exit_code (get result :exit_code)
            :started started :finished finished
            :stdout (get result :stdout "")
            :stderr (get result :stderr "")
            :timed_out (get result :timed_out false)})))
      ([e] nil)))

  (put result :started started)
  (put result :finished finished)
  (put result :seq seq)
  result)

(defn make-manager [config]
  @{
    :config config
    :sandboxes @{}
    :lock (lock/make-lock)
  })

(defn manager-create [manager id opts]
  "Create sandbox. Returns sandbox or throws."
  (lock/with-lock (get manager :lock)
    (when (get (get manager :sandboxes) id)
      (error (string/format "sandbox already exists: %s" id)))
    (def count (length (keys (get manager :sandboxes))))
    (when (>= count (get (get manager :config) :max-sandboxes))
      (error (string/format "sandbox limit reached: %d"
               (get (get manager :config) :max-sandboxes))))
    # Reserve slot to prevent TOCTOU race
    (put (get manager :sandboxes) id :creating))
  # Create outside lock (slow I/O: mounts, netns, cgroup or VM start)
  (def cfg (get manager :config))
  (try
    (do
      (def sb
        (if (fc-backend? cfg)
          (fc-create-sandbox cfg id opts)
          (sandbox/create-sandbox cfg id opts)))
      (lock/with-lock (get manager :lock)
        (put (get manager :sandboxes) id sb))
      sb)
    ([e]
      (lock/with-lock (get manager :lock)
        (put (get manager :sandboxes) id nil))
      (error e))))

(defn manager-destroy [manager id]
  "Destroy sandbox by id. Removes from registry first, tears down outside lock."
  (var sb nil)
  (lock/with-lock (get manager :lock)
    (set sb (get (get manager :sandboxes) id))
    (when (not sb)
      (error (string/format "sandbox not found: %s" id)))
    (when (= sb :creating)
      (error (string/format "sandbox is still being created: %s" id)))
    (put (get manager :sandboxes) id nil))
  # Tear down outside lock
  (def cfg (get manager :config))
  (if (fc-backend? cfg)
    (fc-destroy-sandbox cfg sb)
    (sandbox/destroy-sandbox sb))
  (def sdir (config/sandboxes-dir cfg))
  (try (os/execute ["rm" "-rf" (string sdir "/" id)] :p) ([e] nil))
  sb)

(defn manager-get [manager id]
  (def sb (get (get manager :sandboxes) id))
  (when (= sb :creating) nil)
  sb)

(defn manager-list [manager]
  (def result @[])
  (each [k v] (pairs (get manager :sandboxes))
    (when (and v (not= v :creating))
      (array/push result v)))
  result)

(defn manager-exec [manager id cmd opts]
  "Execute cmd in sandbox."
  (var sb nil)
  (lock/with-lock (get manager :lock)
    (set sb (get (get manager :sandboxes) id))
    (when (not sb)
      (error (string/format "sandbox not found: %s" id)))
    (when (= sb :creating)
      (error (string/format "sandbox is still being created: %s" id))))
  (def cfg (get manager :config))
  (def sdir (config/sandboxes-dir cfg))
  (if (fc-backend? cfg)
    (fc-exec-in-sandbox sb cmd opts (string sdir "/" id))
    (exec/exec-in-sandbox sb cmd (merge opts @{:sandbox-dir (string sdir "/" id)}))))

(defn manager-sandbox-count [manager]
  (var n 0)
  (each [_ v] (pairs (get manager :sandboxes))
    (when (and v (not= v :creating)) (++ n)))
  n)

(defn sandbox-to-info [s]
  (when (and s (not= s :creating))
    @{
      :id (get s :id)
      :state (string (get s :state))
      :created (get s :created)
      :last_active (get s :last-active)
      :exec_count (get s :exec-count 0)
      :max_lifetime_s (get s :max-lifetime-s 0)
    }))

(defn manager-activate-module [manager id module-name]
  "Add module layer to sandbox. Remounts overlay (chroot) or hot-adds drive (firecracker)."
  (var sb nil)
  (lock/with-lock (get manager :lock)
    (set sb (get (get manager :sandboxes) id))
    (when (or (not sb) (= sb :creating))
      (error (string/format "sandbox not found: %s" id))))
  (def cfg (get manager :config))
  (def mod-path (string (config/modules-dir cfg) "/" module-name ".squashfs"))
  (when (not (modules/module-exists? cfg module-name))
    # Try S3 pull
    (def s3-key (string "modules/" module-name ".squashfs"))
    (mounts/ensure-dir (config/modules-dir cfg))
    (when (not (s3/pull cfg s3-key mod-path))
      (error (string/format "module not found: %s" module-name))))
  (if (fc-backend? cfg)
    # Firecracker: hot-add drive + guest remount
    (do
      (def cid (get sb :cid))
      (def meta-dir (string (get sb :dir) "/.meta"))
      (fc/fc-add-drive id module-name mod-path cid meta-dir))
    # Chroot: remount overlay with new layer
    (do
      (def sdir (get sb :dir))
      (def mp (string sdir "/images/" module-name ".squashfs"))
      (when (os/stat mp)
        (error (string/format "already active: %s" module-name)))
      (def sqfs-mount (mounts/mount-squashfs mod-path mp))
      (def mounts (get sb :mounts))
      (def sqfs (get mounts :squashfs-mounts))
      (array/push sqfs sqfs-mount)
      (def lower (map |(get $ :mount-point) sqfs))
      (def overlay (get mounts :overlay))
      (mounts/unmount-overlay overlay)
      (def new-overlay (mounts/mount-overlay
                         lower
                         (string sdir "/upper/data")
                         (string sdir "/upper/work")
                         (string sdir "/merged")))
      (put mounts :overlay new-overlay))))

(defn format-timestamp-label []
  (def t (os/date))
  (string/format "%04d%02d%02d-%02d%02d%02d"
    (get t :year) (+ 1 (get t :month)) (+ 1 (get t :month-day))
    (get t :hours) (get t :minutes) (get t :seconds)))

(defn manager-snapshot [manager id label]
  "Create snapshot. Returns [label size-bytes]."
  (var sb nil)
  (lock/with-lock (get manager :lock)
    (set sb (get (get manager :sandboxes) id))
    (when (or (not sb) (= sb :creating))
      (error (string/format "sandbox not found: %s" id))))
  (def cfg (get manager :config))
  (def lbl (or label (format-timestamp-label)))
  (when (not (validate/valid-label? lbl))
    (error "label: alphanumeric/dash/underscore/dot only"))
  (def sdir (get sb :dir))
  (def snapdir (string sdir "/snapshots"))
  (def snapfile (string snapdir "/" lbl ".squashfs"))
  (when (os/stat snapfile)
    (error (string/format "snapshot exists: %s" lbl)))
  (mounts/ensure-dir snapdir)

  (if (fc-backend? cfg)
    # Firecracker: ask guest to create snapshot via vsock
    (do
      (def cid (get sb :cid))
      (def resp (fc/fc-exec cid
                  (string "__squash_snapshot " snapfile)
                  "/" 300))
      (when (not= (get resp :exit_code) 0)
        (error (string/format "guest snapshot failed: %s" (get resp :stderr "")))))
    # Chroot: mksquashfs on host upper layer
    (do
      (def upper-data (string sdir "/upper/data"))
      (def rc (os/execute ["mksquashfs" upper-data snapfile
                           "-comp" "gzip" "-b" "256K" "-noappend" "-quiet"] :p))
      (when (not= rc 0)
        (error (string/format "mksquashfs failed: %s" snapfile)))))

  (def st (try (os/stat snapfile) ([e] nil)))
  (def size (if st (get st :size 0) 0))
  # S3 push in background
  (def s3-key (string "snapshots/" id "/" lbl ".squashfs"))
  (s3/push-bg cfg snapfile s3-key)
  [lbl size])

(defn manager-restore [manager id label]
  "Restore sandbox from snapshot."
  (var sb nil)
  (lock/with-lock (get manager :lock)
    (set sb (get (get manager :sandboxes) id))
    (when (or (not sb) (= sb :creating))
      (error (string/format "sandbox not found: %s" id))))
  (def cfg (get manager :config))
  (when (not (validate/valid-label? label))
    (error "label: alphanumeric/dash/underscore/dot only"))
  (def sdir (get sb :dir))
  (def snapfile (string sdir "/snapshots/" label ".squashfs"))
  (when (not (os/stat snapfile))
    # Try S3 pull
    (def s3-key (string "snapshots/" id "/" label ".squashfs"))
    (mounts/ensure-dir (string sdir "/snapshots"))
    (when (not (s3/pull cfg s3-key snapfile))
      (error (string/format "snapshot not found: %s" label))))
  (if (fc-backend? cfg)
    # Firecracker: stop VM, restart with snapshot layer
    (do
      (def meta-dir (string sdir "/.meta"))
      (fc/fc-stop-vm id meta-dir)
      # Collect squashfs paths: snapshot first, then original layers
      (def squashfs-paths @[snapfile])
      (each layer (get sb :layers @[])
        (def layer-path (string (config/modules-dir cfg) "/" layer ".squashfs"))
        (array/push squashfs-paths layer-path))
      (def cid (get sb :cid))
      (fc/fc-start-vm id
        (get sb :cpu 2) (get sb :memory-mb 1024)
        squashfs-paths cid meta-dir))
    # Chroot: remount overlay with snapshot as top lower layer
    (do
      (def mounts (get sb :mounts))
      (mounts/unmount-overlay (get mounts :overlay))
      (when (get mounts :snapshot-mount)
        (try (mounts/unmount-squashfs (get mounts :snapshot-mount)) ([e] nil)))
      (def upper-data (string sdir "/upper/data"))
      (def upper-work (string sdir "/upper/work"))
      (try (os/execute ["rm" "-rf" upper-data upper-work] :p) ([e] nil))
      (mounts/ensure-dir upper-data)
      (mounts/ensure-dir upper-work)
      (def snap-mp (string sdir "/images/_snapshot"))
      (def snap-mount (mounts/mount-squashfs snapfile snap-mp))
      (put mounts :snapshot-mount snap-mount)
      (def lower @[])
      (array/push lower snap-mp)
      (each m (get mounts :squashfs-mounts)
        (array/push lower (get m :mount-point)))
      (def new-overlay (mounts/mount-overlay lower upper-data upper-work (string sdir "/merged")))
      (put mounts :overlay new-overlay))))

(defn manager-exec-logs [manager id]
  "Return exec log entries as sorted array."
  (lock/with-lock (get manager :lock)
    (when (not (get (get manager :sandboxes) id))
      (error (string/format "sandbox not found: %s" id))))
  (def sdir (string (config/sandboxes-dir (get manager :config)) "/" id))
  (def log-dir (string sdir "/.meta/log"))
  (def result @[])
  (try
    (do
      (each f (sort (os/dir log-dir))
        (when (string/has-suffix? f ".json")
          (try
            (do
              (def content (slurp (string log-dir "/" f)))
              (when content (array/push result (json/decode content))))
            ([e] nil)))))
    ([e] nil))
  result)
