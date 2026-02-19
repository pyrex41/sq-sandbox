# Init — recovery on startup
# Scans sandboxes dir, remounts layers, re-registers with manager.

(import config :prefix "config/")
(import sandbox :prefix "sandbox/")
(import mounts :prefix "mounts/")
(import meta)
(import lock)

(defn- fc-backend? [config]
  (= (get config :backend) "firecracker"))

(defn- read-int-file
  "Read integer from file, or nil on failure."
  [path]
  (try
    (when (os/stat path)
      (scan-number (string/trim (slurp path))))
    ([e] nil)))

(defn scan-sandbox-dirs [data-dir]
  (def sb-dir (string data-dir "/sandboxes"))
  (def result @[])
  (try
    (each name (os/dir sb-dir)
      (def full (string sb-dir "/" name))
      (def st (os/stat full))
      (when (and st (= (get st :type) "directory"))
        (array/push result name)))
    ([e] nil))
  result)

(defn remount-sandbox [config sandbox-dir id layers]
  "Remount squashfs layers and overlay. Returns mounts table or nil."
  (def mod-dir (config/modules-dir config))
  (def upper-path (string sandbox-dir "/upper"))
  (def merged-path (string sandbox-dir "/merged"))
  (try (do
    (mounts/ensure-dir (string upper-path "/data"))
    (mounts/ensure-dir (string upper-path "/work"))
    (def sqfs-mounts @[])
    (each layer layers
      (def mod-path (string mod-dir "/" layer ".squashfs"))
      (def mp (string sandbox-dir "/images/" layer ".squashfs"))
      (array/push sqfs-mounts (mounts/mount-squashfs mod-path mp)))
    (def lower (array/slice (map |(get $ :mount-point) sqfs-mounts)))
    (def overlay (mounts/mount-overlay lower (string upper-path "/data") (string upper-path "/work") merged-path))
    @{:squashfs-mounts sqfs-mounts :overlay overlay :snapshot-mount nil})
    ([e] (eprintf "init remount error for %s: %s\n" id (string e)) nil)))

(defn recover-sandbox [manager id data-dir]
  "Attempt to recover sandbox. Returns sandbox or nil."
  (def sandbox-dir (string data-dir "/sandboxes/" id))
  (def cfg (get manager :config))
  (def meta-data (meta/read-sandbox-meta sandbox-dir))
  (when (not meta-data)
    (eprintf "init: %s has no metadata, skipping\n" id)
    (return nil))
  (if (fc-backend? cfg)
    (do
      # Firecracker recovery: no host overlay reconstruction.
      (def cid (or (get meta-data "cid")
                   (read-int-file (string sandbox-dir "/.meta/cid"))))
      (when (not cid)
        (eprintf "init: %s missing Firecracker CID, skipping\n" id)
        (return nil))
      (def index (% cid 255))
      (var layers (get meta-data "layers" @["000-base-alpine"]))
      (when (string? layers)
        (set layers @[layers]))
      (def sandbox @{
        :id id
        :dir sandbox-dir
        :state :ready
        :backend :firecracker
        :cid cid
        :net-info @{:index index}
        :layers layers
        :cpu (get meta-data "cpu" 2)
        :memory-mb (get meta-data "memory_mb" 1024)
        :created (get meta-data "created" (math/floor (os/time)))
        :last-active (math/floor (os/time))
        :exec-count 0
        :owner (get meta-data "owner" "anon")
        :task (get meta-data "task" "")
        :max-lifetime-s (get meta-data "max_lifetime_s" 0)
      })
      (lock/with-lock (get manager :lock)
        (put (get manager :sandboxes) id sandbox))
      (eprintf "init: recovered firecracker %s\n" id)
      sandbox)
    (do
      (var layers (get meta-data "layers" @["000-base-alpine"]))
      (when (string? layers)
        (set layers @[layers]))
      (def mounts (remount-sandbox cfg sandbox-dir id layers))
      (when (not mounts)
        (try (os/execute ["rm" "-rf" sandbox-dir] :p) ([e] nil))
        (return nil))
      (def sandbox @{
        :id id
        :dir sandbox-dir
        :state :ready
        :mounts mounts
        :created (get meta-data "created" (math/floor (os/time)))
        :last-active (math/floor (os/time))
        :exec-count 0
        :max-lifetime-s (get meta-data "max_lifetime_s" 0)
      })
      (lock/with-lock (get manager :lock)
        (put (get manager :sandboxes) id sandbox))
      (eprintf "init: recovered %s\n" id)
      sandbox)))

(defn init-recover [config manager]
  "Main init entry. Returns [recovered failed]."
  (def data-dir (get config :data-dir))
  (def ids (scan-sandbox-dirs data-dir))
  (var recovered 0)
  (var failed 0)
  (each id ids
    (if (recover-sandbox manager id data-dir)
      (set recovered (+ recovered 1))
      (set failed (+ failed 1))))
  [recovered failed])
