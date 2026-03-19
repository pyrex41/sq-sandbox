# Mounts — squashfs, overlay via shared helper scripts.
# Each mount type has mount/unmount. Teardown order: overlay, snapshot, squashfs (reverse).

(defn safe-path? [path]
  "Reject paths with .. or leading/trailing slashes that could escape."
  (and (string? path)
       (> (length path) 0)
       (<= (length path) 4096)
       (not (string/find ".." path))))

(defn ensure-dir [path]
  "Create directory and parents. Ignores if exists."
  (when (not (safe-path? path))
    (error (string/format "invalid path: %s" path)))
  (os/execute ["mkdir" "-p" path] :p))

(defn mount-squashfs [source-path mount-point]
  "Mount squashfs read-only via sq-mount-layer. Returns table with :mount-point :active."
  (when (not (and (safe-path? source-path) (safe-path? mount-point)))
    (error (string/format "invalid path for squashfs: %s -> %s" source-path mount-point)))
  (when (not (os/stat source-path))
    (error (string/format "squashfs source not found: %s" source-path)))
  (ensure-dir mount-point)
  (def rc (os/execute ["sq-mount-layer" source-path mount-point] :p))
  (when (not= rc 0)
    (error (string/format "mount squashfs %s -> %s failed (rc=%d)" source-path mount-point rc)))
  @{:mount-point mount-point :active true})

(defn unmount-squashfs [m]
  (when (get m :active)
    (def rc (os/execute ["sq-mount-layer" "--unmount" (get m :mount-point)] :p))
    (when (not= rc 0)
      (eprintf "warn: umount squashfs %s failed\n" (get m :mount-point)))
    (put m :active false)))

(defn mount-overlay [lower-components upper-data work merged]
  "Mount overlayfs via sq-mount-overlay. lower-components is array of paths, highest-priority first."
  (ensure-dir merged)
  (def lowerdir (string/join lower-components ":"))
  (def rc (os/execute ["sq-mount-overlay" lowerdir upper-data work merged] :p))
  (when (not= rc 0)
    (error (string/format "mount overlay %s failed" merged)))
  @{:merged-path merged :active true})

(defn unmount-overlay [m]
  (when (get m :active)
    (def rc (os/execute ["sq-mount-overlay" "--unmount" (get m :merged-path)] :p))
    (when (not= rc 0)
      (eprintf "warn: umount overlay %s failed\n" (get m :merged-path)))
    (put m :active false)))

(defn destroy-sandbox-mounts [mounts]
  "Unmount everything in reverse creation order. Never throws."
  (when mounts
    (when (get mounts :overlay)
      (try (unmount-overlay (get mounts :overlay)) ([e] nil)))
    (when (get mounts :snapshot-mount)
      (try (unmount-squashfs (get mounts :snapshot-mount)) ([e] nil)))
    (when (get mounts :squashfs-mounts)
      (each m (reverse (get mounts :squashfs-mounts))
        (try (unmount-squashfs m) ([e] nil))))))
