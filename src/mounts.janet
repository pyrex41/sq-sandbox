# Mounts — squashfs, tmpfs, overlay
# Each mount type has mount/unmount. Teardown order: overlay, snapshot, tmpfs, squashfs (reverse).

(import syscalls)

(defn ensure-dir [path]
  "Create directory and parents. Ignores if exists."
  (os/execute ["mkdir" "-p" path] :p))

(defn mount-squashfs [source-path mount-point]
  "Mount squashfs read-only. Returns table with :mount-point :active."
  (ensure-dir mount-point)
  (def rc (syscalls/mount-syscall source-path mount-point "squashfs" syscalls/MS_RDONLY nil))
  (when (not= rc 0)
    (error (string/format "mount squashfs %s -> %s failed" source-path mount-point)))
  @{:mount-point mount-point :active true})

(defn unmount-squashfs [m]
  (when (get m :active)
    (def rc (syscalls/umount-syscall (get m :mount-point) syscalls/MNT_DETACH))
    (when (not= rc 0)
      (eprintf "warn: umount squashfs %s failed\n" (get m :mount-point)))
    (put m :active false)))

(defn mount-tmpfs [mount-point size-mb]
  "Mount tmpfs with size limit. Creates data/ and work/ for overlay."
  (ensure-dir mount-point)
  (def opts (string/format "size=%dm" size-mb))
  (def rc (syscalls/mount-syscall "tmpfs" mount-point "tmpfs" 0 opts))
  (when (not= rc 0)
    (error (string/format "mount tmpfs %s failed" mount-point)))
  (ensure-dir (string mount-point "/data"))
  (ensure-dir (string mount-point "/work"))
  @{:mount-point mount-point :active true})

(defn unmount-tmpfs [m]
  (when (get m :active)
    (def rc (syscalls/umount-syscall (get m :mount-point) syscalls/MNT_DETACH))
    (when (not= rc 0)
      (eprintf "warn: umount tmpfs %s failed\n" (get m :mount-point)))
    (put m :active false)))

(defn mount-overlay [lower-components upper-data work merged]
  "Mount overlayfs. lower-components is array of paths, highest-priority first."
  (ensure-dir merged)
  (def lowerdir (string/join ":" lower-components))
  (def opts (string/format "lowerdir=%s,upperdir=%s,workdir=%s" lowerdir upper-data work))
  (def rc (syscalls/mount-syscall "overlay" merged "overlay" 0 opts))
  (when (not= rc 0)
    (error (string/format "mount overlay %s failed" merged)))
  @{:merged-path merged :active true})

(defn unmount-overlay [m]
  (when (get m :active)
    (def rc (syscalls/umount-syscall (get m :merged-path) syscalls/MNT_DETACH))
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
    (when (get mounts :tmpfs)
      (try (unmount-tmpfs (get mounts :tmpfs)) ([e] nil)))
    (when (get mounts :squashfs-mounts)
      (each m (reverse (get mounts :squashfs-mounts))
        (try (unmount-squashfs m) ([e] nil))))))
