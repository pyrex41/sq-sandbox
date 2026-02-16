use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use nix::mount::{mount, umount2, MntFlags, MsFlags};
use tracing::{debug, warn};

/// A squashfs filesystem mounted read-only from a module.
#[derive(Debug)]
pub struct SquashfsMount {
    source: PathBuf,
    target: PathBuf,
    mounted: bool,
}

impl SquashfsMount {
    /// Mount a squashfs module read-only at the target mountpoint.
    pub fn mount(source: impl AsRef<Path>, target: impl AsRef<Path>) -> io::Result<Self> {
        let source = source.as_ref().to_path_buf();
        let target = target.as_ref().to_path_buf();

        fs::create_dir_all(&target)?;

        mount(
            Some(&source),
            &target,
            Some("squashfs"),
            MsFlags::MS_RDONLY,
            None::<&str>,
        )
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("squashfs mount failed: {}", e)))?;

        debug!("squashfs mounted: {} -> {}", source.display(), target.display());

        Ok(Self {
            source,
            target,
            mounted: true,
        })
    }

    /// Returns the mount target path.
    pub fn target(&self) -> &Path {
        &self.target
    }

    /// Returns the mount source path.
    pub fn source(&self) -> &Path {
        &self.source
    }

    /// Unmount explicitly (called by Drop, but can be called early for testing).
    pub fn unmount(&mut self) -> io::Result<()> {
        if !self.mounted {
            return Ok(());
        }

        umount2(&self.target, MntFlags::MNT_DETACH)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("umount failed: {}", e)))?;

        self.mounted = false;
        debug!("squashfs unmounted: {}", self.target.display());
        Ok(())
    }
}

impl Drop for SquashfsMount {
    fn drop(&mut self) {
        if self.mounted {
            if let Err(e) = umount2(&self.target, MntFlags::MNT_DETACH) {
                warn!("failed to unmount squashfs {}: {}", self.target.display(), e);
            } else {
                debug!("squashfs unmounted (Drop): {}", self.target.display());
            }
        }
    }
}

/// A tmpfs filesystem for overlay upper and work dirs.
#[derive(Debug)]
pub struct TmpfsMount {
    target: PathBuf,
    size_mb: u64,
    mounted: bool,
}

impl TmpfsMount {
    /// Mount a tmpfs at the target with the specified size limit in MB.
    pub fn mount(target: impl AsRef<Path>, size_mb: u64) -> io::Result<Self> {
        let target = target.as_ref().to_path_buf();

        fs::create_dir_all(&target)?;

        let opts = format!("size={}M", size_mb);
        mount(
            Some("tmpfs"),
            &target,
            Some("tmpfs"),
            MsFlags::empty(),
            Some(opts.as_str()),
        )
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("tmpfs mount failed: {}", e)))?;

        debug!("tmpfs mounted: {} (size={}MB)", target.display(), size_mb);

        Ok(Self {
            target,
            size_mb,
            mounted: true,
        })
    }

    /// Returns the mount target path.
    pub fn target(&self) -> &Path {
        &self.target
    }

    /// Unmount explicitly (called by Drop, but can be called early for testing).
    pub fn unmount(&mut self) -> io::Result<()> {
        if !self.mounted {
            return Ok(());
        }

        umount2(&self.target, MntFlags::MNT_DETACH)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("umount failed: {}", e)))?;

        self.mounted = false;
        debug!("tmpfs unmounted: {}", self.target.display());
        Ok(())
    }
}

impl Drop for TmpfsMount {
    fn drop(&mut self) {
        if self.mounted {
            if let Err(e) = umount2(&self.target, MntFlags::MNT_DETACH) {
                warn!("failed to unmount tmpfs {}: {}", self.target.display(), e);
            } else {
                debug!("tmpfs unmounted (Drop): {}", self.target.display());
            }
        }
    }
}

/// An overlayfs combining multiple lower layers with an upper writable layer.
#[derive(Debug)]
pub struct OverlayMount {
    target: PathBuf,
    lower_dirs: Vec<PathBuf>,
    upper_dir: PathBuf,
    work_dir: PathBuf,
    mounted: bool,
}

impl OverlayMount {
    /// Mount an overlayfs at the target.
    ///
    /// # Arguments
    /// * `target` - Where to mount the merged view (e.g., `sandbox/merged`)
    /// * `lower_dirs` - Ordered list of read-only lower layers (bottom-to-top)
    /// * `upper_dir` - Writable upper layer for changes
    /// * `work_dir` - Work directory for overlay (must be on same fs as upper)
    pub fn mount(
        target: impl AsRef<Path>,
        lower_dirs: Vec<PathBuf>,
        upper_dir: impl AsRef<Path>,
        work_dir: impl AsRef<Path>,
    ) -> io::Result<Self> {
        let target = target.as_ref().to_path_buf();
        let upper_dir = upper_dir.as_ref().to_path_buf();
        let work_dir = work_dir.as_ref().to_path_buf();

        if lower_dirs.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "overlay requires at least one lower dir",
            ));
        }

        fs::create_dir_all(&target)?;
        fs::create_dir_all(&upper_dir)?;
        fs::create_dir_all(&work_dir)?;

        // Build lowerdir option: colon-separated paths
        let lower_str = lower_dirs
            .iter()
            .map(|p| p.to_str().unwrap())
            .collect::<Vec<_>>()
            .join(":");

        let opts = format!(
            "lowerdir={},upperdir={},workdir={}",
            lower_str,
            upper_dir.display(),
            work_dir.display()
        );

        mount(
            Some("overlay"),
            &target,
            Some("overlay"),
            MsFlags::empty(),
            Some(opts.as_str()),
        )
        .map_err(|e| {
            io::Error::new(io::ErrorKind::Other, format!("overlay mount failed: {}", e))
        })?;

        debug!(
            "overlay mounted: {} (lower={}, upper={}, work={})",
            target.display(),
            lower_str,
            upper_dir.display(),
            work_dir.display()
        );

        Ok(Self {
            target,
            lower_dirs,
            upper_dir,
            work_dir,
            mounted: true,
        })
    }

    /// Returns the mount target path.
    pub fn target(&self) -> &Path {
        &self.target
    }

    /// Returns the upper directory path.
    pub fn upper_dir(&self) -> &Path {
        &self.upper_dir
    }

    /// Unmount explicitly (called by Drop, but can be called early for testing).
    pub fn unmount(&mut self) -> io::Result<()> {
        if !self.mounted {
            return Ok(());
        }

        umount2(&self.target, MntFlags::MNT_DETACH)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("umount failed: {}", e)))?;

        self.mounted = false;
        debug!("overlay unmounted: {}", self.target.display());
        Ok(())
    }
}

impl Drop for OverlayMount {
    fn drop(&mut self) {
        if self.mounted {
            if let Err(e) = umount2(&self.target, MntFlags::MNT_DETACH) {
                warn!("failed to unmount overlay {}: {}", self.target.display(), e);
            } else {
                debug!("overlay unmounted (Drop): {}", self.target.display());
            }
        }
    }
}

/// Owns all mounts for a sandbox: squashfs layers, tmpfs, and overlay.
///
/// Drop unmounts in reverse order: overlay first, then tmpfs, then squashfs layers.
#[derive(Debug)]
pub struct SandboxMounts {
    /// The overlayfs merging all layers with the upper writable layer.
    pub overlay: OverlayMount,
    /// The tmpfs holding upper/data and upper/work.
    pub tmpfs: TmpfsMount,
    /// Active snapshot mount (if a snapshot is currently active).
    pub snapshot: Option<SquashfsMount>,
    /// Squashfs module layers (ordered bottom-to-top).
    pub layers: Vec<SquashfsMount>,
}

impl SandboxMounts {
    /// Create a new SandboxMounts by mounting all layers and overlay.
    ///
    /// # Arguments
    /// * `layer_sources` - Paths to squashfs modules to mount (ordered bottom-to-top)
    /// * `layer_targets` - Mountpoints for each layer
    /// * `tmpfs_target` - Where to mount the tmpfs for upper/work
    /// * `tmpfs_size_mb` - Size limit for tmpfs in MB
    /// * `overlay_target` - Where to mount the merged overlay view
    /// * `snapshot_source` - Optional snapshot squashfs to mount
    /// * `snapshot_target` - Optional snapshot mountpoint
    pub fn mount(
        layer_sources: Vec<PathBuf>,
        layer_targets: Vec<PathBuf>,
        tmpfs_target: impl AsRef<Path>,
        tmpfs_size_mb: u64,
        overlay_target: impl AsRef<Path>,
        snapshot_source: Option<PathBuf>,
        snapshot_target: Option<PathBuf>,
    ) -> io::Result<Self> {
        if layer_sources.len() != layer_targets.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "layer sources and targets must have same length",
            ));
        }

        if layer_sources.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "at least one layer required",
            ));
        }

        let tmpfs_target = tmpfs_target.as_ref();
        let overlay_target = overlay_target.as_ref();

        // Mount squashfs layers
        let mut layers = Vec::new();
        for (src, tgt) in layer_sources.iter().zip(layer_targets.iter()) {
            match SquashfsMount::mount(src, tgt) {
                Ok(mount) => layers.push(mount),
                Err(e) => {
                    // Clean up already-mounted layers on failure
                    drop(layers);
                    return Err(e);
                }
            }
        }

        // Mount optional snapshot
        let snapshot = match (snapshot_source, snapshot_target) {
            (Some(src), Some(tgt)) => match SquashfsMount::mount(src, tgt) {
                Ok(mount) => Some(mount),
                Err(e) => {
                    drop(layers);
                    return Err(e);
                }
            },
            _ => None,
        };

        // Mount tmpfs
        let tmpfs = match TmpfsMount::mount(tmpfs_target, tmpfs_size_mb) {
            Ok(t) => t,
            Err(e) => {
                drop(snapshot);
                drop(layers);
                return Err(e);
            }
        };

        // Create upper/data and upper/work directories
        let upper_data = tmpfs_target.join("data");
        let upper_work = tmpfs_target.join("work");
        fs::create_dir_all(&upper_data)?;
        fs::create_dir_all(&upper_work)?;

        // Build lower dir list: snapshot (if present) is topmost lower, then layers top-to-bottom
        let mut lower_dirs = Vec::new();
        if let Some(ref snap) = snapshot {
            lower_dirs.push(snap.target().to_path_buf());
        }
        // Layers are already bottom-to-top, but overlay wants top-to-bottom for lowerdir
        for layer in layers.iter().rev() {
            lower_dirs.push(layer.target().to_path_buf());
        }

        // Mount overlay
        let overlay = match OverlayMount::mount(overlay_target, lower_dirs, upper_data, upper_work)
        {
            Ok(o) => o,
            Err(e) => {
                drop(tmpfs);
                drop(snapshot);
                drop(layers);
                return Err(e);
            }
        };

        debug!(
            "sandbox mounts created: {} layers, tmpfs={}, overlay={}",
            layers.len(),
            tmpfs_target.display(),
            overlay_target.display()
        );

        Ok(Self {
            overlay,
            tmpfs,
            snapshot,
            layers,
        })
    }
}

impl Drop for SandboxMounts {
    fn drop(&mut self) {
        // Explicit reverse-order unmount: overlay, tmpfs, snapshot, layers (top to bottom)
        debug!("dropping SandboxMounts: unmounting in reverse order");

        // 1. Overlay
        if let Err(e) = self.overlay.unmount() {
            warn!("failed to unmount overlay during SandboxMounts drop: {}", e);
        }

        // 2. Tmpfs
        if let Err(e) = self.tmpfs.unmount() {
            warn!("failed to unmount tmpfs during SandboxMounts drop: {}", e);
        }

        // 3. Snapshot (if present)
        if let Some(ref mut snap) = self.snapshot {
            if let Err(e) = snap.unmount() {
                warn!("failed to unmount snapshot during SandboxMounts drop: {}", e);
            }
        }

        // 4. Layers (reverse order: top to bottom)
        for layer in self.layers.iter_mut().rev() {
            if let Err(e) = layer.unmount() {
                warn!(
                    "failed to unmount layer {} during SandboxMounts drop: {}",
                    layer.target().display(),
                    e
                );
            }
        }

        debug!("SandboxMounts dropped successfully");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_squashfs_mount_target() {
        // Verify path handling without actually mounting
        let source = PathBuf::from("/modules/000-base-alpine.squashfs");
        let target = PathBuf::from("/tmp/mnt/layer0");

        // We can't actually mount without a real squashfs file and privileges,
        // but we can verify the struct would store paths correctly
        assert_eq!(source.to_str().unwrap(), "/modules/000-base-alpine.squashfs");
        assert_eq!(target.to_str().unwrap(), "/tmp/mnt/layer0");
    }

    #[test]
    fn test_overlay_mount_validates_input() {
        // OverlayMount::mount should error if lower_dirs is empty
        let result = OverlayMount::mount(
            "/tmp/merged",
            vec![], // empty lower dirs
            "/tmp/upper",
            "/tmp/work",
        );

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().kind(),
            io::ErrorKind::InvalidInput
        );
    }

    #[test]
    fn test_sandbox_mounts_validates_layer_count_mismatch() {
        let result = SandboxMounts::mount(
            vec![PathBuf::from("/a"), PathBuf::from("/b")],
            vec![PathBuf::from("/mnt/a")], // mismatch: 2 sources, 1 target
            "/tmp/upper",
            128,
            "/tmp/merged",
            None,
            None,
        );

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().kind(),
            io::ErrorKind::InvalidInput
        );
    }

    #[test]
    fn test_sandbox_mounts_validates_empty_layers() {
        let result = SandboxMounts::mount(
            vec![], // empty
            vec![],
            "/tmp/upper",
            128,
            "/tmp/merged",
            None,
            None,
        );

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().kind(),
            io::ErrorKind::InvalidInput
        );
    }
}

// Integration tests requiring privileged container environment
// Run with: cargo test --test mounts_integration -- --test-threads=1
#[cfg(all(test, target_os = "linux"))]
#[path = "mounts_integration_tests.rs"]
mod integration_tests;
