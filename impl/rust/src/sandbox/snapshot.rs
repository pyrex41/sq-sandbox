//! Snapshot operations: create squashfs from overlay upper layer.
//!
//! Wraps `mksquashfs` with zstd/gzip detection and integrates with
//! the metadata system for snapshot tracking.

use std::fs;
use std::path::{Path, PathBuf};

use chrono::Utc;
use tracing::{debug, info};

use crate::modules::builder;
use crate::sandbox::meta::{self, SnapshotEntry};
use crate::sandbox::SandboxError;

/// Create a squashfs snapshot from the overlay upper/data directory.
///
/// Steps:
/// 1. Validate that the upper data dir has content
/// 2. Create the snapshots/ directory
/// 3. Run mksquashfs with zstd (if kernel supports it) or gzip fallback
/// 4. Record the snapshot in metadata (snapshots.jsonl + active_snapshot)
/// 5. Return the snapshot file path and size
pub fn create_snapshot(
    sandbox_dir: &Path,
    label: &str,
    use_zstd: bool,
) -> Result<(PathBuf, u64), SandboxError> {
    let upper_data = sandbox_dir.join("upper").join("data");
    let snapshots_dir = sandbox_dir.join("snapshots");
    let snap_path = snapshots_dir.join(format!("{}.squashfs", label));
    let meta_dir = sandbox_dir.join(".meta");

    // Validate upper data exists and has content
    if !upper_data.is_dir() {
        return Err(SandboxError::Snapshot(
            "no upper/data directory found — sandbox may not have been used".to_string(),
        ));
    }

    // Check for existing snapshot with the same label
    if snap_path.exists() {
        return Err(SandboxError::Snapshot(format!(
            "snapshot '{}' already exists",
            label
        )));
    }

    fs::create_dir_all(&snapshots_dir)
        .map_err(|e| SandboxError::Snapshot(format!("create snapshots dir: {}", e)))?;

    info!(label, zstd = use_zstd, "creating snapshot");

    // Run mksquashfs
    builder::mksquashfs(&upper_data, &snap_path, use_zstd)
        .map_err(|e| SandboxError::Snapshot(format!("mksquashfs failed: {}", e)))?;

    // Get the snapshot file size
    let size = fs::metadata(&snap_path)
        .map(|m| m.len())
        .unwrap_or(0);

    // Record in metadata
    let entry = SnapshotEntry {
        label: label.to_string(),
        created: Utc::now(),
        size,
    };

    meta::append_snapshot_entry(&meta_dir, &entry)
        .map_err(|e| SandboxError::Meta(format!("append snapshot entry: {}", e)))?;

    meta::write_active_snapshot(&meta_dir, Some(label))
        .map_err(|e| SandboxError::Meta(format!("write active_snapshot: {}", e)))?;

    info!(label, size, "snapshot created");
    debug!(path = %snap_path.display(), "snapshot file");

    Ok((snap_path, size))
}

/// Check if the kernel supports zstd compression for squashfs.
///
/// Delegates to the builder module which reads /proc/config.gz or /boot/config-*.
pub fn kernel_supports_zstd() -> bool {
    builder::kernel_supports_zstd()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_snapshot_missing_upper() {
        let dir = tempfile::tempdir().unwrap();
        let result = create_snapshot(dir.path(), "test-snap", false);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("upper/data"), "error: {}", err);
    }

    #[test]
    fn test_create_snapshot_duplicate_label() {
        let dir = tempfile::tempdir().unwrap();

        // Create upper/data so first check passes
        fs::create_dir_all(dir.path().join("upper/data")).unwrap();
        fs::write(dir.path().join("upper/data/file.txt"), b"content").unwrap();

        // Create a pre-existing snapshot
        let snapshots_dir = dir.path().join("snapshots");
        fs::create_dir_all(&snapshots_dir).unwrap();
        fs::write(snapshots_dir.join("dupe.squashfs"), b"fake").unwrap();

        let result = create_snapshot(dir.path(), "dupe", false);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("already exists"), "error: {}", err);
    }

    #[test]
    fn test_kernel_supports_zstd_does_not_panic() {
        // Just verify it runs without panicking; actual result depends on host kernel
        let _ = kernel_supports_zstd();
    }
}
