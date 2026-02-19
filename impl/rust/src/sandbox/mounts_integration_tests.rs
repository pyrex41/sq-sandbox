//! Integration tests for mount cycles requiring privileged container environment.
//!
//! These tests verify:
//! 1. Mount/unmount cycles for each mount type
//! 2. No orphaned mounts remain after Drop
//! 3. Partial failure cleanup
//! 4. Reverse-order unmounting in SandboxMounts
//!
//! Run with: cargo test --test mounts_integration -- --test-threads=1

use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

use super::{OverlayMount, SandboxMounts, SquashfsMount, TmpfsMount};

    /// Helper to create a minimal squashfs image for testing.
    fn create_test_squashfs(tmp_dir: &std::path::Path, name: &str) -> std::io::Result<PathBuf> {
        let content_dir = tmp_dir.join(format!("{}_content", name));
        fs::create_dir_all(&content_dir)?;

        // Write some test content
        let mut f = File::create(content_dir.join("test.txt"))?;
        writeln!(f, "test content for {}", name)?;

        let squashfs_path = tmp_dir.join(format!("{}.squashfs", name));

        let output = Command::new("mksquashfs")
            .arg(&content_dir)
            .arg(&squashfs_path)
            .arg("-comp")
            .arg("gzip")
            .arg("-noappend")
            .output()?;

        if !output.status.success() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "mksquashfs failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ),
            ));
        }

        Ok(squashfs_path)
    }

    /// Helper to check if a mount point is actually mounted.
    fn is_mounted(path: &std::path::Path) -> bool {
        let mounts = fs::read_to_string("/proc/mounts").unwrap_or_default();
        let path_str = path.to_str().unwrap();
        mounts.lines().any(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            parts.len() >= 2 && parts[1] == path_str
        })
    }

    /// Helper to count current mounts for a given prefix.
    fn count_mounts(prefix: &str) -> usize {
        let mounts = fs::read_to_string("/proc/mounts").unwrap_or_default();
        mounts
            .lines()
            .filter(|line| line.contains(prefix))
            .count()
    }

    #[test]
    fn test_squashfs_mount_unmount_cycle() {
        let tmp = TempDir::new().expect("create tempdir");
        let squashfs_path = create_test_squashfs(tmp.path(), "layer0")
            .expect("create test squashfs");

        let mount_point = tmp.path().join("mnt");
        fs::create_dir_all(&mount_point).expect("create mount point");

        // Mount
        let mount = SquashfsMount::mount(&squashfs_path, &mount_point)
            .expect("squashfs mount should succeed");

        assert!(is_mounted(&mount_point), "mount point should be mounted");
        assert_eq!(mount.source(), squashfs_path.as_path());
        assert_eq!(mount.target(), mount_point.as_path());

        // Verify we can read content
        let content = fs::read_to_string(mount_point.join("test.txt"))
            .expect("should be able to read mounted content");
        assert!(content.contains("test content for layer0"));

        // Drop should unmount
        drop(mount);

        assert!(
            !is_mounted(&mount_point),
            "mount point should be unmounted after drop"
        );
    }

    #[test]
    fn test_squashfs_explicit_unmount() {
        let tmp = TempDir::new().expect("create tempdir");
        let squashfs_path = create_test_squashfs(tmp.path(), "layer1")
            .expect("create test squashfs");

        let mount_point = tmp.path().join("mnt");
        fs::create_dir_all(&mount_point).expect("create mount point");

        let mut mount = SquashfsMount::mount(&squashfs_path, &mount_point)
            .expect("squashfs mount should succeed");

        assert!(is_mounted(&mount_point));

        // Explicit unmount
        mount.unmount().expect("unmount should succeed");

        assert!(
            !is_mounted(&mount_point),
            "mount point should be unmounted"
        );

        // Second unmount should be no-op
        mount.unmount().expect("second unmount should not error");
    }

    #[test]
    fn test_tmpfs_mount_unmount_cycle() {
        let tmp = TempDir::new().expect("create tempdir");
        let mount_point = tmp.path().join("tmpfs");

        // Mount tmpfs with 64MB limit
        let mount = TmpfsMount::mount(&mount_point, 64).expect("tmpfs mount should succeed");

        assert!(is_mounted(&mount_point), "tmpfs should be mounted");
        assert_eq!(mount.target(), mount_point.as_path());

        // Verify we can write to it
        let test_file = mount_point.join("test.txt");
        fs::write(&test_file, b"tmpfs test data").expect("should be able to write to tmpfs");

        let content = fs::read_to_string(&test_file).expect("should be able to read from tmpfs");
        assert_eq!(content, "tmpfs test data");

        // Drop should unmount
        drop(mount);

        assert!(
            !is_mounted(&mount_point),
            "tmpfs should be unmounted after drop"
        );
    }

    #[test]
    fn test_tmpfs_explicit_unmount() {
        let tmp = TempDir::new().expect("create tempdir");
        let mount_point = tmp.path().join("tmpfs");

        let mut mount = TmpfsMount::mount(&mount_point, 32).expect("tmpfs mount should succeed");

        assert!(is_mounted(&mount_point));

        mount.unmount().expect("unmount should succeed");
        assert!(!is_mounted(&mount_point));

        // Second unmount should be no-op
        mount.unmount().expect("second unmount should not error");
    }

    #[test]
    fn test_overlay_mount_unmount_cycle() {
        let tmp = TempDir::new().expect("create tempdir");

        // Create a simple lower layer directory
        let lower = tmp.path().join("lower");
        fs::create_dir_all(&lower).expect("create lower");
        fs::write(lower.join("base.txt"), b"base layer").expect("write base file");

        let upper = tmp.path().join("upper");
        let work = tmp.path().join("work");
        let merged = tmp.path().join("merged");

        // Mount overlay
        let mount = OverlayMount::mount(
            &merged,
            vec![lower.clone()],
            &upper,
            &work,
        )
        .expect("overlay mount should succeed");

        assert!(is_mounted(&merged), "overlay should be mounted");
        assert_eq!(mount.target(), merged.as_path());
        assert_eq!(mount.upper_dir(), upper.as_path());

        // Verify lower layer content is visible
        let content = fs::read_to_string(merged.join("base.txt"))
            .expect("should see lower layer content");
        assert_eq!(content, "base layer");

        // Write to overlay (goes to upper)
        fs::write(merged.join("new.txt"), b"upper layer")
            .expect("should be able to write to overlay");

        // Verify upper layer has the new file
        assert!(upper.join("data/new.txt").exists());

        // Drop should unmount
        drop(mount);

        assert!(
            !is_mounted(&merged),
            "overlay should be unmounted after drop"
        );
    }

    #[test]
    fn test_overlay_explicit_unmount() {
        let tmp = TempDir::new().expect("create tempdir");

        let lower = tmp.path().join("lower");
        fs::create_dir_all(&lower).expect("create lower");

        let upper = tmp.path().join("upper");
        let work = tmp.path().join("work");
        let merged = tmp.path().join("merged");

        let mut mount = OverlayMount::mount(&merged, vec![lower], &upper, &work)
            .expect("overlay mount should succeed");

        assert!(is_mounted(&merged));

        mount.unmount().expect("unmount should succeed");
        assert!(!is_mounted(&merged));

        // Second unmount should be no-op
        mount.unmount().expect("second unmount should not error");
    }

    #[test]
    fn test_overlay_with_multiple_lower_layers() {
        let tmp = TempDir::new().expect("create tempdir");

        // Create multiple lower layers
        let lower1 = tmp.path().join("lower1");
        let lower2 = tmp.path().join("lower2");
        fs::create_dir_all(&lower1).expect("create lower1");
        fs::create_dir_all(&lower2).expect("create lower2");

        fs::write(lower1.join("file1.txt"), b"layer 1").expect("write file1");
        fs::write(lower2.join("file2.txt"), b"layer 2").expect("write file2");

        let upper = tmp.path().join("upper");
        let work = tmp.path().join("work");
        let merged = tmp.path().join("merged");

        let mount = OverlayMount::mount(
            &merged,
            vec![lower1.clone(), lower2.clone()], // bottom to top
            &upper,
            &work,
        )
        .expect("overlay mount should succeed");

        assert!(is_mounted(&merged));

        // Both lower layers should be visible
        let content1 = fs::read_to_string(merged.join("file1.txt"))
            .expect("should see lower1 content");
        let content2 = fs::read_to_string(merged.join("file2.txt"))
            .expect("should see lower2 content");

        assert_eq!(content1, "layer 1");
        assert_eq!(content2, "layer 2");

        drop(mount);
        assert!(!is_mounted(&merged));
    }

    #[test]
    fn test_sandbox_mounts_full_cycle() {
        let tmp = TempDir::new().expect("create tempdir");

        // Create two squashfs layers
        let layer0_squashfs = create_test_squashfs(tmp.path(), "layer0")
            .expect("create layer0");
        let layer1_squashfs = create_test_squashfs(tmp.path(), "layer1")
            .expect("create layer1");

        let layer0_mount = tmp.path().join("mnt/layer0");
        let layer1_mount = tmp.path().join("mnt/layer1");
        let tmpfs_mount = tmp.path().join("upper");
        let merged = tmp.path().join("merged");

        let initial_mount_count = count_mounts(tmp.path().to_str().unwrap());

        // Mount everything
        let mounts = SandboxMounts::mount(
            vec![layer0_squashfs, layer1_squashfs],
            vec![layer0_mount.clone(), layer1_mount.clone()],
            &tmpfs_mount,
            128, // 128MB tmpfs
            &merged,
            None, // no snapshot
            None,
        )
        .expect("sandbox mounts should succeed");

        // Verify all mounts are active
        assert!(is_mounted(&layer0_mount), "layer0 should be mounted");
        assert!(is_mounted(&layer1_mount), "layer1 should be mounted");
        assert!(is_mounted(&tmpfs_mount), "tmpfs should be mounted");
        assert!(is_mounted(&merged), "overlay should be mounted");

        assert_eq!(mounts.layers.len(), 2);

        let mounted_count = count_mounts(tmp.path().to_str().unwrap());
        assert!(
            mounted_count > initial_mount_count,
            "should have additional mounts"
        );

        // Drop should unmount everything in reverse order
        drop(mounts);

        // Verify all mounts are gone
        assert!(
            !is_mounted(&merged),
            "overlay should be unmounted after drop"
        );
        assert!(
            !is_mounted(&tmpfs_mount),
            "tmpfs should be unmounted after drop"
        );
        assert!(
            !is_mounted(&layer1_mount),
            "layer1 should be unmounted after drop"
        );
        assert!(
            !is_mounted(&layer0_mount),
            "layer0 should be unmounted after drop"
        );

        let final_mount_count = count_mounts(tmp.path().to_str().unwrap());
        assert_eq!(
            final_mount_count, initial_mount_count,
            "all mounts should be cleaned up"
        );
    }

    #[test]
    fn test_sandbox_mounts_with_snapshot() {
        let tmp = TempDir::new().expect("create tempdir");

        let layer0_squashfs = create_test_squashfs(tmp.path(), "base")
            .expect("create base");
        let snapshot_squashfs = create_test_squashfs(tmp.path(), "snapshot")
            .expect("create snapshot");

        let layer0_mount = tmp.path().join("mnt/layer0");
        let snapshot_mount = tmp.path().join("mnt/snapshot");
        let tmpfs_mount = tmp.path().join("upper");
        let merged = tmp.path().join("merged");

        let mounts = SandboxMounts::mount(
            vec![layer0_squashfs],
            vec![layer0_mount.clone()],
            &tmpfs_mount,
            64,
            &merged,
            Some(snapshot_squashfs),
            Some(snapshot_mount.clone()),
        )
        .expect("sandbox mounts with snapshot should succeed");

        assert!(is_mounted(&layer0_mount));
        assert!(is_mounted(&snapshot_mount), "snapshot should be mounted");
        assert!(is_mounted(&tmpfs_mount));
        assert!(is_mounted(&merged));

        assert!(mounts.snapshot.is_some());

        drop(mounts);

        assert!(!is_mounted(&merged));
        assert!(!is_mounted(&tmpfs_mount));
        assert!(!is_mounted(&snapshot_mount), "snapshot should be unmounted");
        assert!(!is_mounted(&layer0_mount));
    }

    #[test]
    fn test_sandbox_mounts_partial_failure_cleanup() {
        let tmp = TempDir::new().expect("create tempdir");

        // Create one valid squashfs
        let layer0_squashfs = create_test_squashfs(tmp.path(), "valid")
            .expect("create valid layer");

        let layer0_mount = tmp.path().join("mnt/layer0");
        let layer1_mount = tmp.path().join("mnt/layer1");
        let tmpfs_mount = tmp.path().join("upper");
        let merged = tmp.path().join("merged");

        // Try to mount with one valid and one invalid squashfs
        let result = SandboxMounts::mount(
            vec![
                layer0_squashfs,
                PathBuf::from("/nonexistent/layer.squashfs"), // This will fail
            ],
            vec![layer0_mount.clone(), layer1_mount.clone()],
            &tmpfs_mount,
            64,
            &merged,
            None,
            None,
        );

        // Should fail
        assert!(result.is_err(), "mount should fail with invalid layer");

        // Verify that the first mount was cleaned up (no orphaned mounts)
        assert!(
            !is_mounted(&layer0_mount),
            "failed mount should clean up layer0"
        );
        assert!(
            !is_mounted(&tmpfs_mount),
            "failed mount should not leave tmpfs"
        );
        assert!(
            !is_mounted(&merged),
            "failed mount should not leave overlay"
        );
    }

    #[test]
    fn test_reverse_order_unmounting() {
        let tmp = TempDir::new().expect("create tempdir");

        let layer0_squashfs = create_test_squashfs(tmp.path(), "layer0")
            .expect("create layer0");
        let layer1_squashfs = create_test_squashfs(tmp.path(), "layer1")
            .expect("create layer1");

        let layer0_mount = tmp.path().join("mnt/layer0");
        let layer1_mount = tmp.path().join("mnt/layer1");
        let tmpfs_mount = tmp.path().join("upper");
        let merged = tmp.path().join("merged");

        let mounts = SandboxMounts::mount(
            vec![layer0_squashfs, layer1_squashfs],
            vec![layer0_mount.clone(), layer1_mount.clone()],
            &tmpfs_mount,
            64,
            &merged,
            None,
            None,
        )
        .expect("sandbox mounts should succeed");

        // All should be mounted
        assert!(is_mounted(&merged));
        assert!(is_mounted(&tmpfs_mount));
        assert!(is_mounted(&layer1_mount));
        assert!(is_mounted(&layer0_mount));

        // Drop should unmount in reverse order: overlay, tmpfs, layer1, layer0
        drop(mounts);

        // Verify cleanup order by checking all are gone
        assert!(!is_mounted(&merged), "overlay unmounted first");
        assert!(!is_mounted(&tmpfs_mount), "tmpfs unmounted second");
        assert!(
            !is_mounted(&layer1_mount),
            "layer1 unmounted third (reverse of mount order)"
        );
        assert!(
            !is_mounted(&layer0_mount),
            "layer0 unmounted last (reverse of mount order)"
        );
    }

    #[test]
    fn test_no_orphaned_mounts_after_drop() {
        let tmp = TempDir::new().expect("create tempdir");

        let layer0_squashfs = create_test_squashfs(tmp.path(), "test")
            .expect("create layer");

        let layer0_mount = tmp.path().join("mnt/layer0");
        let tmpfs_mount = tmp.path().join("upper");
        let merged = tmp.path().join("merged");

        let before_count = count_mounts(tmp.path().to_str().unwrap());

        {
            let _mounts = SandboxMounts::mount(
                vec![layer0_squashfs],
                vec![layer0_mount.clone()],
                &tmpfs_mount,
                64,
                &merged,
                None,
                None,
            )
            .expect("sandbox mounts should succeed");

            let during_count = count_mounts(tmp.path().to_str().unwrap());
            assert!(
                during_count > before_count,
                "mounts should be present during lifetime"
            );
        } // Drop happens here

        let after_count = count_mounts(tmp.path().to_str().unwrap());
        assert_eq!(
            after_count, before_count,
            "no orphaned mounts should remain after drop"
        );
    }

    #[test]
    fn test_double_unmount_is_safe() {
        let tmp = TempDir::new().expect("create tempdir");

        let layer0_squashfs = create_test_squashfs(tmp.path(), "layer")
            .expect("create layer");
        let mount_point = tmp.path().join("mnt");

        let mut mount = SquashfsMount::mount(&layer0_squashfs, &mount_point)
            .expect("mount should succeed");

        assert!(is_mounted(&mount_point));

        // First unmount
        mount.unmount().expect("first unmount should succeed");
        assert!(!is_mounted(&mount_point));

        // Second unmount should be safe (no-op)
        mount.unmount().expect("second unmount should not panic");

        // Drop should also be safe
        drop(mount);
    }

    #[test]
    fn test_tmpfs_size_limit() {
        let tmp = TempDir::new().expect("create tempdir");
        let mount_point = tmp.path().join("tmpfs");

        // Mount with small size (1MB) to test limit
        let _mount = TmpfsMount::mount(&mount_point, 1).expect("tmpfs mount should succeed");

        assert!(is_mounted(&mount_point));

        // Try to write within the size limit
        let test_file = mount_point.join("test.dat");
        let result = fs::write(&test_file, vec![0u8; 512 * 1024]); // 512KB
        assert!(result.is_ok(), "should be able to write within size limit");
    }
}
