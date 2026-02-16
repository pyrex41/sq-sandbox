use std::fs;
use std::io;
use std::path::PathBuf;

use tracing::{debug, warn};

/// Manages a cgroups v2 resource group at /sys/fs/cgroup/squash-{id}.
///
/// On drop, migrates all processes to the root cgroup and removes the directory.
#[derive(Debug)]
pub struct CgroupHandle {
    path: PathBuf,
}

impl CgroupHandle {
    /// Create a new cgroup with CPU and memory limits.
    ///
    /// Returns `Ok(None)` if cgroup creation fails (matching the shell `|| true` behavior —
    /// cgroups may not be available in all environments).
    pub fn create(id: &str, cpu: f64, memory_mb: u64) -> io::Result<Option<Self>> {
        let path = PathBuf::from(format!("/sys/fs/cgroup/squash-{}", id));

        if let Err(e) = fs::create_dir_all(&path) {
            warn!("cgroup create failed for {}: {}, continuing without cgroup", id, e);
            return Ok(None);
        }

        // CPU: quota = cpu * 100000, period = 100000µs
        let quota = (cpu * 100_000.0) as u64;
        if let Err(e) = fs::write(path.join("cpu.max"), format!("{} 100000", quota)) {
            warn!("cgroup cpu.max write failed for {}: {}, cleaning up", id, e);
            let _ = fs::remove_dir(&path);
            return Ok(None);
        }

        // Memory limit in bytes
        let mem_bytes = memory_mb * 1024 * 1024;
        if let Err(e) = fs::write(path.join("memory.max"), mem_bytes.to_string()) {
            warn!("cgroup memory.max write failed for {}: {}, cleaning up", id, e);
            let _ = fs::remove_dir(&path);
            return Ok(None);
        }

        debug!("cgroup created: {} (cpu={}, mem={}MB)", path.display(), cpu, memory_mb);
        Ok(Some(Self { path }))
    }

    /// Add a process to this cgroup by writing its PID to cgroup.procs.
    pub fn add_process(&self, pid: u32) -> io::Result<()> {
        fs::write(self.path.join("cgroup.procs"), pid.to_string())
    }

    /// Returns the cgroup directory path.
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Re-associate with an already existing cgroup on disk during recovery.
    pub fn from_existing(id: &str) -> Option<Self> {
        let path = PathBuf::from(format!("/sys/fs/cgroup/squash-{}", id));
        if path.is_dir() {
            Some(Self { path })
        } else {
            None
        }
    }
}

impl Drop for CgroupHandle {
    fn drop(&mut self) {
        // Migrate all processes to root cgroup before rmdir (otherwise rmdir fails with EBUSY)
        if let Ok(pids) = fs::read_to_string(self.path.join("cgroup.procs")) {
            if !pids.trim().is_empty() {
                let root_procs = PathBuf::from("/sys/fs/cgroup/cgroup.procs");
                for pid_line in pids.lines() {
                    let pid = pid_line.trim();
                    if !pid.is_empty() {
                        if let Err(e) = fs::write(&root_procs, pid) {
                            warn!("failed to migrate pid {} to root cgroup: {}", pid, e);
                        }
                    }
                }
            }
        }

        if let Err(e) = fs::remove_dir(&self.path) {
            warn!("cgroup rmdir failed for {}: {}", self.path.display(), e);
        } else {
            debug!("cgroup removed: {}", self.path.display());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_cgroup_path_format() {
        // Verify the path format without actually creating cgroups
        let expected = PathBuf::from("/sys/fs/cgroup/squash-test-123");
        assert_eq!(
            format!("/sys/fs/cgroup/squash-{}", "test-123"),
            expected.to_str().unwrap()
        );
    }

    #[test]
    fn test_cpu_quota_calculation() {
        // cpu=1.0 should give quota=100000 (100% of one core)
        assert_eq!((1.0_f64 * 100_000.0) as u64, 100_000);
        // cpu=0.5 should give quota=50000 (50% of one core)
        assert_eq!((0.5_f64 * 100_000.0) as u64, 50_000);
        // cpu=2.0 should give quota=200000 (200% = 2 cores)
        assert_eq!((2.0_f64 * 100_000.0) as u64, 200_000);
    }

    #[test]
    fn test_memory_bytes_calculation() {
        // 512 MB in bytes
        assert_eq!(512u64 * 1024 * 1024, 536_870_912);
        // 1024 MB in bytes
        assert_eq!(1024u64 * 1024 * 1024, 1_073_741_824);
    }

    #[test]
    fn test_create_fails_silently_on_nonexistent_cgroup_fs() {
        // On macOS or systems without cgroups, create should return Ok(None)
        let result = CgroupHandle::create("unit-test-nonexistent", 1.0, 512);
        assert!(result.is_ok());
        // On systems without /sys/fs/cgroup, this will be None
        if !Path::new("/sys/fs/cgroup").exists() {
            assert!(result.unwrap().is_none());
        }
    }
}
