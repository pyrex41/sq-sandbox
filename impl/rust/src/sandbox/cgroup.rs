use std::io;
use std::path::PathBuf;

use tracing::debug;

/// Stub handle for cgroup resource limits.
///
/// In unprivileged mode, resource limits are not enforced via cgroups.
/// sq-exec (bubblewrap/timeout) handles basic containment.
#[derive(Debug)]
pub struct CgroupHandle {
    path: PathBuf,
}

impl CgroupHandle {
    /// Stub: always returns Ok(None) since cgroups are not used in unprivileged mode.
    pub fn create(_id: &str, _cpu: f64, _memory_mb: u64) -> io::Result<Option<Self>> {
        debug!("cgroup creation skipped (unprivileged mode)");
        Ok(None)
    }

    pub fn add_process(&self, _pid: u32) -> io::Result<()> {
        Ok(())
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Stub: always returns None.
    pub fn from_existing(_id: &str) -> Option<Self> {
        None
    }
}

impl Drop for CgroupHandle {
    fn drop(&mut self) {
        debug!("cgroup handle dropped (no-op in unprivileged mode)");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cgroup_stub_returns_none() {
        let result = CgroupHandle::create("test", 1.0, 512).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_cgroup_from_existing_returns_none() {
        assert!(CgroupHandle::from_existing("test").is_none());
    }
}
