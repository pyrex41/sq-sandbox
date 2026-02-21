use std::io;
use std::path::Path;

use tracing::debug;

/// Stub handle for network namespace setup.
///
/// In unprivileged mode, network isolation is handled by sq-exec (bubblewrap),
/// so explicit netns setup is not needed.
pub struct NetnsHandle {
    name: String,
    index: u8,
}

impl NetnsHandle {
    /// Stub: returns Ok(Self) with a placeholder name/index.
    /// Network isolation is delegated to sq-exec in unprivileged mode.
    pub fn setup(
        id: &str,
        _data_dir: &Path,
        _sandboxes_dir: &Path,
        _meta_dir: &Path,
        _allow_net: Option<&[String]>,
    ) -> io::Result<Self> {
        debug!(id, "netns setup skipped (unprivileged mode)");
        Ok(Self {
            name: format!("squash-{}", id),
            index: 0,
        })
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn index(&self) -> u8 {
        self.index
    }

    pub fn gateway_ip(&self) -> String {
        "127.0.0.1".to_string()
    }

    pub fn sandbox_ip(&self) -> String {
        "127.0.0.1".to_string()
    }

    pub fn veth_host(&self) -> &str {
        ""
    }

    pub fn from_recovered(
        _id: &str,
        _meta_dir: &Path,
        _allow_net: Option<&[String]>,
    ) -> io::Result<Option<Self>> {
        Ok(None)
    }
}

impl Drop for NetnsHandle {
    fn drop(&mut self) {
        debug!(netns = %self.name, "netns handle dropped (no-op in unprivileged mode)");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_stub_setup() {
        let dir = PathBuf::from("/tmp");
        let handle = NetnsHandle::setup("test", &dir, &dir, &dir, None).unwrap();
        assert_eq!(handle.name(), "squash-test");
        assert_eq!(handle.index(), 0);
    }

    #[test]
    fn test_stub_from_recovered() {
        let dir = PathBuf::from("/tmp");
        let result = NetnsHandle::from_recovered("test", &dir, None).unwrap();
        assert!(result.is_none());
    }
}
