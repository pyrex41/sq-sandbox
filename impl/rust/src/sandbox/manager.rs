use std::sync::Arc;

use dashmap::DashMap;
use tokio::sync::Mutex;
use tracing::info;

use crate::sandbox::{Sandbox, SandboxError, SandboxHandle};

/// Manages all active sandboxes with concurrent access via DashMap.
///
/// Each sandbox is wrapped in `Arc<Mutex<Sandbox>>` so that operations on
/// different sandboxes run in parallel, while operations on the *same* sandbox
/// (exec, snapshot, destroy) are serialized by the per-sandbox mutex.
pub struct SandboxManager {
    sandboxes: DashMap<String, SandboxHandle>,
    max_sandboxes: usize,
}

impl SandboxManager {
    /// Create a new manager with the given concurrency limit.
    /// Pass 0 for unlimited sandboxes.
    pub fn new(max_sandboxes: usize) -> Self {
        Self {
            sandboxes: DashMap::new(),
            max_sandboxes,
        }
    }

    /// Insert a new sandbox into the manager.
    ///
    /// Returns `AlreadyExists` if a sandbox with the same ID is already tracked.
    /// Returns `LimitReached` if the max sandbox count would be exceeded.
    pub fn insert(&self, sandbox: Sandbox) -> Result<SandboxHandle, SandboxError> {
        if self.max_sandboxes > 0 && self.sandboxes.len() >= self.max_sandboxes {
            return Err(SandboxError::LimitReached(self.max_sandboxes));
        }

        let id = sandbox.id.clone();
        let handle = Arc::new(Mutex::new(sandbox));

        // Use entry API to avoid TOCTOU race
        use dashmap::mapref::entry::Entry;
        match self.sandboxes.entry(id.clone()) {
            Entry::Occupied(_) => Err(SandboxError::AlreadyExists(id)),
            Entry::Vacant(v) => {
                v.insert(Arc::clone(&handle));
                info!(sandbox_id = %id, "sandbox registered");
                Ok(handle)
            }
        }
    }

    /// Get a handle to an existing sandbox.
    pub fn get(&self, id: &str) -> Result<SandboxHandle, SandboxError> {
        self.sandboxes
            .get(id)
            .map(|entry| Arc::clone(entry.value()))
            .ok_or_else(|| SandboxError::NotFound(id.to_string()))
    }

    /// Remove a sandbox from the manager. Returns the handle for final cleanup.
    ///
    /// The caller is responsible for holding the sandbox mutex during destruction
    /// to prevent concurrent access during teardown.
    pub fn remove(&self, id: &str) -> Result<SandboxHandle, SandboxError> {
        self.sandboxes
            .remove(id)
            .map(|(_, handle)| handle)
            .ok_or_else(|| SandboxError::NotFound(id.to_string()))
    }

    /// List all sandbox IDs.
    pub fn list_ids(&self) -> Vec<String> {
        self.sandboxes.iter().map(|e| e.key().clone()).collect()
    }

    /// Number of currently tracked sandboxes.
    pub fn len(&self) -> usize {
        self.sandboxes.len()
    }

    /// Whether the manager has no sandboxes.
    pub fn is_empty(&self) -> bool {
        self.sandboxes.is_empty()
    }

    /// Check if a sandbox exists.
    pub fn contains(&self, id: &str) -> bool {
        self.sandboxes.contains_key(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sandbox::meta::SandboxMetadata;
    use crate::sandbox::SandboxState;
    use chrono::Utc;
    use std::path::PathBuf;

    fn make_sandbox(id: &str) -> Sandbox {
        Sandbox {
            id: id.into(),
            state: SandboxState::Creating,
            dir: PathBuf::from(format!("/data/sandboxes/{}", id)),
            metadata: SandboxMetadata {
                owner: "test".into(),
                task: "unit test".into(),
                layers: vec!["000-base-alpine".into()],
                created: Utc::now(),
                last_active: Utc::now(),
                cpu: 1.0,
                memory_mb: 512,
                max_lifetime_s: 0,
                allow_net: None,
                active_snapshot: None,
                netns_index: None,
            },
            cgroup: None,
            netns: None,
            mounts: None,
        }
    }

    #[test]
    fn test_insert_and_get() {
        let mgr = SandboxManager::new(0);
        let sb = make_sandbox("test-1");

        let handle = mgr.insert(sb).unwrap();
        assert_eq!(mgr.len(), 1);

        let retrieved = mgr.get("test-1").unwrap();
        assert!(Arc::ptr_eq(&handle, &retrieved));
    }

    #[test]
    fn test_insert_duplicate_fails() {
        let mgr = SandboxManager::new(0);

        mgr.insert(make_sandbox("dup")).unwrap();
        let err = mgr.insert(make_sandbox("dup")).unwrap_err();
        assert!(matches!(err, SandboxError::AlreadyExists(_)));
    }

    #[test]
    fn test_limit_reached() {
        let mgr = SandboxManager::new(2);

        mgr.insert(make_sandbox("a")).unwrap();
        mgr.insert(make_sandbox("b")).unwrap();

        let err = mgr.insert(make_sandbox("c")).unwrap_err();
        assert!(matches!(err, SandboxError::LimitReached(2)));
    }

    #[test]
    fn test_unlimited_when_zero() {
        let mgr = SandboxManager::new(0);

        for i in 0..100 {
            mgr.insert(make_sandbox(&format!("sb-{}", i))).unwrap();
        }
        assert_eq!(mgr.len(), 100);
    }

    #[test]
    fn test_remove() {
        let mgr = SandboxManager::new(0);
        mgr.insert(make_sandbox("rm-me")).unwrap();

        assert!(mgr.contains("rm-me"));
        let handle = mgr.remove("rm-me").unwrap();
        assert!(!mgr.contains("rm-me"));
        assert_eq!(mgr.len(), 0);

        // Handle is still valid after removal
        drop(handle);
    }

    #[test]
    fn test_remove_not_found() {
        let mgr = SandboxManager::new(0);
        let err = mgr.remove("ghost").unwrap_err();
        assert!(matches!(err, SandboxError::NotFound(_)));
    }

    #[test]
    fn test_get_not_found() {
        let mgr = SandboxManager::new(0);
        let err = mgr.get("ghost").unwrap_err();
        assert!(matches!(err, SandboxError::NotFound(_)));
    }

    #[test]
    fn test_list_ids() {
        let mgr = SandboxManager::new(0);
        mgr.insert(make_sandbox("alpha")).unwrap();
        mgr.insert(make_sandbox("beta")).unwrap();
        mgr.insert(make_sandbox("gamma")).unwrap();

        let mut ids = mgr.list_ids();
        ids.sort();
        assert_eq!(ids, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn test_is_empty() {
        let mgr = SandboxManager::new(0);
        assert!(mgr.is_empty());

        mgr.insert(make_sandbox("x")).unwrap();
        assert!(!mgr.is_empty());

        mgr.remove("x").unwrap();
        assert!(mgr.is_empty());
    }

    #[test]
    fn test_insert_after_remove_reuses_slot() {
        let mgr = SandboxManager::new(1);

        mgr.insert(make_sandbox("a")).unwrap();
        mgr.remove("a").unwrap();

        // Should succeed since we freed the slot
        mgr.insert(make_sandbox("b")).unwrap();
        assert_eq!(mgr.len(), 1);
    }

    #[tokio::test]
    async fn test_concurrent_access() {
        let mgr = Arc::new(SandboxManager::new(0));
        mgr.insert(make_sandbox("shared")).unwrap();

        let handle = mgr.get("shared").unwrap();

        // Simulate concurrent state transition
        {
            let mut sb = handle.lock().await;
            sb.transition(SandboxState::Ready).unwrap();
            assert_eq!(sb.state, SandboxState::Ready);
        }

        // Another "task" can now access it
        {
            let sb = handle.lock().await;
            assert_eq!(sb.state, SandboxState::Ready);
        }
    }
}
