//! Background reaper task — destroys sandboxes that exceed their max lifetime.
//!
//! Runs every 10 seconds, scanning all tracked sandboxes. A sandbox is reaped
//! when `max_lifetime_s > 0` and `now - created > max_lifetime_s`.

use std::sync::Arc;
use std::time::Duration;

use chrono::Utc;
use tracing::{info, warn};

use crate::config::Config;
use crate::s3::S3Store;
use crate::sandbox::lifecycle;
use crate::sandbox::manager::SandboxManager;
use crate::sandbox::SandboxState;

const REAPER_INTERVAL: Duration = Duration::from_secs(10);

/// Spawn the reaper as a tokio background task.
///
/// The task runs indefinitely, sleeping between scans. It is safe to call
/// this once at startup — the task will be cancelled when the runtime shuts down.
pub fn spawn(manager: Arc<SandboxManager>, config: Arc<Config>, s3: Option<Arc<S3Store>>) {
    tokio::spawn(async move {
        loop {
            reap_expired(&manager, &config, s3.as_deref()).await;
            tokio::time::sleep(REAPER_INTERVAL).await;
        }
    });
}

/// Single reap pass: collect expired sandbox IDs, then destroy each one.
async fn reap_expired(manager: &SandboxManager, config: &Config, s3: Option<&S3Store>) {
    let now = Utc::now();
    let mut expired_ids = Vec::new();

    for id in manager.list_ids() {
        let handle = match manager.get(&id) {
            Ok(h) => h,
            Err(_) => continue, // removed between list and get
        };

        let sb = handle.lock().await;
        if sb.state == SandboxState::Destroying {
            continue;
        }
        let max = sb.metadata.max_lifetime_s;
        if max == 0 {
            continue;
        }

        let age_secs = (now - sb.metadata.created).num_seconds();
        if age_secs > max as i64 {
            expired_ids.push((id.clone(), age_secs, max));
        }
    }

    for (id, age_secs, max) in expired_ids {
        info!(
            sandbox_id = %id,
            age_secs,
            max_lifetime_s = max,
            "reaper: destroying expired sandbox"
        );

        match lifecycle::destroy_sandbox(manager, config, &id, s3).await {
            Ok(_) => info!(sandbox_id = %id, "reaper: sandbox destroyed"),
            Err(e) => warn!(sandbox_id = %id, error = %e, "reaper: destroy failed"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::sandbox::meta::SandboxMetadata;
    use crate::sandbox::{Sandbox, SandboxState};
    use chrono::{Duration as ChronoDuration, Utc};
    use std::path::PathBuf;

    fn make_sandbox(id: &str, max_lifetime_s: u64, age_secs: i64) -> Sandbox {
        let created = Utc::now() - ChronoDuration::seconds(age_secs);
        Sandbox {
            id: id.into(),
            state: SandboxState::Ready,
            dir: PathBuf::from(format!("/data/sandboxes/{}", id)),
            metadata: SandboxMetadata {
                owner: "test".into(),
                task: "unit test".into(),
                layers: vec!["000-base-alpine".into()],
                created,
                last_active: Utc::now(),
                cpu: 1.0,
                memory_mb: 512,
                max_lifetime_s,
                allow_net: None,
                active_snapshot: None,
                netns_index: None,
            },
            cgroup: None,
            netns: None,
            mounts: None,
        }
    }

    #[tokio::test]
    async fn test_reap_expired_sandbox() {
        let mgr = SandboxManager::new(0);

        // Expired: max_lifetime_s=60, age=120s
        mgr.insert(make_sandbox("expired", 60, 120)).unwrap();

        // Not expired: max_lifetime_s=3600, age=10s
        mgr.insert(make_sandbox("fresh", 3600, 10)).unwrap();

        // No lifetime limit: max_lifetime_s=0, age=99999s
        mgr.insert(make_sandbox("immortal", 0, 99999)).unwrap();

        assert_eq!(mgr.len(), 3);

        let config = Config::default();
        reap_expired(&mgr, &config, None).await;

        assert_eq!(mgr.len(), 2);
        assert!(!mgr.contains("expired"));
        assert!(mgr.contains("fresh"));
        assert!(mgr.contains("immortal"));
    }

    #[tokio::test]
    async fn test_reap_skips_zero_lifetime() {
        let mgr = SandboxManager::new(0);
        mgr.insert(make_sandbox("no-limit", 0, 999999)).unwrap();

        let config = Config::default();
        reap_expired(&mgr, &config, None).await;

        assert_eq!(mgr.len(), 1);
        assert!(mgr.contains("no-limit"));
    }

    #[tokio::test]
    async fn test_reap_empty_manager() {
        let mgr = SandboxManager::new(0);
        let config = Config::default();
        reap_expired(&mgr, &config, None).await;
        assert_eq!(mgr.len(), 0);
    }

    #[tokio::test]
    async fn test_reap_skips_creating_state() {
        let mgr = SandboxManager::new(0);

        // Sandbox in Creating state — transition to Destroying is invalid
        let mut sb = make_sandbox("creating", 60, 120);
        sb.state = SandboxState::Creating;
        mgr.insert(sb).unwrap();

        let config = Config::default();
        reap_expired(&mgr, &config, None).await;

        // Should still be there (can't transition Creating -> Destroying)
        assert!(mgr.contains("creating"));
    }

    #[tokio::test]
    async fn test_reap_multiple_expired() {
        let mgr = SandboxManager::new(0);

        mgr.insert(make_sandbox("a", 10, 100)).unwrap();
        mgr.insert(make_sandbox("b", 20, 200)).unwrap();
        mgr.insert(make_sandbox("c", 30, 300)).unwrap();
        mgr.insert(make_sandbox("keeper", 3600, 5)).unwrap();

        let config = Config::default();
        reap_expired(&mgr, &config, None).await;

        assert_eq!(mgr.len(), 1);
        assert!(mgr.contains("keeper"));
    }
}
