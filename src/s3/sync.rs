//! Bidirectional S3 sync for modules and snapshots, plus ephemeral mode helpers.
//!
//! Matches the shell implementation in `bin/sq-s3` (sync_modules, sync_snapshots)
//! and `cgi-bin/common.sh` (ephemeral manifest push/pull, auto-snapshot, auto-restore).

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use tokio::fs;
use tracing::{debug, info, warn};

use super::S3Store;

/// Manifest stored in S3 for ephemeral mode sandboxes.
///
/// Captures enough metadata to recreate a sandbox on a different host.
/// Stored at `sandboxes/{id}/manifest.json`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub id: String,
    /// Comma-separated layer names (matches .meta/layers format).
    pub layers: String,
    pub owner: String,
    pub task: String,
    pub cpu: f64,
    pub memory_mb: u64,
    pub max_lifetime_s: u64,
    pub allow_net: Vec<String>,
    pub latest_snapshot: String,
}

/// Result of a sync operation.
#[derive(Debug, Default)]
pub struct SyncResult {
    pub pushed: Vec<String>,
    pub pulled: Vec<String>,
}

impl S3Store {
    /// Bidirectional sync of modules between local disk and S3.
    ///
    /// Matches `sync_modules()` in `bin/sq-s3:312-334`:
    /// 1. Push local-only `.squashfs` files to `modules/{name}`
    /// 2. Pull remote-only modules to local `modules/` directory
    pub async fn sync_modules(&self, modules_dir: &Path) -> Result<SyncResult, super::S3Error> {
        let mut result = SyncResult::default();

        // Collect local module names
        let local_names = list_squashfs_files(modules_dir).await;
        let local_set: HashSet<&str> = local_names.iter().map(|s| s.as_str()).collect();

        // Push local-only modules
        for name in &local_names {
            let key = format!("modules/{}", name);
            match self.exists(&key).await {
                Ok(true) => {}
                Ok(false) => {
                    let local_path = modules_dir.join(name);
                    info!("sync_modules: push (local-only): {}", name);
                    self.push(&local_path, &key).await?;
                    result.pushed.push(name.clone());
                }
                Err(e) => {
                    warn!("sync_modules: exists check failed for {}: {}", key, e);
                }
            }
        }

        // Pull remote-only modules
        let remote_keys = self.list("modules/").await?;
        for key in &remote_keys {
            let name = match key.strip_prefix("modules/") {
                Some(n) if !n.is_empty() && n.ends_with(".squashfs") => n,
                _ => continue,
            };
            if local_set.contains(name) {
                continue;
            }
            let local_path = modules_dir.join(name);
            info!("sync_modules: pull (remote-only): {}", name);
            self.pull(key, &local_path).await?;
            result.pulled.push(name.to_string());
        }

        debug!(
            "sync_modules: pushed={}, pulled={}",
            result.pushed.len(),
            result.pulled.len()
        );
        Ok(result)
    }

    /// Bidirectional sync of snapshots for a specific sandbox.
    ///
    /// Matches `sync_snapshots()` in `bin/sq-s3:336-363`:
    /// 1. Push local-only `.squashfs` snapshots to `sandboxes/{id}/snapshots/{name}`
    /// 2. Pull remote-only snapshots to local `sandboxes/{id}/snapshots/` directory
    pub async fn sync_snapshots(
        &self,
        sandbox_id: &str,
        sandboxes_dir: &Path,
    ) -> Result<SyncResult, super::S3Error> {
        let mut result = SyncResult::default();
        let snap_dir = sandboxes_dir.join(sandbox_id).join("snapshots");
        let s3_prefix = format!("sandboxes/{}/snapshots/", sandbox_id);

        // Collect local snapshot names
        let local_names = list_squashfs_files(&snap_dir).await;
        let local_set: HashSet<&str> = local_names.iter().map(|s| s.as_str()).collect();

        // Push local-only snapshots
        for name in &local_names {
            let key = format!("{}{}", s3_prefix, name);
            match self.exists(&key).await {
                Ok(true) => {}
                Ok(false) => {
                    let local_path = snap_dir.join(name);
                    info!("sync_snapshots({}): push (local-only): {}", sandbox_id, name);
                    self.push(&local_path, &key).await?;
                    result.pushed.push(name.clone());
                }
                Err(e) => {
                    warn!(
                        "sync_snapshots({}): exists check failed for {}: {}",
                        sandbox_id, key, e
                    );
                }
            }
        }

        // Pull remote-only snapshots
        fs::create_dir_all(&snap_dir).await.map_err(super::S3Error::Io)?;
        let remote_keys = self.list(&s3_prefix).await?;
        for key in &remote_keys {
            let name = match key.strip_prefix(&s3_prefix) {
                Some(n) if !n.is_empty() && n.ends_with(".squashfs") => n,
                _ => continue,
            };
            if local_set.contains(name) {
                continue;
            }
            let local_path = snap_dir.join(name);
            info!("sync_snapshots({}): pull (remote-only): {}", sandbox_id, name);
            self.pull(key, &local_path).await?;
            result.pulled.push(name.to_string());
        }

        debug!(
            "sync_snapshots({}): pushed={}, pulled={}",
            sandbox_id,
            result.pushed.len(),
            result.pulled.len()
        );
        Ok(result)
    }

    /// Push a sandbox manifest to S3 for ephemeral mode.
    ///
    /// Matches `_s3_push_manifest()` in `cgi-bin/common.sh:35-66`.
    pub async fn push_manifest(
        &self,
        manifest: &Manifest,
    ) -> Result<(), super::S3Error> {
        let key = format!("sandboxes/{}/manifest.json", manifest.id);
        let json = serde_json::to_vec(manifest)
            .map_err(|e| super::S3Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e)))?;

        let tmp = std::env::temp_dir().join(format!("sq-manifest-{}.json", manifest.id));
        fs::write(&tmp, &json).await?;

        let result = self.push(&tmp, &key).await;
        let _ = fs::remove_file(&tmp).await;
        result
    }

    /// Pull a sandbox manifest from S3 for ephemeral mode.
    ///
    /// Matches `_s3_pull_manifest()` in `cgi-bin/common.sh:68-73`.
    /// Returns `None` if the manifest doesn't exist.
    pub async fn pull_manifest(
        &self,
        sandbox_id: &str,
    ) -> Result<Option<Manifest>, super::S3Error> {
        let key = format!("sandboxes/{}/manifest.json", sandbox_id);

        match self.exists(&key).await {
            Ok(false) => return Ok(None),
            Err(e) => {
                warn!("manifest exists check failed for {}: {}", sandbox_id, e);
                return Ok(None);
            }
            Ok(true) => {}
        }

        let tmp = std::env::temp_dir().join(format!("sq-manifest-pull-{}.json", sandbox_id));
        // Remove any stale file so pull doesn't short-circuit
        let _ = fs::remove_file(&tmp).await;

        self.pull(&key, &tmp).await?;

        let data = fs::read(&tmp).await?;
        let _ = fs::remove_file(&tmp).await;

        let manifest: Manifest = serde_json::from_slice(&data)
            .map_err(|e| super::S3Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e)))?;

        Ok(Some(manifest))
    }

    /// Find the latest snapshot for a sandbox in S3.
    ///
    /// Matches `_s3_latest_snapshot()` in `cgi-bin/common.sh:75-81`:
    /// Lists `sandboxes/{id}/snapshots/`, extracts labels, sorts, returns last.
    pub async fn latest_snapshot(
        &self,
        sandbox_id: &str,
    ) -> Result<Option<String>, super::S3Error> {
        let prefix = format!("sandboxes/{}/snapshots/", sandbox_id);
        let keys = self.list(&prefix).await?;

        let mut labels: Vec<String> = keys
            .iter()
            .filter_map(|key| {
                key.strip_prefix(&prefix)
                    .and_then(|name| name.strip_suffix(".squashfs"))
                    .filter(|label| !label.is_empty())
                    .map(|label| label.to_string())
            })
            .collect();

        labels.sort();
        Ok(labels.pop())
    }

    /// Stage a file to a temp location and push to S3 in the background.
    ///
    /// Matches `_ephemeral_stage_and_push()` in `cgi-bin/common.sh:83-88`:
    /// Copies file to /tmp, then spawns background push that cleans up after.
    pub fn ephemeral_stage_and_push(
        &self,
        local_path: PathBuf,
        key: String,
    ) -> tokio::task::JoinHandle<()> {
        let store = self.clone();
        tokio::spawn(async move {
            let tmp = std::env::temp_dir().join(format!(
                "sq-push-{}",
                local_path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
            ));

            // Copy to temp first so the original can be freed
            if let Err(e) = fs::copy(&local_path, &tmp).await {
                warn!("ephemeral_stage_and_push: copy failed: {}", e);
                return;
            }

            match store.push(&tmp, &key).await {
                Ok(()) => debug!("ephemeral_stage_and_push complete: {}", key),
                Err(e) => warn!("ephemeral_stage_and_push failed for {}: {}", key, e),
            }

            let _ = fs::remove_file(&tmp).await;
        })
    }
}

/// Build a Manifest from sandbox metadata on disk.
///
/// Reads the `.meta/` directory and combines with the latest S3 snapshot label.
pub fn manifest_from_metadata(
    meta: &crate::sandbox::meta::SandboxMetadata,
    sandbox_id: &str,
    latest_snapshot: &str,
) -> Manifest {
    Manifest {
        id: sandbox_id.to_string(),
        layers: meta.layers.join(","),
        owner: meta.owner.clone(),
        task: meta.task.clone(),
        cpu: meta.cpu,
        memory_mb: meta.memory_mb,
        max_lifetime_s: meta.max_lifetime_s,
        allow_net: meta.allow_net.clone().unwrap_or_default(),
        latest_snapshot: latest_snapshot.to_string(),
    }
}

/// List `*.squashfs` files in a directory, returning filenames.
async fn list_squashfs_files(dir: &Path) -> Vec<String> {
    let mut names = Vec::new();
    let mut entries = match fs::read_dir(dir).await {
        Ok(entries) => entries,
        Err(_) => return names,
    };
    while let Ok(Some(entry)) = entries.next_entry().await {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("squashfs") {
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                names.push(name.to_string());
            }
        }
    }
    names.sort();
    names
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manifest_serialization_roundtrip() {
        let manifest = Manifest {
            id: "test-sb".to_string(),
            layers: "000-base-alpine,100-python312".to_string(),
            owner: "alice".to_string(),
            task: "run tests".to_string(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 1800,
            allow_net: vec!["api.anthropic.com".to_string(), "pypi.org".to_string()],
            latest_snapshot: "snap-20260214-1200".to_string(),
        };

        let json = serde_json::to_string(&manifest).unwrap();
        let parsed: Manifest = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.id, "test-sb");
        assert_eq!(parsed.layers, "000-base-alpine,100-python312");
        assert_eq!(parsed.owner, "alice");
        assert_eq!(parsed.task, "run tests");
        assert_eq!(parsed.cpu, 2.0);
        assert_eq!(parsed.memory_mb, 1024);
        assert_eq!(parsed.max_lifetime_s, 1800);
        assert_eq!(parsed.allow_net, vec!["api.anthropic.com", "pypi.org"]);
        assert_eq!(parsed.latest_snapshot, "snap-20260214-1200");
    }

    #[test]
    fn test_manifest_empty_allow_net() {
        let manifest = Manifest {
            id: "test".to_string(),
            layers: "000-base-alpine".to_string(),
            owner: "bob".to_string(),
            task: "quick".to_string(),
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: vec![],
            latest_snapshot: String::new(),
        };

        let json = serde_json::to_string(&manifest).unwrap();
        assert!(json.contains("\"allow_net\":[]"));

        let parsed: Manifest = serde_json::from_str(&json).unwrap();
        assert!(parsed.allow_net.is_empty());
        assert!(parsed.latest_snapshot.is_empty());
    }

    #[test]
    fn test_manifest_from_metadata() {
        let meta = crate::sandbox::meta::SandboxMetadata {
            owner: "alice".into(),
            task: "dev".into(),
            layers: vec!["000-base-alpine".into(), "100-python312".into()],
            created: chrono::Utc::now(),
            last_active: chrono::Utc::now(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 1800,
            allow_net: Some(vec!["pypi.org".into()]),
            active_snapshot: None,
            netns_index: None,
        };

        let manifest = manifest_from_metadata(&meta, "my-sandbox", "snap-latest");

        assert_eq!(manifest.id, "my-sandbox");
        assert_eq!(manifest.layers, "000-base-alpine,100-python312");
        assert_eq!(manifest.owner, "alice");
        assert_eq!(manifest.cpu, 2.0);
        assert_eq!(manifest.allow_net, vec!["pypi.org"]);
        assert_eq!(manifest.latest_snapshot, "snap-latest");
    }

    #[test]
    fn test_manifest_from_metadata_no_allow_net() {
        let meta = crate::sandbox::meta::SandboxMetadata {
            owner: "bob".into(),
            task: "test".into(),
            layers: vec!["000-base-alpine".into()],
            created: chrono::Utc::now(),
            last_active: chrono::Utc::now(),
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: None,
            active_snapshot: None,
            netns_index: None,
        };

        let manifest = manifest_from_metadata(&meta, "sb-1", "");
        assert!(manifest.allow_net.is_empty());
    }

    #[tokio::test]
    async fn test_list_squashfs_files() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("000-base.squashfs"), b"a")
            .await
            .unwrap();
        fs::write(dir.path().join("100-python.squashfs"), b"b")
            .await
            .unwrap();
        fs::write(dir.path().join("not-a-module.txt"), b"c")
            .await
            .unwrap();
        fs::write(dir.path().join("200-node.squashfs"), b"d")
            .await
            .unwrap();

        let files = list_squashfs_files(dir.path()).await;
        assert_eq!(
            files,
            vec![
                "000-base.squashfs",
                "100-python.squashfs",
                "200-node.squashfs"
            ]
        );
    }

    #[tokio::test]
    async fn test_list_squashfs_files_empty_dir() {
        let dir = tempfile::tempdir().unwrap();
        let files = list_squashfs_files(dir.path()).await;
        assert!(files.is_empty());
    }

    #[tokio::test]
    async fn test_list_squashfs_files_nonexistent_dir() {
        let files = list_squashfs_files(Path::new("/nonexistent/dir")).await;
        assert!(files.is_empty());
    }

    #[test]
    fn test_sync_result_default() {
        let result = SyncResult::default();
        assert!(result.pushed.is_empty());
        assert!(result.pulled.is_empty());
    }
}
