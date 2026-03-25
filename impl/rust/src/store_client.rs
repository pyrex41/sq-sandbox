//! Request-response client for the sq-store sidecar.
//!
//! Communicates with the Irmin-backed content-addressed snapshot store
//! over a Unix domain socket. Unlike the fire-and-forget bus client,
//! this client sends a JSON request and reads a JSON response.
//!
//! Protocol: send one JSON line, read one JSON line back.

use std::io::{self, BufRead, Write};
use std::net::Shutdown;
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::time::Duration;

use serde::{Deserialize, Serialize};
use tracing::{debug, warn};

/// Client for the sq-store sidecar.
#[derive(Debug, Clone)]
pub struct StoreClient {
    sock_path: PathBuf,
}

/// Request sent to sq-store.
#[derive(Debug, Serialize)]
#[serde(tag = "op", rename_all = "snake_case")]
enum StoreRequest {
    Snapshot {
        sandbox_id: String,
        label: String,
        upper_data: String,
    },
    Restore {
        sandbox_id: String,
        label: String,
        upper_data: String,
    },
    Fork {
        source_id: String,
        source_label: String,
        target_id: String,
    },
    Diff {
        sandbox_id: String,
        from: String,
        to: String,
    },
    History {
        sandbox_id: String,
    },
}

/// Stats returned from a snapshot operation.
#[derive(Debug, Deserialize)]
pub struct SnapshotStats {
    pub files: u64,
    pub blobs_new: u64,
    pub blobs_reused: u64,
}

/// Response from sq-store.
#[derive(Debug, Deserialize)]
pub struct StoreResponse {
    pub ok: bool,
    #[serde(default)]
    pub error: Option<String>,
    #[serde(default)]
    pub label: Option<String>,
    #[serde(default)]
    pub size: Option<u64>,
    #[serde(default)]
    pub commit: Option<String>,
    #[serde(default)]
    pub stats: Option<SnapshotStats>,
    #[serde(default)]
    pub files_written: Option<u64>,
    #[serde(default)]
    pub files_deleted: Option<u64>,
    #[serde(default)]
    pub added: Option<Vec<String>>,
    #[serde(default)]
    pub modified: Option<Vec<String>>,
    #[serde(default)]
    pub deleted: Option<Vec<String>>,
    #[serde(default)]
    pub snapshots: Option<Vec<SnapshotHistoryEntry>>,
}

/// A single entry in snapshot history.
#[derive(Debug, Deserialize)]
pub struct SnapshotHistoryEntry {
    pub label: String,
    pub created: String,
    pub parent: Option<String>,
    pub size: u64,
}

impl StoreClient {
    /// Create a new store client.
    pub fn new(sock_path: PathBuf) -> Self {
        Self { sock_path }
    }

    /// Create from config.
    pub fn from_config(config: &crate::config::Config) -> Self {
        Self::new(config.store_sock_path())
    }

    /// Check if the store sidecar socket exists.
    pub fn is_available(&self) -> bool {
        self.sock_path.exists()
    }

    /// Send a request and read the response.
    fn request(&self, req: &StoreRequest) -> io::Result<StoreResponse> {
        let json = serde_json::to_string(req)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        debug!(msg = %json, "store: sending request");

        let stream = UnixStream::connect(&self.sock_path)?;
        stream.set_read_timeout(Some(Duration::from_secs(300)))?;
        stream.set_write_timeout(Some(Duration::from_secs(10)))?;

        // Send request
        {
            let mut writer = io::BufWriter::new(&stream);
            writeln!(writer, "{}", json)?;
            writer.flush()?;
        }

        // Signal end of request
        stream.shutdown(Shutdown::Write)?;

        // Read response
        let mut reader = io::BufReader::new(&stream);
        let mut response_line = String::new();
        reader.read_line(&mut response_line)?;

        debug!(msg = %response_line.trim(), "store: received response");

        let resp: StoreResponse = serde_json::from_str(response_line.trim())
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        if !resp.ok {
            let err_msg = resp.error.as_deref().unwrap_or("unknown error");
            return Err(io::Error::new(io::ErrorKind::Other, err_msg.to_string()));
        }

        Ok(resp)
    }

    /// Create a snapshot of a sandbox's upper/data directory.
    pub fn snapshot(
        &self,
        sandbox_id: &str,
        label: &str,
        upper_data: &Path,
    ) -> io::Result<StoreResponse> {
        self.request(&StoreRequest::Snapshot {
            sandbox_id: sandbox_id.to_string(),
            label: label.to_string(),
            upper_data: upper_data.to_string_lossy().to_string(),
        })
    }

    /// Restore a snapshot by materializing files into upper/data.
    pub fn restore(
        &self,
        sandbox_id: &str,
        label: &str,
        upper_data: &Path,
    ) -> io::Result<StoreResponse> {
        self.request(&StoreRequest::Restore {
            sandbox_id: sandbox_id.to_string(),
            label: label.to_string(),
            upper_data: upper_data.to_string_lossy().to_string(),
        })
    }

    /// Fork a sandbox's snapshot history to a new sandbox ID (O(1)).
    pub fn fork(
        &self,
        source_id: &str,
        source_label: &str,
        target_id: &str,
    ) -> io::Result<StoreResponse> {
        self.request(&StoreRequest::Fork {
            source_id: source_id.to_string(),
            source_label: source_label.to_string(),
            target_id: target_id.to_string(),
        })
    }

    /// Diff two snapshots, returning added/modified/deleted file lists.
    pub fn diff(
        &self,
        sandbox_id: &str,
        from: &str,
        to: &str,
    ) -> io::Result<StoreResponse> {
        self.request(&StoreRequest::Diff {
            sandbox_id: sandbox_id.to_string(),
            from: from.to_string(),
            to: to.to_string(),
        })
    }

    /// Get snapshot history for a sandbox.
    pub fn history(&self, sandbox_id: &str) -> io::Result<StoreResponse> {
        self.request(&StoreRequest::History {
            sandbox_id: sandbox_id.to_string(),
        })
    }

    /// Convenience: snapshot or warn (never panics).
    pub fn snapshot_or_warn(
        &self,
        sandbox_id: &str,
        label: &str,
        upper_data: &Path,
    ) -> Option<StoreResponse> {
        match self.snapshot(sandbox_id, label, upper_data) {
            Ok(resp) => Some(resp),
            Err(e) => {
                warn!(error = %e, "store: snapshot failed");
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_store_request_snapshot_serialization() {
        let req = StoreRequest::Snapshot {
            sandbox_id: "sb-1".to_string(),
            label: "cp1".to_string(),
            upper_data: "/data/sandboxes/sb-1/upper/data".to_string(),
        };
        let json = serde_json::to_string(&req).unwrap();
        assert!(json.contains("\"op\":\"snapshot\""));
        assert!(json.contains("\"sandbox_id\":\"sb-1\""));
        assert!(json.contains("\"label\":\"cp1\""));
    }

    #[test]
    fn test_store_request_fork_serialization() {
        let req = StoreRequest::Fork {
            source_id: "sb-1".to_string(),
            source_label: "cp1".to_string(),
            target_id: "sb-2".to_string(),
        };
        let json = serde_json::to_string(&req).unwrap();
        assert!(json.contains("\"op\":\"fork\""));
        assert!(json.contains("\"source_id\":\"sb-1\""));
        assert!(json.contains("\"target_id\":\"sb-2\""));
    }

    #[test]
    fn test_store_response_deserialization() {
        let json = r#"{"ok":true,"label":"cp1","size":12345,"commit":"abc123","stats":{"files":42,"blobs_new":5,"blobs_reused":37}}"#;
        let resp: StoreResponse = serde_json::from_str(json).unwrap();
        assert!(resp.ok);
        assert_eq!(resp.label.as_deref(), Some("cp1"));
        assert_eq!(resp.size, Some(12345));
        assert_eq!(resp.commit.as_deref(), Some("abc123"));
        let stats = resp.stats.unwrap();
        assert_eq!(stats.files, 42);
        assert_eq!(stats.blobs_new, 5);
        assert_eq!(stats.blobs_reused, 37);
    }

    #[test]
    fn test_store_response_error() {
        let json = r#"{"ok":false,"error":"not found"}"#;
        let resp: StoreResponse = serde_json::from_str(json).unwrap();
        assert!(!resp.ok);
        assert_eq!(resp.error.as_deref(), Some("not found"));
    }

    #[test]
    fn test_store_client_unavailable() {
        let client = StoreClient::new(PathBuf::from("/tmp/nonexistent-sq-store-test.sock"));
        assert!(!client.is_available());
        let result = client.snapshot("sb-1", "cp1", Path::new("/tmp"));
        assert!(result.is_err());
    }

    #[test]
    fn test_store_response_diff() {
        let json = r#"{"ok":true,"added":["a.py"],"modified":["b.py"],"deleted":["c.py"]}"#;
        let resp: StoreResponse = serde_json::from_str(json).unwrap();
        assert!(resp.ok);
        assert_eq!(resp.added.as_deref(), Some(&["a.py".to_string()][..]));
        assert_eq!(resp.modified.as_deref(), Some(&["b.py".to_string()][..]));
        assert_eq!(resp.deleted.as_deref(), Some(&["c.py".to_string()][..]));
    }

    #[test]
    fn test_store_response_history() {
        let json = r#"{"ok":true,"snapshots":[{"label":"cp1","created":"2026-03-25","parent":null,"size":1234}]}"#;
        let resp: StoreResponse = serde_json::from_str(json).unwrap();
        assert!(resp.ok);
        let snaps = resp.snapshots.unwrap();
        assert_eq!(snaps.len(), 1);
        assert_eq!(snaps[0].label, "cp1");
        assert!(snaps[0].parent.is_none());
    }
}
