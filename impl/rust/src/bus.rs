//! IPC bus client for communicating with the sq-sync sidecar.
//!
//! The sandbox process writes files to local disk and sends a small JSON
//! notification over a Unix domain socket. The sidecar handles all S3
//! operations, caching, and retries. The sandbox never blocks on S3.
//!
//! Protocol: newline-delimited JSON over `ipc:///data/.sq-bus.sock`.
//! Messages are fire-and-forget; if the sidecar is down, messages are
//! silently dropped (the sidecar drains a file-based spool on restart).

use std::io;
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};

use serde::Serialize;
use tracing::{debug, warn};

/// Handle to the IPC bus.
///
/// `Clone` is cheap — each send opens a new short-lived connection.
#[derive(Debug, Clone)]
pub struct Bus {
    sock_path: PathBuf,
}

/// A message sent to the sidecar.
#[derive(Debug, Serialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum BusMessage {
    /// Push a local file to S3.
    Push { path: String, key: String },
    /// Push a sandbox manifest to S3.
    PushManifest { id: String, path: String },
    /// Trigger bidirectional module sync.
    SyncModules,
    /// Trigger bidirectional snapshot sync for a sandbox.
    SyncSnapshots { id: String },
    /// Cache a module file locally.
    CacheModule { path: String },
    /// Shut down the sidecar.
    Shutdown,
}

impl Bus {
    /// Create a new bus client.
    ///
    /// Does not connect immediately — each `send` opens a fresh connection.
    pub fn new(sock_path: PathBuf) -> Self {
        Self { sock_path }
    }

    /// Create a bus from the standard data directory.
    pub fn from_data_dir(data_dir: &Path) -> Self {
        let sock_path = std::env::var("SQUASH_BUS_SOCK")
            .map(PathBuf::from)
            .unwrap_or_else(|_| data_dir.join(".sq-bus.sock"));
        Self::new(sock_path)
    }

    /// Send a message to the sidecar. Fire-and-forget.
    ///
    /// Returns Ok(()) if the message was sent, Err if the socket is
    /// unavailable (sidecar not running). The caller should not retry —
    /// the sidecar will drain a file-based spool on restart.
    pub fn send(&self, msg: &BusMessage) -> io::Result<()> {
        let json = serde_json::to_string(msg)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let stream = UnixStream::connect(&self.sock_path)?;
        use std::io::Write;
        let mut writer = io::BufWriter::new(stream);
        writeln!(writer, "{}", json)?;
        writer.flush()?;

        debug!(msg = %json, "bus: sent");
        Ok(())
    }

    /// Send a message, logging a warning on failure (never panics).
    pub fn send_or_warn(&self, msg: &BusMessage) {
        if let Err(e) = self.send(msg) {
            warn!(error = %e, "bus: sidecar unreachable (message dropped)");
        }
    }

    /// Convenience: notify sidecar to push a snapshot to S3.
    pub fn notify_push(&self, local_path: &Path, s3_key: &str) {
        self.send_or_warn(&BusMessage::Push {
            path: local_path.to_string_lossy().to_string(),
            key: s3_key.to_string(),
        });
    }

    /// Convenience: notify sidecar to cache a module.
    pub fn notify_cache_module(&self, module_path: &Path) {
        self.send_or_warn(&BusMessage::CacheModule {
            path: module_path.to_string_lossy().to_string(),
        });
    }

    /// Check if the sidecar bus socket exists (does not connect).
    pub fn is_available(&self) -> bool {
        self.sock_path.exists()
    }
}

/// Spawn the sidecar as a background process if not already running.
///
/// This is called from main.rs during startup. The sidecar handles its
/// own lifecycle (PID file, cleanup).
pub fn spawn_sidecar(data_dir: &Path) -> io::Result<()> {
    let sock_path = std::env::var("SQUASH_BUS_SOCK")
        .map(PathBuf::from)
        .unwrap_or_else(|_| data_dir.join(".sq-bus.sock"));

    if sock_path.exists() {
        debug!("sidecar already running");
        return Ok(());
    }

    let status = std::process::Command::new("sq-sync")
        .arg("--daemon")
        .status()?;

    if status.success() {
        debug!("sidecar started");
    } else {
        warn!("sidecar failed to start (exit {})", status);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bus_message_push_serialization() {
        let msg = BusMessage::Push {
            path: "/data/snap.squashfs".to_string(),
            key: "snapshots/sb1/snap.squashfs".to_string(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains("\"op\":\"push\""));
        assert!(json.contains("\"path\":\"/data/snap.squashfs\""));
        assert!(json.contains("\"key\":\"snapshots/sb1/snap.squashfs\""));
    }

    #[test]
    fn test_bus_message_sync_modules_serialization() {
        let msg = BusMessage::SyncModules;
        let json = serde_json::to_string(&msg).unwrap();
        assert_eq!(json, "{\"op\":\"sync_modules\"}");
    }

    #[test]
    fn test_bus_message_push_manifest_serialization() {
        let msg = BusMessage::PushManifest {
            id: "sb-1".to_string(),
            path: "/tmp/manifest.json".to_string(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains("\"op\":\"push_manifest\""));
        assert!(json.contains("\"id\":\"sb-1\""));
    }

    #[test]
    fn test_bus_message_shutdown_serialization() {
        let msg = BusMessage::Shutdown;
        let json = serde_json::to_string(&msg).unwrap();
        assert_eq!(json, "{\"op\":\"shutdown\"}");
    }

    #[test]
    fn test_bus_from_data_dir() {
        let bus = Bus::from_data_dir(Path::new("/data"));
        assert!(bus.sock_path.to_str().unwrap().contains("sq-bus.sock"));
    }

    #[test]
    fn test_bus_send_to_nonexistent_socket() {
        let bus = Bus::new(PathBuf::from("/tmp/nonexistent-sq-test.sock"));
        let msg = BusMessage::SyncModules;
        let result = bus.send(&msg);
        assert!(result.is_err()); // socket doesn't exist — that's expected
    }

    #[test]
    fn test_bus_send_or_warn_doesnt_panic() {
        let bus = Bus::new(PathBuf::from("/tmp/nonexistent-sq-test.sock"));
        bus.send_or_warn(&BusMessage::SyncModules); // should not panic
    }

    #[test]
    fn test_bus_is_available_nonexistent() {
        let bus = Bus::new(PathBuf::from("/tmp/nonexistent-sq-test.sock"));
        assert!(!bus.is_available());
    }
}
