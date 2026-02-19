// NOTE: Backend abstraction (SandboxBackend trait) is deferred; lifecycle
// module handles chroot operations directly for now.
pub mod cgroup;
#[cfg(target_os = "linux")]
pub mod exec;
pub mod exec_types;
#[cfg(target_os = "linux")]
pub mod firecracker;
pub mod lifecycle;
pub mod manager;
pub mod meta;
#[cfg(target_os = "linux")]
pub mod mounts;
#[cfg(target_os = "linux")]
pub mod netns;
pub mod snapshot;

use std::path::PathBuf;
use std::sync::Arc;

use thiserror::Error;
use tokio::sync::Mutex;

use crate::sandbox::cgroup::CgroupHandle;
#[cfg(target_os = "linux")]
use crate::sandbox::mounts::SandboxMounts;
#[cfg(target_os = "linux")]
use crate::sandbox::netns::NetnsHandle;

/// Stub type for non-Linux platforms where network namespaces are unavailable.
#[cfg(not(target_os = "linux"))]
#[derive(Debug)]
pub struct NetnsHandle;

/// Stub type for non-Linux platforms where mount operations are unavailable.
#[cfg(not(target_os = "linux"))]
#[derive(Debug)]
pub struct SandboxMounts;

// ── Sandbox error ───────────────────────────────────────────────────

#[derive(Debug, Error)]
pub enum SandboxError {
    #[error("sandbox not found: {0}")]
    NotFound(String),

    #[error("invalid sandbox id: {0}")]
    InvalidId(String),

    #[error("invalid label: {0}")]
    InvalidLabel(String),

    #[error("invalid module name: {0}")]
    InvalidModule(String),

    #[error("sandbox already exists: {0}")]
    AlreadyExists(String),

    #[error("sandbox in wrong state: expected {expected}, got {actual}")]
    WrongState {
        expected: &'static str,
        actual: String,
    },

    #[error("mount failed: {0}")]
    Mount(String),

    #[error("unmount failed: {0}")]
    Unmount(String),

    #[error("cgroup error: {0}")]
    Cgroup(String),

    #[error("network namespace error: {0}")]
    Netns(String),

    #[error("exec failed: {0}")]
    Exec(String),

    #[error("snapshot error: {0}")]
    Snapshot(String),

    #[error("module not found: {0}")]
    ModuleNotFound(String),

    #[error("metadata error: {0}")]
    Meta(String),

    #[error("max sandboxes reached: {0}")]
    LimitReached(usize),

    #[error("timeout: {0}")]
    Timeout(String),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("json error: {0}")]
    Json(#[from] serde_json::Error),
}

// ── Sandbox state ───────────────────────────────────────────────────

/// Lifecycle states for a sandbox. Enforces valid transitions at runtime.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SandboxState {
    /// Mounts and namespace are being set up.
    Creating,
    /// Sandbox is ready for exec requests.
    Ready,
    /// A command is currently running inside the sandbox.
    Executing,
    /// A snapshot is being created from the upper layer.
    Snapshotting,
    /// Mounts and namespace are being torn down.
    Destroying,
}

impl std::fmt::Display for SandboxState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SandboxState::Creating => write!(f, "creating"),
            SandboxState::Ready => write!(f, "ready"),
            SandboxState::Executing => write!(f, "executing"),
            SandboxState::Snapshotting => write!(f, "snapshotting"),
            SandboxState::Destroying => write!(f, "destroying"),
        }
    }
}

// ── Sandbox struct ──────────────────────────────────────────────────

/// A running sandbox with its owned kernel resources.
///
/// Resources are cleaned up via Drop on the individual handle types
/// (CgroupHandle, NetnsHandle, mount handles). The Sandbox struct
/// owns them and drops them in reverse creation order when destroyed.
#[derive(Debug)]
pub struct Sandbox {
    /// Unique sandbox identifier (e.g., "dev", "ci-run-42").
    pub id: String,
    /// Current lifecycle state.
    pub state: SandboxState,
    /// Path to the sandbox directory: {sandboxes_dir}/{id}.
    pub dir: PathBuf,
    /// Creation metadata.
    pub metadata: meta::SandboxMetadata,
    /// Cgroup resource limits (None if cgroups unavailable).
    pub cgroup: Option<CgroupHandle>,
    /// Network namespace with veth pair and iptables rules.
    /// On non-Linux platforms, this uses a stub type and is always None.
    pub netns: Option<NetnsHandle>,
    /// All filesystem mounts for this sandbox (squashfs layers, tmpfs, overlay).
    /// None during creation before mounts are set up, or on non-Linux platforms.
    pub mounts: Option<SandboxMounts>,
}

impl Sandbox {
    /// Path to this sandbox's .meta/ directory.
    pub fn meta_dir(&self) -> PathBuf {
        self.dir.join(".meta")
    }

    /// Path to the merged overlayfs mount (chroot target).
    pub fn merged_path(&self) -> PathBuf {
        self.dir.join("merged")
    }

    /// Transition to a new state, returning an error if the transition is invalid.
    pub fn transition(&mut self, to: SandboxState) -> Result<(), SandboxError> {
        let valid = matches!(
            (&self.state, &to),
            (SandboxState::Creating, SandboxState::Ready)
                | (SandboxState::Ready, SandboxState::Executing)
                | (SandboxState::Ready, SandboxState::Snapshotting)
                | (SandboxState::Ready, SandboxState::Destroying)
                | (SandboxState::Executing, SandboxState::Ready)
                | (SandboxState::Snapshotting, SandboxState::Ready)
                | (SandboxState::Destroying, SandboxState::Ready) // recovery case
        );

        if valid {
            self.state = to;
            Ok(())
        } else {
            Err(SandboxError::WrongState {
                expected: "valid transition target",
                actual: format!("{} -> {}", self.state, to),
            })
        }
    }
}

/// Thread-safe handle to a sandbox, used in the manager's DashMap.
pub type SandboxHandle = Arc<Mutex<Sandbox>>;

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    #[test]
    fn test_sandbox_state_display() {
        assert_eq!(SandboxState::Creating.to_string(), "creating");
        assert_eq!(SandboxState::Ready.to_string(), "ready");
        assert_eq!(SandboxState::Executing.to_string(), "executing");
        assert_eq!(SandboxState::Snapshotting.to_string(), "snapshotting");
        assert_eq!(SandboxState::Destroying.to_string(), "destroying");
    }

    #[test]
    fn test_valid_transitions() {
        let meta = meta::SandboxMetadata {
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
        };

        let mut sb = Sandbox {
            id: "test".into(),
            state: SandboxState::Creating,
            dir: PathBuf::from("/tmp/test"),
            metadata: meta,
            cgroup: None,
            netns: None,
            mounts: None,
        };

        // Creating -> Ready
        assert!(sb.transition(SandboxState::Ready).is_ok());
        assert_eq!(sb.state, SandboxState::Ready);

        // Ready -> Executing
        assert!(sb.transition(SandboxState::Executing).is_ok());
        assert_eq!(sb.state, SandboxState::Executing);

        // Executing -> Ready
        assert!(sb.transition(SandboxState::Ready).is_ok());

        // Ready -> Snapshotting
        assert!(sb.transition(SandboxState::Snapshotting).is_ok());

        // Snapshotting -> Ready
        assert!(sb.transition(SandboxState::Ready).is_ok());

        // Ready -> Destroying
        assert!(sb.transition(SandboxState::Destroying).is_ok());
    }

    #[test]
    fn test_invalid_transitions() {
        let meta = meta::SandboxMetadata {
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
        };

        let mut sb = Sandbox {
            id: "test".into(),
            state: SandboxState::Creating,
            dir: PathBuf::from("/tmp/test"),
            metadata: meta,
            cgroup: None,
            netns: None,
            mounts: None,
        };

        // Creating -> Executing (invalid: must go through Ready)
        assert!(sb.transition(SandboxState::Executing).is_err());

        // Creating -> Destroying (invalid)
        assert!(sb.transition(SandboxState::Destroying).is_err());

        // Creating -> Snapshotting (invalid)
        assert!(sb.transition(SandboxState::Snapshotting).is_err());
    }

    #[test]
    fn test_sandbox_paths() {
        let meta = meta::SandboxMetadata {
            owner: "alice".into(),
            task: "dev".into(),
            layers: vec!["000-base-alpine".into()],
            created: Utc::now(),
            last_active: Utc::now(),
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: None,
            active_snapshot: None,
            netns_index: None,
        };

        let sb = Sandbox {
            id: "dev".into(),
            state: SandboxState::Ready,
            dir: PathBuf::from("/data/sandboxes/dev"),
            metadata: meta,
            cgroup: None,
            netns: None,
            mounts: None,
        };

        assert_eq!(sb.meta_dir(), PathBuf::from("/data/sandboxes/dev/.meta"));
        assert_eq!(
            sb.merged_path(),
            PathBuf::from("/data/sandboxes/dev/merged")
        );
    }

    #[test]
    fn test_sandbox_error_display() {
        let err = SandboxError::NotFound("abc".into());
        assert_eq!(err.to_string(), "sandbox not found: abc");

        let err = SandboxError::WrongState {
            expected: "Ready",
            actual: "Creating".into(),
        };
        assert!(err.to_string().contains("expected Ready"));

        let err = SandboxError::LimitReached(100);
        assert!(err.to_string().contains("100"));
    }

    #[test]
    fn test_sandbox_error_from_io() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file missing");
        let sb_err: SandboxError = io_err.into();
        assert!(matches!(sb_err, SandboxError::Io(_)));
    }

    #[test]
    fn test_sandbox_error_from_json() {
        let json_err = serde_json::from_str::<serde_json::Value>("invalid").unwrap_err();
        let sb_err: SandboxError = json_err.into();
        assert!(matches!(sb_err, SandboxError::Json(_)));
    }
}
