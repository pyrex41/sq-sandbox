//! SandboxBackend trait and implementations.
//!
//! Defines the interface for sandbox isolation mechanisms. Currently two
//! implementations exist:
//! - `ChrootBackend`: Uses chroot + overlayfs + namespaces (production, Linux-only)
//! - `FirecrackerBackend`: Uses Firecracker microVMs (stubbed for future)

use std::path::Path;

use crate::sandbox::exec_types::{ExecRequest, ExecResult};
use crate::sandbox::SandboxError;

/// Backend-agnostic interface for sandbox operations.
///
/// Each backend must implement the low-level mechanisms for isolation,
/// execution, and filesystem management. The lifecycle module calls
/// these methods after handling state machine transitions and metadata.
pub trait SandboxBackend: Send + Sync + std::fmt::Debug {
    /// Prepare the execution environment for a sandbox.
    ///
    /// For chroot: mount squashfs layers, set up overlay, create namespaces.
    /// For Firecracker: boot a microVM with the specified rootfs.
    fn setup(&self, sandbox_dir: &Path, merged_path: &Path) -> Result<(), SandboxError>;

    /// Execute a command inside the sandbox.
    fn exec(&self, sandbox_dir: &Path, req: &ExecRequest) -> Result<ExecResult, SandboxError>;

    /// Tear down the execution environment.
    ///
    /// For chroot: unmount overlay and layers, remove cgroup, tear down netns.
    /// For Firecracker: stop the microVM.
    fn teardown(&self, sandbox_dir: &Path) -> Result<(), SandboxError>;

    /// Return the backend name for logging and API responses.
    fn name(&self) -> &'static str;
}

/// Chroot-based backend using overlayfs, namespaces, and cgroups.
///
/// This is the primary production backend. All mount/exec operations are
/// handled by the lifecycle module and low-level exec.rs/mounts.rs/netns.rs;
/// this struct is a marker type that the lifecycle module dispatches on.
#[derive(Debug, Clone)]
pub struct ChrootBackend;

impl SandboxBackend for ChrootBackend {
    fn setup(&self, _sandbox_dir: &Path, _merged_path: &Path) -> Result<(), SandboxError> {
        // Chroot setup is handled directly by lifecycle::create_sandbox which
        // orchestrates mounts, cgroups, and netns in the correct order.
        // This is intentionally a no-op — the trait method exists so that
        // FirecrackerBackend can implement its own setup.
        Ok(())
    }

    fn exec(&self, _sandbox_dir: &Path, _req: &ExecRequest) -> Result<ExecResult, SandboxError> {
        // Chroot exec is handled by lifecycle::sandbox_exec which coordinates
        // the state machine and calls exec::exec_in_sandbox directly.
        Err(SandboxError::Exec(
            "ChrootBackend::exec should not be called directly; use lifecycle::sandbox_exec"
                .to_string(),
        ))
    }

    fn teardown(&self, _sandbox_dir: &Path) -> Result<(), SandboxError> {
        // Chroot teardown is handled by lifecycle::destroy_sandbox which
        // drops mounts, cgroup, and netns handles in reverse order.
        Ok(())
    }

    fn name(&self) -> &'static str {
        "chroot"
    }
}

/// Firecracker microVM backend (stubbed for future implementation).
///
/// When complete, this will boot a Firecracker VM with the sandbox rootfs
/// as a block device and communicate with sq-guest-agent via vsock.
#[derive(Debug, Clone)]
pub struct FirecrackerBackend;

impl SandboxBackend for FirecrackerBackend {
    fn setup(&self, _sandbox_dir: &Path, _merged_path: &Path) -> Result<(), SandboxError> {
        Err(SandboxError::Exec(
            "Firecracker backend not yet implemented".to_string(),
        ))
    }

    fn exec(&self, _sandbox_dir: &Path, _req: &ExecRequest) -> Result<ExecResult, SandboxError> {
        Err(SandboxError::Exec(
            "Firecracker backend not yet implemented".to_string(),
        ))
    }

    fn teardown(&self, _sandbox_dir: &Path) -> Result<(), SandboxError> {
        Err(SandboxError::Exec(
            "Firecracker backend not yet implemented".to_string(),
        ))
    }

    fn name(&self) -> &'static str {
        "firecracker"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chroot_backend_name() {
        let backend = ChrootBackend;
        assert_eq!(backend.name(), "chroot");
    }

    #[test]
    fn test_firecracker_backend_name() {
        let backend = FirecrackerBackend;
        assert_eq!(backend.name(), "firecracker");
    }

    #[test]
    fn test_firecracker_backend_returns_not_implemented() {
        let backend = FirecrackerBackend;
        let path = Path::new("/tmp/test");
        assert!(backend.setup(path, path).is_err());
        assert!(backend.teardown(path).is_err());
    }

    #[test]
    fn test_chroot_backend_setup_is_noop() {
        let backend = ChrootBackend;
        let path = Path::new("/tmp/test");
        assert!(backend.setup(path, path).is_ok());
        assert!(backend.teardown(path).is_ok());
    }

    #[test]
    fn test_backend_trait_object() {
        // Verify the trait is object-safe
        let backend: Box<dyn SandboxBackend> = Box::new(ChrootBackend);
        assert_eq!(backend.name(), "chroot");
    }
}
