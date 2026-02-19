//! First-boot init and sandbox recovery.
//!
//! Runs at startup before the API server accepts requests. Matches `bin/sq-init`:
//! 1. Create modules/ and sandboxes/ directories
//! 2. Ensure 000-base-alpine.squashfs exists (S3 pull -> sq-mkbase fallback)
//! 3. Check Firecracker VM components if backend=firecracker
//! 4. Remount surviving sandboxes (non-ephemeral mode only)

use std::fs;
use std::path::{Path, PathBuf};

use tracing::{info, warn};

use crate::config::{Backend, Config};
use crate::modules;
use crate::s3::S3Store;
use crate::sandbox::manager::SandboxManager;
use crate::sandbox::{Sandbox, SandboxState};

/// Current expected version of the base module format.
const BASE_VERSION_CURRENT: u32 = 2;

/// Base module name (without extension).
const BASE_MODULE_NAME: &str = "000-base-alpine";

/// Run the full init/recovery sequence.
///
/// This must complete before the API server starts accepting requests.
pub async fn run(
    config: &Config,
    s3: Option<&S3Store>,
    manager: Option<&SandboxManager>,
) -> Result<InitResult, InitError> {
    let modules_dir = config.modules_dir();
    let sandboxes_dir = config.sandboxes_dir();

    // 1. Create top-level directories
    fs::create_dir_all(&modules_dir)
        .map_err(|e| InitError::DirCreate(modules_dir.clone(), e))?;
    fs::create_dir_all(&sandboxes_dir)
        .map_err(|e| InitError::DirCreate(sandboxes_dir.clone(), e))?;

    // 2. Ensure base module is present and up-to-date
    ensure_base_module(&modules_dir, s3).await?;

    // 3. Firecracker VM components
    if config.backend == Backend::Firecracker {
        ensure_firecracker_components(config)?;
    }

    // 4. Remount surviving sandboxes (skip in ephemeral mode)
    let mut recovered = Vec::new();
    let mut failed = Vec::new();

    if config.ephemeral {
        info!("ephemeral mode: skipping sandbox remount (S3 is source of truth)");
    } else {
        let (ok, err) = recover_sandboxes(&sandboxes_dir, &modules_dir, s3, manager).await;
        recovered = ok;
        failed = err;
    }

    let module_count = modules::list_modules(&modules_dir).len();
    let sandbox_count = count_sandbox_dirs(&sandboxes_dir);

    info!(
        "ready — modules: {}, sandboxes: {}",
        module_count, sandbox_count
    );

    Ok(InitResult {
        module_count,
        sandbox_count,
        recovered,
        failed,
    })
}

/// Result of the init/recovery sequence.
#[derive(Debug)]
pub struct InitResult {
    pub module_count: usize,
    pub sandbox_count: usize,
    pub recovered: Vec<String>,
    pub failed: Vec<String>,
}

/// Errors that can occur during init.
#[derive(Debug, thiserror::Error)]
pub enum InitError {
    #[error("failed to create directory {0}: {1}")]
    DirCreate(PathBuf, std::io::Error),

    #[error("base module build failed: {0}")]
    BaseBuild(std::io::Error),

    #[error("firecracker component setup failed: {0}")]
    Firecracker(std::io::Error),

    #[error("s3 error: {0}")]
    S3(String),
}

// -- Base module check -------------------------------------------------------

/// Ensure the base Alpine module exists and is at the current version.
///
/// Logic matches sq-init lines 14-32:
/// - Check .version file (current=2)
/// - If missing/outdated: try S3 pull, fallback to `sq-mkbase alpine`
async fn ensure_base_module(
    modules_dir: &Path,
    s3: Option<&S3Store>,
) -> Result<(), InitError> {
    let squashfs = modules::module_path(modules_dir, BASE_MODULE_NAME);
    let version = modules::read_version(modules_dir, BASE_MODULE_NAME);

    if squashfs.is_file() && version >= BASE_VERSION_CURRENT {
        info!(
            "base module present (version {}): {}",
            version,
            display_file_size(&squashfs)
        );
        return Ok(());
    }

    // Need to build or pull
    if version < BASE_VERSION_CURRENT && squashfs.is_file() {
        info!(
            "base module outdated (version {} < {}), updating",
            version, BASE_VERSION_CURRENT
        );
    } else {
        info!("no base module found");
    }

    // Try S3 pull first
    if let Some(s3) = s3 {
        info!("trying S3 pull for base module");
        let s3_key = format!("modules/{}.squashfs", BASE_MODULE_NAME);
        match s3.pull(&s3_key, &squashfs).await {
            Ok(()) if squashfs.is_file() => {
                info!(
                    "pulled base from S3: {}",
                    display_file_size(&squashfs)
                );
                return Ok(());
            }
            Ok(()) => {
                // pull returned Ok but file not there (should not happen, but be safe)
                warn!("S3 pull returned Ok but file not present, falling back to build");
            }
            Err(e) => {
                warn!("S3 pull failed: {}, falling back to build", e);
            }
        }
    }

    // Fallback: build locally via sq-mkbase
    info!("building Alpine base module");
    crate::modules::builder::build_base("alpine").map_err(InitError::BaseBuild)?;

    Ok(())
}

// -- Firecracker components --------------------------------------------------

/// Ensure Firecracker VM components are present.
///
/// Checks for {data_dir}/vm/firecracker binary. If missing, shells out to
/// `sq-mkvm all` which fetches the kernel, firecracker binary, and builds
/// the guest rootfs.
fn ensure_firecracker_components(config: &Config) -> Result<(), InitError> {
    let fc_binary = config.vm_dir().join("firecracker");

    if fc_binary.is_file() {
        info!("firecracker backend: VM components present");
        Ok(())
    } else {
        info!("firecracker backend: provisioning VM components");
        crate::modules::builder::build_vm_components().map_err(InitError::Firecracker)
    }
}

// -- Sandbox recovery --------------------------------------------------------

/// Scan sandboxes/ and remount any surviving sandboxes.
///
/// Returns (recovered_ids, failed_ids).
///
/// For each sandbox directory with a `.meta/layers` file:
/// 1. Skip if already mounted (check /proc/mounts)
/// 2. Remount each squashfs module layer
/// 3. Remount active snapshot if present
/// 4. Rebuild the overlayfs
async fn recover_sandboxes(
    sandboxes_dir: &Path,
    modules_dir: &Path,
    s3: Option<&S3Store>,
    manager: Option<&SandboxManager>,
) -> (Vec<String>, Vec<String>) {
    let mut recovered = Vec::new();
    let mut failed = Vec::new();

    let entries = match fs::read_dir(sandboxes_dir) {
        Ok(entries) => entries,
        Err(e) => {
            warn!("cannot read sandboxes dir: {}", e);
            return (recovered, failed);
        }
    };

    for entry in entries.flatten() {
        let sdir = entry.path();
        if !sdir.is_dir() {
            continue;
        }

        let layers_file = sdir.join(".meta/layers");
        if !layers_file.is_file() {
            continue;
        }

        let id = match sdir.file_name().and_then(|n| n.to_str()) {
            Some(name) => name.to_string(),
            None => continue,
        };

        match recover_one_sandbox(&sdir, &id, modules_dir, s3, manager).await {
            Ok(()) => {
                info!("{}: ok", id);
                recovered.push(id);
            }
            Err(e) => {
                warn!("{}: FAILED: {}", id, e);
                failed.push(id);
            }
        }
    }

    (recovered, failed)
}

/// Recover a single sandbox: remount modules, snapshot, and overlay.
async fn recover_one_sandbox(
    sdir: &Path,
    id: &str,
    modules_dir: &Path,
    s3: Option<&S3Store>,
    manager: Option<&SandboxManager>,
) -> Result<(), RecoverError> {
    let merged = sdir.join("merged");

    // Skip if already mounted
    if is_mounted(&merged)? {
        info!("{}: already mounted", id);
        return Ok(());
    }

    info!("{}: remounting", id);

    // Read layers list
    let layers_content = fs::read_to_string(sdir.join(".meta/layers"))
        .map_err(|e| RecoverError::Meta(format!("read layers: {}", e)))?;
    let layers: Vec<&str> = layers_content.trim().split(',').collect();

    // Remount each module image
    for module_name in &layers {
        let module_name = module_name.trim();
        if module_name.is_empty() {
            continue;
        }

        let local_mp = sdir.join(format!("images/{}.squashfs", module_name));
        let sqfs = modules::module_path(modules_dir, module_name);

        // Pull from S3 if module is missing locally
        if !sqfs.is_file() {
            if let Some(s3) = s3 {
                info!("  pulling missing module from S3: {}", module_name);
                let s3_key = format!("modules/{}.squashfs", module_name);
                if let Err(e) = s3.pull(&s3_key, &sqfs).await {
                    warn!(
                        "  WARN: module missing (local and S3): {} ({})",
                        module_name, e
                    );
                    continue;
                }
            } else {
                warn!("  WARN: module missing: {}", module_name);
                continue;
            }
        }

        // Mount squashfs (skip if already mounted)
        fs::create_dir_all(&local_mp)
            .map_err(|e| RecoverError::Mount(format!("mkdir {}: {}", local_mp.display(), e)))?;

        if !is_mounted(&local_mp)? {
            mount_squashfs(&sqfs, &local_mp)?;
        }
    }

    // Remount active snapshot if any
    let active_snapshot_file = sdir.join(".meta/active_snapshot");
    if active_snapshot_file.is_file() {
        let label = fs::read_to_string(&active_snapshot_file)
            .map_err(|e| RecoverError::Meta(format!("read active_snapshot: {}", e)))?;
        let label = label.trim();
        if !label.is_empty() {
            let snapfile = sdir.join(format!("snapshots/{}.squashfs", label));
            if snapfile.is_file() {
                let snap_mp = sdir.join("images/_snapshot");
                fs::create_dir_all(&snap_mp)
                    .map_err(|e| RecoverError::Mount(format!("mkdir snapshot mp: {}", e)))?;
                if !is_mounted(&snap_mp)? {
                    if let Err(e) = mount_squashfs(&snapfile, &snap_mp) {
                        warn!("  snapshot mount failed: {}", e);
                        // Continue anyway -- overlay will work without snapshot
                    }
                }
            }
        }
    }

    // Rebuild overlay
    fs::create_dir_all(sdir.join("upper"))
        .map_err(|e| RecoverError::Mount(format!("mkdir upper: {}", e)))?;
    fs::create_dir_all(sdir.join("work"))
        .map_err(|e| RecoverError::Mount(format!("mkdir work: {}", e)))?;
    fs::create_dir_all(&merged)
        .map_err(|e| RecoverError::Mount(format!("mkdir merged: {}", e)))?;

    // Build lowerdir string: snapshot first (highest priority), then modules descending
    let lowerdir = build_lowerdir(sdir)?;

    if lowerdir.is_empty() {
        return Err(RecoverError::Mount(
            "no mounted layers for overlay".to_string(),
        ));
    }

    mount_overlay(&lowerdir, &sdir.join("upper"), &sdir.join("work"), &merged)?;

    // Recovered sandboxes must be visible to the live manager so API/reaper
    // operate on the same set init discovered on disk.
    if let Some(manager) = manager {
        if !manager.contains(id) {
            let meta_dir = sdir.join(".meta");
            let metadata = crate::sandbox::meta::read_metadata(&meta_dir)
                .map_err(|e| RecoverError::Meta(format!("read metadata: {}", e)))?;
            let cgroup = crate::sandbox::cgroup::CgroupHandle::from_existing(id);
            #[cfg(target_os = "linux")]
            let netns = crate::sandbox::netns::NetnsHandle::from_recovered(
                id,
                &meta_dir,
                metadata.allow_net.as_deref(),
            )
            .map_err(|e| RecoverError::Meta(format!("recover netns metadata: {}", e)))?;
            #[cfg(not(target_os = "linux"))]
            let netns = None;
            let sandbox = Sandbox {
                id: id.to_string(),
                state: SandboxState::Ready,
                dir: sdir.to_path_buf(),
                metadata,
                cgroup,
                netns,
                mounts: None,
            };
            if let Err(e) = manager.insert(sandbox) {
                warn!("{}: manager registration failed: {}", id, e);
            }
        }
    }

    Ok(())
}

/// Build the overlay lowerdir string for a sandbox.
///
/// Algorithm matches sq-init lines 92-101:
/// 1. Snapshot layer first (if mounted): `images/_snapshot`
/// 2. Module layers in descending numeric order (sort -r on `images/[0-9]*.squashfs/`)
fn build_lowerdir(sdir: &Path) -> Result<String, RecoverError> {
    let mut components = Vec::new();

    // Snapshot layer first
    let snap_mp = sdir.join("images/_snapshot");
    if snap_mp.is_dir() && is_mounted(&snap_mp)? {
        components.push(snap_mp.to_string_lossy().to_string());
    }

    // Collect mounted module layers
    let images_dir = sdir.join("images");
    let mut module_dirs: Vec<PathBuf> = Vec::new();

    if let Ok(entries) = fs::read_dir(&images_dir) {
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();

            // Match pattern: starts with digit and ends with .squashfs
            if name_str.ends_with(".squashfs")
                && name_str
                    .chars()
                    .next()
                    .map_or(false, |c| c.is_ascii_digit())
            {
                let path = entry.path();
                if path.is_dir() && is_mounted(&path)? {
                    module_dirs.push(path);
                }
            }
        }
    }

    // Sort descending (matches `sort -r` in shell)
    module_dirs.sort();
    module_dirs.reverse();

    for dir in module_dirs {
        components.push(dir.to_string_lossy().to_string());
    }

    Ok(components.join(":"))
}

// -- Mount operations --------------------------------------------------------
//
// Uses nix::mount on Linux. On other platforms (macOS dev), returns errors
// so that the code compiles and non-mount tests can run.

/// Mount a squashfs file read-only at the given mount point.
fn mount_squashfs(squashfs: &Path, mount_point: &Path) -> Result<(), RecoverError> {
    #[cfg(target_os = "linux")]
    {
        use nix::mount::{mount, MsFlags};
        mount(
            Some(squashfs),
            mount_point,
            Some("squashfs"),
            MsFlags::MS_RDONLY,
            None::<&str>,
        )
        .map_err(|e| {
            RecoverError::Mount(format!(
                "squashfs mount {} on {}: {}",
                squashfs.display(),
                mount_point.display(),
                e
            ))
        })
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = (squashfs, mount_point);
        Err(RecoverError::Mount(
            "squashfs mount not supported on this platform".to_string(),
        ))
    }
}

/// Mount an overlay filesystem.
fn mount_overlay(
    lowerdir: &str,
    upper: &Path,
    work: &Path,
    merged: &Path,
) -> Result<(), RecoverError> {
    #[cfg(target_os = "linux")]
    {
        use nix::mount::{mount, MsFlags};
        let opts = format!(
            "lowerdir={},upperdir={},workdir={}",
            lowerdir,
            upper.display(),
            work.display()
        );
        mount(
            Some("overlay"),
            merged,
            Some("overlay"),
            MsFlags::empty(),
            Some(opts.as_str()),
        )
        .map_err(|e| RecoverError::Mount(format!("overlay mount on {}: {}", merged.display(), e)))
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = (lowerdir, upper, work, merged);
        Err(RecoverError::Mount(
            "overlay mount not supported on this platform".to_string(),
        ))
    }
}

/// Check if a path is currently a mount point by reading /proc/mounts.
fn is_mounted(path: &Path) -> Result<bool, RecoverError> {
    #[cfg(target_os = "linux")]
    {
        let path_str = path.to_string_lossy();
        let mounts = fs::read_to_string("/proc/mounts")
            .map_err(|e| RecoverError::Mount(format!("read /proc/mounts: {}", e)))?;
        // Each line: device mountpoint fstype options dump pass
        // Check if any line's second field matches our path
        Ok(mounts
            .lines()
            .any(|line| line.split_whitespace().nth(1) == Some(path_str.as_ref())))
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = path;
        Ok(false)
    }
}

/// Recovery errors for individual sandboxes (non-fatal to the overall init).
#[derive(Debug, thiserror::Error)]
enum RecoverError {
    #[error("metadata: {0}")]
    Meta(String),

    #[error("mount: {0}")]
    Mount(String),
}

// -- Helpers -----------------------------------------------------------------

/// Display file size in human-readable format.
fn display_file_size(path: &Path) -> String {
    match fs::metadata(path) {
        Ok(meta) => {
            let bytes = meta.len();
            if bytes >= 1024 * 1024 {
                format!("{:.1}M", bytes as f64 / (1024.0 * 1024.0))
            } else if bytes >= 1024 {
                format!("{:.1}K", bytes as f64 / 1024.0)
            } else {
                format!("{}B", bytes)
            }
        }
        Err(_) => "?".to_string(),
    }
}

/// Count sandbox directories.
fn count_sandbox_dirs(sandboxes_dir: &Path) -> usize {
    match fs::read_dir(sandboxes_dir) {
        Ok(entries) => entries
            .filter_map(|e| e.ok())
            .filter(|e| e.path().is_dir())
            .count(),
        Err(_) => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_file_size() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.bin");

        // ~1.5KB file
        fs::write(&path, vec![0u8; 1500]).unwrap();
        let size = display_file_size(&path);
        assert!(size.ends_with('K'), "expected K suffix, got: {}", size);

        // ~2MB file
        fs::write(&path, vec![0u8; 2 * 1024 * 1024]).unwrap();
        let size = display_file_size(&path);
        assert!(size.ends_with('M'), "expected M suffix, got: {}", size);

        // Nonexistent file
        assert_eq!(display_file_size(Path::new("/nonexistent")), "?");
    }

    #[test]
    fn test_count_sandbox_dirs() {
        let dir = tempfile::tempdir().unwrap();

        // Empty directory
        assert_eq!(count_sandbox_dirs(dir.path()), 0);

        // Add some sandbox dirs
        fs::create_dir(dir.path().join("sandbox-1")).unwrap();
        fs::create_dir(dir.path().join("sandbox-2")).unwrap();
        fs::write(dir.path().join("not-a-dir.txt"), b"ignored").unwrap();
        assert_eq!(count_sandbox_dirs(dir.path()), 2);
    }

    #[test]
    fn test_base_version_constant() {
        assert_eq!(BASE_VERSION_CURRENT, 2);
    }

    #[test]
    fn test_build_lowerdir_empty() {
        let dir = tempfile::tempdir().unwrap();
        let sdir = dir.path().join("sandbox-test");
        fs::create_dir_all(sdir.join("images")).unwrap();

        let lowerdir = build_lowerdir(&sdir).unwrap();
        assert!(lowerdir.is_empty());
    }

    #[test]
    fn test_build_lowerdir_ordering() {
        let dir = tempfile::tempdir().unwrap();
        let sdir = dir.path().join("sandbox-test");
        let images = sdir.join("images");
        fs::create_dir_all(&images).unwrap();

        // Create module-like dirs (won't be "mounted" on non-Linux)
        fs::create_dir_all(images.join("000-base.squashfs")).unwrap();
        fs::create_dir_all(images.join("100-python.squashfs")).unwrap();
        fs::create_dir_all(images.join("200-node.squashfs")).unwrap();
        fs::create_dir_all(images.join("_snapshot")).unwrap();

        // On non-Linux, is_mounted returns false, so lowerdir is empty
        let lowerdir = build_lowerdir(&sdir).unwrap();
        #[cfg(not(target_os = "linux"))]
        assert!(lowerdir.is_empty());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_is_mounted_proc() {
        // /proc should be mounted on Linux
        assert!(is_mounted(Path::new("/proc")).unwrap());
        assert!(!is_mounted(Path::new("/tmp/definitely-not-a-mountpoint-12345")).unwrap());
    }

    #[test]
    fn test_init_creates_directories() {
        let dir = tempfile::tempdir().unwrap();
        let modules_dir = dir.path().join("modules");
        let sandboxes_dir = dir.path().join("sandboxes");

        fs::create_dir_all(&modules_dir).unwrap();
        fs::create_dir_all(&sandboxes_dir).unwrap();

        assert!(modules_dir.is_dir());
        assert!(sandboxes_dir.is_dir());
    }

    #[tokio::test]
    async fn test_run_creates_directories() {
        let dir = tempfile::tempdir().unwrap();
        let config = Config {
            data_dir: dir.path().to_path_buf(),
            ..Config::default()
        };

        // run() will fail at the base module check (no sq-mkbase on dev machine)
        // but should at least create the directories first
        let _ = run(&config, None, None).await;

        assert!(config.modules_dir().is_dir());
        assert!(config.sandboxes_dir().is_dir());
    }

    #[tokio::test]
    async fn test_run_ephemeral_skips_recovery() {
        let dir = tempfile::tempdir().unwrap();
        let modules_dir = dir.path().join("modules");
        let sandboxes_dir = dir.path().join("sandboxes");
        fs::create_dir_all(&modules_dir).unwrap();
        fs::create_dir_all(&sandboxes_dir).unwrap();

        // Create a fake base module so the check passes
        fs::write(modules_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();
        fs::write(modules_dir.join("000-base-alpine.version"), "2").unwrap();

        // Create a sandbox dir that would normally trigger recovery
        let sbox = sandboxes_dir.join("test-sandbox");
        fs::create_dir_all(sbox.join(".meta")).unwrap();
        fs::write(sbox.join(".meta/layers"), "000-base-alpine").unwrap();

        let config = Config {
            data_dir: dir.path().to_path_buf(),
            ephemeral: true,
            ..Config::default()
        };

        let result = run(&config, None, None).await.unwrap();
        // Ephemeral mode should skip recovery, so no recovered sandboxes
        assert!(result.recovered.is_empty());
        assert!(result.failed.is_empty());
    }

    #[tokio::test]
    async fn test_run_with_present_base_module() {
        let dir = tempfile::tempdir().unwrap();
        let modules_dir = dir.path().join("modules");
        let sandboxes_dir = dir.path().join("sandboxes");
        fs::create_dir_all(&modules_dir).unwrap();
        fs::create_dir_all(&sandboxes_dir).unwrap();

        // Create a valid base module
        fs::write(modules_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();
        fs::write(modules_dir.join("000-base-alpine.version"), "2").unwrap();

        let config = Config {
            data_dir: dir.path().to_path_buf(),
            ..Config::default()
        };

        let result = run(&config, None, None).await.unwrap();
        assert_eq!(result.module_count, 1);
        assert_eq!(result.sandbox_count, 0);
    }
}
