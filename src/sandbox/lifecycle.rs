//! Sandbox lifecycle operations that integrate exec, cgroups, netns, and state management.
//!
//! These functions coordinate the per-sandbox mutex, state machine transitions,
//! and low-level exec/snapshot operations. They are the bridge between the API
//! layer and the individual sandbox subsystems.
//!
//! # Lifecycle flows
//!
//! - **create**: validate → check limits → create dirs → mount layers → overlay →
//!   cgroup → netns → seed DNS → inject secrets → (ephemeral auto-restore) → Ready
//! - **exec**: acquire mutex → Ready→Executing → fork/exec → Executing→Ready
//! - **activate**: acquire mutex → Ready→Executing → mount new layer → remount overlay →
//!   update metadata → Executing→Ready
//! - **snapshot**: acquire mutex → Ready→Snapshotting → mksquashfs → record metadata →
//!   (S3 push) → Snapshotting→Ready
//! - **restore**: acquire mutex → Ready→Snapshotting → unmount overlay → clear upper →
//!   mount snapshot → re-inject secrets → remount overlay → Snapshotting→Ready
//! - **destroy**: acquire mutex → Ready→Destroying → (ephemeral auto-snapshot) →
//!   drop mounts → drop cgroup → drop netns → rm dirs → remove from manager

/// Parameters for creating a new sandbox.
pub struct CreateParams {
    pub id: String,
    pub owner: String,
    pub task: String,
    pub layers: Vec<String>,
    pub cpu: f64,
    pub memory_mb: u64,
    pub max_lifetime_s: u64,
    pub allow_net: Vec<String>,
}

#[cfg(target_os = "linux")]
mod linux {
    use std::fs;
    use std::io::Write as _;
    use std::os::unix::io::AsRawFd;
    use std::path::{Path, PathBuf};

    use chrono::Utc;
    use tracing::{debug, info, warn};

    use crate::config::Config;
    use crate::modules;
    use crate::s3::S3Store;
    use crate::sandbox::cgroup::CgroupHandle;
    use crate::sandbox::exec::{self, ExecContext};
    use crate::sandbox::exec_types::{ExecRequest, ExecResult};
    use crate::sandbox::manager::SandboxManager;
    use crate::sandbox::meta::{self, SandboxMetadata};
    use crate::sandbox::mounts::SandboxMounts;
    use crate::sandbox::netns::NetnsHandle;
    use crate::sandbox::snapshot as snap;
    use crate::sandbox::{Sandbox, SandboxError, SandboxHandle, SandboxState};
    use crate::validate;

    use super::CreateParams;

    // ── Create ──────────────────────────────────────────────────────────

    /// Create a new sandbox with the full 16-step lifecycle.
    ///
    /// Steps:
    ///  1. Validate sandbox ID
    ///  2. Validate module names
    ///  3. Check sandbox limit
    ///  4. Verify all modules exist locally
    ///  5. Create sandbox directory tree
    ///  6. Write metadata
    ///  7. Mount squashfs layers
    ///  8. Mount tmpfs for overlay upper/work
    ///  9. Mount overlayfs
    /// 10. Set up cgroups
    /// 11. Set up network namespace
    /// 12. Seed DNS resolv.conf
    /// 13. Inject proxy env vars (if proxy enabled)
    /// 14. Inject CA certs (if proxy enabled)
    /// 15. Insert into manager
    /// 16. Transition Creating → Ready
    pub async fn create_sandbox(
        manager: &SandboxManager,
        config: &Config,
        params: CreateParams,
    ) -> Result<SandboxHandle, SandboxError> {
        // 1. Validate sandbox ID
        if !validate::valid_id(&params.id) {
            return Err(SandboxError::InvalidId(params.id));
        }

        // 2. Validate module names
        for layer in &params.layers {
            if !validate::valid_module(layer) {
                return Err(SandboxError::InvalidModule(layer.clone()));
            }
        }

        // 3. Check duplicate (manager.insert will also check, but fail early)
        if manager.contains(&params.id) {
            return Err(SandboxError::AlreadyExists(params.id));
        }

        // 4. Verify all modules exist
        let modules_dir = config.modules_dir();
        for layer in &params.layers {
            if !modules::module_exists(&modules_dir, layer) {
                return Err(SandboxError::ModuleNotFound(layer.clone()));
            }
        }

        // 5. Create sandbox directory tree
        let sandbox_dir = config.sandboxes_dir().join(&params.id);
        let meta_dir = sandbox_dir.join(".meta");
        let images_dir = sandbox_dir.join("images");
        let merged_dir = sandbox_dir.join("merged");
        let upper_dir = sandbox_dir.join("upper");
        let snapshots_dir = sandbox_dir.join("snapshots");

        fs::create_dir_all(&meta_dir)?;
        fs::create_dir_all(&images_dir)?;
        fs::create_dir_all(&merged_dir)?;
        fs::create_dir_all(&upper_dir)?;
        fs::create_dir_all(&snapshots_dir)?;
        fs::create_dir_all(meta_dir.join("log"))?;

        let now = Utc::now();
        let allow_net = if params.allow_net.is_empty() {
            None
        } else {
            Some(params.allow_net.clone())
        };

        // 6. Write metadata
        let metadata = SandboxMetadata {
            owner: params.owner,
            task: params.task,
            layers: params.layers.clone(),
            created: now,
            last_active: now,
            cpu: params.cpu,
            memory_mb: params.memory_mb,
            max_lifetime_s: params.max_lifetime_s,
            allow_net: allow_net.clone(),
            active_snapshot: None,
            netns_index: None,
        };

        meta::write_metadata(&meta_dir, &metadata)
            .map_err(|e| SandboxError::Meta(format!("write metadata: {}", e)))?;

        // 7-9. Mount layers, tmpfs, and overlay
        let mounts = mount_sandbox_layers(
            &sandbox_dir,
            &params.layers,
            &modules_dir,
            config.upper_limit_mb,
            None, // no active snapshot for new sandboxes
        )?;

        // 10. Set up cgroups
        let cgroup =
            CgroupHandle::create(&params.id, params.cpu, params.memory_mb)
                .map_err(|e| SandboxError::Cgroup(e.to_string()))?;

        // 11. Set up network namespace
        let sandboxes_dir = config.sandboxes_dir();
        let netns = setup_netns(
            &params.id,
            &config.data_dir,
            &sandboxes_dir,
            &meta_dir,
            allow_net.as_deref(),
        )?;

        // 12. Seed DNS resolv.conf in the merged directory
        seed_dns_resolv_conf(&merged_dir);

        // 13-14. Inject proxy env vars and CA certs
        if config.proxy_https {
            inject_proxy_env(&merged_dir, config);
            inject_ca_certs(&merged_dir, config);
        }

        // Update metadata with netns index if netns was set up
        let mut sandbox_metadata = metadata;
        if let Some(ref ns) = netns {
            sandbox_metadata.netns_index = Some(ns.index());
        }

        let sandbox = Sandbox {
            id: params.id.clone(),
            state: SandboxState::Creating,
            dir: sandbox_dir,
            metadata: sandbox_metadata,
            cgroup,
            netns,
            mounts: Some(mounts),
        };

        // 15. Insert into manager (checks limit atomically)
        let handle = manager.insert(sandbox)?;

        // 16. Transition Creating → Ready
        {
            let mut sb = handle.lock().await;
            sb.transition(SandboxState::Ready)?;
        }

        info!(sandbox_id = %params.id, "sandbox created");
        Ok(handle)
    }

    /// Mount all squashfs layers, tmpfs, and overlay for a sandbox.
    fn mount_sandbox_layers(
        sandbox_dir: &Path,
        layers: &[String],
        modules_dir: &Path,
        upper_limit_mb: u64,
        active_snapshot: Option<&str>,
    ) -> Result<SandboxMounts, SandboxError> {
        let images_dir = sandbox_dir.join("images");
        let upper_dir = sandbox_dir.join("upper");
        let merged_dir = sandbox_dir.join("merged");

        // Build source/target pairs for each layer
        let mut layer_sources = Vec::new();
        let mut layer_targets = Vec::new();

        for layer in layers {
            let source = modules::module_path(modules_dir, layer);
            let target = images_dir.join(format!("{}.squashfs", layer));
            layer_sources.push(source);
            layer_targets.push(target);
        }

        // Snapshot source/target if active
        let (snap_source, snap_target) = match active_snapshot {
            Some(label) => {
                let snap_src = sandbox_dir.join(format!("snapshots/{}.squashfs", label));
                let snap_tgt = images_dir.join("_snapshot");
                (Some(snap_src), Some(snap_tgt))
            }
            None => (None, None),
        };

        SandboxMounts::mount(
            layer_sources,
            layer_targets,
            &upper_dir,
            upper_limit_mb,
            &merged_dir,
            snap_source,
            snap_target,
        )
        .map_err(|e| SandboxError::Mount(e.to_string()))
    }

    /// Set up network namespace if possible.
    fn setup_netns(
        id: &str,
        data_dir: &Path,
        sandboxes_dir: &Path,
        meta_dir: &Path,
        allow_net: Option<&[String]>,
    ) -> Result<Option<NetnsHandle>, SandboxError> {
        match NetnsHandle::setup(id, data_dir, sandboxes_dir, meta_dir, allow_net) {
            Ok(ns) => Ok(Some(ns)),
            Err(e) => {
                warn!(sandbox_id = %id, error = %e, "netns setup failed, continuing without networking");
                Ok(None)
            }
        }
    }

    /// Seed /etc/resolv.conf inside the merged directory with the host DNS.
    fn seed_dns_resolv_conf(merged_dir: &Path) {
        let etc = merged_dir.join("etc");
        if let Err(e) = fs::create_dir_all(&etc) {
            warn!("failed to create etc dir in merged: {}", e);
            return;
        }

        let resolv_path = etc.join("resolv.conf");
        match fs::read_to_string("/etc/resolv.conf") {
            Ok(content) => {
                if let Err(e) = fs::write(&resolv_path, content) {
                    warn!("failed to write resolv.conf: {}", e);
                }
            }
            Err(e) => {
                warn!("cannot read host /etc/resolv.conf: {}", e);
                // Write a minimal fallback
                let _ = fs::write(&resolv_path, "nameserver 8.8.8.8\n");
            }
        }
    }

    /// Inject proxy environment variables into the sandbox.
    fn inject_proxy_env(merged_dir: &Path, _config: &Config) {
        let etc = merged_dir.join("etc");
        let _ = fs::create_dir_all(&etc);

        let proxy_url = format!("http://10.200.0.1:{}", 8888);
        let env_content = format!(
            "http_proxy={proxy_url}\nhttps_proxy={proxy_url}\nHTTP_PROXY={proxy_url}\nHTTPS_PROXY={proxy_url}\n"
        );

        let env_path = etc.join("environment");
        let mut file = match fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&env_path)
        {
            Ok(f) => f,
            Err(e) => {
                warn!("failed to open /etc/environment for proxy env: {}", e);
                return;
            }
        };

        if let Err(e) = file.write_all(env_content.as_bytes()) {
            warn!("failed to write proxy env: {}", e);
        }

        // Also write a profile.d script for interactive shells
        let profile_d = etc.join("profile.d");
        let _ = fs::create_dir_all(&profile_d);
        let script = format!(
            "export http_proxy={proxy_url}\nexport https_proxy={proxy_url}\nexport HTTP_PROXY={proxy_url}\nexport HTTPS_PROXY={proxy_url}\n"
        );
        let _ = fs::write(profile_d.join("proxy.sh"), script);
    }

    /// Inject CA certificates into the sandbox for HTTPS MITM proxy.
    fn inject_ca_certs(merged_dir: &Path, config: &Config) {
        let ca_cert_src = config.proxy_ca_dir().join("ca-cert.pem");
        if !ca_cert_src.is_file() {
            debug!("no CA cert found at {}, skipping injection", ca_cert_src.display());
            return;
        }

        let targets = [
            "usr/local/share/ca-certificates/squash-proxy.crt",
            "etc/ssl/certs/squash-proxy.pem",
        ];

        for target in &targets {
            let dest = merged_dir.join(target);
            if let Some(parent) = dest.parent() {
                let _ = fs::create_dir_all(parent);
            }
            if let Err(e) = fs::copy(&ca_cert_src, &dest) {
                debug!("CA cert copy to {} failed: {} (non-fatal)", target, e);
            }
        }

        // Set env vars for Python/Node CA trust
        let etc = merged_dir.join("etc");
        let profile_d = etc.join("profile.d");
        let _ = fs::create_dir_all(&profile_d);
        let _ = fs::write(
            profile_d.join("ca-bundle.sh"),
            "export REQUESTS_CA_BUNDLE=/etc/ssl/certs/squash-proxy.pem\nexport SSL_CERT_FILE=/etc/ssl/certs/squash-proxy.pem\nexport NODE_EXTRA_CA_CERTS=/etc/ssl/certs/squash-proxy.pem\n",
        );
    }

    // ── Exec ────────────────────────────────────────────────────────────

    /// Execute a command inside a sandbox, coordinating state transitions and serialization.
    pub async fn sandbox_exec(
        manager: &SandboxManager,
        sandbox_id: &str,
        cmd: String,
        workdir: String,
        timeout: u64,
    ) -> Result<ExecResult, SandboxError> {
        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;

        sandbox.transition(SandboxState::Executing)?;

        let merged_path = sandbox.merged_path();
        let sandbox_dir = sandbox.dir.clone();
        let sb_id = sandbox.id.clone();
        let cgroup_path = sandbox.cgroup.as_ref().map(|cg| cg.path().clone());

        let netns_file = sandbox.netns.as_ref().map(|ns| {
            let netns_path = PathBuf::from(format!("/run/netns/{}", ns.name()));
            std::fs::File::open(&netns_path)
        });

        let netns_fd = match netns_file {
            Some(Ok(ref f)) => Some(f.as_raw_fd()),
            Some(Err(ref e)) => {
                let _ = sandbox.transition(SandboxState::Ready);
                return Err(SandboxError::Netns(format!("failed to open netns fd: {}", e)));
            }
            None => None,
        };

        let ctx = ExecContext {
            sandbox_id: sb_id.clone(),
            merged_path,
            sandbox_dir,
            netns_fd,
            cgroup_path,
        };

        let req = exec::ExecRequest { cmd, workdir, timeout };

        debug!(sandbox_id = %sb_id, cmd = %req.cmd, "executing in sandbox");

        let result = tokio::task::spawn_blocking(move || exec::exec_in_sandbox(&ctx, &req))
            .await
            .map_err(|e| SandboxError::Exec(format!("spawn_blocking failed: {}", e)))?
            .map_err(|e| SandboxError::Exec(e.to_string()))?;

        sandbox.metadata.last_active = Utc::now();
        sandbox.transition(SandboxState::Ready)?;

        info!(sandbox_id = %sb_id, seq = result.seq, exit_code = result.exit_code, "exec completed");
        Ok(result)
    }

    // ── Activate ────────────────────────────────────────────────────────

    /// Activate (hot-add) a new module layer into a running sandbox.
    pub async fn activate_module(
        manager: &SandboxManager,
        config: &Config,
        sandbox_id: &str,
        module_name: &str,
    ) -> Result<(), SandboxError> {
        if !validate::valid_module(module_name) {
            return Err(SandboxError::InvalidModule(module_name.to_string()));
        }

        let modules_dir = config.modules_dir();
        if !modules::module_exists(&modules_dir, module_name) {
            return Err(SandboxError::ModuleNotFound(module_name.to_string()));
        }

        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;

        sandbox.transition(SandboxState::Executing)?;

        let sandbox_dir = sandbox.dir.clone();
        let sb_id = sandbox.id.clone();

        if sandbox.metadata.layers.contains(&module_name.to_string()) {
            let _ = sandbox.transition(SandboxState::Ready);
            return Err(SandboxError::Mount(format!(
                "module '{}' already active in sandbox",
                module_name
            )));
        }

        // Mount the new squashfs layer
        let source = modules::module_path(&modules_dir, module_name);
        let target = sandbox_dir.join(format!("images/{}.squashfs", module_name));
        fs::create_dir_all(&target).map_err(|e| {
            let _ = sandbox.transition(SandboxState::Ready);
            SandboxError::Mount(format!("create mount target: {}", e))
        })?;

        use crate::sandbox::mounts::SquashfsMount;
        let new_layer = SquashfsMount::mount(&source, &target).map_err(|e| {
            let _ = sandbox.transition(SandboxState::Ready);
            SandboxError::Mount(format!("mount module {}: {}", module_name, e))
        })?;

        let mut old_mounts = sandbox.mounts.take().ok_or_else(|| {
            let _ = sandbox.transition(SandboxState::Ready);
            SandboxError::Mount("sandbox has no mounts".to_string())
        })?;

        if let Err(e) = old_mounts.overlay.unmount() {
            warn!(sandbox_id = %sb_id, error = %e, "failed to unmount overlay for activate");
        }

        let mut updated_layers: Vec<String> = sandbox.metadata.layers.clone();
        updated_layers.push(module_name.to_string());

        // Rebuild overlay with new layer set
        let merged_dir = sandbox_dir.join("merged");
        let upper_data = sandbox_dir.join("upper").join("data");
        let upper_work = sandbox_dir.join("upper").join("work");
        let images_dir = sandbox_dir.join("images");

        let mut lower_dirs = Vec::new();
        if old_mounts.snapshot.is_some() {
            lower_dirs.push(images_dir.join("_snapshot"));
        }
        for layer in updated_layers.iter().rev() {
            lower_dirs.push(images_dir.join(format!("{}.squashfs", layer)));
        }

        use crate::sandbox::mounts::OverlayMount;
        let overlay = OverlayMount::mount(&merged_dir, lower_dirs, &upper_data, &upper_work)
            .map_err(|e| {
                let _ = sandbox.transition(SandboxState::Ready);
                SandboxError::Mount(format!("remount overlay: {}", e))
            })?;

        old_mounts.layers.push(new_layer);
        old_mounts.overlay = overlay;
        sandbox.mounts = Some(old_mounts);

        sandbox.metadata.layers = updated_layers.clone();
        sandbox.metadata.last_active = Utc::now();

        let meta_dir = sandbox.meta_dir();
        let _ = fs::write(meta_dir.join("layers"), updated_layers.join(","));
        let _ = meta::touch_last_active(&meta_dir);

        sandbox.transition(SandboxState::Ready)?;

        info!(sandbox_id = %sb_id, module = %module_name, "module activated");
        Ok(())
    }

    // ── Snapshot ────────────────────────────────────────────────────────

    /// Create a snapshot of the sandbox's writable upper layer.
    pub async fn snapshot_sandbox(
        manager: &SandboxManager,
        sandbox_id: &str,
        label: Option<&str>,
        s3: Option<&S3Store>,
    ) -> Result<(String, u64), SandboxError> {
        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;

        sandbox.transition(SandboxState::Snapshotting)?;

        let sb_id = sandbox.id.clone();
        let sandbox_dir = sandbox.dir.clone();

        let label = match label {
            Some(l) => {
                if !validate::valid_label(l) {
                    let _ = sandbox.transition(SandboxState::Ready);
                    return Err(SandboxError::InvalidLabel(l.to_string()));
                }
                l.to_string()
            }
            None => Utc::now().format("snap-%Y%m%d-%H%M%S").to_string(),
        };

        let use_zstd = snap::kernel_supports_zstd();

        let label_clone = label.clone();
        let sandbox_dir_clone = sandbox_dir.clone();
        let (snap_path, size) = tokio::task::spawn_blocking(move || {
            snap::create_snapshot(&sandbox_dir_clone, &label_clone, use_zstd)
        })
        .await
        .map_err(|e| {
            let _ = sandbox.transition(SandboxState::Ready);
            SandboxError::Snapshot(format!("spawn_blocking: {}", e))
        })?
        .map_err(|e| {
            let _ = sandbox.transition(SandboxState::Ready);
            e
        })?;

        sandbox.metadata.active_snapshot = Some(label.clone());
        sandbox.metadata.last_active = Utc::now();

        if let Some(s3) = s3 {
            let s3_key = format!("snapshots/{}/{}.squashfs", sb_id, label);
            s3.push_bg(snap_path, s3_key.clone());
            info!(sandbox_id = %sb_id, s3_key, "snapshot S3 push started");
        }

        sandbox.transition(SandboxState::Ready)?;
        info!(sandbox_id = %sb_id, label = %label, size, "snapshot complete");
        Ok((label, size))
    }

    // ── Restore ─────────────────────────────────────────────────────────

    /// Restore a sandbox from a named snapshot.
    pub async fn restore_sandbox(
        manager: &SandboxManager,
        config: &Config,
        sandbox_id: &str,
        label: &str,
    ) -> Result<(), SandboxError> {
        if !validate::valid_label(label) {
            return Err(SandboxError::InvalidLabel(label.to_string()));
        }

        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;

        sandbox.transition(SandboxState::Snapshotting)?;

        let sb_id = sandbox.id.clone();
        let sandbox_dir = sandbox.dir.clone();

        let snap_path = sandbox_dir.join(format!("snapshots/{}.squashfs", label));
        if !snap_path.is_file() {
            let _ = sandbox.transition(SandboxState::Ready);
            return Err(SandboxError::Snapshot(format!("snapshot '{}' not found", label)));
        }

        let mut old_mounts = sandbox.mounts.take().ok_or_else(|| {
            let _ = sandbox.transition(SandboxState::Ready);
            SandboxError::Mount("sandbox has no mounts".to_string())
        })?;

        // Unmount overlay
        if let Err(e) = old_mounts.overlay.unmount() {
            warn!(sandbox_id = %sb_id, error = %e, "failed to unmount overlay for restore");
        }

        // Clear upper/data and upper/work
        let upper_data = sandbox_dir.join("upper").join("data");
        let upper_work = sandbox_dir.join("upper").join("work");
        if upper_data.is_dir() {
            let _ = fs::remove_dir_all(&upper_data);
            let _ = fs::create_dir_all(&upper_data);
        }
        if upper_work.is_dir() {
            let _ = fs::remove_dir_all(&upper_work);
            let _ = fs::create_dir_all(&upper_work);
        }

        // Unmount old snapshot if present
        if let Some(ref mut old_snap) = old_mounts.snapshot {
            let _ = old_snap.unmount();
        }

        // Mount new snapshot
        let images_dir = sandbox_dir.join("images");
        let snap_target = images_dir.join("_snapshot");
        let _ = fs::create_dir_all(&snap_target);

        use crate::sandbox::mounts::SquashfsMount;
        let new_snap = SquashfsMount::mount(&snap_path, &snap_target).map_err(|e| {
            let _ = sandbox.transition(SandboxState::Ready);
            SandboxError::Mount(format!("mount snapshot {}: {}", label, e))
        })?;

        // Rebuild overlay with snapshot as top lowerdir
        let merged_dir = sandbox_dir.join("merged");
        let mut lower_dirs = Vec::new();
        lower_dirs.push(snap_target.clone());
        for layer in sandbox.metadata.layers.iter().rev() {
            lower_dirs.push(images_dir.join(format!("{}.squashfs", layer)));
        }

        use crate::sandbox::mounts::OverlayMount;
        let overlay = OverlayMount::mount(&merged_dir, lower_dirs, &upper_data, &upper_work)
            .map_err(|e| {
                let _ = sandbox.transition(SandboxState::Ready);
                SandboxError::Mount(format!("remount overlay after restore: {}", e))
            })?;

        old_mounts.snapshot = Some(new_snap);
        old_mounts.overlay = overlay;
        sandbox.mounts = Some(old_mounts);

        // Re-inject config
        if config.proxy_https {
            inject_proxy_env(&merged_dir, config);
            inject_ca_certs(&merged_dir, config);
        }
        seed_dns_resolv_conf(&merged_dir);

        sandbox.metadata.active_snapshot = Some(label.to_string());
        sandbox.metadata.last_active = Utc::now();

        let meta_dir = sandbox.meta_dir();
        let _ = meta::write_active_snapshot(&meta_dir, Some(label));
        let _ = meta::touch_last_active(&meta_dir);

        sandbox.transition(SandboxState::Ready)?;

        info!(sandbox_id = %sb_id, label, "sandbox restored");
        Ok(())
    }

    // ── Destroy ─────────────────────────────────────────────────────────

    /// Destroy a sandbox, cleaning up all resources.
    pub async fn destroy_sandbox(
        manager: &SandboxManager,
        config: &Config,
        sandbox_id: &str,
        s3: Option<&S3Store>,
    ) -> Result<(), SandboxError> {
        let handle = manager.get(sandbox_id)?;

        {
            let mut sandbox = handle.lock().await;

            if sandbox.state == SandboxState::Destroying {
                return Ok(());
            }

            sandbox.transition(SandboxState::Destroying)?;

            let sb_id = sandbox.id.clone();
            let sandbox_dir = sandbox.dir.clone();

            // Ephemeral auto-snapshot before teardown
            if config.ephemeral {
                let upper_data = sandbox_dir.join("upper").join("data");
                if upper_data.is_dir() {
                    let use_zstd = snap::kernel_supports_zstd();
                    let label = format!("auto-{}", Utc::now().format("%Y%m%d-%H%M%S"));
                    let sandbox_dir_clone = sandbox_dir.clone();
                    let label_clone = label.clone();

                    match tokio::task::spawn_blocking(move || {
                        snap::create_snapshot(&sandbox_dir_clone, &label_clone, use_zstd)
                    })
                    .await
                    {
                        Ok(Ok((snap_path, _size))) => {
                            info!(sandbox_id = %sb_id, label = %label, "ephemeral auto-snapshot created");
                            if let Some(s3) = s3 {
                                let s3_key = format!("snapshots/{}/{}.squashfs", sb_id, label);
                                s3.push_bg(snap_path, s3_key);
                            }
                        }
                        Ok(Err(e)) => {
                            warn!(sandbox_id = %sb_id, error = %e, "ephemeral auto-snapshot failed");
                        }
                        Err(e) => {
                            warn!(sandbox_id = %sb_id, error = %e, "ephemeral auto-snapshot task failed");
                        }
                    }
                }
            }

            // Drop resources in reverse order
            sandbox.mounts.take();
            sandbox.cgroup.take();
            sandbox.netns.take();

            // Remove sandbox directory
            if sandbox_dir.is_dir() {
                if let Err(e) = fs::remove_dir_all(&sandbox_dir) {
                    warn!(sandbox_id = %sb_id, error = %e, "failed to remove sandbox directory");
                }
            }

            info!(sandbox_id = %sb_id, "sandbox destroyed");
        }

        // Remove from manager after releasing the mutex
        manager.remove(sandbox_id)?;
        Ok(())
    }

    // ── Log reading ─────────────────────────────────────────────────────

    /// Read all execution log entries for a sandbox from .meta/log/*.json.
    pub fn read_exec_logs(sandbox_dir: &Path) -> Result<Vec<ExecResult>, SandboxError> {
        super::read_exec_logs_impl(sandbox_dir)
    }
}

#[cfg(target_os = "linux")]
pub use linux::*;

#[cfg(not(target_os = "linux"))]
mod stubs {
    use std::fs;
    use std::path::Path;

    use chrono::Utc;

    use crate::config::Config;
    use crate::modules;
    use crate::s3::S3Store;
    use crate::sandbox::exec_types::ExecResult;
    use crate::sandbox::manager::SandboxManager;
    use crate::sandbox::meta::{self, SandboxMetadata, SnapshotEntry};
    use crate::sandbox::{Sandbox, SandboxError, SandboxHandle, SandboxState};
    use crate::validate;

    use super::CreateParams;

    pub async fn create_sandbox(
        manager: &SandboxManager,
        config: &Config,
        params: CreateParams,
    ) -> Result<SandboxHandle, SandboxError> {
        if !validate::valid_id(&params.id) {
            return Err(SandboxError::InvalidId(params.id));
        }
        if manager.contains(&params.id) {
            return Err(SandboxError::AlreadyExists(params.id));
        }
        for layer in &params.layers {
            if !validate::valid_module(layer) {
                return Err(SandboxError::InvalidModule(layer.clone()));
            }
            if !modules::module_exists(&config.modules_dir(), layer) {
                return Err(SandboxError::ModuleNotFound(layer.clone()));
            }
        }

        let sandbox_dir = config.sandboxes_dir().join(&params.id);
        let meta_dir = sandbox_dir.join(".meta");
        fs::create_dir_all(meta_dir.join("log"))?;

        let now = Utc::now();
        let metadata = SandboxMetadata {
            owner: params.owner,
            task: params.task,
            layers: params.layers,
            created: now,
            last_active: now,
            cpu: params.cpu,
            memory_mb: params.memory_mb,
            max_lifetime_s: params.max_lifetime_s,
            allow_net: if params.allow_net.is_empty() {
                None
            } else {
                Some(params.allow_net)
            },
            active_snapshot: None,
            netns_index: None,
        };
        meta::write_metadata(&meta_dir, &metadata)
            .map_err(|e| SandboxError::Meta(format!("write metadata: {}", e)))?;

        let sandbox = Sandbox {
            id: params.id,
            state: SandboxState::Creating,
            dir: sandbox_dir,
            metadata,
            cgroup: None,
            netns: None,
            mounts: None,
        };

        let handle = manager.insert(sandbox)?;
        {
            let mut sb = handle.lock().await;
            sb.transition(SandboxState::Ready)?;
        }
        Ok(handle)
    }

    pub async fn sandbox_exec(
        _manager: &SandboxManager,
        _sandbox_id: &str,
        _cmd: String,
        _workdir: String,
        _timeout: u64,
    ) -> Result<ExecResult, SandboxError> {
        Err(SandboxError::Exec("sandbox execution requires Linux".to_string()))
    }

    pub async fn activate_module(
        manager: &SandboxManager,
        config: &Config,
        sandbox_id: &str,
        module_name: &str,
    ) -> Result<(), SandboxError> {
        if !validate::valid_module(module_name) {
            return Err(SandboxError::InvalidModule(module_name.to_string()));
        }
        if !modules::module_exists(&config.modules_dir(), module_name) {
            return Err(SandboxError::ModuleNotFound(module_name.to_string()));
        }

        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;
        sandbox.transition(SandboxState::Executing)?;

        if sandbox.metadata.layers.contains(&module_name.to_string()) {
            let _ = sandbox.transition(SandboxState::Ready);
            return Err(SandboxError::Mount(format!(
                "module '{}' already active in sandbox",
                module_name
            )));
        }

        sandbox.metadata.layers.push(module_name.to_string());
        sandbox.metadata.last_active = Utc::now();
        let meta_dir = sandbox.meta_dir();
        fs::write(meta_dir.join("layers"), sandbox.metadata.layers.join(","))
            .map_err(|e| SandboxError::Meta(format!("write layers: {}", e)))?;
        let _ = meta::touch_last_active(&meta_dir);
        sandbox.transition(SandboxState::Ready)?;
        Ok(())
    }

    pub async fn snapshot_sandbox(
        manager: &SandboxManager,
        sandbox_id: &str,
        label: Option<&str>,
        _s3: Option<&S3Store>,
    ) -> Result<(String, u64), SandboxError> {
        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;
        sandbox.transition(SandboxState::Snapshotting)?;

        let label = match label {
            Some(l) => {
                if !validate::valid_label(l) {
                    let _ = sandbox.transition(SandboxState::Ready);
                    return Err(SandboxError::InvalidLabel(l.to_string()));
                }
                l.to_string()
            }
            None => Utc::now().format("snap-%Y%m%d-%H%M%S").to_string(),
        };

        let snapshots_dir = sandbox.dir.join("snapshots");
        fs::create_dir_all(&snapshots_dir)
            .map_err(|e| SandboxError::Snapshot(format!("create snapshots dir: {}", e)))?;
        let snapfile = snapshots_dir.join(format!("{}.squashfs", label));
        if snapfile.exists() {
            let _ = sandbox.transition(SandboxState::Ready);
            return Err(SandboxError::Snapshot(format!(
                "snapshot '{}' already exists",
                label
            )));
        }
        fs::File::create(&snapfile)
            .map_err(|e| SandboxError::Snapshot(format!("create snapshot file: {}", e)))?;

        let entry = SnapshotEntry {
            label: label.clone(),
            created: Utc::now(),
            size: 0,
        };
        meta::append_snapshot_entry(&sandbox.meta_dir(), &entry)
            .map_err(|e| SandboxError::Meta(format!("append snapshot entry: {}", e)))?;

        sandbox.metadata.active_snapshot = Some(label.clone());
        sandbox.metadata.last_active = Utc::now();
        let _ = meta::write_active_snapshot(&sandbox.meta_dir(), Some(&label));
        let _ = meta::touch_last_active(&sandbox.meta_dir());
        sandbox.transition(SandboxState::Ready)?;
        Ok((label, 0))
    }

    pub async fn restore_sandbox(
        manager: &SandboxManager,
        _config: &Config,
        sandbox_id: &str,
        label: &str,
    ) -> Result<(), SandboxError> {
        if !validate::valid_label(label) {
            return Err(SandboxError::InvalidLabel(label.to_string()));
        }

        let handle = manager.get(sandbox_id)?;
        let mut sandbox = handle.lock().await;
        sandbox.transition(SandboxState::Snapshotting)?;

        let snapfile = sandbox.dir.join("snapshots").join(format!("{}.squashfs", label));
        if !snapfile.exists() {
            let _ = sandbox.transition(SandboxState::Ready);
            return Err(SandboxError::Snapshot(format!(
                "snapshot '{}' not found",
                label
            )));
        }

        sandbox.metadata.active_snapshot = Some(label.to_string());
        sandbox.metadata.last_active = Utc::now();
        meta::write_active_snapshot(&sandbox.meta_dir(), Some(label))
            .map_err(|e| SandboxError::Meta(format!("write active_snapshot: {}", e)))?;
        let _ = meta::touch_last_active(&sandbox.meta_dir());
        sandbox.transition(SandboxState::Ready)?;
        Ok(())
    }

    pub async fn destroy_sandbox(
        manager: &SandboxManager,
        _config: &Config,
        sandbox_id: &str,
        _s3: Option<&S3Store>,
    ) -> Result<(), SandboxError> {
        let handle = manager.get(sandbox_id)?;
        {
            let mut sandbox = handle.lock().await;
            if sandbox.state != SandboxState::Destroying {
                sandbox.transition(SandboxState::Destroying)?;
            }
            let _ = fs::remove_dir_all(&sandbox.dir);
        }
        let _ = manager.remove(sandbox_id)?;
        Ok(())
    }

    pub fn read_exec_logs(sandbox_dir: &Path) -> Result<Vec<ExecResult>, SandboxError> {
        // This is pure filesystem I/O, works on all platforms
        super::read_exec_logs_impl(sandbox_dir)
    }
}

#[cfg(not(target_os = "linux"))]
pub use stubs::*;

/// Read all execution log entries for a sandbox from .meta/log/*.json.
/// Platform-independent implementation.
fn read_exec_logs_impl(sandbox_dir: &std::path::Path) -> Result<Vec<crate::sandbox::exec_types::ExecResult>, crate::sandbox::SandboxError> {
    use std::fs;
    use crate::sandbox::SandboxError;

    let log_dir = sandbox_dir.join(".meta").join("log");
    let entries = match fs::read_dir(&log_dir) {
        Ok(entries) => entries,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(SandboxError::Io(e)),
    };

    let mut logs: Vec<(u32, crate::sandbox::exec_types::ExecResult)> = Vec::new();
    for entry in entries.flatten() {
        let name = entry.file_name();
        let name = name.to_string_lossy();
        if let Some(stem) = name.strip_suffix(".json") {
            if let Ok(seq) = stem.parse::<u32>() {
                match fs::read_to_string(entry.path()) {
                    Ok(content) => match serde_json::from_str::<crate::sandbox::exec_types::ExecResult>(&content) {
                        Ok(result) => logs.push((seq, result)),
                        Err(e) => {
                            tracing::warn!(file = %name, error = %e, "skipping malformed exec log");
                        }
                    },
                    Err(e) => {
                        tracing::warn!(file = %name, error = %e, "failed to read exec log");
                    }
                }
            }
        }
    }

    logs.sort_by_key(|(seq, _)| *seq);
    Ok(logs.into_iter().map(|(_, result)| result).collect())
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::sandbox::manager::SandboxManager;
    use crate::sandbox::meta::SandboxMetadata;
    use crate::sandbox::{Sandbox, SandboxState};
    use chrono::Utc;
    use std::path::PathBuf;

    fn make_ready_sandbox(id: &str) -> Sandbox {
        Sandbox {
            id: id.into(),
            state: SandboxState::Ready,
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
    fn test_create_params_struct() {
        let params = CreateParams {
            id: "test-1".into(),
            owner: "alice".into(),
            task: "dev".into(),
            layers: vec!["000-base-alpine".into()],
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: vec![],
        };
        assert_eq!(params.id, "test-1");
        assert_eq!(params.cpu, 2.0);
    }

    #[tokio::test]
    async fn test_exec_requires_ready_state() {
        let mgr = SandboxManager::new(0);
        let mut sb = make_ready_sandbox("exec-test");
        sb.state = SandboxState::Creating;
        mgr.insert(sb).unwrap();

        let result = sandbox_exec(&mgr, "exec-test", "echo hi".into(), "/".into(), 10).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("wrong state") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_exec_not_found() {
        let mgr = SandboxManager::new(0);
        let result = sandbox_exec(&mgr, "ghost", "echo".into(), "/".into(), 10).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("not found") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_snapshot_not_found() {
        let mgr = SandboxManager::new(0);
        let result = snapshot_sandbox(&mgr, "ghost", None, None).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_destroy_not_found() {
        let mgr = SandboxManager::new(0);
        let config = Config::default();
        let result = destroy_sandbox(&mgr, &config, "ghost", None).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_activate_invalid_module() {
        let mgr = SandboxManager::new(0);
        let config = Config::default();
        let result = activate_module(&mgr, &config, "sb-1", "../evil").await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid module") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_restore_invalid_label() {
        let mgr = SandboxManager::new(0);
        let config = Config::default();
        let result = restore_sandbox(&mgr, &config, "sb-1", "../evil").await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid label") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[test]
    fn test_read_exec_logs_empty() {
        let dir = tempfile::tempdir().unwrap();
        let logs = read_exec_logs(dir.path()).unwrap();
        assert!(logs.is_empty());
    }

    #[test]
    fn test_read_exec_logs_with_entries() {
        let dir = tempfile::tempdir().unwrap();
        let log_dir = dir.path().join(".meta").join("log");
        std::fs::create_dir_all(&log_dir).unwrap();

        let entry1 = crate::sandbox::exec_types::ExecResult {
            seq: 1,
            cmd: "echo hello".into(),
            workdir: "/".into(),
            exit_code: 0,
            started: Utc::now(),
            finished: Utc::now(),
            stdout: "hello\n".into(),
            stderr: String::new(),
        };
        let entry2 = crate::sandbox::exec_types::ExecResult {
            seq: 2,
            cmd: "ls".into(),
            workdir: "/".into(),
            exit_code: 0,
            started: Utc::now(),
            finished: Utc::now(),
            stdout: "bin\netc\n".into(),
            stderr: String::new(),
        };

        std::fs::write(
            log_dir.join("0001.json"),
            serde_json::to_string(&entry1).unwrap(),
        )
        .unwrap();
        std::fs::write(
            log_dir.join("0002.json"),
            serde_json::to_string(&entry2).unwrap(),
        )
        .unwrap();
        std::fs::write(log_dir.join("notes.txt"), "ignored").unwrap();

        let logs = read_exec_logs(dir.path()).unwrap();
        assert_eq!(logs.len(), 2);
        assert_eq!(logs[0].seq, 1);
        assert_eq!(logs[1].seq, 2);
        assert_eq!(logs[0].cmd, "echo hello");
    }

    #[tokio::test]
    async fn test_create_sandbox_invalid_id() {
        let mgr = SandboxManager::new(0);
        let config = Config::default();
        let params = CreateParams {
            id: "../evil".into(),
            owner: "test".into(),
            task: "test".into(),
            layers: vec!["000-base-alpine".into()],
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: vec![],
        };
        let result = create_sandbox(&mgr, &config, params).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid sandbox id") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_create_sandbox_invalid_module() {
        let mgr = SandboxManager::new(0);
        let config = Config::default();
        let params = CreateParams {
            id: "test-sb".into(),
            owner: "test".into(),
            task: "test".into(),
            layers: vec!["../evil-module".into()],
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: vec![],
        };
        let result = create_sandbox(&mgr, &config, params).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid module") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_snapshot_invalid_label() {
        let mgr = SandboxManager::new(0);
        mgr.insert(make_ready_sandbox("snap-test")).unwrap();

        let result = snapshot_sandbox(&mgr, "snap-test", Some("../evil"), None).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid label") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_create_sandbox_module_not_found() {
        let mgr = SandboxManager::new(0);
        let dir = tempfile::tempdir().unwrap();
        let config = Config {
            data_dir: dir.path().to_path_buf(),
            ..Config::default()
        };

        // Create modules dir but don't put any modules in it
        std::fs::create_dir_all(config.modules_dir()).unwrap();
        std::fs::create_dir_all(config.sandboxes_dir()).unwrap();

        let params = CreateParams {
            id: "test-sb".into(),
            owner: "test".into(),
            task: "test".into(),
            layers: vec!["nonexistent-module".into()],
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: vec![],
        };

        let result = create_sandbox(&mgr, &config, params).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("module not found") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }

    #[tokio::test]
    async fn test_create_sandbox_duplicate() {
        let mgr = SandboxManager::new(0);
        mgr.insert(make_ready_sandbox("dupe")).unwrap();

        let config = Config::default();
        let params = CreateParams {
            id: "dupe".into(),
            owner: "test".into(),
            task: "test".into(),
            layers: vec!["000-base-alpine".into()],
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: vec![],
        };

        let result = create_sandbox(&mgr, &config, params).await;
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("already exists") || err.contains("requires Linux"),
            "err: {}",
            err
        );
    }
}
