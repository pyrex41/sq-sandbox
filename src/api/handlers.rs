use std::path::Path;

use axum::extract::{Path as AxumPath, State};
use axum::http::StatusCode;
use axum::Json;
use tracing::info;

use crate::config::Backend;
use crate::modules;
use crate::sandbox::exec_types;
use crate::sandbox::lifecycle;
use crate::sandbox::meta;
use crate::sandbox::{Sandbox, SandboxError};
use crate::validate;

use super::models::*;
use super::AppState;

// ── Health ──────────────────────────────────────────────────────────

/// GET /cgi-bin/health
pub async fn health(State(state): State<AppState>) -> Json<HealthResponse> {
    let modules_dir = state.config.modules_dir();
    let mod_count = modules::list_modules(&modules_dir).len() as u64;
    let base_ready = modules::module_exists(&modules_dir, "000-base-alpine");
    let sb_count = state.sandbox_manager.len() as u64;

    let backend = match state.config.backend {
        Backend::Chroot => "chroot",
        Backend::Firecracker => "firecracker",
    };

    // Check tailscale status (best-effort)
    let (ts_status, ts_ip) = check_tailscale().await;

    Json(HealthResponse {
        status: "ok".to_string(),
        backend: backend.to_string(),
        tailscale: TailscaleStatus {
            status: ts_status,
            ip: ts_ip,
        },
        sandboxes: sb_count,
        modules: mod_count,
        base_ready,
    })
}

/// Best-effort tailscale status check.
async fn check_tailscale() -> (String, String) {
    match tokio::process::Command::new("tailscale")
        .args(["ip", "-4"])
        .output()
        .await
    {
        Ok(output) if output.status.success() => {
            let ip = String::from_utf8_lossy(&output.stdout).trim().to_string();
            if ip.is_empty() {
                ("off".to_string(), String::new())
            } else {
                ("connected".to_string(), ip)
            }
        }
        _ => ("off".to_string(), String::new()),
    }
}

// ── Modules ─────────────────────────────────────────────────────────

/// GET /cgi-bin/api/modules
pub async fn list_modules(State(state): State<AppState>) -> Json<Vec<ModuleInfo>> {
    let modules_dir = state.config.modules_dir();
    let mut result = Vec::new();

    // Local modules
    let entries = match std::fs::read_dir(&modules_dir) {
        Ok(entries) => entries,
        Err(_) => return Json(result),
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("squashfs") {
            if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                let size = std::fs::metadata(&path).map(|m| m.len()).unwrap_or(0);
                result.push(ModuleInfo {
                    name: name.to_string(),
                    size,
                    location: "local".to_string(),
                });
            }
        }
    }

    // TODO: Add S3 remote modules (remote-only, not already in local list)
    // This matches the shell list_modules() which merges local + S3.

    result.sort_by(|a, b| a.name.cmp(&b.name));
    Json(result)
}

// ── Sandboxes CRUD ──────────────────────────────────────────────────

/// GET /cgi-bin/api/sandboxes
pub async fn list_sandboxes(State(state): State<AppState>) -> Json<Vec<SandboxInfo>> {
    let ids = state.sandbox_manager.list_ids();
    let mut infos = Vec::new();

    for id in ids {
        let handle = match state.sandbox_manager.get(&id) {
            Ok(h) => h,
            Err(_) => continue,
        };
        let sandbox = handle.lock().await;
        infos.push(build_sandbox_info(&sandbox));
    }

    Json(infos)
}

/// POST /cgi-bin/api/sandboxes — returns 201 on success.
pub async fn create_sandbox(
    State(state): State<AppState>,
    Json(body): Json<CreateSandboxRequest>,
) -> Result<(StatusCode, Json<SandboxInfo>), ApiError> {
    if body.id.is_empty() {
        return Err(ApiError::bad_request("id required"));
    }
    if !validate::valid_id(&body.id) {
        return Err(ApiError::bad_request(
            "id: alphanumeric/dash/underscore only",
        ));
    }
    let modules_dir = state.config.modules_dir();
    for layer in &body.layers {
        if !modules::module_exists(&modules_dir, layer) {
            return Err(ApiError::bad_request(format!(
                "module not found: {}",
                layer
            )));
        }
    }

    let handle = lifecycle::create_sandbox(
        &state.sandbox_manager,
        &state.config,
        lifecycle::CreateParams {
            id: body.id.clone(),
            owner: body.owner,
            task: body.task,
            layers: body.layers,
            cpu: body.cpu,
            memory_mb: body.memory_mb,
            max_lifetime_s: body.max_lifetime_s,
            allow_net: body.allow_net.unwrap_or_default(),
        },
    )
    .await
    .map_err(sandbox_error_to_api)?;

    info!(sandbox_id = %body.id, "sandbox created");

    let sb = handle.lock().await;
    let info = build_sandbox_info(&sb);
    Ok((StatusCode::CREATED, Json(info)))
}

/// GET /cgi-bin/api/sandboxes/{id}
pub async fn get_sandbox(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
) -> Result<Json<SandboxInfo>, ApiError> {
    let handle = state
        .sandbox_manager
        .get(&id)
        .map_err(|_| ApiError::not_found(format!("not found: {}", id)))?;
    let sandbox = handle.lock().await;
    Ok(Json(build_sandbox_info(&sandbox)))
}

/// DELETE /cgi-bin/api/sandboxes/{id} — returns 204 with empty body.
pub async fn destroy_sandbox(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
) -> Result<StatusCode, ApiError> {
    lifecycle::destroy_sandbox(
        &state.sandbox_manager,
        &state.config,
        &id,
        state.s3.as_deref(),
    )
        .await
        .map_err(sandbox_error_to_api)?;

    info!(sandbox_id = %id, "sandbox destroyed");
    Ok(StatusCode::NO_CONTENT)
}

// ── Exec ────────────────────────────────────────────────────────────

/// POST /cgi-bin/api/sandboxes/{id}/exec
pub async fn exec_in_sandbox(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
    Json(body): Json<ExecRequest>,
) -> Result<Json<ExecResult>, ApiError> {
    // Validate cmd is present
    if body.cmd.is_empty() {
        return Err(ApiError::bad_request("cmd required"));
    }

    // Check sandbox exists
    let _handle = state
        .sandbox_manager
        .get(&id)
        .map_err(|_| ApiError::not_found(format!("not found: {}", id)))?;

    // TODO: Check mounted status once mount infrastructure is wired up
    // For now, delegate to lifecycle which handles state transitions

    let result = lifecycle::sandbox_exec(
        &state.sandbox_manager,
        &id,
        body.cmd,
        body.workdir,
        body.timeout,
    )
    .await
    .map_err(sandbox_error_to_api)?;

    Ok(Json(exec_result_to_api(result)))
}

// ── Activate Module ─────────────────────────────────────────────────

/// POST /cgi-bin/api/sandboxes/{id}/activate
pub async fn activate_module(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
    Json(body): Json<ActivateModuleRequest>,
) -> Result<Json<SandboxInfo>, ApiError> {
    // Validate module name
    if body.module.is_empty() {
        return Err(ApiError::bad_request("module required"));
    }
    if !validate::valid_module(&body.module) {
        return Err(ApiError::bad_request(
            "module: alphanumeric/dash/underscore/dot only",
        ));
    }

    // Check module exists
    let modules_dir = state.config.modules_dir();
    if !modules::module_exists(&modules_dir, &body.module) {
        return Err(ApiError::bad_request(format!(
            "module not found: {}",
            body.module
        )));
    }

    lifecycle::activate_module(
        &state.sandbox_manager,
        &state.config,
        &id,
        &body.module,
    )
    .await
    .map_err(sandbox_error_to_api)?;

    info!(sandbox_id = %id, module = %body.module, "module activated");

    let handle = state
        .sandbox_manager
        .get(&id)
        .map_err(|_| ApiError::not_found(format!("not found: {}", id)))?;
    let sb = handle.lock().await;
    Ok(Json(build_sandbox_info(&sb)))
}

// ── Snapshot ────────────────────────────────────────────────────────

/// POST /cgi-bin/api/sandboxes/{id}/snapshot
pub async fn snapshot_sandbox(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
    Json(body): Json<SnapshotRequest>,
) -> Result<Json<SnapshotResponse>, ApiError> {
    if let Some(ref label) = body.label {
        if !validate::valid_label(label) {
            return Err(ApiError::bad_request(
                "label: alphanumeric/dash/underscore/dot only",
            ));
        }
    }

    let (label, size) = lifecycle::snapshot_sandbox(
        &state.sandbox_manager,
        &id,
        body.label.as_deref(),
        state.s3.as_deref(),
    )
    .await
    .map_err(sandbox_error_to_api)?;

    info!(sandbox_id = %id, label = %label, size, "snapshot created");

    Ok(Json(SnapshotResponse {
        snapshot: label,
        size,
    }))
}

// ── Restore ─────────────────────────────────────────────────────────

/// POST /cgi-bin/api/sandboxes/{id}/restore
pub async fn restore_sandbox(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
    Json(body): Json<RestoreRequest>,
) -> Result<Json<SandboxInfo>, ApiError> {
    // Validate label
    if body.label.is_empty() {
        return Err(ApiError::bad_request("label required"));
    }
    if !validate::valid_label(&body.label) {
        return Err(ApiError::bad_request(
            "label: alphanumeric/dash/underscore/dot only",
        ));
    }

    lifecycle::restore_sandbox(
        &state.sandbox_manager,
        &state.config,
        &id,
        &body.label,
    )
    .await
    .map_err(sandbox_error_to_api)?;

    info!(sandbox_id = %id, label = %body.label, "snapshot restored");

    let handle = state
        .sandbox_manager
        .get(&id)
        .map_err(|_| ApiError::not_found(format!("not found: {}", id)))?;
    let sb = handle.lock().await;
    Ok(Json(build_sandbox_info(&sb)))
}

// ── Logs ────────────────────────────────────────────────────────────

/// GET /cgi-bin/api/sandboxes/{id}/logs
pub async fn get_logs(
    State(state): State<AppState>,
    AxumPath(id): AxumPath<String>,
) -> Result<Json<Vec<ExecResult>>, ApiError> {
    let handle = state
        .sandbox_manager
        .get(&id)
        .map_err(|_| ApiError::not_found(format!("not found: {}", id)))?;
    let sandbox = handle.lock().await;
    let sandbox_dir = sandbox.dir.clone();
    drop(sandbox);

    let logs = lifecycle::read_exec_logs(&sandbox_dir).map_err(sandbox_error_to_api)?;

    Ok(Json(logs.into_iter().map(exec_result_to_api).collect()))
}

// ── Helpers ─────────────────────────────────────────────────────────

/// Build a SandboxInfo response from a locked Sandbox.
fn build_sandbox_info(sb: &Sandbox) -> SandboxInfo {
    let meta_dir = sb.meta_dir();

    // Read snapshots from JSONL file
    let snapshots: Vec<SnapshotInfo> = meta::read_snapshots(&meta_dir)
        .into_iter()
        .map(|e| SnapshotInfo {
            label: e.label,
            created: e.created,
            size: e.size,
        })
        .collect();

    // Count exec log entries
    let log_dir = meta_dir.join("log");
    let exec_count = std::fs::read_dir(&log_dir)
        .map(|entries| {
            entries
                .flatten()
                .filter(|e| {
                    e.path()
                        .extension()
                        .and_then(|ext| ext.to_str())
                        == Some("json")
                })
                .count() as u64
        })
        .unwrap_or(0);

    // Compute upper_bytes (du -sb equivalent)
    let upper_data = sb.dir.join("upper").join("data");
    let upper_bytes = dir_size(&upper_data).unwrap_or(0);

    // Check if sandbox is mounted (overlay at merged/ exists in /proc/mounts)
    let mounted = is_mounted(&sb.merged_path());

    SandboxInfo {
        id: sb.id.clone(),
        owner: sb.metadata.owner.clone(),
        task: sb.metadata.task.clone(),
        layers: sb.metadata.layers.clone(),
        created: sb.metadata.created,
        last_active: sb.metadata.last_active,
        mounted,
        exec_count,
        upper_bytes,
        snapshots,
        active_snapshot: sb.metadata.active_snapshot.clone(),
        cpu: sb.metadata.cpu,
        memory_mb: sb.metadata.memory_mb,
        max_lifetime_s: sb.metadata.max_lifetime_s,
        allow_net: sb.metadata.allow_net.clone(),
    }
}

/// Check if a path is a mount point by reading /proc/mounts (Linux only).
fn is_mounted(path: &Path) -> bool {
    #[cfg(target_os = "linux")]
    {
        let path_str = path.to_string_lossy();
        std::fs::read_to_string("/proc/mounts")
            .map(|mounts| {
                mounts
                    .lines()
                    .any(|line| line.split_whitespace().nth(1) == Some(&path_str))
            })
            .unwrap_or(false)
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = path;
        false
    }
}

/// Recursively compute directory size in bytes (like `du -sb`).
fn dir_size(path: &Path) -> std::io::Result<u64> {
    let mut total = 0u64;
    if !path.exists() {
        return Ok(0);
    }
    for entry in std::fs::read_dir(path)? {
        let entry = entry?;
        let meta = entry.metadata()?;
        if meta.is_dir() {
            total += dir_size(&entry.path())?;
        } else {
            total += meta.len();
        }
    }
    Ok(total)
}

/// Convert a sandbox exec_types::ExecResult to an API ExecResult.
fn exec_result_to_api(r: exec_types::ExecResult) -> ExecResult {
    ExecResult {
        seq: r.seq as u64,
        cmd: r.cmd,
        workdir: r.workdir,
        exit_code: r.exit_code,
        started: r.started,
        finished: r.finished,
        stdout: r.stdout,
        stderr: r.stderr,
    }
}

/// Convert a SandboxError to an ApiError with wire-compatible status code and message.
fn sandbox_error_to_api(e: SandboxError) -> ApiError {
    match e {
        SandboxError::NotFound(id) => ApiError::not_found(format!("not found: {}", id)),
        SandboxError::AlreadyExists(_) => ApiError::bad_request(e.to_string()),
        SandboxError::LimitReached(max) => {
            ApiError::bad_request(format!("sandbox limit reached ({})", max))
        }
        SandboxError::InvalidId(_) => ApiError::bad_request(e.to_string()),
        SandboxError::InvalidLabel(_) => ApiError::bad_request(e.to_string()),
        SandboxError::InvalidModule(_) => ApiError::bad_request(e.to_string()),
        SandboxError::ModuleNotFound(m) => {
            ApiError::bad_request(format!("module not found: {}", m))
        }
        SandboxError::WrongState { expected, actual } => ApiError::bad_request(format!(
            "sandbox in wrong state: expected {}, got {}",
            expected, actual
        )),
        SandboxError::Mount(msg) if msg.contains("already active") => {
            ApiError::bad_request(msg)
        }
        SandboxError::Snapshot(msg) => ApiError::bad_request(msg),
        SandboxError::Exec(msg) if msg.contains("not mounted") => {
            ApiError::bad_request("not mounted".to_string())
        }
        _ => ApiError::internal(e.to_string()),
    }
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::AppState;
    use crate::config::Config;
    use crate::sandbox::manager::SandboxManager;
    use std::sync::Arc;

    fn test_state(data_dir: &Path) -> AppState {
        let mut config = Config::default();
        config.data_dir = data_dir.to_path_buf();
        config.auth_token = None;
        config.max_sandboxes = 100;
        AppState {
            config: Arc::new(config),
            sandbox_manager: Arc::new(SandboxManager::new(100)),
            s3: None,
        }
    }

    #[tokio::test]
    async fn test_health_endpoint() {
        let dir = tempfile::tempdir().unwrap();
        let state = test_state(dir.path());

        let response = health(State(state)).await;
        assert_eq!(response.0.status, "ok");
    }

    #[tokio::test]
    async fn test_list_modules_empty() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("modules")).unwrap();
        let state = test_state(dir.path());

        let response = list_modules(State(state)).await;
        assert!(response.0.is_empty());
    }

    #[tokio::test]
    async fn test_list_modules_with_files() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();
        std::fs::write(mods_dir.join("100-python312.squashfs"), b"fakefake").unwrap();

        let state = test_state(dir.path());
        let response = list_modules(State(state)).await;
        assert_eq!(response.0.len(), 2);
        assert_eq!(response.0[0].name, "000-base-alpine");
        assert_eq!(response.0[0].location, "local");
        assert_eq!(response.0[1].name, "100-python312");
    }

    #[tokio::test]
    async fn test_create_sandbox_missing_id() {
        let dir = tempfile::tempdir().unwrap();
        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: String::new(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };

        let result = create_sandbox(State(state), Json(body)).await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.status, StatusCode::BAD_REQUEST);
        assert_eq!(err.message, "id required");
    }

    #[tokio::test]
    async fn test_create_sandbox_invalid_id() {
        let dir = tempfile::tempdir().unwrap();
        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "../../etc".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };

        let result = create_sandbox(State(state), Json(body)).await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.status, StatusCode::BAD_REQUEST);
        assert!(err.message.contains("alphanumeric"));
    }

    #[tokio::test]
    async fn test_create_and_get_sandbox() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "test-1".into(),
            owner: "alice".into(),
            layers: vec!["000-base-alpine".into()],
            task: "run tests".into(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };

        let (status, Json(info)) =
            create_sandbox(State(state.clone()), Json(body)).await.unwrap();
        assert_eq!(status, StatusCode::CREATED);
        assert_eq!(info.id, "test-1");
        assert_eq!(info.owner, "alice");
        assert_eq!(info.task, "run tests");
        assert_eq!(info.layers, vec!["000-base-alpine"]);
        assert!(!info.mounted);
        assert_eq!(info.exec_count, 0);
        assert!(info.allow_net.is_none());

        // Get the same sandbox
        let Json(info2) = get_sandbox(State(state.clone()), AxumPath("test-1".into()))
            .await
            .unwrap();
        assert_eq!(info2.id, "test-1");
        assert_eq!(info2.owner, "alice");
    }

    #[tokio::test]
    async fn test_create_duplicate_sandbox() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "dup".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };

        create_sandbox(State(state.clone()), Json(body.clone()))
            .await
            .unwrap();
        let result = create_sandbox(State(state), Json(body)).await;
        assert!(result.is_err());
        // Shell returns 400 for "already exists"
        assert_eq!(result.unwrap_err().status, StatusCode::BAD_REQUEST);
    }

    #[tokio::test]
    async fn test_destroy_sandbox() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "to-delete".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        create_sandbox(State(state.clone()), Json(body)).await.unwrap();

        let status = destroy_sandbox(State(state.clone()), AxumPath("to-delete".into()))
            .await
            .unwrap();
        assert_eq!(status, StatusCode::NO_CONTENT);

        // Should be gone
        let result = get_sandbox(State(state), AxumPath("to-delete".into())).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().status, StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn test_destroy_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let state = test_state(dir.path());

        let result = destroy_sandbox(State(state), AxumPath("ghost".into())).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().status, StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn test_get_sandbox_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let state = test_state(dir.path());

        let result = get_sandbox(State(state), AxumPath("nonexistent".into())).await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.status, StatusCode::NOT_FOUND);
        assert!(err.message.contains("not found"));
    }

    #[tokio::test]
    async fn test_list_sandboxes_empty() {
        let dir = tempfile::tempdir().unwrap();
        let state = test_state(dir.path());

        let Json(list) = list_sandboxes(State(state)).await;
        assert!(list.is_empty());
    }

    #[tokio::test]
    async fn test_list_sandboxes_with_entries() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        for name in &["sb-a", "sb-b"] {
            let body = CreateSandboxRequest {
                id: name.to_string(),
                owner: "anon".into(),
                layers: vec!["000-base-alpine".into()],
                task: String::new(),
                cpu: 2.0,
                memory_mb: 1024,
                max_lifetime_s: 0,
                allow_net: None,
            };
            create_sandbox(State(state.clone()), Json(body))
                .await
                .unwrap();
        }

        let Json(list) = list_sandboxes(State(state)).await;
        assert_eq!(list.len(), 2);
    }

    #[tokio::test]
    async fn test_exec_cmd_required() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "exec-test".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        create_sandbox(State(state.clone()), Json(body)).await.unwrap();

        let exec_body = ExecRequest {
            cmd: String::new(),
            workdir: "/".into(),
            timeout: 300,
        };
        let result =
            exec_in_sandbox(State(state), AxumPath("exec-test".into()), Json(exec_body)).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().message, "cmd required");
    }

    #[tokio::test]
    async fn test_activate_module_validation() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "act-test".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        create_sandbox(State(state.clone()), Json(body)).await.unwrap();

        // Empty module name
        let result = activate_module(
            State(state.clone()),
            AxumPath("act-test".into()),
            Json(ActivateModuleRequest {
                module: String::new(),
            }),
        )
        .await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().message, "module required");

        // Invalid module name
        let result = activate_module(
            State(state.clone()),
            AxumPath("act-test".into()),
            Json(ActivateModuleRequest {
                module: "../../etc".into(),
            }),
        )
        .await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("alphanumeric"));

        // Module not found
        let result = activate_module(
            State(state.clone()),
            AxumPath("act-test".into()),
            Json(ActivateModuleRequest {
                module: "999-nonexistent".into(),
            }),
        )
        .await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("module not found"));

        // Already active
        let result = activate_module(
            State(state),
            AxumPath("act-test".into()),
            Json(ActivateModuleRequest {
                module: "000-base-alpine".into(),
            }),
        )
        .await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("already active"));
    }

    #[tokio::test]
    async fn test_snapshot_label_validation() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "snap-test".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        create_sandbox(State(state.clone()), Json(body)).await.unwrap();

        // Invalid label
        let result = snapshot_sandbox(
            State(state),
            AxumPath("snap-test".into()),
            Json(SnapshotRequest {
                label: Some("../../etc".into()),
            }),
        )
        .await;
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("alphanumeric"));
    }

    #[tokio::test]
    async fn test_restore_label_required() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "restore-test".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        create_sandbox(State(state.clone()), Json(body)).await.unwrap();

        let result = restore_sandbox(
            State(state),
            AxumPath("restore-test".into()),
            Json(RestoreRequest {
                label: String::new(),
            }),
        )
        .await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().message, "label required");
    }

    #[tokio::test]
    async fn test_get_logs_empty() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "logs-test".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        create_sandbox(State(state.clone()), Json(body)).await.unwrap();

        let Json(logs) = get_logs(State(state), AxumPath("logs-test".into()))
            .await
            .unwrap();
        assert!(logs.is_empty());
    }

    #[tokio::test]
    async fn test_create_sandbox_with_allow_net() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let state = test_state(dir.path());

        let body = CreateSandboxRequest {
            id: "net-test".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: Some(vec!["api.anthropic.com".into(), "pypi.org".into()]),
        };

        let (_, Json(info)) = create_sandbox(State(state), Json(body)).await.unwrap();
        assert_eq!(
            info.allow_net,
            Some(vec!["api.anthropic.com".into(), "pypi.org".into()])
        );
    }

    #[tokio::test]
    async fn test_sandbox_limit() {
        let dir = tempfile::tempdir().unwrap();
        let mods_dir = dir.path().join("modules");
        std::fs::create_dir_all(&mods_dir).unwrap();
        std::fs::write(mods_dir.join("000-base-alpine.squashfs"), b"fake").unwrap();

        let mut config = Config::default();
        config.data_dir = dir.path().to_path_buf();
        config.auth_token = None;
        config.max_sandboxes = 2;

        let state = AppState {
            config: Arc::new(config),
            sandbox_manager: Arc::new(SandboxManager::new(2)),
            s3: None,
        };

        for name in &["a", "b"] {
            let body = CreateSandboxRequest {
                id: name.to_string(),
                owner: "anon".into(),
                layers: vec!["000-base-alpine".into()],
                task: String::new(),
                cpu: 2.0,
                memory_mb: 1024,
                max_lifetime_s: 0,
                allow_net: None,
            };
            create_sandbox(State(state.clone()), Json(body))
                .await
                .unwrap();
        }

        let body = CreateSandboxRequest {
            id: "c".into(),
            owner: "anon".into(),
            layers: vec!["000-base-alpine".into()],
            task: String::new(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 0,
            allow_net: None,
        };
        let result = create_sandbox(State(state), Json(body)).await;
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.status, StatusCode::BAD_REQUEST);
        assert!(err.message.contains("sandbox limit reached"));
    }
}
