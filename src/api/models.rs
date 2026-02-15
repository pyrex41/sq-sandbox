use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::Json;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Deserializer, Serialize};

// ---------------------------------------------------------------------------
// Error response
// ---------------------------------------------------------------------------

/// Standard error response: `{"error": "message"}`.
#[derive(Debug, Serialize)]
pub struct ErrorResponse {
    pub error: String,
}

impl ErrorResponse {
    pub fn new(status: StatusCode, message: impl Into<String>) -> (StatusCode, Json<ErrorResponse>) {
        (
            status,
            Json(ErrorResponse {
                error: message.into(),
            }),
        )
    }
}

/// Convenience: an `ApiError` that can be returned from handlers with `?`.
#[derive(Debug)]
pub struct ApiError {
    pub status: StatusCode,
    pub message: String,
}

impl ApiError {
    pub fn bad_request(msg: impl Into<String>) -> Self {
        Self {
            status: StatusCode::BAD_REQUEST,
            message: msg.into(),
        }
    }

    pub fn not_found(msg: impl Into<String>) -> Self {
        Self {
            status: StatusCode::NOT_FOUND,
            message: msg.into(),
        }
    }

    pub fn internal(msg: impl Into<String>) -> Self {
        Self {
            status: StatusCode::INTERNAL_SERVER_ERROR,
            message: msg.into(),
        }
    }
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let (status, body) = ErrorResponse::new(self.status, self.message);
        (status, body).into_response()
    }
}

// ---------------------------------------------------------------------------
// Create sandbox request
// ---------------------------------------------------------------------------

/// POST /cgi-bin/api/sandboxes request body.
///
/// Only `id` is required. Everything else has defaults matching the shell API.
#[derive(Debug, Clone, Deserialize)]
pub struct CreateSandboxRequest {
    pub id: String,
    #[serde(default = "default_owner")]
    pub owner: String,
    #[serde(default = "default_layers", deserialize_with = "deserialize_layers")]
    pub layers: Vec<String>,
    #[serde(default)]
    pub task: String,
    #[serde(default = "default_cpu")]
    pub cpu: f64,
    #[serde(default = "default_memory_mb")]
    pub memory_mb: u64,
    #[serde(default)]
    pub max_lifetime_s: u64,
    #[serde(default)]
    pub allow_net: Option<Vec<String>>,
}

fn default_owner() -> String {
    "anon".to_string()
}

fn default_layers() -> Vec<String> {
    vec!["000-base-alpine".to_string()]
}

fn default_cpu() -> f64 {
    2.0
}

fn default_memory_mb() -> u64 {
    1024
}

/// Accept layers as either a comma-separated string or a JSON array.
fn deserialize_layers<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum StringOrVec {
        String(String),
        Vec(Vec<String>),
    }

    match StringOrVec::deserialize(deserializer)? {
        StringOrVec::String(s) if s.is_empty() => Ok(default_layers()),
        StringOrVec::String(s) => Ok(s.split(',').map(|l| l.trim().to_string()).collect()),
        StringOrVec::Vec(v) if v.is_empty() => Ok(default_layers()),
        StringOrVec::Vec(v) => Ok(v),
    }
}

// ---------------------------------------------------------------------------
// Sandbox info response
// ---------------------------------------------------------------------------

/// SandboxInfo — returned from GET/POST sandbox endpoints.
#[derive(Debug, Serialize, Clone)]
pub struct SandboxInfo {
    pub id: String,
    pub owner: String,
    pub task: String,
    pub layers: Vec<String>,
    pub created: DateTime<Utc>,
    pub last_active: DateTime<Utc>,
    pub mounted: bool,
    pub exec_count: u64,
    pub upper_bytes: u64,
    pub snapshots: Vec<SnapshotInfo>,
    pub active_snapshot: Option<String>,
    pub cpu: f64,
    pub memory_mb: u64,
    pub max_lifetime_s: u64,
    /// null when no restrictions (allow all), otherwise a list of allowed hosts.
    pub allow_net: Option<Vec<String>>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SnapshotInfo {
    pub label: String,
    pub created: DateTime<Utc>,
    pub size: u64,
}

// ---------------------------------------------------------------------------
// Exec request / result
// ---------------------------------------------------------------------------

/// POST /cgi-bin/api/sandboxes/{id}/exec request body.
#[derive(Debug, Deserialize)]
pub struct ExecRequest {
    pub cmd: String,
    #[serde(default = "default_workdir")]
    pub workdir: String,
    #[serde(default = "default_timeout")]
    pub timeout: u64,
}

fn default_workdir() -> String {
    "/".to_string()
}

fn default_timeout() -> u64 {
    300
}

/// Exec result — returned from the exec endpoint and read from log files.
#[derive(Debug, Serialize, Deserialize)]
pub struct ExecResult {
    pub seq: u64,
    pub cmd: String,
    pub workdir: String,
    pub exit_code: i32,
    pub started: DateTime<Utc>,
    pub finished: DateTime<Utc>,
    pub stdout: String,
    pub stderr: String,
}

// ---------------------------------------------------------------------------
// Module activation
// ---------------------------------------------------------------------------

/// POST /cgi-bin/api/sandboxes/{id}/activate request body.
#[derive(Debug, Deserialize)]
pub struct ActivateModuleRequest {
    pub module: String,
}

// ---------------------------------------------------------------------------
// Snapshot / Restore
// ---------------------------------------------------------------------------

/// POST /cgi-bin/api/sandboxes/{id}/snapshot request body.
#[derive(Debug, Deserialize)]
pub struct SnapshotRequest {
    pub label: Option<String>,
}

/// Snapshot response.
#[derive(Debug, Serialize)]
pub struct SnapshotResponse {
    pub snapshot: String,
    pub size: u64,
}

/// POST /cgi-bin/api/sandboxes/{id}/restore request body.
#[derive(Debug, Deserialize)]
pub struct RestoreRequest {
    pub label: String,
}

// ---------------------------------------------------------------------------
// Health
// ---------------------------------------------------------------------------

/// GET /cgi-bin/health response.
#[derive(Debug, Serialize)]
pub struct HealthResponse {
    pub status: String,
    pub backend: String,
    pub tailscale: TailscaleStatus,
    pub sandboxes: u64,
    pub modules: u64,
    pub base_ready: bool,
}

#[derive(Debug, Serialize)]
pub struct TailscaleStatus {
    pub status: String,
    pub ip: String,
}

// ---------------------------------------------------------------------------
// Modules
// ---------------------------------------------------------------------------

/// Module info — returned from GET /cgi-bin/api/modules.
#[derive(Debug, Serialize)]
pub struct ModuleInfo {
    pub name: String,
    pub size: u64,
    pub location: String,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_request_defaults() {
        let json = r#"{"id": "test-1"}"#;
        let req: CreateSandboxRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.id, "test-1");
        assert_eq!(req.owner, "anon");
        assert_eq!(req.layers, vec!["000-base-alpine"]);
        assert_eq!(req.task, "");
        assert_eq!(req.cpu, 2.0);
        assert_eq!(req.memory_mb, 1024);
        assert_eq!(req.max_lifetime_s, 0);
        assert!(req.allow_net.is_none());
    }

    #[test]
    fn test_create_request_layers_string() {
        let json = r#"{"id": "x", "layers": "000-base-alpine,100-python312"}"#;
        let req: CreateSandboxRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.layers, vec!["000-base-alpine", "100-python312"]);
    }

    #[test]
    fn test_create_request_layers_array() {
        let json = r#"{"id": "x", "layers": ["000-base-alpine", "100-python312"]}"#;
        let req: CreateSandboxRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.layers, vec!["000-base-alpine", "100-python312"]);
    }

    #[test]
    fn test_create_request_all_fields() {
        let json = r#"{
            "id": "dev",
            "owner": "alice",
            "layers": "000-base-alpine",
            "task": "run tests",
            "cpu": 1.0,
            "memory_mb": 512,
            "max_lifetime_s": 1800,
            "allow_net": ["api.anthropic.com", "pypi.org"]
        }"#;
        let req: CreateSandboxRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.owner, "alice");
        assert_eq!(req.cpu, 1.0);
        assert_eq!(req.memory_mb, 512);
        assert_eq!(req.max_lifetime_s, 1800);
        assert_eq!(req.allow_net, Some(vec!["api.anthropic.com".into(), "pypi.org".into()]));
    }

    #[test]
    fn test_exec_request_defaults() {
        let json = r#"{"cmd": "echo hello"}"#;
        let req: ExecRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.cmd, "echo hello");
        assert_eq!(req.workdir, "/");
        assert_eq!(req.timeout, 300);
    }

    #[test]
    fn test_error_response_shape() {
        let err = ErrorResponse {
            error: "not found".to_string(),
        };
        let json = serde_json::to_string(&err).unwrap();
        assert_eq!(json, r#"{"error":"not found"}"#);
    }

    #[test]
    fn test_snapshot_request_optional_label() {
        let json = r#"{}"#;
        let req: SnapshotRequest = serde_json::from_str(json).unwrap();
        assert!(req.label.is_none());

        let json = r#"{"label": "checkpoint-1"}"#;
        let req: SnapshotRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.label.unwrap(), "checkpoint-1");
    }

    #[test]
    fn test_layers_empty_string_uses_default() {
        let json = r#"{"id": "x", "layers": ""}"#;
        let req: CreateSandboxRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.layers, vec!["000-base-alpine"]);
    }

    #[test]
    fn test_layers_empty_array_uses_default() {
        let json = r#"{"id": "x", "layers": []}"#;
        let req: CreateSandboxRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.layers, vec!["000-base-alpine"]);
    }
}
