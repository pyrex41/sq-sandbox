//! Execution request/result types shared across platforms.
//!
//! These are split from exec.rs so that API handlers can reference them
//! on non-Linux platforms (where the actual fork/exec code is unavailable).

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Maximum bytes captured from stdout or stderr (64 KiB, matching `head -c 65536` in common.sh).
pub const MAX_OUTPUT_BYTES: usize = 65_536;

/// Exit code returned when the command is killed due to timeout (matches coreutils `timeout`).
pub const TIMEOUT_EXIT_CODE: i32 = 124;

/// Request to execute a command inside a sandbox.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecRequest {
    /// Shell command string passed to `/bin/sh -c`.
    pub cmd: String,
    /// Working directory inside the chroot (default: "/").
    #[serde(default = "default_workdir")]
    pub workdir: String,
    /// Timeout in seconds (default: 300).
    #[serde(default = "default_timeout")]
    pub timeout: u64,
}

fn default_workdir() -> String {
    "/".to_string()
}

fn default_timeout() -> u64 {
    300
}

/// Result of executing a command inside a sandbox.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecResult {
    /// Execution sequence number (1-based).
    pub seq: u32,
    /// The command that was executed.
    pub cmd: String,
    /// Working directory used.
    pub workdir: String,
    /// Process exit code (124 = timeout).
    pub exit_code: i32,
    /// ISO 8601 timestamp when execution started.
    pub started: DateTime<Utc>,
    /// ISO 8601 timestamp when execution finished.
    pub finished: DateTime<Utc>,
    /// Stdout output, capped at 65536 bytes.
    pub stdout: String,
    /// Stderr output, capped at 65536 bytes.
    pub stderr: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exec_request_defaults() {
        let json = r#"{"cmd": "echo hello"}"#;
        let req: ExecRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.cmd, "echo hello");
        assert_eq!(req.workdir, "/");
        assert_eq!(req.timeout, 300);
    }

    #[test]
    fn test_exec_request_custom() {
        let json = r#"{"cmd": "ls -la", "workdir": "/tmp", "timeout": 60}"#;
        let req: ExecRequest = serde_json::from_str(json).unwrap();
        assert_eq!(req.cmd, "ls -la");
        assert_eq!(req.workdir, "/tmp");
        assert_eq!(req.timeout, 60);
    }

    #[test]
    fn test_exec_result_serialization() {
        let result = ExecResult {
            seq: 1,
            cmd: "echo hello".to_string(),
            workdir: "/".to_string(),
            exit_code: 0,
            started: Utc::now(),
            finished: Utc::now(),
            stdout: "hello\n".to_string(),
            stderr: String::new(),
        };
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"seq\":1"));
        assert!(json.contains("\"exit_code\":0"));

        // Round-trip
        let parsed: ExecResult = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.seq, 1);
        assert_eq!(parsed.exit_code, 0);
    }

    #[test]
    fn test_exec_result_json_field_names() {
        let result = ExecResult {
            seq: 42,
            cmd: "python3 -c \"print(42)\"".to_string(),
            workdir: "/home".to_string(),
            exit_code: 0,
            started: "2025-02-14T10:35:00+00:00".parse().unwrap(),
            finished: "2025-02-14T10:35:01+00:00".parse().unwrap(),
            stdout: "42\n".to_string(),
            stderr: String::new(),
        };
        let json: serde_json::Value = serde_json::to_value(&result).unwrap();
        // Shell writes: {seq, cmd, workdir, exit_code, started, finished, stdout, stderr}
        assert!(json.get("seq").is_some());
        assert!(json.get("cmd").is_some());
        assert!(json.get("workdir").is_some());
        assert!(json.get("exit_code").is_some());
        assert!(json.get("started").is_some());
        assert!(json.get("finished").is_some());
        assert!(json.get("stdout").is_some());
        assert!(json.get("stderr").is_some());
    }

    #[test]
    fn test_constants() {
        assert_eq!(TIMEOUT_EXIT_CODE, 124);
        assert_eq!(MAX_OUTPUT_BYTES, 65_536);
    }
}
