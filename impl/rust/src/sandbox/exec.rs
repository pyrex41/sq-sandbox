use std::fs;
use std::io::{self, Read as _};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::Duration;

use chrono::Utc;
use tracing::{debug, error, warn};

// Re-export types from exec_types so existing callers still find them here.
pub use super::exec_types::{ExecRequest, ExecResult, MAX_OUTPUT_BYTES, TIMEOUT_EXIT_CODE};

/// Context needed to execute a command in a sandbox.
pub struct ExecContext {
    /// Sandbox ID (for logging).
    pub sandbox_id: String,
    /// Path to the merged overlayfs root (chroot target).
    pub merged_path: PathBuf,
    /// Path to the sandbox directory (for .meta/log/).
    pub sandbox_dir: PathBuf,
}

/// Execute a command inside a sandbox via sq-exec.
///
/// This is the core execution flow:
/// 1. Allocate log sequence number
/// 2. Spawn sq-exec with merged root, command, workdir, timeout
/// 3. Collect stdout/stderr with capping
/// 4. Get exit code
/// 5. Write JSON log to .meta/log/{seq:04}.json
/// 6. Return ExecResult
pub fn exec_in_sandbox(ctx: &ExecContext, req: &ExecRequest) -> io::Result<ExecResult> {
    // Update last_active timestamp
    let meta_dir = ctx.sandbox_dir.join(".meta");
    let _ = fs::write(meta_dir.join("last_active"), Utc::now().to_rfc3339());

    // Allocate log sequence number
    let log_dir = meta_dir.join("log");
    fs::create_dir_all(&log_dir)?;
    let seq = next_log_seq(&log_dir)?;

    let started = Utc::now();

    // Spawn sq-exec
    let mut child = Command::new("sq-exec")
        .arg(&ctx.merged_path)
        .arg(&req.cmd)
        .arg(&req.workdir)
        .arg(req.timeout.to_string())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("sq-exec spawn: {}", e)))?;

    // Read stdout/stderr in threads with capping
    let child_stdout = child.stdout.take();
    let child_stderr = child.stderr.take();

    let stdout_handle = std::thread::spawn(move || {
        match child_stdout {
            Some(reader) => read_capped_from_reader(reader, MAX_OUTPUT_BYTES),
            None => String::new(),
        }
    });

    let stderr_handle = std::thread::spawn(move || {
        match child_stderr {
            Some(reader) => read_capped_from_reader(reader, MAX_OUTPUT_BYTES),
            None => String::new(),
        }
    });

    // Wait for the process to complete
    let status = child.wait()?;

    let stdout = stdout_handle.join().unwrap_or_default();
    let stderr = stderr_handle.join().unwrap_or_default();

    let exit_code = status.code().unwrap_or(1);
    let finished = Utc::now();

    let result = ExecResult {
        seq,
        cmd: req.cmd.clone(),
        workdir: req.workdir.clone(),
        exit_code,
        started,
        finished,
        stdout,
        stderr,
    };

    // Write JSON log
    if let Err(e) = write_log_entry(&log_dir, &result) {
        error!("failed to write exec log for sandbox {}: {}", ctx.sandbox_id, e);
    }

    Ok(result)
}

/// Determine the next log sequence number by counting existing log files.
fn next_log_seq(log_dir: &Path) -> io::Result<u32> {
    let mut max_seq = 0u32;
    match fs::read_dir(log_dir) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let name = entry.file_name();
                let name = name.to_string_lossy();
                // Parse "0001.json" -> 1, "0042.json" -> 42
                if let Some(stem) = name.strip_suffix(".json") {
                    if let Ok(n) = stem.parse::<u32>() {
                        max_seq = max_seq.max(n);
                    }
                }
            }
        }
        Err(e) if e.kind() == io::ErrorKind::NotFound => {}
        Err(e) => return Err(e),
    }
    Ok(max_seq + 1)
}

/// Read up to `max_bytes` from a reader and return as a String.
fn read_capped_from_reader(mut reader: impl io::Read, max_bytes: usize) -> String {
    let mut buf = vec![0u8; max_bytes];
    let mut total = 0;

    loop {
        match reader.read(&mut buf[total..]) {
            Ok(0) => break,
            Ok(n) => {
                total += n;
                if total >= max_bytes {
                    break;
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(_) => break,
        }
    }

    buf.truncate(total);
    String::from_utf8_lossy(&buf).into_owned()
}

/// Write the execution log entry as JSON to .meta/log/{seq:04}.json.
fn write_log_entry(log_dir: &Path, result: &ExecResult) -> io::Result<()> {
    let path = log_dir.join(format!("{:04}.json", result.seq));
    let json = serde_json::to_string_pretty(result)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
    fs::write(&path, json)?;
    debug!("wrote exec log: {}", path.display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_log_seq_empty_dir() {
        let dir = std::env::temp_dir().join("sq-test-log-seq-empty");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        assert_eq!(next_log_seq(&dir).unwrap(), 1);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_next_log_seq_with_existing() {
        let dir = std::env::temp_dir().join("sq-test-log-seq-existing");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        fs::write(dir.join("0001.json"), "{}").unwrap();
        fs::write(dir.join("0002.json"), "{}").unwrap();
        fs::write(dir.join("0005.json"), "{}").unwrap();

        assert_eq!(next_log_seq(&dir).unwrap(), 6);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_next_log_seq_ignores_non_json() {
        let dir = std::env::temp_dir().join("sq-test-log-seq-nonjson");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        fs::write(dir.join("0001.json"), "{}").unwrap();
        fs::write(dir.join("notes.txt"), "test").unwrap();
        fs::write(dir.join(".hidden"), "test").unwrap();

        assert_eq!(next_log_seq(&dir).unwrap(), 2);
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_write_log_entry() {
        let dir = std::env::temp_dir().join("sq-test-write-log");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let result = ExecResult {
            seq: 3,
            cmd: "echo test".to_string(),
            workdir: "/".to_string(),
            exit_code: 0,
            started: Utc::now(),
            finished: Utc::now(),
            stdout: "test\n".to_string(),
            stderr: String::new(),
        };

        write_log_entry(&dir, &result).unwrap();

        let path = dir.join("0003.json");
        assert!(path.exists());

        let content = fs::read_to_string(&path).unwrap();
        let parsed: ExecResult = serde_json::from_str(&content).unwrap();
        assert_eq!(parsed.seq, 3);
        assert_eq!(parsed.cmd, "echo test");
        assert_eq!(parsed.stdout, "test\n");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_capped_limits_output() {
        let data = vec![b'A'; 1024];
        let cursor = std::io::Cursor::new(data);
        let result = read_capped_from_reader(cursor, 512);
        assert_eq!(result.len(), 512);
        assert!(result.chars().all(|c| c == 'A'));
    }

    #[test]
    fn test_read_capped_handles_empty() {
        let data: Vec<u8> = vec![];
        let cursor = std::io::Cursor::new(data);
        let result = read_capped_from_reader(cursor, MAX_OUTPUT_BYTES);
        assert!(result.is_empty());
    }
}
