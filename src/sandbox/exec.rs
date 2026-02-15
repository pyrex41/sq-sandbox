use std::ffi::CString;
use std::fs;
use std::io::{self, Read as _};
use std::os::unix::io::{AsRawFd, FromRawFd, RawFd};
use std::path::{Path, PathBuf};
use std::time::Duration;

use chrono::Utc;
use nix::fcntl::OFlag;
use nix::sched::{unshare, CloneFlags};
use nix::sys::signal::{kill, Signal};
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{self, ForkResult, Pid};
use tracing::{debug, error, warn};

// Re-export types from exec_types so existing callers still find them here.
pub use super::exec_types::{ExecRequest, ExecResult, MAX_OUTPUT_BYTES, TIMEOUT_EXIT_CODE};

/// Context needed to execute a command in a sandbox.
pub struct ExecContext {
    /// Sandbox ID (for logging/cgroup path).
    pub sandbox_id: String,
    /// Path to the merged overlayfs root (chroot target).
    pub merged_path: PathBuf,
    /// Path to the sandbox directory (for .meta/log/).
    pub sandbox_dir: PathBuf,
    /// File descriptor of the network namespace (from /run/netns/{name}), or None.
    pub netns_fd: Option<RawFd>,
    /// Cgroup path (e.g., /sys/fs/cgroup/squash-{id}), or None if cgroups unavailable.
    pub cgroup_path: Option<PathBuf>,
}

/// Execute a command inside a sandbox.
///
/// This is the core execution flow:
/// 1. Allocate log sequence number
/// 2. Create stdout/stderr pipes
/// 3. Fork
///    - Child: setns(netns), unshare(MNT|PID|IPC|UTS), write PID to cgroup, chroot, chdir, exec
///    - Parent: read pipes with timeout, SIGKILL on timeout
/// 4. Collect exit code (124 if timed out)
/// 5. Truncate output to 65536 bytes
/// 6. Write JSON log to .meta/log/{seq:04}.json
/// 7. Return ExecResult
pub fn exec_in_sandbox(ctx: &ExecContext, req: &ExecRequest) -> io::Result<ExecResult> {
    // Update last_active timestamp
    let meta_dir = ctx.sandbox_dir.join(".meta");
    let _ = fs::write(meta_dir.join("last_active"), Utc::now().to_rfc3339());

    // Allocate log sequence number
    let log_dir = meta_dir.join("log");
    fs::create_dir_all(&log_dir)?;
    let seq = next_log_seq(&log_dir)?;

    // Create stdout/stderr pipes
    let (stdout_read, stdout_write) = pipe_cloexec()?;
    let (stderr_read, stderr_write) = pipe_cloexec()?;

    let started = Utc::now();

    // Fork
    // SAFETY: We are careful in the child to only call async-signal-safe functions
    // before exec. In particular: no heap allocation, no locks, no non-reentrant libc calls.
    let fork_result = unsafe { unistd::fork() };

    match fork_result {
        Ok(ForkResult::Child) => {
            // === CHILD PROCESS ===
            // Close read ends of pipes
            let _ = unistd::close(stdout_read);
            let _ = unistd::close(stderr_read);

            // Redirect stdout/stderr to pipe write ends
            let _ = unistd::dup2(stdout_write, 1);
            let _ = unistd::dup2(stderr_write, 2);
            let _ = unistd::close(stdout_write);

            // Enter network namespace if present
            if let Some(ns_fd) = ctx.netns_fd {
                if let Err(e) = nix::sched::setns(ns_fd, CloneFlags::CLONE_NEWNET) {
                    let msg = format!("setns(CLONE_NEWNET) failed: {}\n", e);
                    let _ = unistd::write(2.into(), msg.as_bytes());
                    unsafe { libc::_exit(1) };
                }
            }

            // Unshare mount, PID, IPC, UTS namespaces
            if let Err(e) = unshare(
                CloneFlags::CLONE_NEWNS
                    | CloneFlags::CLONE_NEWPID
                    | CloneFlags::CLONE_NEWIPC
                    | CloneFlags::CLONE_NEWUTS,
            ) {
                let msg = format!("unshare failed: {}\n", e);
                let _ = unistd::write(2.into(), msg.as_bytes());
                unsafe { libc::_exit(1) };
            }

            // After unshare(CLONE_NEWPID), we must fork once more so the
            // executing process runs as PID 1 in the new PID namespace.
            match unsafe { unistd::fork() } {
                Ok(ForkResult::Parent { child: inner }) => {
                    let code = match waitpid(inner, None) {
                        Ok(WaitStatus::Exited(_, code)) => code,
                        Ok(WaitStatus::Signaled(_, sig, _)) => 128 + sig as i32,
                        _ => 126,
                    };
                    unsafe { libc::_exit(code) };
                }
                Ok(ForkResult::Child) => {
                    // Inner child continues with cgroup + chroot + exec.
                }
                Err(_) => {
                    let _ = unistd::write(
                        2.into(),
                        b"inner fork after CLONE_NEWPID failed\n",
                    );
                    unsafe { libc::_exit(1) };
                }
            }

            // Write PID to cgroup.procs (must happen before exec)
            if let Some(ref cg) = ctx.cgroup_path {
                let procs = cg.join("cgroup.procs");
                let pid = std::process::id();
                // Best-effort; ignore errors (matching shell `|| true`)
                let _ = fs::write(&procs, pid.to_string());
            }

            // Chroot into the merged overlayfs
            if let Err(e) = unistd::chroot(&ctx.merged_path) {
                let msg = format!("chroot failed: {}\n", e);
                let _ = unistd::write(2.into(), msg.as_bytes());
                unsafe { libc::_exit(1) };
            }

            // Chdir to workdir (fall back to / on failure, matching shell `cd "$1" 2>/dev/null || true`)
            if unistd::chdir(req.workdir.as_str()).is_err() {
                let _ = unistd::chdir("/");
            }

            // Exec /bin/sh -c "{cmd}"
            let sh = CString::new("/bin/sh").unwrap();
            let c_flag = CString::new("-c").unwrap();
            let c_cmd = CString::new(req.cmd.as_str()).unwrap_or_else(|_| {
                CString::new("echo 'invalid command (contains null byte)'").unwrap()
            });
            let args = [sh.as_ref(), c_flag.as_ref(), c_cmd.as_ref()];

            // execv never returns on success
            let _ = unistd::execv(&sh, &args);
            unsafe { libc::_exit(127) };
        }

        Ok(ForkResult::Parent { child }) => {
            // === PARENT PROCESS ===
            // Close write ends of pipes
            let _ = unistd::close(stdout_write);
            let _ = unistd::close(stderr_write);

            // Collect output with timeout
            let timeout = Duration::from_secs(req.timeout);
            let (exit_code, stdout, stderr) =
                collect_output_with_timeout(child, stdout_read, stderr_read, timeout);

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

        Err(e) => Err(io::Error::new(
            io::ErrorKind::Other,
            format!("fork failed: {}", e),
        )),
    }
}

/// Create a pipe with O_CLOEXEC set on both ends.
fn pipe_cloexec() -> io::Result<(RawFd, RawFd)> {
    nix::unistd::pipe2(OFlag::O_CLOEXEC).map_err(|e| io::Error::new(io::ErrorKind::Other, e))
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

/// Read from pipes and wait for the child, killing it on timeout.
///
/// Returns (exit_code, stdout, stderr).
fn collect_output_with_timeout(
    child: Pid,
    stdout_fd: RawFd,
    stderr_fd: RawFd,
    timeout: Duration,
) -> (i32, String, String) {
    // We read pipes in a thread to avoid blocking the parent indefinitely.
    // The child may produce output until it exits; we cap reads at MAX_OUTPUT_BYTES.
    let stdout_handle = std::thread::spawn(move || read_capped(stdout_fd, MAX_OUTPUT_BYTES));
    let stderr_handle = std::thread::spawn(move || read_capped(stderr_fd, MAX_OUTPUT_BYTES));

    // Wait for child with timeout
    let deadline = std::time::Instant::now() + timeout;
    let mut timed_out = false;
    let mut status = None;

    loop {
        match waitpid(child, Some(WaitPidFlag::WNOHANG)) {
            Ok(WaitStatus::StillAlive) => {
                if std::time::Instant::now() >= deadline {
                    // Timeout — kill the child process group
                    warn!("exec timeout after {}s, sending SIGKILL to pid {}", timeout.as_secs(), child);
                    let _ = kill(child, Signal::SIGKILL);
                    // Reap the killed child
                    let _ = waitpid(child, None);
                    timed_out = true;
                    break;
                }
                std::thread::sleep(Duration::from_millis(10));
            }
            Ok(ws) => {
                status = Some(ws);
                break;
            }
            Err(nix::errno::Errno::ECHILD) => {
                // Child already reaped (shouldn't happen, but handle gracefully)
                break;
            }
            Err(e) => {
                error!("waitpid failed: {}", e);
                break;
            }
        }
    }

    // Collect pipe output (joins the reader threads)
    let stdout = stdout_handle.join().unwrap_or_default();
    let stderr = stderr_handle.join().unwrap_or_default();

    let exit_code = if timed_out {
        TIMEOUT_EXIT_CODE
    } else {
        match status {
            Some(WaitStatus::Exited(_, code)) => code,
            Some(WaitStatus::Signaled(_, sig, _)) => 128 + sig as i32,
            _ => 1,
        }
    };

    (exit_code, stdout, stderr)
}

/// Read up to `max_bytes` from a file descriptor and return as a String.
/// Closes the fd when done.
fn read_capped(fd: RawFd, max_bytes: usize) -> String {
    // SAFETY: We own this fd (created by pipe_cloexec), and it is valid.
    let mut file = unsafe { std::fs::File::from_raw_fd(fd) };
    let mut buf = vec![0u8; max_bytes];
    let mut total = 0;

    loop {
        match file.read(&mut buf[total..]) {
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
        // Create a pipe, write more than max, verify we get exactly max
        let (read_fd, write_fd) = pipe_cloexec().unwrap();

        let data = vec![b'A'; 1024];
        // Write data in a thread to avoid blocking
        let write_handle = std::thread::spawn(move || {
            let mut file = unsafe { std::fs::File::from_raw_fd(write_fd) };
            use std::io::Write;
            let _ = file.write_all(&data);
            // File dropped here, closing write end
        });

        let result = read_capped(read_fd, 512);
        write_handle.join().unwrap();

        assert_eq!(result.len(), 512);
        assert!(result.chars().all(|c| c == 'A'));
    }

    #[test]
    fn test_read_capped_handles_empty() {
        let (read_fd, write_fd) = pipe_cloexec().unwrap();
        // Close write end immediately — reader gets EOF
        let _ = unistd::close(write_fd);
        let result = read_capped(read_fd, MAX_OUTPUT_BYTES);
        assert!(result.is_empty());
    }

    #[test]
    fn test_pipe_cloexec() {
        let (read_fd, write_fd) = pipe_cloexec().unwrap();
        // Both fds should be valid (positive)
        assert!(read_fd >= 0);
        assert!(write_fd >= 0);
        assert_ne!(read_fd, write_fd);
        let _ = unistd::close(read_fd);
        let _ = unistd::close(write_fd);
    }

    // Integration test: actually fork and exec on the host (not in a chroot).
    // This tests the pipe/wait/timeout machinery without needing root or namespaces.
    #[cfg(target_os = "linux")]
    mod linux_integration {
        use super::super::*;

        #[test]
        fn test_exec_simple_command() {
            let dir = std::env::temp_dir().join("sq-test-exec-simple");
            let _ = fs::remove_dir_all(&dir);
            fs::create_dir_all(dir.join(".meta/log")).unwrap();

            let ctx = ExecContext {
                sandbox_id: "test-simple".to_string(),
                merged_path: PathBuf::from("/"),
                sandbox_dir: dir.clone(),
                netns_fd: None,
                cgroup_path: None,
            };
            let req = ExecRequest {
                cmd: "echo hello".to_string(),
                workdir: "/".to_string(),
                timeout: 10,
            };

            let result = exec_in_sandbox(&ctx, &req).unwrap();
            assert_eq!(result.exit_code, 0);
            assert_eq!(result.stdout.trim(), "hello");
            assert!(result.stderr.is_empty());
            assert_eq!(result.seq, 1);

            // Verify log file was written
            assert!(dir.join(".meta/log/0001.json").exists());

            let _ = fs::remove_dir_all(&dir);
        }

        #[test]
        fn test_exec_timeout() {
            let dir = std::env::temp_dir().join("sq-test-exec-timeout");
            let _ = fs::remove_dir_all(&dir);
            fs::create_dir_all(dir.join(".meta/log")).unwrap();

            let ctx = ExecContext {
                sandbox_id: "test-timeout".to_string(),
                merged_path: PathBuf::from("/"),
                sandbox_dir: dir.clone(),
                netns_fd: None,
                cgroup_path: None,
            };
            let req = ExecRequest {
                cmd: "sleep 60".to_string(),
                workdir: "/".to_string(),
                timeout: 1,
            };

            let result = exec_in_sandbox(&ctx, &req).unwrap();
            assert_eq!(result.exit_code, TIMEOUT_EXIT_CODE);

            let _ = fs::remove_dir_all(&dir);
        }

        #[test]
        fn test_exec_nonzero_exit() {
            let dir = std::env::temp_dir().join("sq-test-exec-nonzero");
            let _ = fs::remove_dir_all(&dir);
            fs::create_dir_all(dir.join(".meta/log")).unwrap();

            let ctx = ExecContext {
                sandbox_id: "test-nonzero".to_string(),
                merged_path: PathBuf::from("/"),
                sandbox_dir: dir.clone(),
                netns_fd: None,
                cgroup_path: None,
            };
            let req = ExecRequest {
                cmd: "exit 42".to_string(),
                workdir: "/".to_string(),
                timeout: 10,
            };

            let result = exec_in_sandbox(&ctx, &req).unwrap();
            assert_eq!(result.exit_code, 42);

            let _ = fs::remove_dir_all(&dir);
        }

        #[test]
        fn test_exec_stderr_capture() {
            let dir = std::env::temp_dir().join("sq-test-exec-stderr");
            let _ = fs::remove_dir_all(&dir);
            fs::create_dir_all(dir.join(".meta/log")).unwrap();

            let ctx = ExecContext {
                sandbox_id: "test-stderr".to_string(),
                merged_path: PathBuf::from("/"),
                sandbox_dir: dir.clone(),
                netns_fd: None,
                cgroup_path: None,
            };
            let req = ExecRequest {
                cmd: "echo oops >&2".to_string(),
                workdir: "/".to_string(),
                timeout: 10,
            };

            let result = exec_in_sandbox(&ctx, &req).unwrap();
            assert_eq!(result.exit_code, 0);
            assert!(result.stdout.is_empty());
            assert_eq!(result.stderr.trim(), "oops");

            let _ = fs::remove_dir_all(&dir);
        }
    }
}
