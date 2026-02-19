//! Firecracker VM backend for sandbox execution.
//!
//! Shells out to `sq-firecracker` CLI for VM lifecycle and uses `socat` for
//! vsock-based command execution. This mirrors the CL and Janet implementations.

use std::fs;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use tracing::{debug, warn};

use crate::sandbox::exec_types::ExecResult;
use crate::sandbox::SandboxError;

/// Allocate a unique CID for a Firecracker VM using a file-based counter with flock.
///
/// The counter file lives at `{data_dir}/.fc-cid-counter`. CIDs start at 3
/// (0-2 are reserved by vsock spec). File locking ensures thread-safety across
/// concurrent allocations.
pub fn allocate_cid(data_dir: &Path) -> Result<u32, SandboxError> {
    use std::io::{Read, Seek, SeekFrom};

    let counter_path = data_dir.join(".fc-cid-counter");

    let file = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&counter_path)?;

    // Acquire exclusive lock
    nix::fcntl::flock(
        std::os::unix::io::AsRawFd::as_raw_fd(&file),
        nix::fcntl::FlockArg::LockExclusive,
    )
    .map_err(|e| SandboxError::Io(std::io::Error::new(std::io::ErrorKind::Other, e)))?;

    let mut file = file;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let current: u32 = contents.trim().parse().unwrap_or(2);
    let next = current + 1;

    file.seek(SeekFrom::Start(0))?;
    file.set_len(0)?;
    write!(file, "{}", next)?;

    // Lock released when file is dropped
    Ok(next)
}

/// Set up a tap device and NAT rules for a Firecracker VM.
///
/// IP scheme: 10.0.{index}.1/30 (host side), 10.0.{index}.2 (guest side).
/// Uses a tap device named `sq-{id}-tap`.
pub fn setup_network(
    id: &str,
    index: u8,
    allow_net: Option<&[String]>,
) -> Result<(), SandboxError> {
    let tap_name = format!("sq-{}-tap", id);
    let host_ip = format!("10.0.{}.1/30", index);
    let guest_subnet = format!("10.0.{}.0/30", index);

    // Create tap device
    run_cmd("ip", &["tuntap", "add", "dev", &tap_name, "mode", "tap"])?;
    run_cmd("ip", &["addr", "add", &host_ip, "dev", &tap_name])?;
    run_cmd("ip", &["link", "set", &tap_name, "up"])?;

    // Enable IP forwarding
    run_cmd("sysctl", &["-w", "net.ipv4.ip_forward=1"])?;

    // NAT masquerade for guest traffic
    run_cmd(
        "iptables",
        &[
            "-t", "nat", "-A", "POSTROUTING",
            "-s", &guest_subnet,
            "-j", "MASQUERADE",
        ],
    )?;

    // If allow_net is specified, restrict egress to those hosts only
    if let Some(hosts) = allow_net {
        // Default: drop forwarded traffic from this subnet
        run_cmd(
            "iptables",
            &[
                "-A", "FORWARD",
                "-s", &guest_subnet,
                "-j", "DROP",
            ],
        )?;

        // Allow traffic to specified hosts (insert before the DROP)
        for host in hosts {
            run_cmd(
                "iptables",
                &[
                    "-I", "FORWARD",
                    "-s", &guest_subnet,
                    "-d", host,
                    "-j", "ACCEPT",
                ],
            )?;
        }

        // Allow established/related return traffic
        run_cmd(
            "iptables",
            &[
                "-I", "FORWARD",
                "-d", &guest_subnet,
                "-m", "state", "--state", "ESTABLISHED,RELATED",
                "-j", "ACCEPT",
            ],
        )?;
    }

    debug!(sandbox_id = %id, tap = %tap_name, "firecracker network configured");
    Ok(())
}

/// Tear down tap device and NAT rules for a Firecracker VM.
pub fn teardown_network(id: &str, index: u8) -> Result<(), SandboxError> {
    let tap_name = format!("sq-{}-tap", id);
    let guest_subnet = format!("10.0.{}.0/30", index);

    // Remove NAT rule (ignore errors - may already be gone)
    let _ = run_cmd(
        "iptables",
        &[
            "-t", "nat", "-D", "POSTROUTING",
            "-s", &guest_subnet,
            "-j", "MASQUERADE",
        ],
    );

    // Remove any FORWARD rules for this subnet (best-effort)
    let _ = run_cmd(
        "iptables",
        &["-D", "FORWARD", "-s", &guest_subnet, "-j", "DROP"],
    );

    // Delete tap device
    let _ = run_cmd("ip", &["link", "del", &tap_name]);

    debug!(sandbox_id = %id, tap = %tap_name, "firecracker network torn down");
    Ok(())
}

/// Start a Firecracker VM via the `sq-firecracker` CLI.
///
/// Writes the CID and PID to the meta directory for later reference.
pub fn start_vm(
    id: &str,
    cpu: f64,
    memory_mb: u64,
    squashfs_paths: &[PathBuf],
    cid: u32,
    meta_dir: &Path,
) -> Result<(), SandboxError> {
    let mut cmd = Command::new("sq-firecracker");
    cmd.args(["start", id, &cpu.to_string(), &memory_mb.to_string()]);

    for path in squashfs_paths {
        cmd.arg(path);
    }

    cmd.env("FC_CID", cid.to_string());

    let output = cmd
        .output()
        .map_err(|e| SandboxError::Exec(format!("failed to run sq-firecracker start: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(SandboxError::Exec(format!(
            "sq-firecracker start failed ({}): {}",
            output.status, stderr
        )));
    }

    // Write CID to metadata for exec lookups
    fs::write(meta_dir.join("fc_cid"), cid.to_string())?;

    debug!(sandbox_id = %id, cid, "firecracker VM started");
    Ok(())
}

/// Stop a Firecracker VM via the `sq-firecracker` CLI.
pub fn stop_vm(id: &str, meta_dir: &Path) -> Result<(), SandboxError> {
    let output = Command::new("sq-firecracker")
        .args(["stop", id])
        .output()
        .map_err(|e| SandboxError::Exec(format!("failed to run sq-firecracker stop: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        warn!(sandbox_id = %id, stderr = %stderr, "sq-firecracker stop returned non-zero");
    }

    // Clean up CID file
    let _ = fs::remove_file(meta_dir.join("fc_cid"));

    debug!(sandbox_id = %id, "firecracker VM stopped");
    Ok(())
}

/// Execute a command inside a Firecracker VM via vsock + socat.
///
/// Sends a JSON request to the guest agent listening on vsock port 5000:
/// `{"cmd":"...","workdir":"...","timeout":N}`
///
/// Parses the JSON response:
/// `{"exit_code":N,"stdout":"...","stderr":"..."}`
pub fn exec_vsock(
    cid: u32,
    cmd: &str,
    workdir: &str,
    timeout_s: u64,
) -> Result<ExecResult, SandboxError> {
    let request = serde_json::json!({
        "cmd": cmd,
        "workdir": workdir,
        "timeout": timeout_s,
    });
    let request_json = serde_json::to_string(&request)
        .map_err(|e| SandboxError::Exec(format!("failed to serialize exec request: {}", e)))?;

    let vsock_addr = format!("VSOCK-CONNECT:{}:5000", cid);
    let timeout_arg = format!("-T{}", timeout_s + 5); // socat timeout slightly longer

    let mut child = Command::new("socat")
        .args([&timeout_arg, "-", &vsock_addr])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| SandboxError::Exec(format!("failed to spawn socat: {}", e)))?;

    // Write request JSON to stdin
    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(request_json.as_bytes())
            .map_err(|e| SandboxError::Exec(format!("failed to write to socat stdin: {}", e)))?;
        // stdin is dropped here, closing the pipe
    }

    let output = child
        .wait_with_output()
        .map_err(|e| SandboxError::Exec(format!("socat wait failed: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(SandboxError::Exec(format!(
            "socat failed ({}): {}",
            output.status, stderr
        )));
    }

    let response_str = String::from_utf8_lossy(&output.stdout);

    #[derive(serde::Deserialize)]
    struct VsockResponse {
        exit_code: i32,
        stdout: String,
        stderr: String,
        #[serde(default)]
        seq: Option<u32>,
    }

    let resp: VsockResponse = serde_json::from_str(&response_str).map_err(|e| {
        SandboxError::Exec(format!(
            "failed to parse vsock response: {} (raw: {})",
            e, response_str
        ))
    })?;

    let now = chrono::Utc::now();
    Ok(ExecResult {
        seq: resp.seq.unwrap_or(0),
        cmd: cmd.to_string(),
        workdir: workdir.to_string(),
        exit_code: resp.exit_code,
        started: now,
        finished: now,
        stdout: resp.stdout,
        stderr: resp.stderr,
    })
}

/// Hot-add a drive to a running Firecracker VM and trigger guest remount.
///
/// Uses the sq-firecracker CLI to patch in the drive, then sends a
/// `__squash_remount` command via vsock to tell the guest agent to remount.
pub fn add_drive(
    id: &str,
    drive_id: &str,
    squashfs_path: &Path,
    cid: u32,
    meta_dir: &Path,
) -> Result<(), SandboxError> {
    let _ = meta_dir; // reserved for future metadata updates

    let output = Command::new("sq-firecracker")
        .args([
            "add-drive",
            id,
            drive_id,
            &squashfs_path.to_string_lossy(),
        ])
        .output()
        .map_err(|e| SandboxError::Exec(format!("failed to run sq-firecracker add-drive: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(SandboxError::Exec(format!(
            "sq-firecracker add-drive failed ({}): {}",
            output.status, stderr
        )));
    }

    // Tell the guest agent to remount
    exec_vsock(cid, "__squash_remount", "/", 30)?;

    debug!(sandbox_id = %id, drive_id, "drive added and guest remounted");
    Ok(())
}

/// Read the CID from a sandbox's metadata directory.
pub fn read_cid(meta_dir: &Path) -> Result<u32, SandboxError> {
    let cid_path = meta_dir.join("fc_cid");
    let raw = fs::read_to_string(&cid_path).map_err(|e| {
        SandboxError::Meta(format!("failed to read fc_cid: {}", e))
    })?;
    raw.trim()
        .parse::<u32>()
        .map_err(|e| SandboxError::Meta(format!("invalid fc_cid value: {}", e)))
}

// ── Helpers ─────────────────────────────────────────────────────────

/// Run a command, returning an error if it fails.
fn run_cmd(program: &str, args: &[&str]) -> Result<(), SandboxError> {
    let output = Command::new(program)
        .args(args)
        .output()
        .map_err(|e| SandboxError::Exec(format!("failed to run {} {:?}: {}", program, args, e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(SandboxError::Exec(format!(
            "{} {:?} failed ({}): {}",
            program, args, output.status, stderr
        )));
    }

    Ok(())
}
