use std::io;
use std::process::Command;

use tracing::{debug, info};

/// Shell out to `sq-mkbase <base>` to build a base module.
///
/// The sq-mkbase script downloads a minirootfs, installs packages, and creates
/// a squashfs image in the modules directory. It remains as a shell script because
/// it runs once during setup, not on the hot path.
pub fn build_base(base: &str) -> io::Result<()> {
    info!("building base module: {}", base);

    let status = Command::new("sq-mkbase")
        .arg(base)
        .status()?;

    if status.success() {
        info!("base module built successfully: {}", base);
        Ok(())
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("sq-mkbase {} exited with status {}", base, status),
        ))
    }
}

/// Shell out to `sq-mkvm all` to provision Firecracker VM components.
///
/// Downloads the kernel, firecracker binary, and builds the guest rootfs.
pub fn build_vm_components() -> io::Result<()> {
    info!("provisioning Firecracker VM components");

    let status = Command::new("sq-mkvm")
        .arg("all")
        .status()?;

    if status.success() {
        info!("VM components provisioned successfully");
        Ok(())
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("sq-mkvm all exited with status {}", status),
        ))
    }
}

/// Shell out to `mksquashfs` to create a squashfs image.
///
/// Uses zstd compression if the kernel supports it, otherwise falls back to gzip.
/// This is used by snapshot creation.
pub fn mksquashfs(
    source_dir: &std::path::Path,
    output: &std::path::Path,
    use_zstd: bool,
) -> io::Result<()> {
    debug!(
        "mksquashfs {} -> {} (zstd={})",
        source_dir.display(),
        output.display(),
        use_zstd
    );

    let mut cmd = Command::new("mksquashfs");
    cmd.arg(source_dir).arg(output);

    if use_zstd {
        cmd.args(["-comp", "zstd", "-Xcompression-level", "3", "-b", "128K"]);
    } else {
        cmd.args(["-comp", "gzip", "-b", "256K"]);
    }

    cmd.args(["-noappend", "-quiet"]);

    let status = cmd.status()?;
    if status.success() {
        Ok(())
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("mksquashfs exited with status {}", status),
        ))
    }
}

/// Check if the kernel supports zstd compression for squashfs.
///
/// Reads /proc/config.gz or /boot/config-* to check for CONFIG_SQUASHFS_ZSTD=y.
pub fn kernel_supports_zstd() -> bool {
    // Try /proc/config.gz first (requires CONFIG_IKCONFIG_PROC)
    if let Ok(output) = Command::new("zcat")
        .arg("/proc/config.gz")
        .output()
    {
        if output.status.success() {
            let config = String::from_utf8_lossy(&output.stdout);
            return config.contains("CONFIG_SQUASHFS_ZSTD=y");
        }
    }

    // Fallback: check /boot/config-*
    if let Ok(entries) = std::fs::read_dir("/boot") {
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if name.starts_with("config-") {
                if let Ok(content) = std::fs::read_to_string(entry.path()) {
                    if content.contains("CONFIG_SQUASHFS_ZSTD=y") {
                        return true;
                    }
                }
            }
        }
    }

    false
}
