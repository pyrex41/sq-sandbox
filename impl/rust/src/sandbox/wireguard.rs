//! Kernel-level WireGuard support for chroot backend sandboxes.
//!
//! Creates a `wg0` interface inside the sandbox's network namespace using
//! the kernel wireguard module. Private keys never leave the sandbox; they
//! are written to a temp file, passed to `wg set`, and immediately deleted.
//!
//! For gVisor: falls back to userspace (not implemented here — document only).
//! For Firecracker: config is passed to guest via vsock/cloud-init.

use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;

use serde::{Deserialize, Serialize};
use tracing::{debug, info, warn};

/// A WireGuard peer configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WgPeer {
    pub public_key: String,
    #[serde(default)]
    pub endpoint: String,
    #[serde(default = "default_allowed_ips")]
    pub allowed_ips: String,
    #[serde(default)]
    pub preshared_key: Option<String>,
}

fn default_allowed_ips() -> String {
    "0.0.0.0/0".to_string()
}

/// Handle to a WireGuard interface inside a sandbox netns.
pub struct WgHandle {
    sandbox_id: String,
    netns: String,
    listen_port: u16,
}

impl WgHandle {
    /// Create and configure wg0 inside the sandbox's network namespace.
    ///
    /// # Security
    /// - Private key is written to a temp file with mode 0600 and deleted immediately after use.
    /// - Private key is never logged.
    pub fn setup(
        sandbox_id: &str,
        netns: &str,
        private_key: &str,
        listen_port: u16,
    ) -> io::Result<Self> {
        // Try to load the kernel module (non-fatal if already loaded)
        let _ = Command::new("modprobe").arg("wireguard").output();

        // Create wg0 inside the namespace
        run_in_netns(netns, "ip", &["link", "add", "wg0", "type", "wireguard"])?;

        // Write private key to a temp file
        let keyfile = std::env::temp_dir().join(format!("sq-wg-{}.key", sandbox_id));
        fs::write(&keyfile, private_key)?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            fs::set_permissions(&keyfile, fs::Permissions::from_mode(0o600))?;
        }

        // Configure wg0
        let result = run_in_netns(
            netns,
            "wg",
            &[
                "set",
                "wg0",
                "private-key",
                keyfile.to_str().unwrap(),
                "listen-port",
                &listen_port.to_string(),
            ],
        );

        // Always remove the key file
        let _ = fs::remove_file(&keyfile);
        result?;

        // Bring the interface up
        run_in_netns(netns, "ip", &["link", "set", "wg0", "up"])?;

        info!(sandbox_id, listen_port, "wg0 created in netns {}", netns);

        Ok(Self {
            sandbox_id: sandbox_id.to_string(),
            netns: netns.to_string(),
            listen_port,
        })
    }

    /// Add a peer to the wg0 interface.
    pub fn add_peer(&self, peer: &WgPeer) -> io::Result<()> {
        let mut args = vec![
            "set".to_string(),
            "wg0".to_string(),
            "peer".to_string(),
            peer.public_key.clone(),
            "allowed-ips".to_string(),
            peer.allowed_ips.clone(),
        ];

        if !peer.endpoint.is_empty() {
            args.push("endpoint".to_string());
            args.push(peer.endpoint.clone());
        }

        // Handle preshared key securely
        let psk_file = if let Some(ref psk) = peer.preshared_key {
            let path = std::env::temp_dir().join(format!("sq-wg-psk-{}.key", self.sandbox_id));
            fs::write(&path, psk)?;
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                fs::set_permissions(&path, fs::Permissions::from_mode(0o600))?;
            }
            args.push("preshared-key".to_string());
            args.push(path.to_str().unwrap().to_string());
            Some(path)
        } else {
            None
        };

        let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
        let result = run_in_netns(&self.netns, "wg", &args_ref);

        // Clean up PSK file
        if let Some(ref path) = psk_file {
            let _ = fs::remove_file(path);
        }
        result?;

        // Add routes for allowed IPs through wg0
        for cidr in peer.allowed_ips.split(',') {
            let cidr = cidr.trim();
            if !cidr.is_empty() {
                let _ = run_in_netns(
                    &self.netns,
                    "ip",
                    &["route", "add", cidr, "dev", "wg0"],
                );
            }
        }

        info!(
            sandbox_id = %self.sandbox_id,
            peer = %peer.public_key,
            allowed_ips = %peer.allowed_ips,
            "peer added"
        );
        Ok(())
    }

    /// Remove a peer from wg0.
    pub fn remove_peer(&self, public_key: &str) -> io::Result<()> {
        run_in_netns(
            &self.netns,
            "wg",
            &["set", "wg0", "peer", public_key, "remove"],
        )?;
        info!(sandbox_id = %self.sandbox_id, peer = public_key, "peer removed");
        Ok(())
    }

    /// Store WireGuard metadata (listen port only — never store private keys).
    pub fn write_metadata(&self, meta_dir: &Path) -> io::Result<()> {
        fs::write(
            meta_dir.join("wg_listen_port"),
            self.listen_port.to_string(),
        )
    }

    pub fn listen_port(&self) -> u16 {
        self.listen_port
    }
}

impl Drop for WgHandle {
    fn drop(&mut self) {
        let _ = run_in_netns(&self.netns, "ip", &["link", "delete", "wg0"]);
        debug!(sandbox_id = %self.sandbox_id, "wg0 cleaned up");
    }
}

/// Generate a WireGuard keypair. Returns (private_key, public_key).
pub fn generate_keypair() -> io::Result<(String, String)> {
    let priv_out = Command::new("wg").arg("genkey").output()?;
    if !priv_out.status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "wg genkey failed",
        ));
    }
    let privkey = String::from_utf8_lossy(&priv_out.stdout).trim().to_string();

    let mut pub_cmd = Command::new("wg");
    pub_cmd.arg("pubkey");
    pub_cmd.stdin(std::process::Stdio::piped());
    pub_cmd.stdout(std::process::Stdio::piped());

    let mut child = pub_cmd.spawn()?;
    if let Some(ref mut stdin) = child.stdin {
        use std::io::Write;
        stdin.write_all(privkey.as_bytes())?;
    }
    let output = child.wait_with_output()?;
    let pubkey = String::from_utf8_lossy(&output.stdout).trim().to_string();

    Ok((privkey, pubkey))
}

// ── Firecracker passthrough ────────────────────────────────────────

/// Generate a cloud-init/vsock-compatible WireGuard config snippet for Firecracker guests.
///
/// The guest kernel is expected to have wireguard.ko built-in. This config is
/// passed via vsock and applied by the guest agent.
pub fn firecracker_wg_config(
    private_key: &str,
    listen_port: u16,
    peers: &[WgPeer],
) -> String {
    let mut cfg = format!(
        "[Interface]\nPrivateKey = {}\nListenPort = {}\n",
        private_key, listen_port
    );

    for peer in peers {
        cfg.push_str(&format!("\n[Peer]\nPublicKey = {}\n", peer.public_key));
        if !peer.endpoint.is_empty() {
            cfg.push_str(&format!("Endpoint = {}\n", peer.endpoint));
        }
        cfg.push_str(&format!("AllowedIPs = {}\n", peer.allowed_ips));
        if let Some(ref psk) = peer.preshared_key {
            cfg.push_str(&format!("PresharedKey = {}\n", psk));
        }
    }

    cfg
}

// ── Helpers ────────────────────────────────────────────────────────

fn run_in_netns(netns: &str, program: &str, args: &[&str]) -> io::Result<()> {
    let mut full_args = vec!["netns", "exec", netns, program];
    full_args.extend_from_slice(args);
    let output = Command::new("ip").args(&full_args).output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!("{} {} failed: {}", program, args.join(" "), stderr.trim()),
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wg_peer_deserialize() {
        let json = r#"{"publicKey":"abc123","endpoint":"1.2.3.4:51820","allowedIPs":"10.0.0.0/24"}"#;
        let peer: WgPeer = serde_json::from_str(json).unwrap();
        assert_eq!(peer.public_key, "abc123");
        assert_eq!(peer.endpoint, "1.2.3.4:51820");
        assert_eq!(peer.allowed_ips, "10.0.0.0/24");
        assert!(peer.preshared_key.is_none());
    }

    #[test]
    fn test_wg_peer_defaults() {
        let json = r#"{"publicKey":"abc123"}"#;
        let peer: WgPeer = serde_json::from_str(json).unwrap();
        assert_eq!(peer.public_key, "abc123");
        assert_eq!(peer.endpoint, "");
        assert_eq!(peer.allowed_ips, "0.0.0.0/0");
    }

    #[test]
    fn test_wg_peer_with_psk() {
        let json = r#"{"publicKey":"abc","presharedKey":"psk123","allowedIPs":"10.0.0.0/8"}"#;
        let peer: WgPeer = serde_json::from_str(json).unwrap();
        assert_eq!(peer.preshared_key.as_deref(), Some("psk123"));
    }

    #[test]
    fn test_firecracker_wg_config() {
        let peers = vec![WgPeer {
            public_key: "PUBKEY".to_string(),
            endpoint: "1.2.3.4:51820".to_string(),
            allowed_ips: "10.0.0.0/24".to_string(),
            preshared_key: None,
        }];
        let config = firecracker_wg_config("PRIVKEY", 51820, &peers);
        assert!(config.contains("[Interface]"));
        assert!(config.contains("PrivateKey = PRIVKEY"));
        assert!(config.contains("ListenPort = 51820"));
        assert!(config.contains("[Peer]"));
        assert!(config.contains("PublicKey = PUBKEY"));
        assert!(config.contains("Endpoint = 1.2.3.4:51820"));
        assert!(config.contains("AllowedIPs = 10.0.0.0/24"));
    }

    #[test]
    fn test_firecracker_wg_config_multi_peer() {
        let peers = vec![
            WgPeer {
                public_key: "A".to_string(),
                endpoint: "".to_string(),
                allowed_ips: "10.0.0.0/24".to_string(),
                preshared_key: Some("PSK_A".to_string()),
            },
            WgPeer {
                public_key: "B".to_string(),
                endpoint: "5.6.7.8:51820".to_string(),
                allowed_ips: "10.0.1.0/24".to_string(),
                preshared_key: None,
            },
        ];
        let config = firecracker_wg_config("PRIV", 51821, &peers);
        assert!(config.contains("PresharedKey = PSK_A"));
        assert!(config.contains("PublicKey = B"));
        assert!(!config.contains("Endpoint = \n")); // empty endpoint not emitted for peer A
    }

    #[test]
    fn test_default_allowed_ips_fn() {
        assert_eq!(default_allowed_ips(), "0.0.0.0/0");
    }
}
