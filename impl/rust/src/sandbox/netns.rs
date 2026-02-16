use std::fs;
use std::io::{self, BufRead};
use std::net::IpAddr;
use std::path::{Path, PathBuf};
use std::process::Command;

use tracing::{debug, info, warn};

/// Handle to a network namespace with veth pair, NAT, DNS forwarding, and egress rules.
///
/// Implements the full 12-step setup sequence from common.sh:327-383.
/// Drop cleans up all iptables rules, veth, and netns (common.sh:424-454).
pub struct NetnsHandle {
    /// Netns name, e.g. "squash-{id}"
    name: String,
    /// Subnet index 1-254 for 10.200.{index}.0/30
    index: u8,
    /// Host-side veth name, e.g. "sq-{id}-h"
    veth_host: String,
    /// Custom iptables chain name if egress rules were applied
    iptables_chain: Option<String>,
    /// Cached host DNS resolver for teardown of DNAT rules
    host_dns: Option<String>,
}

impl NetnsHandle {
    /// Full network namespace setup matching the shell implementation.
    ///
    /// # Arguments
    /// * `id` - Sandbox ID (used for naming netns, veth, iptables chain)
    /// * `data_dir` - Base data directory (for the .netns-index.lock file)
    /// * `sandboxes_dir` - Sandboxes directory (for scanning existing index allocations)
    /// * `meta_dir` - This sandbox's .meta/ directory (for writing netns metadata)
    /// * `allow_net` - Optional list of allowed hosts for egress filtering
    pub fn setup(
        id: &str,
        data_dir: &Path,
        sandboxes_dir: &Path,
        meta_dir: &Path,
        allow_net: Option<&[String]>,
    ) -> io::Result<Self> {
        let netns_name = format!("squash-{}", id);
        let veth_host = format!("sq-{}-h", id);
        let veth_sandbox = format!("sq-{}-s", id);

        // Step 1: Allocate unique subnet index under flock
        let index = allocate_netns_index(data_dir, sandboxes_dir)?;
        debug!(id, index, "allocated netns index");

        let subnet = Subnet::new(index);

        // Step 2: Create persistent network namespace
        run_cmd("ip", &["netns", "add", &netns_name])?;

        // From here on, if anything fails, we need to clean up what we've done.
        // We build the handle incrementally and rely on Drop for cleanup on error.
        let mut handle = NetnsHandle {
            name: netns_name.clone(),
            index,
            veth_host: veth_host.clone(),
            iptables_chain: None,
            host_dns: None,
        };

        // Step 3: Create veth pair
        if let Err(e) = run_cmd(
            "ip",
            &[
                "link", "add", &veth_host, "type", "veth", "peer", "name", &veth_sandbox,
            ],
        ) {
            return Err(e);
        }

        // Step 4: Move sandbox end into the namespace
        run_cmd("ip", &["link", "set", &veth_sandbox, "netns", &netns_name])?;

        // Step 5: Configure host end
        run_cmd(
            "ip",
            &[
                "addr",
                "add",
                &format!("{}/30", subnet.gateway),
                "dev",
                &veth_host,
            ],
        )?;
        run_cmd("ip", &["link", "set", &veth_host, "up"])?;

        // Step 6: Configure sandbox end (inside namespace)
        run_in_netns(
            &netns_name,
            "ip",
            &[
                "addr",
                "add",
                &format!("{}/30", subnet.sandbox),
                "dev",
                &veth_sandbox,
            ],
        )?;
        run_in_netns(&netns_name, "ip", &["link", "set", &veth_sandbox, "up"])?;
        run_in_netns(&netns_name, "ip", &["link", "set", "lo", "up"])?;
        run_in_netns(
            &netns_name,
            "ip",
            &["route", "add", "default", "via", &subnet.gateway],
        )?;

        // Step 7: Store metadata for cleanup and exec
        fs::write(meta_dir.join("netns_index"), index.to_string())?;
        fs::write(meta_dir.join("veth_host"), &veth_host)?;
        fs::write(meta_dir.join("veth_sandbox"), &veth_sandbox)?;
        fs::write(meta_dir.join("netns_name"), &netns_name)?;

        // Step 8: Enable IP forwarding
        let _ = fs::write("/proc/sys/net/ipv4/ip_forward", "1");

        // Step 9: NAT on host for outbound
        run_cmd(
            "iptables",
            &[
                "-t",
                "nat",
                "-A",
                "POSTROUTING",
                "-s",
                &subnet.cidr,
                "-j",
                "MASQUERADE",
            ],
        )?;

        // Step 10: DNS forwarding — redirect queries to gateway to host's real resolver
        let host_dns = parse_host_dns();
        if let Some(ref dns) = host_dns {
            // UDP
            run_cmd(
                "iptables",
                &[
                    "-t",
                    "nat",
                    "-A",
                    "PREROUTING",
                    "-s",
                    &subnet.cidr,
                    "-d",
                    &subnet.gateway,
                    "-p",
                    "udp",
                    "--dport",
                    "53",
                    "-j",
                    "DNAT",
                    "--to-destination",
                    dns,
                ],
            )?;
            // TCP
            run_cmd(
                "iptables",
                &[
                    "-t",
                    "nat",
                    "-A",
                    "PREROUTING",
                    "-s",
                    &subnet.cidr,
                    "-d",
                    &subnet.gateway,
                    "-p",
                    "tcp",
                    "--dport",
                    "53",
                    "-j",
                    "DNAT",
                    "--to-destination",
                    dns,
                ],
            )?;
        }
        handle.host_dns = host_dns;

        // Step 11: Egress filtering (if allow_net specified and non-empty)
        if let Some(hosts) = allow_net {
            if !hosts.is_empty() {
                apply_egress_rules(id, &veth_host, hosts)?;
                handle.iptables_chain = Some(format!("squash-{}", id));
            }
        }

        info!(id, index, netns = %netns_name, "network namespace ready");
        Ok(handle)
    }

    /// The netns name (e.g. "squash-{id}"), used by exec to enter the namespace.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// The subnet index (1-254).
    pub fn index(&self) -> u8 {
        self.index
    }

    /// The gateway IP for this sandbox (10.200.{index}.1).
    pub fn gateway_ip(&self) -> String {
        format!("10.200.{}.1", self.index)
    }

    /// The sandbox IP (10.200.{index}.2).
    pub fn sandbox_ip(&self) -> String {
        format!("10.200.{}.2", self.index)
    }

    /// The host-side veth interface name.
    pub fn veth_host(&self) -> &str {
        &self.veth_host
    }

    /// Re-associate with an existing namespace based on persisted metadata.
    pub fn from_recovered(
        id: &str,
        meta_dir: &Path,
        allow_net: Option<&[String]>,
    ) -> io::Result<Option<Self>> {
        let index = match fs::read_to_string(meta_dir.join("netns_index")) {
            Ok(raw) => match raw.trim().parse::<u8>() {
                Ok(i) if (1..=254).contains(&i) => i,
                _ => return Ok(None),
            },
            Err(_) => return Ok(None),
        };

        let name = fs::read_to_string(meta_dir.join("netns_name"))
            .ok()
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| format!("squash-{}", id));

        let veth_host = fs::read_to_string(meta_dir.join("veth_host"))
            .ok()
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| format!("sq-{}-h", id));

        let iptables_chain = allow_net.and_then(|hosts| {
            if hosts.is_empty() {
                None
            } else {
                Some(format!("squash-{}", id))
            }
        });

        Ok(Some(Self {
            name,
            index,
            veth_host,
            iptables_chain,
            host_dns: parse_host_dns(),
        }))
    }
}

impl Drop for NetnsHandle {
    fn drop(&mut self) {
        let id_chain = self
            .iptables_chain
            .as_deref()
            .unwrap_or("");
        let subnet = Subnet::new(self.index);

        // 1. Remove iptables egress chain (if applied)
        if let Some(ref chain) = self.iptables_chain {
            // Remove FORWARD jump
            let _ = run_cmd_ignore(
                "iptables",
                &["-D", "FORWARD", "-i", &self.veth_host, "-j", chain],
            );
            // Flush chain
            let _ = run_cmd_ignore("iptables", &["-F", chain]);
            // Delete chain
            let _ = run_cmd_ignore("iptables", &["-X", chain]);
        }
        let _ = id_chain; // suppress unused warning

        // 2. Remove NAT POSTROUTING
        let _ = run_cmd_ignore(
            "iptables",
            &[
                "-t",
                "nat",
                "-D",
                "POSTROUTING",
                "-s",
                &subnet.cidr,
                "-j",
                "MASQUERADE",
            ],
        );

        // 3. Remove DNS DNAT rules
        if let Some(ref dns) = self.host_dns {
            // UDP
            let _ = run_cmd_ignore(
                "iptables",
                &[
                    "-t",
                    "nat",
                    "-D",
                    "PREROUTING",
                    "-s",
                    &subnet.cidr,
                    "-d",
                    &subnet.gateway,
                    "-p",
                    "udp",
                    "--dport",
                    "53",
                    "-j",
                    "DNAT",
                    "--to-destination",
                    dns,
                ],
            );
            // TCP
            let _ = run_cmd_ignore(
                "iptables",
                &[
                    "-t",
                    "nat",
                    "-D",
                    "PREROUTING",
                    "-s",
                    &subnet.cidr,
                    "-d",
                    &subnet.gateway,
                    "-p",
                    "tcp",
                    "--dport",
                    "53",
                    "-j",
                    "DNAT",
                    "--to-destination",
                    dns,
                ],
            );
        }

        // 4. Delete host veth (peer auto-deletes)
        let _ = run_cmd_ignore("ip", &["link", "delete", &self.veth_host]);

        // 5. Delete network namespace
        let _ = run_cmd_ignore("ip", &["netns", "delete", &self.name]);

        info!(netns = %self.name, "network namespace cleaned up");
    }
}

// ── Subnet helper ────────────────────────────────────────────────────

/// Computed addresses for a 10.200.{index}.0/30 subnet.
struct Subnet {
    /// CIDR notation, e.g. "10.200.5.0/30"
    cidr: String,
    /// Gateway (host-side), e.g. "10.200.5.1"
    gateway: String,
    /// Sandbox address, e.g. "10.200.5.2"
    sandbox: String,
}

impl Subnet {
    fn new(index: u8) -> Self {
        Subnet {
            cidr: format!("10.200.{}.0/30", index),
            gateway: format!("10.200.{}.1", index),
            sandbox: format!("10.200.{}.2", index),
        }
    }
}

// ── Index allocation ─────────────────────────────────────────────────

/// Allocate a unique netns index (1-254) using flock-based synchronization.
///
/// Matches common.sh:309-325 (`_allocate_netns_index`).
fn allocate_netns_index(data_dir: &Path, sandboxes_dir: &Path) -> io::Result<u8> {
    use nix::fcntl::{flock, FlockArg};
    use std::os::unix::io::AsRawFd;

    let lock_path = data_dir.join(".netns-index.lock");
    let lock_file = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open(&lock_path)?;

    // Acquire exclusive flock
    flock(lock_file.as_raw_fd(), FlockArg::LockExclusive)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("flock failed: {}", e)))?;

    // Scan existing allocations
    let in_use = scan_allocated_indices(sandboxes_dir);

    // Find first free index 1-254
    for index in 1..=254u8 {
        if !in_use.contains(&index) {
            // flock is released when lock_file is dropped
            return Ok(index);
        }
    }

    Err(io::Error::new(
        io::ErrorKind::AddrNotAvailable,
        "no free netns index (all 254 slots in use)",
    ))
}

/// Scan all sandbox .meta/netns_index files to find which indices are in use.
fn scan_allocated_indices(sandboxes_dir: &Path) -> Vec<u8> {
    let mut indices = Vec::new();

    let entries = match fs::read_dir(sandboxes_dir) {
        Ok(e) => e,
        Err(_) => return indices,
    };

    for entry in entries.flatten() {
        let meta_file = entry.path().join(".meta").join("netns_index");
        if let Ok(content) = fs::read_to_string(&meta_file) {
            if let Ok(idx) = content.trim().parse::<u8>() {
                if (1..=254).contains(&idx) {
                    indices.push(idx);
                }
            }
        }
    }

    indices
}

// ── Egress filtering ─────────────────────────────────────────────────

/// Apply egress filtering rules matching common.sh:385-422.
fn apply_egress_rules(id: &str, iface: &str, hosts: &[String]) -> io::Result<()> {
    let chain = format!("squash-{}", id);

    // Create custom chain (ignore error if already exists)
    let _ = run_cmd_ignore("iptables", &["-N", &chain]);

    // Jump from FORWARD on this interface to our chain
    run_cmd("iptables", &["-A", "FORWARD", "-i", iface, "-j", &chain])?;

    // Block ICMP (prevents tunneling)
    run_cmd("iptables", &["-A", &chain, "-p", "icmp", "-j", "DROP"])?;

    // Rate-limited DNS (prevents DNS tunneling)
    // UDP
    run_cmd(
        "iptables",
        &[
            "-A",
            &chain,
            "-p",
            "udp",
            "--dport",
            "53",
            "-m",
            "limit",
            "--limit",
            "10/s",
            "--limit-burst",
            "20",
            "-j",
            "ACCEPT",
        ],
    )?;
    // TCP
    run_cmd(
        "iptables",
        &[
            "-A",
            &chain,
            "-p",
            "tcp",
            "--dport",
            "53",
            "-m",
            "limit",
            "--limit",
            "10/s",
            "--limit-burst",
            "20",
            "-j",
            "ACCEPT",
        ],
    )?;

    // Allow established/related connections
    run_cmd(
        "iptables",
        &[
            "-A",
            &chain,
            "-m",
            "state",
            "--state",
            "ESTABLISHED,RELATED",
            "-j",
            "ACCEPT",
        ],
    )?;

    // Allow each host pattern
    for host in hosts {
        match host.as_str() {
            "none" => {
                // Block everything — the default DROP at the end handles this
            }
            h if h.contains('*') => {
                // Wildcard: log warning (same as shell MVP behavior)
                warn!(host = h, "wildcard host requires proxy mode, not enforced");
            }
            h => {
                // Resolve and allow each IP
                match resolve_host(h) {
                    Ok(ips) => {
                        for ip in ips {
                            run_cmd(
                                "iptables",
                                &["-A", &chain, "-d", &ip.to_string(), "-j", "ACCEPT"],
                            )?;
                        }
                    }
                    Err(e) => {
                        warn!(host = h, error = %e, "failed to resolve host for egress rule");
                    }
                }
            }
        }
    }

    // Default: DROP
    run_cmd("iptables", &["-A", &chain, "-j", "DROP"])?;

    debug!(chain, "egress filtering rules applied");
    Ok(())
}

// ── DNS resolution helpers ───────────────────────────────────────────

/// Parse the first nameserver from /etc/resolv.conf.
fn parse_host_dns() -> Option<String> {
    let file = fs::File::open("/etc/resolv.conf").ok()?;
    let reader = io::BufReader::new(file);
    for line in reader.lines().flatten() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("nameserver") {
            let dns = rest.trim();
            if !dns.is_empty() {
                return Some(dns.to_string());
            }
        }
    }
    None
}

/// Resolve a hostname to IP addresses using getaddrinfo (matching `getent hosts`).
fn resolve_host(host: &str) -> io::Result<Vec<IpAddr>> {
    use std::net::ToSocketAddrs;

    // ToSocketAddrs requires a port; we use 0 and extract just the IPs
    let addrs: Vec<IpAddr> = format!("{}:0", host)
        .to_socket_addrs()?
        .map(|sa| sa.ip())
        .collect();

    if addrs.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("no addresses found for {}", host),
        ));
    }

    Ok(addrs)
}

// ── Command execution helpers ────────────────────────────────────────

/// Run a command, returning an error if it fails.
fn run_cmd(program: &str, args: &[&str]) -> io::Result<()> {
    debug!(cmd = %program, ?args, "exec");
    let output = Command::new(program).args(args).output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!(
                "{} {} failed ({}): {}",
                program,
                args.join(" "),
                output.status,
                stderr.trim()
            ),
        ));
    }
    Ok(())
}

/// Run a command inside a network namespace via `ip netns exec`.
fn run_in_netns(netns: &str, program: &str, args: &[&str]) -> io::Result<()> {
    let mut full_args = vec!["netns", "exec", netns, program];
    full_args.extend_from_slice(args);
    run_cmd("ip", &full_args)
}

/// Run a command, ignoring errors (for teardown). Logs failures at warn level.
fn run_cmd_ignore(program: &str, args: &[&str]) -> Result<(), ()> {
    let output = Command::new(program).args(args).output();
    match output {
        Ok(o) if !o.status.success() => {
            let stderr = String::from_utf8_lossy(&o.stderr);
            warn!(
                cmd = %program,
                ?args,
                stderr = %stderr.trim(),
                "teardown command failed (ignored)"
            );
            Err(())
        }
        Err(e) => {
            warn!(cmd = %program, ?args, error = %e, "teardown command failed (ignored)");
            Err(())
        }
        Ok(_) => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_subnet_addresses() {
        let s = Subnet::new(1);
        assert_eq!(s.cidr, "10.200.1.0/30");
        assert_eq!(s.gateway, "10.200.1.1");
        assert_eq!(s.sandbox, "10.200.1.2");

        let s = Subnet::new(254);
        assert_eq!(s.cidr, "10.200.254.0/30");
        assert_eq!(s.gateway, "10.200.254.1");
        assert_eq!(s.sandbox, "10.200.254.2");
    }

    #[test]
    fn test_scan_allocated_indices_empty_dir() {
        let tmp = tempdir_for_test();
        let indices = scan_allocated_indices(&tmp);
        assert!(indices.is_empty());
    }

    #[test]
    fn test_scan_allocated_indices_with_sandboxes() {
        let tmp = tempdir_for_test();

        // Create fake sandbox with netns_index
        let meta = tmp.join("sandbox-a").join(".meta");
        fs::create_dir_all(&meta).unwrap();
        fs::write(meta.join("netns_index"), "5").unwrap();

        let meta2 = tmp.join("sandbox-b").join(".meta");
        fs::create_dir_all(&meta2).unwrap();
        fs::write(meta2.join("netns_index"), "12").unwrap();

        // Sandbox without netns (no network)
        let meta3 = tmp.join("sandbox-c").join(".meta");
        fs::create_dir_all(&meta3).unwrap();

        let indices = scan_allocated_indices(&tmp);
        assert_eq!(indices.len(), 2);
        assert!(indices.contains(&5));
        assert!(indices.contains(&12));
    }

    #[test]
    fn test_scan_ignores_invalid_indices() {
        let tmp = tempdir_for_test();

        let meta = tmp.join("sandbox-bad").join(".meta");
        fs::create_dir_all(&meta).unwrap();
        fs::write(meta.join("netns_index"), "0").unwrap(); // out of range

        let meta2 = tmp.join("sandbox-bad2").join(".meta");
        fs::create_dir_all(&meta2).unwrap();
        fs::write(meta2.join("netns_index"), "garbage").unwrap();

        let meta3 = tmp.join("sandbox-ok").join(".meta");
        fs::create_dir_all(&meta3).unwrap();
        fs::write(meta3.join("netns_index"), "100").unwrap();

        let indices = scan_allocated_indices(&tmp);
        assert_eq!(indices, vec![100]);
    }

    #[test]
    fn test_allocate_index_sequential() {
        let tmp = tempdir_for_test();
        let sandboxes = tmp.join("sandboxes");
        fs::create_dir_all(&sandboxes).unwrap();

        // Occupy indices 1 and 2
        for idx in [1u8, 2] {
            let meta = sandboxes.join(format!("sb-{}", idx)).join(".meta");
            fs::create_dir_all(&meta).unwrap();
            fs::write(meta.join("netns_index"), idx.to_string()).unwrap();
        }

        let index = allocate_netns_index(&tmp, &sandboxes).unwrap();
        assert_eq!(index, 3); // first free
    }

    #[test]
    fn test_allocate_index_fills_gap() {
        let tmp = tempdir_for_test();
        let sandboxes = tmp.join("sandboxes");
        fs::create_dir_all(&sandboxes).unwrap();

        // Occupy 1 and 3 (gap at 2)
        for idx in [1u8, 3] {
            let meta = sandboxes.join(format!("sb-{}", idx)).join(".meta");
            fs::create_dir_all(&meta).unwrap();
            fs::write(meta.join("netns_index"), idx.to_string()).unwrap();
        }

        let index = allocate_netns_index(&tmp, &sandboxes).unwrap();
        assert_eq!(index, 2); // fills gap
    }

    #[test]
    fn test_parse_host_dns_from_resolv_conf() {
        // This test reads the actual /etc/resolv.conf on the system.
        // It may return None on systems without one, which is fine.
        let dns = parse_host_dns();
        // Just ensure it doesn't panic; value depends on the host
        if let Some(ref d) = dns {
            assert!(!d.is_empty());
        }
    }

    #[test]
    fn test_resolve_host_localhost() {
        let ips = resolve_host("localhost").unwrap();
        assert!(!ips.is_empty());
    }

    #[test]
    fn test_resolve_host_invalid() {
        let result = resolve_host("this.host.definitely.does.not.exist.example");
        assert!(result.is_err());
    }

    /// Create a temporary directory for tests.
    fn tempdir_for_test() -> PathBuf {
        let dir = std::env::temp_dir().join(format!("sq-netns-test-{}", uuid::Uuid::new_v4()));
        fs::create_dir_all(&dir).unwrap();
        dir
    }
}
