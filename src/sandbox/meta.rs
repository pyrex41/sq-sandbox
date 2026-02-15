use std::fs;
use std::io;
use std::path::Path;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use tracing::warn;

// ── Metadata struct ─────────────────────────────────────────────────

/// All metadata for a sandbox, corresponding to the flat files in .meta/.
///
/// The shell implementation stores each field as a separate plain-text file.
/// We read/write individual files to maintain compatibility with shell-created
/// sandboxes (the Rust version must be able to remount them).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SandboxMetadata {
    pub owner: String,
    pub task: String,
    pub layers: Vec<String>,
    pub created: DateTime<Utc>,
    pub last_active: DateTime<Utc>,
    pub cpu: f64,
    pub memory_mb: u64,
    pub max_lifetime_s: u64,
    /// Allowed egress hosts. None = unrestricted, Some(vec) = filtered.
    pub allow_net: Option<Vec<String>>,
    /// Currently active snapshot label (if any).
    pub active_snapshot: Option<String>,
    /// Network namespace index (1-254), if networking is configured.
    pub netns_index: Option<u8>,
}

/// A single snapshot entry from snapshots.jsonl.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotEntry {
    pub label: String,
    pub created: DateTime<Utc>,
    pub size: u64,
}

// ── Write ───────────────────────────────────────────────────────────

/// Write all metadata files to a .meta/ directory.
///
/// Creates the directory if it doesn't exist. Each field is written as a
/// separate plain-text file to match the shell implementation's format.
pub fn write_metadata(meta_dir: &Path, meta: &SandboxMetadata) -> io::Result<()> {
    fs::create_dir_all(meta_dir)?;

    fs::write(meta_dir.join("owner"), &meta.owner)?;
    fs::write(meta_dir.join("task"), &meta.task)?;
    fs::write(meta_dir.join("layers"), meta.layers.join(","))?;
    fs::write(meta_dir.join("created"), meta.created.to_rfc3339())?;
    fs::write(meta_dir.join("last_active"), meta.last_active.to_rfc3339())?;
    fs::write(meta_dir.join("cpu"), meta.cpu.to_string())?;
    fs::write(meta_dir.join("memory_mb"), meta.memory_mb.to_string())?;
    fs::write(meta_dir.join("max_lifetime_s"), meta.max_lifetime_s.to_string())?;

    if let Some(ref hosts) = meta.allow_net {
        let json = serde_json::to_string(hosts)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        fs::write(meta_dir.join("allow_net"), json)?;
    }

    if let Some(ref label) = meta.active_snapshot {
        fs::write(meta_dir.join("active_snapshot"), label)?;
    }

    if let Some(index) = meta.netns_index {
        fs::write(meta_dir.join("netns_index"), index.to_string())?;
    }

    Ok(())
}

/// Update the last_active timestamp.
pub fn touch_last_active(meta_dir: &Path) -> io::Result<()> {
    fs::write(meta_dir.join("last_active"), Utc::now().to_rfc3339())
}

/// Write or clear the active_snapshot label.
pub fn write_active_snapshot(meta_dir: &Path, label: Option<&str>) -> io::Result<()> {
    let path = meta_dir.join("active_snapshot");
    match label {
        Some(l) => fs::write(&path, l),
        None => {
            if path.exists() {
                fs::remove_file(&path)?;
            }
            Ok(())
        }
    }
}

/// Append a snapshot entry to snapshots.jsonl.
pub fn append_snapshot_entry(meta_dir: &Path, entry: &SnapshotEntry) -> io::Result<()> {
    use std::io::Write;

    let path = meta_dir.join("snapshots.jsonl");
    let line = serde_json::to_string(entry)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)?;
    writeln!(file, "{}", line)?;
    Ok(())
}

// ── Read ────────────────────────────────────────────────────────────

/// Read all metadata from a .meta/ directory.
///
/// Reads each plain-text file and parses it into the struct. Missing optional
/// fields (allow_net, active_snapshot, netns_index) are treated as None.
pub fn read_metadata(meta_dir: &Path) -> io::Result<SandboxMetadata> {
    let owner = read_trimmed(meta_dir, "owner")?;
    let task = read_trimmed(meta_dir, "task")?;

    let layers_raw = read_trimmed(meta_dir, "layers")?;
    let layers: Vec<String> = layers_raw.split(',').map(|s| s.trim().to_string()).collect();

    let created = parse_datetime(&read_trimmed(meta_dir, "created")?)?;
    let last_active = parse_datetime(&read_trimmed(meta_dir, "last_active")?)?;

    let cpu = read_trimmed(meta_dir, "cpu")?
        .parse::<f64>()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("bad cpu: {}", e)))?;

    let memory_mb = read_trimmed(meta_dir, "memory_mb")?
        .parse::<u64>()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("bad memory_mb: {}", e)))?;

    let max_lifetime_s = read_trimmed(meta_dir, "max_lifetime_s")?
        .parse::<u64>()
        .map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("bad max_lifetime_s: {}", e),
            )
        })?;

    let allow_net = read_optional(meta_dir, "allow_net")
        .and_then(|raw| {
            serde_json::from_str::<Vec<String>>(&raw)
                .map_err(|e| {
                    warn!(error = %e, "failed to parse allow_net JSON, treating as unrestricted");
                    e
                })
                .ok()
        });

    let active_snapshot = read_optional(meta_dir, "active_snapshot");

    let netns_index = read_optional(meta_dir, "netns_index")
        .and_then(|raw| raw.parse::<u8>().ok())
        .filter(|idx| (1..=254).contains(idx));

    Ok(SandboxMetadata {
        owner,
        task,
        layers,
        created,
        last_active,
        cpu,
        memory_mb,
        max_lifetime_s,
        allow_net,
        active_snapshot,
        netns_index,
    })
}

/// Read all snapshot entries from snapshots.jsonl.
pub fn read_snapshots(meta_dir: &Path) -> Vec<SnapshotEntry> {
    let path = meta_dir.join("snapshots.jsonl");
    let content = match fs::read_to_string(&path) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };

    content
        .lines()
        .filter(|l| !l.trim().is_empty())
        .filter_map(|line| {
            serde_json::from_str::<SnapshotEntry>(line)
                .map_err(|e| {
                    warn!(error = %e, line, "skipping malformed snapshots.jsonl entry");
                    e
                })
                .ok()
        })
        .collect()
}

// ── Helpers ─────────────────────────────────────────────────────────

/// Read a file and trim whitespace. Returns an error if the file is missing.
fn read_trimmed(meta_dir: &Path, name: &str) -> io::Result<String> {
    let path = meta_dir.join(name);
    let content = fs::read_to_string(&path)?;
    Ok(content.trim().to_string())
}

/// Read a file and trim whitespace. Returns None if the file is missing or empty.
fn read_optional(meta_dir: &Path, name: &str) -> Option<String> {
    let path = meta_dir.join(name);
    fs::read_to_string(&path)
        .ok()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
}

/// Parse an ISO 8601 / RFC 3339 datetime string.
fn parse_datetime(s: &str) -> io::Result<DateTime<Utc>> {
    DateTime::parse_from_rfc3339(s)
        .map(|dt| dt.with_timezone(&Utc))
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("bad datetime: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn test_dir() -> PathBuf {
        let dir = std::env::temp_dir().join(format!("sq-meta-test-{}", uuid::Uuid::new_v4()));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn sample_metadata() -> SandboxMetadata {
        SandboxMetadata {
            owner: "alice".into(),
            task: "run tests".into(),
            layers: vec![
                "000-base-alpine".into(),
                "100-python312".into(),
            ],
            created: "2026-02-14T10:00:00+00:00".parse().unwrap(),
            last_active: "2026-02-14T11:30:00+00:00".parse().unwrap(),
            cpu: 2.0,
            memory_mb: 1024,
            max_lifetime_s: 1800,
            allow_net: Some(vec!["api.anthropic.com".into(), "pypi.org".into()]),
            active_snapshot: Some("checkpoint-1".into()),
            netns_index: Some(5),
        }
    }

    #[test]
    fn test_write_and_read_metadata_roundtrip() {
        let dir = test_dir();
        let meta = sample_metadata();

        write_metadata(&dir, &meta).unwrap();
        let read_back = read_metadata(&dir).unwrap();

        assert_eq!(read_back.owner, "alice");
        assert_eq!(read_back.task, "run tests");
        assert_eq!(read_back.layers, vec!["000-base-alpine", "100-python312"]);
        assert_eq!(read_back.cpu, 2.0);
        assert_eq!(read_back.memory_mb, 1024);
        assert_eq!(read_back.max_lifetime_s, 1800);
        assert_eq!(
            read_back.allow_net,
            Some(vec!["api.anthropic.com".into(), "pypi.org".into()])
        );
        assert_eq!(read_back.active_snapshot, Some("checkpoint-1".into()));
        assert_eq!(read_back.netns_index, Some(5));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_optional_fields_absent() {
        let dir = test_dir();
        let meta = SandboxMetadata {
            owner: "bob".into(),
            task: "quick test".into(),
            layers: vec!["000-base-alpine".into()],
            created: Utc::now(),
            last_active: Utc::now(),
            cpu: 1.0,
            memory_mb: 512,
            max_lifetime_s: 0,
            allow_net: None,
            active_snapshot: None,
            netns_index: None,
        };

        write_metadata(&dir, &meta).unwrap();
        let read_back = read_metadata(&dir).unwrap();

        assert_eq!(read_back.allow_net, None);
        assert_eq!(read_back.active_snapshot, None);
        assert_eq!(read_back.netns_index, None);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_metadata_missing_dir() {
        let dir = PathBuf::from("/tmp/sq-meta-test-nonexistent-dir");
        assert!(read_metadata(&dir).is_err());
    }

    #[test]
    fn test_touch_last_active() {
        let dir = test_dir();
        let meta = sample_metadata();
        write_metadata(&dir, &meta).unwrap();

        let before = read_metadata(&dir).unwrap().last_active;
        std::thread::sleep(std::time::Duration::from_millis(10));
        touch_last_active(&dir).unwrap();
        let after = read_metadata(&dir).unwrap().last_active;

        assert!(after > before);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_write_active_snapshot() {
        let dir = test_dir();
        let meta = sample_metadata();
        write_metadata(&dir, &meta).unwrap();

        // Set a new label
        write_active_snapshot(&dir, Some("snap-2")).unwrap();
        let read_back = read_metadata(&dir).unwrap();
        assert_eq!(read_back.active_snapshot, Some("snap-2".into()));

        // Clear it
        write_active_snapshot(&dir, None).unwrap();
        let read_back = read_metadata(&dir).unwrap();
        assert_eq!(read_back.active_snapshot, None);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_snapshot_entries_roundtrip() {
        let dir = test_dir();
        fs::create_dir_all(&dir).unwrap();

        let entry1 = SnapshotEntry {
            label: "snap-1".into(),
            created: "2026-02-14T10:00:00+00:00".parse().unwrap(),
            size: 1024 * 1024,
        };
        let entry2 = SnapshotEntry {
            label: "snap-2".into(),
            created: "2026-02-14T11:00:00+00:00".parse().unwrap(),
            size: 2048 * 1024,
        };

        append_snapshot_entry(&dir, &entry1).unwrap();
        append_snapshot_entry(&dir, &entry2).unwrap();

        let entries = read_snapshots(&dir);
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].label, "snap-1");
        assert_eq!(entries[0].size, 1024 * 1024);
        assert_eq!(entries[1].label, "snap-2");
        assert_eq!(entries[1].size, 2048 * 1024);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_snapshots_missing_file() {
        let dir = test_dir();
        let entries = read_snapshots(&dir);
        assert!(entries.is_empty());
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_snapshots_malformed_lines() {
        let dir = test_dir();
        let path = dir.join("snapshots.jsonl");
        fs::write(
            &path,
            r#"{"label":"good","created":"2026-02-14T10:00:00+00:00","size":100}
not json at all
{"label":"also-good","created":"2026-02-14T11:00:00+00:00","size":200}
"#,
        )
        .unwrap();

        let entries = read_snapshots(&dir);
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].label, "good");
        assert_eq!(entries[1].label, "also-good");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_layers_comma_separated_format() {
        let dir = test_dir();
        let meta = sample_metadata();
        write_metadata(&dir, &meta).unwrap();

        // Verify the raw file format matches shell expectations
        let raw = fs::read_to_string(dir.join("layers")).unwrap();
        assert_eq!(raw, "000-base-alpine,100-python312");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_allow_net_json_format() {
        let dir = test_dir();
        let meta = sample_metadata();
        write_metadata(&dir, &meta).unwrap();

        // Verify the raw file is JSON array (shell compatibility)
        let raw = fs::read_to_string(dir.join("allow_net")).unwrap();
        let parsed: Vec<String> = serde_json::from_str(&raw).unwrap();
        assert_eq!(parsed, vec!["api.anthropic.com", "pypi.org"]);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_netns_index_range_validation() {
        let dir = test_dir();

        // Write an out-of-range index (0)
        fs::write(dir.join("netns_index"), "0").unwrap();
        let idx = read_optional(&dir, "netns_index")
            .and_then(|raw| raw.parse::<u8>().ok())
            .filter(|idx| (1..=254).contains(idx));
        assert_eq!(idx, None);

        // Write a valid index
        fs::write(dir.join("netns_index"), "42").unwrap();
        let idx = read_optional(&dir, "netns_index")
            .and_then(|raw| raw.parse::<u8>().ok())
            .filter(|idx| (1..=254).contains(idx));
        assert_eq!(idx, Some(42));

        let _ = fs::remove_dir_all(&dir);
    }
}
