use std::collections::HashMap;
use std::io;
use std::path::Path;

use serde::Deserialize;
use tracing::{info, warn};

// ── Replaceable header allowlist ────────────────────────────────────
//
// Only replace placeholders in headers that carry credentials.
// This prevents accidental secret leakage into arbitrary headers.
// Must match the Go proxy exactly.

pub const REPLACEABLE_HEADERS: &[&str] = &[
    "authorization",
    "x-api-key",
    "api-key",
    "x-auth-token",
    "x-access-token",
    "proxy-authorization",
];

/// Returns true if the given header name (case-insensitive) is in the
/// replaceable allowlist.
pub fn is_replaceable_header(name: &str) -> bool {
    let lower = name.to_ascii_lowercase();
    REPLACEABLE_HEADERS.iter().any(|&h| h == lower)
}

// ── Secret types ────────────────────────────────────────────────────

#[derive(Debug, Clone, Deserialize)]
pub struct Secret {
    pub placeholder: String,
    pub value: String,
    pub allowed_hosts: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SecretsFile {
    pub secrets: HashMap<String, Secret>,
}

impl SecretsFile {
    /// Load secrets from a JSON file.
    ///
    /// Returns `Ok(None)` if the file does not exist (secrets are optional).
    /// Returns `Err` if the file exists but cannot be parsed.
    pub fn load(path: &Path) -> io::Result<Option<Self>> {
        let data = match std::fs::read_to_string(path) {
            Ok(d) => d,
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                info!(path = %path.display(), "no secrets.json found, proxy will run without secrets");
                return Ok(None);
            }
            Err(e) => return Err(e),
        };

        let sf: SecretsFile = serde_json::from_str(&data).map_err(|e| {
            io::Error::new(io::ErrorKind::InvalidData, format!("parse secrets.json: {e}"))
        })?;

        info!(count = sf.secrets.len(), "loaded secrets");
        Ok(Some(sf))
    }

    /// Check if a hostname appears in any secret's allowed_hosts list.
    pub fn host_allowed(&self, host: &str) -> bool {
        self.secrets
            .values()
            .any(|s| s.allowed_hosts.iter().any(|h| h == host))
    }

    /// Replace placeholder strings with real values in the given header value,
    /// but only if the destination host is in the secret's allowed_hosts.
    ///
    /// Returns the (possibly modified) value and whether any replacement was made.
    pub fn replace_in_value(&self, value: &str, host: &str) -> (String, bool) {
        let mut result = value.to_owned();
        let mut replaced = false;

        for (name, secret) in &self.secrets {
            if !result.contains(&secret.placeholder) {
                continue;
            }
            if secret.allowed_hosts.iter().any(|h| h == host) {
                result = result.replace(&secret.placeholder, &secret.value);
                info!(secret = %name, host, "replaced secret placeholder");
                replaced = true;
            } else {
                warn!(secret = %name, host, "blocked — host not in allowed_hosts");
            }
        }

        (result, replaced)
    }
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn sample_json() -> &'static str {
        r#"{
            "secrets": {
                "TEST_API_KEY": {
                    "placeholder": "sk-placeholder-test-12345",
                    "value": "sk-real-test-key-67890",
                    "allowed_hosts": ["httpbin.org"]
                },
                "OTHER_KEY": {
                    "placeholder": "tok-placeholder",
                    "value": "tok-real-value",
                    "allowed_hosts": ["api.example.com", "httpbin.org"]
                }
            }
        }"#
    }

    fn write_temp_secrets(json: &str) -> NamedTempFile {
        let mut f = NamedTempFile::new().unwrap();
        f.write_all(json.as_bytes()).unwrap();
        f
    }

    #[test]
    fn load_valid_secrets() {
        let f = write_temp_secrets(sample_json());
        let sf = SecretsFile::load(f.path()).unwrap().unwrap();
        assert_eq!(sf.secrets.len(), 2);
        assert_eq!(sf.secrets["TEST_API_KEY"].placeholder, "sk-placeholder-test-12345");
        assert_eq!(sf.secrets["TEST_API_KEY"].value, "sk-real-test-key-67890");
        assert_eq!(sf.secrets["TEST_API_KEY"].allowed_hosts, vec!["httpbin.org"]);
    }

    #[test]
    fn load_missing_file_returns_none() {
        let result = SecretsFile::load(Path::new("/nonexistent/secrets.json")).unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn load_invalid_json_returns_error() {
        let f = write_temp_secrets("not json");
        let result = SecretsFile::load(f.path());
        assert!(result.is_err());
    }

    #[test]
    fn host_allowed_checks_all_secrets() {
        let f = write_temp_secrets(sample_json());
        let sf = SecretsFile::load(f.path()).unwrap().unwrap();

        assert!(sf.host_allowed("httpbin.org"));
        assert!(sf.host_allowed("api.example.com"));
        assert!(!sf.host_allowed("evil.com"));
    }

    #[test]
    fn replace_in_value_for_allowed_host() {
        let f = write_temp_secrets(sample_json());
        let sf = SecretsFile::load(f.path()).unwrap().unwrap();

        let (result, replaced) =
            sf.replace_in_value("Bearer sk-placeholder-test-12345", "httpbin.org");
        assert!(replaced);
        assert_eq!(result, "Bearer sk-real-test-key-67890");
    }

    #[test]
    fn replace_in_value_blocked_for_wrong_host() {
        let f = write_temp_secrets(sample_json());
        let sf = SecretsFile::load(f.path()).unwrap().unwrap();

        let (result, replaced) =
            sf.replace_in_value("Bearer sk-placeholder-test-12345", "evil.com");
        assert!(!replaced);
        assert_eq!(result, "Bearer sk-placeholder-test-12345");
    }

    #[test]
    fn replace_in_value_no_match() {
        let f = write_temp_secrets(sample_json());
        let sf = SecretsFile::load(f.path()).unwrap().unwrap();

        let (result, replaced) = sf.replace_in_value("Bearer some-other-key", "httpbin.org");
        assert!(!replaced);
        assert_eq!(result, "Bearer some-other-key");
    }

    #[test]
    fn is_replaceable_header_case_insensitive() {
        assert!(is_replaceable_header("Authorization"));
        assert!(is_replaceable_header("authorization"));
        assert!(is_replaceable_header("AUTHORIZATION"));
        assert!(is_replaceable_header("X-Api-Key"));
        assert!(is_replaceable_header("x-api-key"));
        assert!(is_replaceable_header("Api-Key"));
        assert!(is_replaceable_header("X-Auth-Token"));
        assert!(is_replaceable_header("X-Access-Token"));
        assert!(is_replaceable_header("Proxy-Authorization"));
        assert!(!is_replaceable_header("Content-Type"));
        assert!(!is_replaceable_header("X-Custom-Header"));
    }
}
