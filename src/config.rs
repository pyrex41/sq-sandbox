use std::path::PathBuf;

/// Sandbox execution backend.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Backend {
    Chroot,
    Firecracker,
}

/// Top-level configuration loaded from environment variables.
#[derive(Debug, Clone)]
pub struct Config {
    pub backend: Backend,
    pub data_dir: PathBuf,
    pub port: u16,
    pub auth_token: Option<String>,
    pub s3_bucket: Option<String>,
    pub s3_endpoint: Option<String>,
    pub s3_region: String,
    pub s3_prefix: String,
    pub ephemeral: bool,
    pub upper_limit_mb: u64,
    pub max_sandboxes: usize,
    pub proxy_https: bool,
    pub tailscale_authkey: Option<String>,
    pub tailscale_hostname: String,
}

impl Config {
    /// Load configuration from environment variables with defaults matching the shell version.
    pub fn from_env() -> Self {
        let backend = match std::env::var("SQUASH_BACKEND").as_deref() {
            Ok("firecracker") => Backend::Firecracker,
            _ => Backend::Chroot,
        };

        let data_dir = PathBuf::from(
            std::env::var("SQUASH_DATA").unwrap_or_else(|_| "/data".to_string()),
        );

        let port = std::env::var("SQUASH_PORT")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(8080);

        let auth_token = std::env::var("SQUASH_AUTH_TOKEN")
            .ok()
            .filter(|s| !s.is_empty());

        let s3_bucket = std::env::var("SQUASH_S3_BUCKET")
            .ok()
            .filter(|s| !s.is_empty());

        let s3_endpoint = std::env::var("SQUASH_S3_ENDPOINT")
            .ok()
            .filter(|s| !s.is_empty());

        let s3_region = std::env::var("SQUASH_S3_REGION")
            .unwrap_or_else(|_| "us-east-1".to_string());

        let s3_prefix = std::env::var("SQUASH_S3_PREFIX").unwrap_or_default();

        let ephemeral = std::env::var("SQUASH_EPHEMERAL").as_deref() == Ok("1");

        let upper_limit_mb = std::env::var("SQUASH_UPPER_LIMIT_MB")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(512);

        let max_sandboxes = std::env::var("SQUASH_MAX_SANDBOXES")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(100);

        let proxy_https = std::env::var("SQUASH_PROXY_HTTPS").as_deref() == Ok("1");

        let tailscale_authkey = std::env::var("TAILSCALE_AUTHKEY")
            .ok()
            .filter(|s| !s.is_empty());

        let tailscale_hostname = std::env::var("TAILSCALE_HOSTNAME")
            .unwrap_or_else(|_| "squash".to_string());

        Self {
            backend,
            data_dir,
            port,
            auth_token,
            s3_bucket,
            s3_endpoint,
            s3_region,
            s3_prefix,
            ephemeral,
            upper_limit_mb,
            max_sandboxes,
            proxy_https,
            tailscale_authkey,
            tailscale_hostname,
        }
    }

    /// Path to modules directory: {data_dir}/modules
    pub fn modules_dir(&self) -> PathBuf {
        self.data_dir.join("modules")
    }

    /// Path to sandboxes directory: {data_dir}/sandboxes
    pub fn sandboxes_dir(&self) -> PathBuf {
        self.data_dir.join("sandboxes")
    }

    /// Path to secrets.json: {data_dir}/secrets.json
    pub fn secrets_path(&self) -> PathBuf {
        self.data_dir.join("secrets.json")
    }

    /// Path to proxy CA directory: {data_dir}/proxy-ca
    pub fn proxy_ca_dir(&self) -> PathBuf {
        self.data_dir.join("proxy-ca")
    }

    /// Path to VM directory: {data_dir}/vm
    pub fn vm_dir(&self) -> PathBuf {
        self.data_dir.join("vm")
    }

    /// Whether S3 is configured (bucket is set).
    pub fn s3_enabled(&self) -> bool {
        self.s3_bucket.is_some()
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            backend: Backend::Chroot,
            data_dir: PathBuf::from("/data"),
            port: 8080,
            auth_token: None,
            s3_bucket: None,
            s3_endpoint: None,
            s3_region: "us-east-1".to_string(),
            s3_prefix: String::new(),
            ephemeral: false,
            upper_limit_mb: 512,
            max_sandboxes: 100,
            proxy_https: false,
            tailscale_authkey: None,
            tailscale_hostname: "squash".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
        ENV_LOCK.lock().unwrap()
    }

    #[test]
    fn test_default_config() {
        let _env = env_lock();
        // Clear relevant env vars for a clean test
        for var in &[
            "SQUASH_BACKEND", "SQUASH_DATA", "SQUASH_PORT", "SQUASH_AUTH_TOKEN",
            "SQUASH_S3_BUCKET", "SQUASH_S3_ENDPOINT", "SQUASH_S3_REGION",
            "SQUASH_S3_PREFIX", "SQUASH_EPHEMERAL", "SQUASH_UPPER_LIMIT_MB",
            "SQUASH_MAX_SANDBOXES", "SQUASH_PROXY_HTTPS",
            "TAILSCALE_AUTHKEY", "TAILSCALE_HOSTNAME",
        ] {
            std::env::remove_var(var);
        }

        let cfg = Config::from_env();
        assert_eq!(cfg.backend, Backend::Chroot);
        assert_eq!(cfg.data_dir, PathBuf::from("/data"));
        assert_eq!(cfg.port, 8080);
        assert!(cfg.auth_token.is_none());
        assert!(cfg.s3_bucket.is_none());
        assert!(!cfg.ephemeral);
        assert_eq!(cfg.upper_limit_mb, 512);
        assert_eq!(cfg.max_sandboxes, 100);
        assert!(!cfg.proxy_https);
        assert_eq!(cfg.s3_region, "us-east-1");
    }

    #[test]
    fn test_derived_paths() {
        let _env = env_lock();
        let mut cfg = Config::from_env();
        cfg.data_dir = PathBuf::from("/tmp/test-data");

        assert_eq!(cfg.modules_dir(), PathBuf::from("/tmp/test-data/modules"));
        assert_eq!(cfg.sandboxes_dir(), PathBuf::from("/tmp/test-data/sandboxes"));
        assert_eq!(cfg.secrets_path(), PathBuf::from("/tmp/test-data/secrets.json"));
        assert_eq!(cfg.proxy_ca_dir(), PathBuf::from("/tmp/test-data/proxy-ca"));
        assert_eq!(cfg.vm_dir(), PathBuf::from("/tmp/test-data/vm"));
    }

    #[test]
    fn test_s3_enabled() {
        let _env = env_lock();
        let mut cfg = Config::from_env();
        cfg.s3_bucket = None;
        assert!(!cfg.s3_enabled());

        cfg.s3_bucket = Some("my-bucket".to_string());
        assert!(cfg.s3_enabled());
    }

    #[test]
    fn test_firecracker_backend() {
        let _env = env_lock();
        std::env::set_var("SQUASH_BACKEND", "firecracker");
        let cfg = Config::from_env();
        assert_eq!(cfg.backend, Backend::Firecracker);
        std::env::remove_var("SQUASH_BACKEND");
    }

    #[test]
    fn test_unknown_backend_defaults_to_chroot() {
        let _env = env_lock();
        std::env::set_var("SQUASH_BACKEND", "unknown-value");
        let cfg = Config::from_env();
        assert_eq!(cfg.backend, Backend::Chroot);
        std::env::remove_var("SQUASH_BACKEND");
    }

    /// NOTE: env-var tests are inherently racy when run in parallel since the
    /// process environment is global shared state.  We test each env var in its
    /// own test (below) rather than one big test so that the set/read/clear
    /// window is as narrow as possible.  For full confidence, run with
    /// `--test-threads=1`.

    #[test]
    fn test_custom_data_dir() {
        let _env = env_lock();
        std::env::set_var("SQUASH_DATA", "/tmp/custom");
        let cfg = Config::from_env();
        assert_eq!(cfg.data_dir, PathBuf::from("/tmp/custom"));
        std::env::remove_var("SQUASH_DATA");
    }

    #[test]
    fn test_custom_port() {
        let _env = env_lock();
        std::env::set_var("SQUASH_PORT", "9090");
        let cfg = Config::from_env();
        assert_eq!(cfg.port, 9090);
        std::env::remove_var("SQUASH_PORT");
    }

    #[test]
    fn test_custom_s3_fields() {
        let _env = env_lock();
        std::env::set_var("SQUASH_S3_BUCKET", "my-bucket");
        std::env::set_var("SQUASH_S3_ENDPOINT", "http://localhost:9000");
        std::env::set_var("SQUASH_S3_REGION", "eu-west-1");
        std::env::set_var("SQUASH_S3_PREFIX", "sandboxes/");
        let cfg = Config::from_env();
        assert_eq!(cfg.s3_bucket.as_deref(), Some("my-bucket"));
        assert_eq!(cfg.s3_endpoint.as_deref(), Some("http://localhost:9000"));
        assert_eq!(cfg.s3_region, "eu-west-1");
        assert_eq!(cfg.s3_prefix, "sandboxes/");
        assert!(cfg.s3_enabled());
        std::env::remove_var("SQUASH_S3_BUCKET");
        std::env::remove_var("SQUASH_S3_ENDPOINT");
        std::env::remove_var("SQUASH_S3_REGION");
        std::env::remove_var("SQUASH_S3_PREFIX");
    }

    #[test]
    fn test_custom_tailscale() {
        let _env = env_lock();
        std::env::set_var("TAILSCALE_AUTHKEY", "tskey-abc");
        std::env::set_var("TAILSCALE_HOSTNAME", "sandbox-1");
        let cfg = Config::from_env();
        assert_eq!(cfg.tailscale_authkey.as_deref(), Some("tskey-abc"));
        assert_eq!(cfg.tailscale_hostname, "sandbox-1");
        std::env::remove_var("TAILSCALE_AUTHKEY");
        std::env::remove_var("TAILSCALE_HOSTNAME");
    }

    #[test]
    fn test_empty_optional_strings_become_none() {
        let _env = env_lock();
        std::env::set_var("SQUASH_AUTH_TOKEN", "");
        std::env::set_var("SQUASH_S3_BUCKET", "");
        std::env::set_var("SQUASH_S3_ENDPOINT", "");
        std::env::set_var("TAILSCALE_AUTHKEY", "");

        let cfg = Config::from_env();
        assert!(cfg.auth_token.is_none());
        assert!(cfg.s3_bucket.is_none());
        assert!(cfg.s3_endpoint.is_none());
        assert!(cfg.tailscale_authkey.is_none());

        std::env::remove_var("SQUASH_AUTH_TOKEN");
        std::env::remove_var("SQUASH_S3_BUCKET");
        std::env::remove_var("SQUASH_S3_ENDPOINT");
        std::env::remove_var("TAILSCALE_AUTHKEY");
    }

    #[test]
    fn test_invalid_port_uses_default() {
        let _env = env_lock();
        std::env::set_var("SQUASH_PORT", "not-a-number");
        let cfg = Config::from_env();
        assert_eq!(cfg.port, 8080);
        std::env::remove_var("SQUASH_PORT");
    }

    #[test]
    fn test_invalid_numeric_fields_use_defaults() {
        let _env = env_lock();
        std::env::set_var("SQUASH_UPPER_LIMIT_MB", "xyz");
        std::env::set_var("SQUASH_MAX_SANDBOXES", "");

        let cfg = Config::from_env();
        assert_eq!(cfg.upper_limit_mb, 512);
        assert_eq!(cfg.max_sandboxes, 100);

        std::env::remove_var("SQUASH_UPPER_LIMIT_MB");
        std::env::remove_var("SQUASH_MAX_SANDBOXES");
    }

    #[test]
    fn test_ephemeral_only_true_for_1() {
        let _env = env_lock();
        std::env::set_var("SQUASH_EPHEMERAL", "0");
        assert!(!Config::from_env().ephemeral);

        std::env::set_var("SQUASH_EPHEMERAL", "true");
        assert!(!Config::from_env().ephemeral);

        std::env::set_var("SQUASH_EPHEMERAL", "1");
        assert!(Config::from_env().ephemeral);

        std::env::remove_var("SQUASH_EPHEMERAL");
    }

    #[test]
    fn test_proxy_https_only_true_for_1() {
        let _env = env_lock();
        std::env::set_var("SQUASH_PROXY_HTTPS", "0");
        assert!(!Config::from_env().proxy_https);

        std::env::set_var("SQUASH_PROXY_HTTPS", "yes");
        assert!(!Config::from_env().proxy_https);

        std::env::set_var("SQUASH_PROXY_HTTPS", "1");
        assert!(Config::from_env().proxy_https);

        std::env::remove_var("SQUASH_PROXY_HTTPS");
    }
}
