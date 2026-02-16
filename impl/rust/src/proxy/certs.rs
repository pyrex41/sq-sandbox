use std::io;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use dashmap::DashMap;
use rcgen::{
    BasicConstraints, Certificate, CertificateParams, DistinguishedName, DnType, IsCa, KeyPair,
    KeyUsagePurpose, PKCS_ECDSA_P256_SHA256,
};
use rustls::crypto::aws_lc_rs as provider;
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use rustls::sign::CertifiedKey;
use tracing::{debug, info, warn};

// ── Constants ───────────────────────────────────────────────────────

/// Maximum cached leaf certificates. When full, clear all and regenerate on demand.
const MAX_CERT_CACHE_SIZE: usize = 1000;

/// CA certificate common name (must match the existing openssl-generated certs).
const CA_COMMON_NAME: &str = "sq-secret-proxy CA";

/// CA validity period in days (~10 years).
const CA_VALIDITY_DAYS: i64 = 3650;

/// Leaf certificate validity period.
const LEAF_VALIDITY: Duration = Duration::from_secs(24 * 60 * 60); // 24 hours

// ── CA keypair holder ───────────────────────────────────────────────

/// Holds the CA certificate and key needed to sign leaf certificates.
///
/// The `ca_cert` + `ca_key` are used by rcgen to sign leaf certs.
/// The `ca_cert_der` is included in the leaf cert chain sent to clients.
pub struct CaAuthority {
    ca_cert: Certificate,
    ca_key: KeyPair,
    ca_cert_der: CertificateDer<'static>,
}

impl CaAuthority {
    /// Generate a new self-signed CA certificate and write PEM files to `ca_dir`.
    ///
    /// Creates `ca_dir/ca.crt` and `ca_dir/ca.key` with EC P-256, matching
    /// the format produced by the existing openssl command in entrypoint.sh.
    pub fn generate(ca_dir: &Path) -> io::Result<Self> {
        std::fs::create_dir_all(ca_dir)?;

        let ca_key = KeyPair::generate_for(&PKCS_ECDSA_P256_SHA256)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("generate CA key: {e}")))?;

        let mut params = CertificateParams::default();
        let mut dn = DistinguishedName::new();
        dn.push(DnType::CommonName, CA_COMMON_NAME);
        params.distinguished_name = dn;
        params.is_ca = IsCa::Ca(BasicConstraints::Unconstrained);
        params.key_usages = vec![
            KeyUsagePurpose::KeyCertSign,
            KeyUsagePurpose::CrlSign,
            KeyUsagePurpose::DigitalSignature,
        ];

        // Set validity: now to +10 years
        let now = time::OffsetDateTime::now_utc();
        params.not_before = now;
        params.not_after = now
            .checked_add(time::Duration::days(CA_VALIDITY_DAYS))
            .unwrap_or(now);

        let ca_cert = params
            .self_signed(&ca_key)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("self-sign CA: {e}")))?;

        // Write PEM files
        std::fs::write(ca_dir.join("ca.crt"), ca_cert.pem())?;
        std::fs::write(ca_dir.join("ca.key"), ca_key.serialize_pem())?;

        let ca_cert_der = CertificateDer::from(ca_cert.der().to_vec());

        info!(cn = CA_COMMON_NAME, "generated new CA certificate");

        Ok(Self {
            ca_cert,
            ca_key,
            ca_cert_der,
        })
    }

    /// Load an existing CA from PEM files in `ca_dir`.
    ///
    /// Reads `ca_dir/ca.crt` and `ca_dir/ca.key`. Compatible with both
    /// openssl-generated and rcgen-generated PEM files.
    pub fn load(ca_dir: &Path) -> io::Result<Self> {
        let cert_pem = std::fs::read_to_string(ca_dir.join("ca.crt"))?;
        let key_pem = std::fs::read_to_string(ca_dir.join("ca.key"))?;

        let ca_key = KeyPair::from_pem(&key_pem)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse CA key: {e}")))?;

        let ca_params = CertificateParams::from_ca_cert_pem(&cert_pem)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse CA cert: {e}")))?;

        // Re-sign to get a Certificate object (needed for DER and as issuer).
        // The signature itself doesn't matter — we need the Certificate type
        // to use as issuer in signed_by().
        let ca_cert = ca_params
            .self_signed(&ca_key)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("re-sign CA: {e}")))?;

        let ca_cert_der = CertificateDer::from(ca_cert.der().to_vec());

        info!("loaded CA certificate from {}", ca_dir.display());

        Ok(Self {
            ca_cert,
            ca_key,
            ca_cert_der,
        })
    }

    /// Load CA from `ca_dir` if files exist, otherwise generate a new CA.
    pub fn load_or_generate(ca_dir: &Path) -> io::Result<Self> {
        if ca_dir.join("ca.crt").exists() && ca_dir.join("ca.key").exists() {
            Self::load(ca_dir)
        } else {
            Self::generate(ca_dir)
        }
    }

    /// Return the CA certificate in DER format (for inclusion in leaf cert chains).
    pub fn cert_der(&self) -> &CertificateDer<'static> {
        &self.ca_cert_der
    }
}

// ── Leaf cert cache ─────────────────────────────────────────────────

/// Thread-safe cache of per-host leaf certificates, with clear-all eviction.
///
/// When the cache reaches `MAX_CERT_CACHE_SIZE`, all entries are evicted and
/// certs regenerate on demand. This matches the Go proxy behavior.
pub struct CertCache {
    ca: CaAuthority,
    cache: DashMap<String, Arc<CertifiedKey>>,
}

impl CertCache {
    pub fn new(ca: CaAuthority) -> Self {
        Self {
            ca,
            cache: DashMap::new(),
        }
    }

    /// Get or generate a leaf certificate for the given hostname.
    ///
    /// The leaf cert is EC P-256, signed by the CA, with CN and SAN set to `host`,
    /// and 24-hour validity. Results are cached in a DashMap.
    pub fn get_or_create(&self, host: &str) -> io::Result<Arc<CertifiedKey>> {
        // Fast path: cache hit
        if let Some(entry) = self.cache.get(host) {
            return Ok(Arc::clone(entry.value()));
        }

        // Slow path: generate and cache
        let certified_key = self.generate_leaf(host)?;
        let arc = Arc::new(certified_key);

        // Evict all if at capacity (matches Go: simple reset)
        if self.cache.len() >= MAX_CERT_CACHE_SIZE {
            self.cache.clear();
            warn!(max = MAX_CERT_CACHE_SIZE, "cert cache full, cleared");
        }

        self.cache.insert(host.to_owned(), Arc::clone(&arc));
        debug!(host, "generated leaf certificate");
        Ok(arc)
    }

    /// Generate a leaf certificate signed by the CA for the given hostname.
    fn generate_leaf(&self, host: &str) -> io::Result<CertifiedKey> {
        let leaf_key = KeyPair::generate_for(&PKCS_ECDSA_P256_SHA256)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("generate leaf key: {e}")))?;

        let mut params = CertificateParams::new(vec![host.to_owned()])
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("leaf params: {e}")))?;

        let mut dn = DistinguishedName::new();
        dn.push(DnType::CommonName, host);
        params.distinguished_name = dn;
        params.is_ca = IsCa::NoCa;
        params.key_usages = vec![KeyUsagePurpose::DigitalSignature];
        params.extended_key_usages = vec![rcgen::ExtendedKeyUsagePurpose::ServerAuth];

        let now = time::OffsetDateTime::now_utc();
        params.not_before = now - time::Duration::minutes(1);
        params.not_after = now + time::Duration::seconds(LEAF_VALIDITY.as_secs() as i64);

        let leaf_cert = params
            .signed_by(&leaf_key, &self.ca.ca_cert, &self.ca.ca_key)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("sign leaf cert: {e}")))?;

        // Build cert chain: [leaf, CA]
        let leaf_der = CertificateDer::from(leaf_cert.der().to_vec());
        let ca_der = self.ca.ca_cert_der.clone();
        let chain = vec![leaf_der, ca_der];

        // Convert leaf key to rustls format
        let key_der =
            PrivateKeyDer::from(PrivatePkcs8KeyDer::from(leaf_key.serialize_der().to_vec()));

        let signing_key = provider::default_provider()
            .key_provider
            .load_private_key(key_der)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("load signing key: {e}")))?;

        Ok(CertifiedKey::new(chain, signing_key))
    }

    /// Return a reference to the underlying CA authority.
    pub fn ca(&self) -> &CaAuthority {
        &self.ca
    }

    /// Number of cached certificates.
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Whether the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn generate_ca_creates_pem_files() {
        let dir = TempDir::new().unwrap();
        let ca_dir = dir.path().join("proxy-ca");

        let ca = CaAuthority::generate(&ca_dir).unwrap();

        // PEM files should exist
        assert!(ca_dir.join("ca.crt").exists());
        assert!(ca_dir.join("ca.key").exists());

        // Cert PEM should contain the expected markers
        let cert_pem = std::fs::read_to_string(ca_dir.join("ca.crt")).unwrap();
        assert!(cert_pem.contains("BEGIN CERTIFICATE"));
        assert!(cert_pem.contains("END CERTIFICATE"));

        // Key PEM should contain PEM markers
        let key_pem = std::fs::read_to_string(ca_dir.join("ca.key")).unwrap();
        assert!(key_pem.contains("BEGIN PRIVATE KEY"));

        // DER should be non-empty
        assert!(!ca.cert_der().is_empty());
    }

    #[test]
    fn load_ca_roundtrips() {
        let dir = TempDir::new().unwrap();
        let ca_dir = dir.path().join("proxy-ca");

        // Generate, then load
        CaAuthority::generate(&ca_dir).unwrap();
        let loaded = CaAuthority::load(&ca_dir).unwrap();

        // Should be able to create a cert cache and generate a leaf
        let cache = CertCache::new(loaded);
        let cert = cache.get_or_create("example.com").unwrap();
        assert_eq!(cert.cert.len(), 2); // leaf + CA
    }

    #[test]
    fn load_or_generate_creates_if_missing() {
        let dir = TempDir::new().unwrap();
        let ca_dir = dir.path().join("proxy-ca");

        let ca = CaAuthority::load_or_generate(&ca_dir).unwrap();
        assert!(ca_dir.join("ca.crt").exists());
        assert!(!ca.cert_der().is_empty());
    }

    #[test]
    fn load_or_generate_loads_if_present() {
        let dir = TempDir::new().unwrap();
        let ca_dir = dir.path().join("proxy-ca");

        // Generate first
        CaAuthority::generate(&ca_dir).unwrap();
        let cert_pem_before = std::fs::read_to_string(ca_dir.join("ca.crt")).unwrap();

        // load_or_generate should load, not regenerate
        let _ca = CaAuthority::load_or_generate(&ca_dir).unwrap();
        let cert_pem_after = std::fs::read_to_string(ca_dir.join("ca.crt")).unwrap();
        assert_eq!(cert_pem_before, cert_pem_after);
    }

    #[test]
    fn cert_cache_generates_and_caches() {
        let dir = TempDir::new().unwrap();
        let ca = CaAuthority::generate(&dir.path().join("ca")).unwrap();
        let cache = CertCache::new(ca);

        assert!(cache.is_empty());

        let cert1 = cache.get_or_create("api.example.com").unwrap();
        assert_eq!(cache.len(), 1);

        // Second call should return cached version (same Arc)
        let cert2 = cache.get_or_create("api.example.com").unwrap();
        assert!(Arc::ptr_eq(&cert1, &cert2));
        assert_eq!(cache.len(), 1);

        // Different host creates new entry
        let _cert3 = cache.get_or_create("other.example.com").unwrap();
        assert_eq!(cache.len(), 2);
    }

    #[test]
    fn cert_cache_leaf_has_correct_chain() {
        let dir = TempDir::new().unwrap();
        let ca = CaAuthority::generate(&dir.path().join("ca")).unwrap();
        let cache = CertCache::new(ca);

        let cert = cache.get_or_create("test.example.com").unwrap();
        // Chain should be [leaf, CA]
        assert_eq!(cert.cert.len(), 2);
    }

    #[test]
    fn cert_cache_evicts_when_full() {
        let dir = TempDir::new().unwrap();
        let ca = CaAuthority::generate(&dir.path().join("ca")).unwrap();
        let cache = CertCache::new(ca);

        // Fill to MAX_CERT_CACHE_SIZE
        for i in 0..MAX_CERT_CACHE_SIZE {
            cache
                .get_or_create(&format!("host-{i}.example.com"))
                .unwrap();
        }
        assert_eq!(cache.len(), MAX_CERT_CACHE_SIZE);

        // One more should trigger eviction + add the new one
        cache.get_or_create("overflow.example.com").unwrap();
        assert_eq!(cache.len(), 1);
    }
}
