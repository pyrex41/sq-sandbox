pub mod sync;

use std::path::{Path, PathBuf};

use aws_config::BehaviorVersion;
use aws_sdk_s3::primitives::ByteStream;
use aws_sdk_s3::Client;
use tokio::fs;
use tracing::{debug, warn};

/// S3 configuration extracted from environment / Config.
#[derive(Debug, Clone)]
pub struct S3Config {
    pub bucket: String,
    pub endpoint: Option<String>,
    pub region: String,
    pub prefix: String,
}

/// S3 client wrapper with push/pull/exists/list operations.
///
/// Supports any S3-compatible service (AWS S3, Cloudflare R2, MinIO, Backblaze B2)
/// via custom `endpoint_url`.
#[derive(Clone)]
pub struct S3Store {
    client: Client,
    bucket: String,
    prefix: String,
}

/// Errors specific to S3 operations.
#[derive(thiserror::Error, Debug)]
pub enum S3Error {
    #[error("s3 put failed for key {key}: {source}")]
    Put {
        key: String,
        source: aws_sdk_s3::error::SdkError<aws_sdk_s3::operation::put_object::PutObjectError>,
    },
    #[error("s3 get failed for key {key}: {source}")]
    Get {
        key: String,
        source: aws_sdk_s3::error::SdkError<aws_sdk_s3::operation::get_object::GetObjectError>,
    },
    #[error("s3 head failed for key {key}: {source}")]
    Head {
        key: String,
        source: aws_sdk_s3::error::SdkError<aws_sdk_s3::operation::head_object::HeadObjectError>,
    },
    #[error("s3 list failed for prefix {prefix}: {source}")]
    List {
        prefix: String,
        source: aws_sdk_s3::error::SdkError<
            aws_sdk_s3::operation::list_objects_v2::ListObjectsV2Error,
        >,
    },
    #[error("s3 stream read failed for key {key}: {source}")]
    StreamRead {
        key: String,
        source: aws_sdk_s3::primitives::ByteStreamError,
    },
    #[error("i/o error: {0}")]
    Io(#[from] std::io::Error),
}

impl S3Store {
    /// Create a new S3Store from configuration.
    ///
    /// Builds the aws-sdk-s3 client with optional custom endpoint for R2/MinIO/B2.
    /// Credentials are resolved from the standard AWS chain (env vars, profiles, IMDS).
    pub async fn new(config: &S3Config) -> Self {
        let mut sdk_config = aws_config::defaults(BehaviorVersion::latest())
            .region(aws_config::Region::new(config.region.clone()));

        if let Some(ref endpoint) = config.endpoint {
            sdk_config = sdk_config.endpoint_url(endpoint);
        }

        let sdk_config = sdk_config.load().await;

        let mut s3_config = aws_sdk_s3::config::Builder::from(&sdk_config);

        // Force path-style for custom endpoints (R2, MinIO, B2 don't support virtual-hosted)
        if config.endpoint.is_some() {
            s3_config = s3_config.force_path_style(true);
        }

        let client = Client::from_conf(s3_config.build());

        Self {
            client,
            bucket: config.bucket.clone(),
            prefix: config.prefix.clone(),
        }
    }

    /// Full S3 key with prefix applied.
    fn full_key(&self, key: &str) -> String {
        if self.prefix.is_empty() {
            key.to_string()
        } else {
            format!("{}{}", self.prefix, key)
        }
    }

    /// Strip the prefix from a full S3 key returned by list.
    fn strip_prefix<'a>(&self, full_key: &'a str) -> &'a str {
        full_key.strip_prefix(&self.prefix).unwrap_or(full_key)
    }

    /// Upload a local file to S3.
    pub async fn push(&self, local_path: &Path, key: &str) -> Result<(), S3Error> {
        let full_key = self.full_key(key);
        let body = ByteStream::from_path(local_path)
            .await
            .map_err(|e| S3Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e)))?;

        debug!("s3 push: {} -> {}", local_path.display(), full_key);

        self.client
            .put_object()
            .bucket(&self.bucket)
            .key(&full_key)
            .body(body)
            .send()
            .await
            .map_err(|e| S3Error::Put {
                key: key.to_string(),
                source: e,
            })?;

        Ok(())
    }

    /// Download a file from S3 to a local path.
    ///
    /// Atomic: writes to `{dest}.s3tmp` then renames. If the destination already
    /// exists, returns immediately (matching the shell double-check-after-lock pattern).
    pub async fn pull(&self, key: &str, dest: &Path) -> Result<(), S3Error> {
        // Fast path: already exists
        if dest.exists() {
            debug!("s3 pull: {} already exists at {}", key, dest.display());
            return Ok(());
        }

        let full_key = self.full_key(key);
        let tmp = dest.with_extension("s3tmp");

        // Ensure parent directory exists
        if let Some(parent) = dest.parent() {
            fs::create_dir_all(parent).await?;
        }

        debug!("s3 pull: {} -> {}", full_key, dest.display());

        let resp = self
            .client
            .get_object()
            .bucket(&self.bucket)
            .key(&full_key)
            .send()
            .await
            .map_err(|e| S3Error::Get {
                key: key.to_string(),
                source: e,
            })?;

        let bytes = resp
            .body
            .collect()
            .await
            .map_err(|e| S3Error::StreamRead {
                key: key.to_string(),
                source: e,
            })?
            .into_bytes();

        // Write to tmp, then atomic rename
        fs::write(&tmp, &bytes).await?;
        fs::rename(&tmp, dest).await?;

        Ok(())
    }

    /// Check if a key exists in S3 (HEAD object).
    pub async fn exists(&self, key: &str) -> Result<bool, S3Error> {
        let full_key = self.full_key(key);

        match self
            .client
            .head_object()
            .bucket(&self.bucket)
            .key(&full_key)
            .send()
            .await
        {
            Ok(_) => Ok(true),
            Err(e) => {
                // NotFound is not an error, just means the key doesn't exist
                if e.as_service_error()
                    .map_or(false, |se| se.is_not_found())
                {
                    Ok(false)
                } else {
                    Err(S3Error::Head {
                        key: key.to_string(),
                        source: e,
                    })
                }
            }
        }
    }

    /// List object keys under a prefix (ListObjectsV2).
    ///
    /// Returns keys with the store's prefix stripped. Handles pagination
    /// automatically via continuation tokens.
    pub async fn list(&self, prefix: &str) -> Result<Vec<String>, S3Error> {
        let full_prefix = self.full_key(prefix);
        let mut keys = Vec::new();
        let mut continuation_token: Option<String> = None;

        loop {
            let mut req = self
                .client
                .list_objects_v2()
                .bucket(&self.bucket)
                .prefix(&full_prefix);

            if let Some(ref token) = continuation_token {
                req = req.continuation_token(token);
            }

            let resp = req.send().await.map_err(|e| S3Error::List {
                prefix: prefix.to_string(),
                source: e,
            })?;

            for obj in resp.contents() {
                if let Some(k) = obj.key() {
                    keys.push(self.strip_prefix(k).to_string());
                }
            }

            match resp.next_continuation_token() {
                Some(token) => continuation_token = Some(token.to_string()),
                None => break,
            }
        }

        Ok(keys)
    }

    /// Spawn a background upload task. Matches the shell `(sq-s3 push ... &)` pattern.
    pub fn push_bg(&self, local_path: PathBuf, key: String) -> tokio::task::JoinHandle<()> {
        let store = self.clone();
        tokio::spawn(async move {
            match store.push(&local_path, &key).await {
                Ok(()) => debug!("s3 push_bg complete: {}", key),
                Err(e) => warn!("s3 push_bg failed for {}: {}", key, e),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_full_key_with_prefix() {
        let store = S3Store {
            client: build_dummy_client(),
            bucket: "test-bucket".to_string(),
            prefix: "myprefix/".to_string(),
        };
        assert_eq!(
            store.full_key("modules/foo.squashfs"),
            "myprefix/modules/foo.squashfs"
        );
    }

    #[test]
    fn test_full_key_without_prefix() {
        let store = S3Store {
            client: build_dummy_client(),
            bucket: "test-bucket".to_string(),
            prefix: String::new(),
        };
        assert_eq!(
            store.full_key("modules/foo.squashfs"),
            "modules/foo.squashfs"
        );
    }

    #[test]
    fn test_strip_prefix() {
        let store = S3Store {
            client: build_dummy_client(),
            bucket: "test-bucket".to_string(),
            prefix: "myprefix/".to_string(),
        };
        assert_eq!(
            store.strip_prefix("myprefix/modules/foo.squashfs"),
            "modules/foo.squashfs"
        );
        assert_eq!(store.strip_prefix("other/path"), "other/path");
    }

    #[test]
    fn test_strip_prefix_empty() {
        let store = S3Store {
            client: build_dummy_client(),
            bucket: "test-bucket".to_string(),
            prefix: String::new(),
        };
        assert_eq!(
            store.strip_prefix("modules/foo.squashfs"),
            "modules/foo.squashfs"
        );
    }

    /// Build a dummy S3 client for unit tests that don't make network calls.
    fn build_dummy_client() -> Client {
        let config = aws_sdk_s3::Config::builder()
            .behavior_version(aws_sdk_s3::config::BehaviorVersion::latest())
            .region(aws_sdk_s3::config::Region::new("us-east-1"))
            .build();
        Client::from_conf(config)
    }
}
