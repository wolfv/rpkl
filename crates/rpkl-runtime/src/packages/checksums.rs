//! Checksum handling for package verification

use std::fmt;
use std::io::Read;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::error::{EvalError, EvalResult};

/// Package checksums for integrity verification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Checksums {
    /// SHA-256 checksum (hex-encoded)
    pub sha256: String,
}

impl Checksums {
    /// Create new checksums with a SHA-256 hash
    pub fn new(sha256: impl Into<String>) -> Self {
        Self {
            sha256: sha256.into(),
        }
    }

    /// Parse checksums from a string (format: "sha256:hexvalue")
    pub fn parse(s: &str) -> EvalResult<Self> {
        if let Some(hash) = s.strip_prefix("sha256:") {
            Ok(Self {
                sha256: hash.to_string(),
            })
        } else {
            Err(EvalError::InvalidPackageUri(format!(
                "Invalid checksum format: {}. Expected 'sha256:...'",
                s
            )))
        }
    }

    /// Compute SHA-256 checksum of bytes
    pub fn compute(data: &[u8]) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(data);
        let result = hasher.finalize();
        Self {
            sha256: hex::encode(result),
        }
    }

    /// Compute SHA-256 checksum of a reader
    pub fn compute_from_reader<R: Read>(mut reader: R) -> std::io::Result<Self> {
        let mut hasher = Sha256::new();
        let mut buffer = [0u8; 8192];
        loop {
            let n = reader.read(&mut buffer)?;
            if n == 0 {
                break;
            }
            hasher.update(&buffer[..n]);
        }
        let result = hasher.finalize();
        Ok(Self {
            sha256: hex::encode(result),
        })
    }

    /// Verify that the given data matches this checksum
    pub fn verify(&self, data: &[u8]) -> bool {
        let computed = Self::compute(data);
        self.sha256 == computed.sha256
    }

    /// Verify that the computed checksum matches
    pub fn verify_computed(&self, computed: &Checksums) -> bool {
        self.sha256 == computed.sha256
    }
}

impl fmt::Display for Checksums {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sha256:{}", self.sha256)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_checksum() {
        let data = b"hello world";
        let checksums = Checksums::compute(data);
        assert_eq!(
            checksums.sha256,
            "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
        );
    }

    #[test]
    fn test_parse_checksum() {
        let checksums = Checksums::parse("sha256:abc123").unwrap();
        assert_eq!(checksums.sha256, "abc123");
    }

    #[test]
    fn test_verify() {
        let data = b"hello world";
        let checksums = Checksums::compute(data);
        assert!(checksums.verify(data));
        assert!(!checksums.verify(b"different data"));
    }
}
