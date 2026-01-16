//! HTTP client for fetching packages

use std::time::Duration;

use crate::error::{EvalError, EvalResult};

/// Default request timeout in seconds
const DEFAULT_TIMEOUT_SECS: u64 = 60;

/// HTTP client for fetching package metadata and archives
pub struct HttpClient {
    client: reqwest::blocking::Client,
}

impl HttpClient {
    /// Create a new HTTP client with default settings
    pub fn new() -> EvalResult<Self> {
        let client = reqwest::blocking::Client::builder()
            .timeout(Duration::from_secs(DEFAULT_TIMEOUT_SECS))
            .user_agent(format!("rpkl/{}", env!("CARGO_PKG_VERSION")))
            .build()
            .map_err(|e| EvalError::IoError(format!("Failed to create HTTP client: {}", e)))?;

        Ok(Self { client })
    }

    /// Fetch text content from a URL
    pub fn fetch_text(&self, url: &str) -> EvalResult<String> {
        let response =
            self.client.get(url).send().map_err(|e| {
                EvalError::IoError(format!("HTTP request failed for {}: {}", url, e))
            })?;

        if !response.status().is_success() {
            return Err(EvalError::IoError(format!(
                "HTTP request failed for {}: status {}",
                url,
                response.status()
            )));
        }

        response
            .text()
            .map_err(|e| EvalError::IoError(format!("Failed to read response from {}: {}", url, e)))
    }

    /// Fetch binary content from a URL
    pub fn fetch_bytes(&self, url: &str) -> EvalResult<Vec<u8>> {
        let response =
            self.client.get(url).send().map_err(|e| {
                EvalError::IoError(format!("HTTP request failed for {}: {}", url, e))
            })?;

        if !response.status().is_success() {
            return Err(EvalError::IoError(format!(
                "HTTP request failed for {}: status {}",
                url,
                response.status()
            )));
        }

        response
            .bytes()
            .map(|b| b.to_vec())
            .map_err(|e| EvalError::IoError(format!("Failed to read response from {}: {}", url, e)))
    }
}

impl Default for HttpClient {
    fn default() -> Self {
        Self::new().expect("Failed to create default HTTP client")
    }
}
