//! PKL Language Server
//!
//! This binary provides LSP support for the PKL configuration language.

use tower_lsp::{LspService, Server};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};

mod backend;
mod document;
mod semantic_tokens;
mod symbols;

use backend::PklLanguageServer;

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::registry()
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")))
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .init();

    tracing::info!("Starting PKL Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(PklLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
