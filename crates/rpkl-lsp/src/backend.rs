//! LSP Backend implementation

use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::document::Document;
use crate::semantic_tokens::{semantic_token_legend, semantic_tokens_full};
use crate::symbols::{
    document_symbols, find_definition, find_import_definition, find_member_in_module,
    get_completions, get_hover, DefinitionResult,
};

/// The PKL Language Server backend
pub struct PklLanguageServer {
    /// LSP client for sending notifications
    client: Client,
    /// Open documents, keyed by URI
    documents: DashMap<Url, Document>,
}

impl PklLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    /// Parse a document and publish diagnostics
    async fn parse_and_publish_diagnostics(&self, uri: Url, text: &str) {
        let document = Document::new(text.to_string());
        let diagnostics = document.diagnostics();
        self.documents.insert(uri.clone(), document);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for PklLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("Initializing PKL Language Server");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // Text document sync
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // Hover support
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                // Completion support
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                // Definition support
                definition_provider: Some(OneOf::Left(true)),
                // Document symbols
                document_symbol_provider: Some(OneOf::Left(true)),
                // Semantic tokens
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_token_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: Some(false),
                            ..Default::default()
                        },
                    ),
                ),
                // Workspace capabilities
                workspace: Some(ServerCapabilities::default().workspace.unwrap_or_default()),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "rpkl-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        tracing::info!("PKL Language Server initialized");
        self.client
            .log_message(MessageType::INFO, "PKL Language Server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("Shutting down PKL Language Server");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        tracing::debug!("Document opened: {}", uri);
        self.parse_and_publish_diagnostics(uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // We use full sync, so take the first (and only) change
        if let Some(change) = params.content_changes.into_iter().next() {
            tracing::debug!("Document changed: {}", uri);
            self.parse_and_publish_diagnostics(uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        tracing::debug!("Document closed: {}", uri);
        self.documents.remove(&uri);
        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(uri) {
            if let Some(offset) = doc.position_to_offset(position) {
                return Ok(get_hover(&doc, offset));
            }
        }
        Ok(None)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        if let Some(doc) = self.documents.get(uri) {
            if let Some(offset) = doc.position_to_offset(position) {
                let items = get_completions(&doc, offset);
                if !items.is_empty() {
                    return Ok(Some(CompletionResponse::Array(items)));
                }
            }
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        if let Some(doc) = self.documents.get(uri) {
            if let Some(offset) = doc.position_to_offset(position) {
                // First, try to find definition in the same file
                if let Some((start, end)) = find_definition(&doc, offset) {
                    if let (Some(start_pos), Some(end_pos)) =
                        (doc.offset_to_position(start), doc.offset_to_position(end))
                    {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: uri.clone(),
                            range: Range::new(start_pos, end_pos),
                        })));
                    }
                }

                // If not found, check if it's an import reference
                let current_file = uri.to_file_path().ok();
                if let Some(ref file_path) = current_file {
                    if let Some(result) =
                        find_import_definition(&doc, offset, file_path.to_string_lossy().as_ref())
                    {
                        match result {
                            DefinitionResult::SameFile { start, end } => {
                                if let (Some(start_pos), Some(end_pos)) =
                                    (doc.offset_to_position(start), doc.offset_to_position(end))
                                {
                                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                        uri: uri.clone(),
                                        range: Range::new(start_pos, end_pos),
                                    })));
                                }
                            }
                            DefinitionResult::OtherFile { path } => {
                                // Navigate to the beginning of the other file
                                if let Ok(target_uri) = Url::from_file_path(&path) {
                                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                        uri: target_uri,
                                        range: Range::new(
                                            Position {
                                                line: 0,
                                                character: 0,
                                            },
                                            Position {
                                                line: 0,
                                                character: 0,
                                            },
                                        ),
                                    })));
                                }
                            }
                            DefinitionResult::OtherFileMember { path, member_name } => {
                                // Load and parse the target file to find the member
                                if let Ok(target_uri) = Url::from_file_path(&path) {
                                    // Try to read and parse the target file
                                    if let Ok(content) = std::fs::read_to_string(&path) {
                                        let target_doc = Document::new(content);
                                        if let Some(ref target_module) = target_doc.ast {
                                            if let Some((start, end)) =
                                                find_member_in_module(target_module, &member_name)
                                            {
                                                if let (Some(start_pos), Some(end_pos)) = (
                                                    target_doc.offset_to_position(start),
                                                    target_doc.offset_to_position(end),
                                                ) {
                                                    return Ok(Some(
                                                        GotoDefinitionResponse::Scalar(Location {
                                                            uri: target_uri,
                                                            range: Range::new(start_pos, end_pos),
                                                        }),
                                                    ));
                                                }
                                            }
                                        }
                                    }
                                    // Fallback to beginning of file if we can't find the member
                                    return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                                        uri: target_uri,
                                        range: Range::new(
                                            Position {
                                                line: 0,
                                                character: 0,
                                            },
                                            Position {
                                                line: 0,
                                                character: 0,
                                            },
                                        ),
                                    })));
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        if let Some(doc) = self.documents.get(uri) {
            let symbols = document_symbols(&doc);
            if !symbols.is_empty() {
                return Ok(Some(DocumentSymbolResponse::Nested(symbols)));
            }
        }
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        if let Some(doc) = self.documents.get(uri) {
            let tokens = semantic_tokens_full(&doc);
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })));
        }
        Ok(None)
    }
}
