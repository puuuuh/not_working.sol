use async_lsp::lsp_types::{DidChangeConfigurationParams, GotoDefinitionParams, GotoDefinitionResponse, HoverProviderCapability, InitializeParams, InitializeResult, Location, OneOf, Position, PositionEncodingKind, Range, ServerCapabilities, Url};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use base_db::{BaseDb, Project};
use futures::future::BoxFuture;
use std::ops::ControlFlow;
use std::sync::{Arc, RwLock};
use line_index::{WideEncoding, WideLineCol};
use tracing::info;
use ide::AnalysisHost;

pub struct Server {
    db: Arc<RwLock<AnalysisHost>>,
}

struct TickEvent;

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::from_language_server(Self {
            db: Arc::new(RwLock::new(AnalysisHost::new())),
        });
        router.event(Self::on_tick);
        router
    }

    fn on_tick(&mut self, _: TickEvent) -> ControlFlow<async_lsp::Result<()>> {
        info!("tick");
        ControlFlow::Continue(())
    }
}

impl LanguageServer for Server {
    type Error = ResponseError;
    type NotifyResult = ControlFlow<async_lsp::Result<()>>;

    fn initialize(
        &mut self,
        params: InitializeParams,
    ) -> BoxFuture<'static, Result<InitializeResult, Self::Error>> {
        info!("Initialize with {params:?}");
        self.db.write().unwrap().change_roots(());
        Box::pin(async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    definition_provider: Some(OneOf::Left(true)),
                    position_encoding: Some(PositionEncodingKind::UTF16),
                    ..ServerCapabilities::default()
                },
                server_info: None,
            })
        })
    }

    fn definition(
        &mut self,
        p: GotoDefinitionParams,
    ) -> BoxFuture<'static, Result<Option<GotoDefinitionResponse>, ResponseError>> {
        let pos = p.text_document_position_params.position;
        let wide_pos = WideLineCol {
            line: pos.line,
            col: pos.character,
        };

        let db = self.db.read().unwrap();
        let db = &*db;
        let raw_path = p.text_document_position_params.text_document.uri.to_file_path().unwrap();
        let f = db.file(raw_path).unwrap();

        let line_index = db.line_index(f);
        let pos = line_index.to_utf8(WideEncoding::Utf16, wide_pos).unwrap();
        let offset = line_index.offset(pos).unwrap();
        let pos = db.goto_definition(f, offset);
        
        Box::pin(async move {
            Ok(None)
        })
    }

    fn did_change_configuration(
        &mut self,
        _: DidChangeConfigurationParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        ControlFlow::Continue(())
    }
}
