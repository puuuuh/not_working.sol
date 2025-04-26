use async_lsp::lsp_types::notification::Notification;
use async_lsp::lsp_types::*;
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use base_db::File;
use camino::Utf8PathBuf;
use futures::future::BoxFuture;
use ide::change::FileChange;
use std::collections::HashSet;
use std::ops::ControlFlow;
use std::sync::Arc;
use line_index::LineIndex;
use tracing::info;
use ide::AnalysisHost;

use crate::from_proto::ToCaminoPathBuf;
use crate::{from_proto, to_proto};

pub struct Server {
    pub(crate) client: ClientSocket,
    pub(crate) opened_files: HashSet<File>,
    pub(crate) snap: AnalysisHost,
}

struct TickEvent;

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::from_language_server(Self {
            client,
            opened_files: Default::default(),
            snap: AnalysisHost::new(),
        });
        router.event(Self::on_tick);
        router
    }

    fn on_tick(&mut self, _: TickEvent) -> ControlFlow<async_lsp::Result<()>> {
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
        info!("Initialize");
        self.snap.reload_project(Utf8PathBuf::from_path_buf(params.root_uri.unwrap().to_file_path().unwrap()).unwrap());
        info!("Initialized");

        Box::pin(async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    definition_provider: Some(OneOf::Left(true)),
                    type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                    position_encoding: Some(PositionEncodingKind::UTF16),
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL)),
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
        let snap = self.snap.clone();

        Box::pin(async move {
            let raw_path = Utf8PathBuf::from_path_buf(p.text_document_position_params.text_document.uri.to_file_path().unwrap()).unwrap();
            let f = snap.file(raw_path).unwrap();

            let line_index = snap.line_index(f);
            let pos = snap.goto_definition(f, from_proto::text_position(&line_index, pos));
            let t = pos.into_iter().map(|p| {
                let path = snap.path(p.file);
                let l = snap.line_index(p.file);

                LocationLink { 
                    origin_selection_range: None, 
                    target_uri: Url::from_file_path(path).unwrap(), 
                    target_range: to_proto::text_range(&l, p.full_range), 
                    target_selection_range: to_proto::text_range(&l, p.focus_range) 
                }
            }).collect();
            Ok(Some(GotoDefinitionResponse::Link(t)))
        })
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams,) -> ControlFlow<async_lsp::Result<()>> {
        let raw_path = params.text_document.uri.to_utf8_path_buf().unwrap();
        let f = self.snap.file_unchecked(raw_path);
        self.opened_files.insert(f);
        self.snap.apply_change(
            f, 
            FileChange::SetContent { data: params.text_document.text.into() }
        );

        self.analyze();
        
        ControlFlow::Continue(())
    }
    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> ControlFlow<async_lsp::Result<()>> {
        ControlFlow::Continue(())
    }
    fn did_rename_files(&mut self, params: RenameFilesParams) -> ControlFlow<async_lsp::Result<()>> {
        for f in params.files {
            let old_path = f.old_uri.to_utf8_path_buf().unwrap();
            let new_path = f.new_uri.to_utf8_path_buf().unwrap();
            let f = self.snap.file_unchecked(old_path);

            self.snap.apply_change(
                f, 
                FileChange::Rename { new_file: self.snap.file_unchecked(new_path) }
            );
        }

        self.analyze();

        ControlFlow::Continue(())
    }
    fn did_create_files(&mut self, params: CreateFilesParams) -> ControlFlow<async_lsp::Result<()>> {
        for f in params.files {
            let raw_path = f.uri.to_utf8_path_buf().unwrap();
            let f = self.snap.file_unchecked(raw_path);

            self.snap.apply_change(
                f, 
                FileChange::Create {}
            );
        }

        self.analyze();

        ControlFlow::Continue(())
    }
    fn did_delete_files(&mut self, params: DeleteFilesParams) -> ControlFlow<async_lsp::Result<()>> {
        for f in params.files {
            let raw_path = f.uri.to_utf8_path_buf().unwrap();
            let f = self.snap.file_unchecked(raw_path);

            self.snap.apply_change(
                f, 
                FileChange::Delete {  }
            );
        }

        self.analyze();

        ControlFlow::Continue(())
    }
    
    fn did_change(&mut self,params: DidChangeTextDocumentParams) -> ControlFlow<async_lsp::Result<()>> {
        let raw_path = params.text_document.uri.to_utf8_path_buf().unwrap();
        let f = self.snap.file(raw_path).unwrap();
        let mut new_content = self.snap.content(f).to_string();
        let mut li: LineIndex;
        for c in params.content_changes {
            if let Some(range) = c.range {
                li = LineIndex::new(&*new_content);
                let range = from_proto::text_range(&li, range);
                let start = &new_content[..range.start().into()];
                let mid = c.text;
                let end = &new_content[range.end().into()..];
                new_content = format!("{start}{mid}{end}");
            } else if c.range_length.is_none() {
                new_content = c.text;
            } else {
                unimplemented!()
            }
        }

        self.snap.apply_change(
            f, 
            FileChange::SetContent { data: Arc::from(new_content) }
        );

        self.analyze();

        ControlFlow::Continue(())
    }

    fn did_save(&mut self, _params: DidSaveTextDocumentParams) -> ControlFlow<async_lsp::Result<()>> {
        ControlFlow::Continue(())
    }

    fn did_change_watched_files(&mut self, _params: DidChangeWatchedFilesParams) -> ControlFlow<async_lsp::Result<()>> {
        ControlFlow::Continue(())
    }
}