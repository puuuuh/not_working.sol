use async_lsp::router::Router;
use async_lsp::{lsp_types::*, LanguageClient};
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use base_db::File;
use camino::Utf8PathBuf;
use futures::future::BoxFuture;
use hir_def::FilePosition;
use ide::change::FileChange;
use ide::AnalysisHost;
use line_index::LineIndex;
use std::collections::HashSet;
use std::ops::ControlFlow;
use std::sync::Arc;

use crate::flycheck::Flycheck;
use crate::from_proto::ToCaminoPathBuf;
use crate::{from_proto, to_proto};

pub struct Server {
    pub(crate) client: ClientSocket,
    pub(crate) opened_files: HashSet<File>,
    pub(crate) snap: AnalysisHost,

    root: Utf8PathBuf,
    flycheck: Flycheck,
}

struct TickEvent;

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::from_language_server(Self {
            client,
            opened_files: Default::default(),
            snap: AnalysisHost::new(),
            flycheck: Flycheck::new(Utf8PathBuf::new()),
            root: Utf8PathBuf::new(),
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
        let root =
            Utf8PathBuf::from_path_buf(params.root_uri.unwrap().to_file_path().unwrap()).unwrap();
        let fly = Flycheck::new(root.clone());

        self.flycheck = fly;
        self.root = root.clone();
        self.snap.reload_project(root);

        Box::pin(async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    definition_provider: Some(OneOf::Left(true)),
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                    position_encoding: Some(PositionEncodingKind::UTF16),
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(
                        TextDocumentSyncKind::INCREMENTAL,
                    )),
                    completion_provider: Some(CompletionOptions {
                        resolve_provider: None,
                        trigger_characters: Some(vec![
                            ".".to_owned(),
                            ",".to_owned(),
                            "[".to_owned(),
                        ]),
                        all_commit_characters: None,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                        completion_item: Some(CompletionOptionsCompletionItem {
                            label_details_support: Some(false),
                        }),
                    }),
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
        let snap = self.snap.clone();

        Box::pin(async move {
            let (f, pos) =
                from_proto::file_position(&snap, p.text_document_position_params).unwrap();

            let pos = snap.goto_definition(f, pos);
            let t = pos
                .into_iter()
                .map(|p| {
                    let path = snap.path(p.file);
                    let l = snap.line_index(p.file);

                    LocationLink {
                        origin_selection_range: None,
                        target_uri: Url::from_file_path(path).unwrap(),
                        target_range: to_proto::text_range(&l, p.full_range),
                        target_selection_range: to_proto::text_range(&l, p.focus_range),
                    }
                })
                .collect();
            Ok(Some(GotoDefinitionResponse::Link(t)))
        })
    }

    fn did_open(
        &mut self,
        params: DidOpenTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let raw_path = params.text_document.uri.to_utf8_path_buf().unwrap();
        let f = self.snap.file_unchecked(raw_path);
        self.opened_files.insert(f);
        self.snap
            .apply_change(f, FileChange::SetContent { data: params.text_document.text.into() });

        self.analyze();

        ControlFlow::Continue(())
    }
    fn did_close(
        &mut self,
        params: DidCloseTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        ControlFlow::Continue(())
    }
    fn did_rename_files(
        &mut self,
        params: RenameFilesParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        for f in params.files {
            let old_path = f.old_uri.to_utf8_path_buf().unwrap();
            let new_path = f.new_uri.to_utf8_path_buf().unwrap();
            let f = self.snap.file_unchecked(old_path);

            self.snap.apply_change(
                f,
                FileChange::Rename { new_file: self.snap.file_unchecked(new_path) },
            );
        }

        self.analyze();

        ControlFlow::Continue(())
    }
    fn did_create_files(
        &mut self,
        params: CreateFilesParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        for f in params.files {
            let raw_path = f.uri.to_utf8_path_buf().unwrap();
            let f = self.snap.file_unchecked(raw_path);

            self.snap.apply_change(f, FileChange::Create {});
        }

        self.analyze();

        ControlFlow::Continue(())
    }
    fn did_delete_files(
        &mut self,
        params: DeleteFilesParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        for f in params.files {
            let raw_path = f.uri.to_utf8_path_buf().unwrap();
            let f = self.snap.file_unchecked(raw_path);

            self.snap.apply_change(f, FileChange::Delete {});
        }

        self.analyze();

        ControlFlow::Continue(())
    }

    fn did_change(
        &mut self,
        params: DidChangeTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
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

        self.snap.apply_change(f, FileChange::SetContent { data: Arc::from(new_content) });

        self.analyze();

        ControlFlow::Continue(())
    }

    fn did_save(
        &mut self,
        params: DidSaveTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let fly = self.flycheck.clone();
        let mut client = self.client.clone();
        let root = self.root.clone();

        tokio::spawn(async move {
            let diagnostics = fly.check().await;
            match diagnostics.await {
                Ok(d) => {
                    for (fname, diags) in to_proto::flycheck_diagnostic(d) {
                        if let Ok(uri) =
                            Url::from_file_path(root.join(fname).as_std_path().to_owned())
                        {
                            let _ = client.publish_diagnostics(PublishDiagnosticsParams {
                                uri,
                                diagnostics: diags,
                                version: None,
                            });
                        }
                    }
                }
                Err(e) => {}
            }
        });
        ControlFlow::Continue(())
    }

    fn did_change_watched_files(
        &mut self,
        _params: DidChangeWatchedFilesParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        ControlFlow::Continue(())
    }

    fn hover(
        &mut self,
        params: HoverParams,
    ) -> BoxFuture<'static, Result<Option<Hover>, ResponseError>> {
        let snap = self.snap.clone();

        Box::pin(async move {
            let (f, pos) =
                from_proto::file_position(&snap, params.text_document_position_params).unwrap();

            let Some(result) = snap.hover(f, FilePosition { file: f, offset: pos }) else {
                return Ok(None);
            };
            let line_index = snap.line_index(f);
            Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "solidity".to_owned(),
                    value: result.1,
                })),
                range: Some(to_proto::text_range(&line_index, result.0)),
            }))
        })
    }

    fn completion(
        &mut self,
        params: CompletionParams,
    ) -> BoxFuture<'static, Result<Option<CompletionResponse>, ResponseError>> {
        let snap = self.snap.clone();

        Box::pin(async move {
            let (f, pos) = from_proto::file_position(&snap, params.text_document_position).unwrap();

            let result =
                snap.completion(f, FilePosition { file: f, offset: pos }).into_iter().flatten();
            let line_index = snap.line_index(f);
            Ok(Some(CompletionResponse::Array(
                result.map(|a| to_proto::completion_item(&line_index, a)).collect(),
            )))
        })
    }
}
