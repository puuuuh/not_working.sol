use async_lsp::{
    lsp_types::{Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams, Url},
    LanguageClient,
};

use crate::{server::Server, to_proto};

impl Server {
    pub(crate) fn analyze(&self) {
        let mut client = self.client.clone();
        let snap = self.snap.clone();
        let files = self.opened_files.iter().copied().collect::<Vec<_>>();
        std::thread::spawn(move || {
            //snap.initial_analyze(files.iter().copied());
            for f in files {
                let line_index = snap.line_index(f);
                let diag = snap.diagnostics(f);
                client
                    .publish_diagnostics(PublishDiagnosticsParams {
                        uri: Url::from_file_path(snap.path(f).as_std_path()).unwrap(),
                        diagnostics: diag
                            .into_iter()
                            .map(|d| {
                                Diagnostic {
                                    range: to_proto::text_range(&line_index, d.range()),
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    code: None,
                                    code_description: None,
                                    source: None,
                                    message: d.text(),
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                }})
                            .collect(),
                        version: None,
                    })
                    .unwrap();
            }
        });
    }
}
