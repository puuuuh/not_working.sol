use std::collections::HashMap;

use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Diagnostic, Position, Range, TextEdit
};
use ide::completion::Completion;
use line_index::{LineIndex, WideEncoding};
use rowan::{TextRange, TextSize};

use crate::flycheck;

pub fn text_range(line_index: &line_index::LineIndex, src: TextRange) -> Range {
    Range {
        start: text_position(line_index, src.start()),
        end: text_position(line_index, src.end()),
    }
}

pub fn text_position(line_index: &line_index::LineIndex, src: TextSize) -> Position {
    line_index
        .to_wide(WideEncoding::Utf16, line_index.line_col(src))
        .map(|w| Position::new(w.line, w.col))
        .unwrap()
}

pub fn completion_item(line_index: &line_index::LineIndex, src: Completion) -> CompletionItem {
    let text_edit = (!src.src_range.is_empty()).then(|| {
        CompletionTextEdit::Edit(TextEdit::new(
            text_range(line_index, src.src_range),
            src.text.clone(),
        ))
    });
    let kind = match src.kind {
        ide::completion::CompletionKind::Item => CompletionItemKind::CLASS,
        ide::completion::CompletionKind::Variable => CompletionItemKind::VARIABLE,
        ide::completion::CompletionKind::Text => CompletionItemKind::TEXT,
        ide::completion::CompletionKind::Method => CompletionItemKind::METHOD,
        ide::completion::CompletionKind::Function => CompletionItemKind::FUNCTION,
        ide::completion::CompletionKind::Constructor => CompletionItemKind::CONSTRUCTOR,
        ide::completion::CompletionKind::Field => CompletionItemKind::FIELD,
        ide::completion::CompletionKind::Class => CompletionItemKind::CLASS,
        ide::completion::CompletionKind::Interface => CompletionItemKind::INTERFACE,
        ide::completion::CompletionKind::Module => CompletionItemKind::MODULE,
        ide::completion::CompletionKind::Property => CompletionItemKind::PROPERTY,
        ide::completion::CompletionKind::Unit => CompletionItemKind::UNIT,
        ide::completion::CompletionKind::Value => CompletionItemKind::VALUE,
        ide::completion::CompletionKind::Enum => CompletionItemKind::ENUM,
        ide::completion::CompletionKind::Keyword => CompletionItemKind::KEYWORD,
        ide::completion::CompletionKind::Snippet => CompletionItemKind::SNIPPET,
        ide::completion::CompletionKind::Color => CompletionItemKind::COLOR,
        ide::completion::CompletionKind::File => CompletionItemKind::FILE,
        ide::completion::CompletionKind::Reference => CompletionItemKind::REFERENCE,
        ide::completion::CompletionKind::Folder => CompletionItemKind::FOLDER,
        ide::completion::CompletionKind::EnumMember => CompletionItemKind::ENUM_MEMBER,
        ide::completion::CompletionKind::Constant => CompletionItemKind::CONSTANT,
        ide::completion::CompletionKind::Struct => CompletionItemKind::STRUCT,
        ide::completion::CompletionKind::Event => CompletionItemKind::EVENT,
        ide::completion::CompletionKind::Operator => CompletionItemKind::OPERATOR,
        ide::completion::CompletionKind::TypeParameter => CompletionItemKind::TYPE_PARAMETER,
    };
    CompletionItem {
        label: src.label,
        label_details: None,
        kind: Some(kind),
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: Some(src.text),
        insert_text_format: None,
        insert_text_mode: None,
        text_edit,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    }
}

pub fn flycheck_diagnostic(mut d: flycheck::Output) -> HashMap<String, Vec<Diagnostic>> {
    let mut res: HashMap<String, Vec<Diagnostic>> = HashMap::with_capacity(d.errors.len());
    let mut li_cache = HashMap::new();
    let inputs = d
        .build_infos
        .into_iter()
        .flat_map(|bi| bi.input.sources.into_iter())
        .collect::<HashMap<_, _>>();
    for diag in &mut d.errors {
        let li = li_cache.entry(diag.source_location.file.as_str()).or_insert_with(|| {
            let content: &str = inputs
                .get(&diag.source_location.file)
                .map(|t| t.content.as_ref())
                .unwrap_or_default();
            LineIndex::new(content)
        });
        let start = text_position(li, TextSize::new(diag.source_location.start));
        let end = text_position(li, TextSize::new(diag.source_location.end));
        res.entry(diag.source_location.file.clone()).or_default().push(Diagnostic::new_simple(
            Range::new(start, end),
            std::mem::take(&mut diag.message),
        ));
    }

    res
}
