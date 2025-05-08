use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Position, Range, TextEdit,
};
use ide::completion::Completion;
use line_index::WideEncoding;
use rowan::{TextRange, TextSize};

pub fn text_range(line_index: &line_index::LineIndex, src: TextRange) -> Range {
    Range {
        start: text_position(line_index, src.start()),
        end: text_position(line_index, src.end()),
    }
}

pub fn text_position(line_index: &line_index::LineIndex, src: TextSize) -> Position {
    line_index
        .to_wide(WideEncoding::Utf16, line_index.try_line_col(src.into()).unwrap())
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
    CompletionItem {
        label: src.label,
        label_details: None,
        kind: None,
        detail: None,
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: None,
        filter_text: None,
        insert_text: Some(src.text),
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: text_edit,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
    }
}
