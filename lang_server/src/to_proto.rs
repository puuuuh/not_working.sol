use async_lsp::lsp_types::{Position, Range};
use line_index::WideEncoding;
use rowan::{TextRange, TextSize};

pub fn text_range(line_index:  &line_index::LineIndex, src: TextRange) -> Range {
    Range {
        start: text_position(line_index, src.start()),
        end: text_position(line_index, src.end())
    }
}

pub fn text_position(line_index:  &line_index::LineIndex, src: TextSize) -> Position {
    line_index
        .to_wide(
            WideEncoding::Utf16, 
            line_index.try_line_col(src.into()).unwrap())
        .map(|w| Position::new(w.line, w.col))
        .unwrap()
}