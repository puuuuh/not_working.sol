use std::str::FromStr;

use async_lsp::lsp_types::{Position, Range, TextDocumentPositionParams, Url};
use base_db::File;
use camino::Utf8PathBuf;
use ide::AnalysisHost;
use line_index::{WideEncoding, WideLineCol};
use rowan::{TextRange, TextSize};

pub fn text_position(li: &line_index::LineIndex, pos: Position) -> TextSize {
    let wide_pos = WideLineCol { line: pos.line, col: pos.character };

    let pos = li.to_utf8(WideEncoding::Utf16, wide_pos).unwrap();
    li.offset(pos).unwrap()
}

pub fn text_range(li: &line_index::LineIndex, range: Range) -> TextRange {
    TextRange::new(text_position(li, range.start), text_position(li, range.end))
}

pub fn file_position(
    db: &AnalysisHost,
    data: TextDocumentPositionParams,
) -> Option<(File, TextSize)> {
    let raw_path =
        Utf8PathBuf::from_path_buf(data.text_document.uri.to_file_path().unwrap()).unwrap();
    let f = db.file(raw_path)?;
    let line_index = db.line_index(f);

    return Some((f, text_position(&line_index, data.position)));
}

pub trait ToCaminoPathBuf {
    fn to_utf8_path_buf(self) -> Result<Utf8PathBuf, ()>;
}

impl ToCaminoPathBuf for Url {
    fn to_utf8_path_buf(self) -> Result<Utf8PathBuf, ()> {
        Utf8PathBuf::from_path_buf(self.to_file_path()?).map_err(|_| ())
    }
}

impl ToCaminoPathBuf for &str {
    fn to_utf8_path_buf(self) -> Result<Utf8PathBuf, ()> {
        Ok(Utf8PathBuf::from_str(self).unwrap())
    }
}
