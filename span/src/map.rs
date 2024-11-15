//! A map that maps a span to every position in a file. Usually maps a span to some range of positions.
//! Allows bidirectional lookup.

use base_db::File;
use std::hash::Hash;

use syntax::{TextRange, TextSize};

use crate::{ErasedFileAstId, Span, SpanAnchor};

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct SpanMap {
    file: File,
    /// Invariant: Sorted vec over TextSize
    pairs: Box<[(TextSize, ErasedFileAstId)]>,
    end: TextSize,
}

impl SpanMap {
    pub fn from_file(file: File, pairs: Box<[(TextSize, ErasedFileAstId)]>, end: TextSize) -> Self {
        Self { file, pairs, end }
    }

    pub fn span_for_range(&self, range: TextRange) -> Span {
        assert!(
            range.end() <= self.end,
            "range {range:?} goes beyond the end of the file {:?}",
            self.end
        );
        let start = range.start();
        let idx = self
            .pairs
            .binary_search_by(|&(it, _)| it.cmp(&start).then(std::cmp::Ordering::Less))
            .unwrap_err();
        let (offset, ast_id) = self.pairs[idx - 1];
        Span { range: range - offset, anchor: SpanAnchor { file: self.file, ast_id } }
    }
}
