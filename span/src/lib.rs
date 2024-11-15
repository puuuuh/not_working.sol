//! File and span related types.
use base_db::{File, VfsPath};
use std::fmt::{self, Write};
use syntax::{TextRange, TextSize};
mod ast_id;
mod map;

pub use self::{
    ast_id::{AstIdMap, AstIdNode, ErasedFileAstId, FileAstId},
    map::SpanMap,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    /// The text range of this span, relative to the anchor.
    /// We need the anchor for incrementality, as storing absolute ranges will require
    /// recomputation on every change in a file at all times.
    pub range: TextRange,
    /// The anchor this span is relative to.
    pub anchor: SpanAnchor,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.anchor.file, f)?;
        f.write_char(':')?;
        fmt::Debug::fmt(&self.anchor.ast_id, f)?;
        f.write_char('@')?;
        fmt::Debug::fmt(&self.range, f)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SpanAnchor {
    pub file: File,
    pub ast_id: ErasedFileAstId,
}

impl fmt::Debug for SpanAnchor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("SpanAnchor").field(&self.file).field(&self.ast_id).finish()
    }
}
