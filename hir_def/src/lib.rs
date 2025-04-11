#![allow(dead_code)]
#![allow(unused)]

use rowan::ast::{AstNode, AstPtr};
use salsa::Accumulator;
use salsa::Database;
use syntax::SyntaxNode;
use std::sync::Arc;
pub mod ast_id;
pub mod hir;
pub mod lower;
pub mod scope;
pub mod source_map;
pub mod items;
pub mod resolver;
pub mod resolution;

pub use ast_id::*;
use base_db::{BaseDb, File};
use line_index::LineIndex;
use parser::parser::lexer::Lexer;
use parser::parser::Parser;
use syntax::ast::nodes::UnitSource;
use syntax::parse::Parse;
use syntax::{TextRange, TextSize};

#[derive(Debug, Clone, Eq, PartialEq, salsa::Update)]
pub struct FilePosition {
    pub file: File,
    pub position: TextSize,
}


#[derive(Debug, Hash, Clone, Eq, PartialEq, salsa::Update)]
pub struct FileAstPtr<N: AstNode + 'static> {
    pub file: File,
    pub ptr: AstPtr<N>,
}

impl<N: AstNode> FileAstPtr<N> {
    pub fn new(file: File, ptr: &N) -> Self {
        Self { file, ptr: AstPtr::new(ptr) }
    }
}

#[salsa::accumulator]
pub struct SyntaxError {
    text: String,
    range: TextRange,
}

#[salsa::tracked]
fn file_parse(db: &dyn BaseDb, file: File) -> Parse<UnitSource> {
    let data = &*file.content(db);
    let lexer = Lexer::new(data);
    let mut pos = 0;
    let tokens = lexer
        .map(move |t| {
            let r = (t.kind, &data[pos..pos + t.len]);
            pos += t.len;
            r
        })
        .collect::<Vec<_>>();
    let (parsed, errors) = Parser::parse(&tokens);
    for e in errors {
        SyntaxError { text: e.0, range: e.1 }.accumulate(db);
    }
    parsed
}

#[salsa::tracked]
pub fn file_line_index(db: &dyn BaseDb, file: File) -> Arc<LineIndex> {
    let c = &*file.content(db);
    Arc::new(LineIndex::new(c))
}

pub trait FileExt {
    fn tree(self, db: &dyn BaseDb) -> UnitSource;
    fn line_index(self, db: &dyn BaseDb) -> Arc<LineIndex>;
}

impl FileExt for File {
    fn tree(self, db: &dyn BaseDb) -> UnitSource {
        file_parse(db, self).tree()
    }

    fn line_index(self, db: &dyn BaseDb) -> Arc<LineIndex> {
        file_line_index(db, self)
    }
}