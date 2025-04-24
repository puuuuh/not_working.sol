#![allow(dead_code)]
#![allow(unused)]

use indexmap::IndexMap;
use rowan::ast::{AstNode, AstPtr};
use rowan::Language;
use salsa::{Accumulator, Update};
use salsa::Database;
use syntax::{SolidityLang, SyntaxNode};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
pub mod hir;
pub mod lower;
pub mod source_map;
pub mod items;
pub mod walk;

pub use hir::*;

use base_db::{BaseDb, File};
use line_index::LineIndex;
use parser::parser::lexer::Lexer;
use parser::parser::Parser;
use syntax::ast::nodes::UnitSource;
use syntax::parse::Parse;
use syntax::{TextRange, TextSize};

pub struct InFile<T> {
    pub file: File,
    pub data: T
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, salsa::Update)]
pub struct FileAstPtr<N: AstNode + 'static> {
    pub file: File,
    pub ptr: AstPtr<N>,
}

impl<N: AstNode<Language = SolidityLang>> FileAstPtr<N> {
    pub fn new(file: File, ptr: &N) -> Self {
        Self { file, ptr: AstPtr::new(ptr) }
    }

    pub fn to_node(self, db: &'_ dyn BaseDb) -> N {
        self.ptr.to_node(self.file.node(db).syntax())
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
    fn node(self, db: &dyn BaseDb) -> UnitSource;
    fn line_index(self, db: &dyn BaseDb) -> Arc<LineIndex>;
}

impl FileExt for File {
    fn node(self, db: &dyn BaseDb) -> UnitSource {
        file_parse(db, self).node()
    }

    fn line_index(self, db: &dyn BaseDb) -> Arc<LineIndex> {
        file_line_index(db, self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexMapUpdate<T: Hash + Eq, T1: PartialOrd>(pub IndexMap<T, T1>);

impl<T: Hash + Eq, T1: PartialOrd> Default for IndexMapUpdate<T, T1> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: Hash + Eq, T1: PartialOrd> Deref for IndexMapUpdate<T, T1> {
    type Target = IndexMap<T, T1>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Hash + Eq, T1: PartialOrd> DerefMut for IndexMapUpdate<T, T1> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Hash + Eq, T1: PartialOrd + Hash> Hash for IndexMapUpdate<T, T1> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in self.0.iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

unsafe impl<K, V> Update for IndexMapUpdate<K, V>
where
    K: Hash + Eq,
    V: PartialOrd<V>,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_map: &mut IndexMapUpdate<K, V> = unsafe { &mut *old_pointer };

        if old_map.0 != new_value.0 {
            *old_pointer = new_value;
            return true;
        }

        false
    }
}