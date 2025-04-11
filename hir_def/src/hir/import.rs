use crate::hir::ident::Ident;
use crate::hir::source_unit::{file_tree, ItemOrigin};
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{AnchoredPath, BaseDb, File, Project};
use rowan::ast::AstPtr;
use smallvec::{smallvec, SmallVec};
use syntax::ast::nodes;
use salsa::tracked;

use super::source_unit::Item;

#[derive(Hash, Clone, Eq, PartialEq, salsa::Update)]
pub enum ImportKind<'db> {
    Path { name: Option<Ident<'db>>, path: String },
    Aliases { symbol_aliases: Vec<SymbolAlias<'db>>, path: String },
    Glob { as_name: Ident<'db>, path: String },
    Error,
}

#[derive(Hash, Clone, Eq, PartialEq, salsa::Update)]
pub struct SymbolAlias<'db> {
    pub(crate) name: Ident<'db>,
    pub(crate) as_name: Option<Ident<'db>>,
}

#[salsa::tracked]
pub struct ImportId<'db> {
    #[salsa::tracked]
    pub kind: ImportKind<'db>,

    #[salsa::tracked]
    pub node: AstPtr<nodes::Import>,
}

lazy_field!(ImportId<'db>, origin, set_origin, ItemOrigin<'db>);

#[salsa::tracked]
impl<'db> ImportId<'db> {
    pub fn path(self, db: &'db dyn BaseDb) -> Option<String> {
        match self.kind(db) {
            ImportKind::Path { name, path } => Some(path),
            ImportKind::Aliases { symbol_aliases, path } => Some(path),
            ImportKind::Glob { as_name, path } => Some(path),
            ImportKind::Error => None,
        }
    }
}
