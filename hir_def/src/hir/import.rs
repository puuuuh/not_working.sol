use crate::hir::ident::Ident;
use crate::hir::HasSourceUnit;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{AnchoredPath, BaseDb, File, Project};
use rowan::ast::AstPtr;
use smallvec::SmallVec;
use syntax::ast::nodes;
use salsa::tracked;


#[derive(Debug, Hash, Clone, Eq, PartialEq, salsa::Update)]
pub enum ImportKind<'db> {
    Path { name: Option<Ident<'db>>, path: String },
    Aliases { symbol_aliases: SmallVec<[SymbolAlias<'db>; 5]>, path: String },
    Glob { as_name: Ident<'db>, path: String },
    Error,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, salsa::Update)]
pub struct SymbolAlias<'db> {
    pub name: Ident<'db>,
    pub as_name: Option<Ident<'db>>,
}

#[salsa::tracked(debug)]
pub struct ImportId<'db> {
    #[tracked]
    pub file: File,

    #[salsa::tracked(debug)]
    pub kind: ImportKind<'db>,

    #[salsa::tracked(debug)]
    pub node: AstPtr<nodes::Import>,
}

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
