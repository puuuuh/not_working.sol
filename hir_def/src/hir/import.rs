use crate::hir::ident::Ident;
use crate::hir::HasSourceUnit;
use crate::nameres::scope::ItemScope;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{AnchoredPath, BaseDb, File, Project};
use rowan::ast::AstPtr;
use smallvec::{smallvec, SmallVec};
use syntax::ast::nodes;
use salsa::tracked;


#[derive(Debug, Hash, Clone, Eq, PartialEq, salsa::Update)]
pub enum ImportKind<'db> {
    Path { name: Option<Ident<'db>>, path: String },
    Aliases { symbol_aliases: Vec<SymbolAlias<'db>>, path: String },
    Glob { as_name: Ident<'db>, path: String },
    Error,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, salsa::Update)]
pub struct SymbolAlias<'db> {
    pub(crate) name: Ident<'db>,
    pub(crate) as_name: Option<Ident<'db>>,
}

#[salsa::tracked(debug)]
pub struct ImportId<'db> {
    #[salsa::tracked(debug)]
    pub kind: ImportKind<'db>,

    #[salsa::tracked(debug)]
    pub node: AstPtr<nodes::Import>,
}

#[salsa::tracked]
impl<'db> ImportId<'db> {
    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> ItemScope<'db> {
        module.source_unit(db).scope(db, project, module)
    }
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
