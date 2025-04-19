use std::env::var;

use base_db::{BaseDb, File, Project};
use hir_def::{hir::{BinaryOp, ElementaryTypeRef, ExprId, HasSourceUnit, Item, StatementId, TypeRef}, nameres::{body::{Definition, StmtOrItem}, scope::Scope}};
use salsa::tracked;
use tys::{Ty, TyKind};

pub mod tys;
pub mod resolver;

/*
pub trait HasType<'db> {
    fn ty(self, db: &'db dyn BaseDb, project: Project, module: File) -> Ty<'db>;
}

#[tracked]
impl<'db> HasType<'db> for ExprId<'db> {
    #[salsa::tracked(debug)]
    fn ty(self, db: &'db dyn BaseDb, project: Project, module: File) -> Ty<'db> {
        let kind = match self.kind(db) {
        };

        Ty::new(db, kind)
    }
}
 */