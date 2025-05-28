use crate::hir::ExprId;
use crate::hir::StatementId;
use crate::IndexMapUpdate;
use base_db::BaseDb;
use rowan::ast::AstPtr;
use std::collections::HashMap;
use syntax::ast::nodes::{Expr, Stmt};

// TODO: Change this to range based mb?
#[salsa::tracked(debug)]
pub struct ItemSourceMap<'db> {
    #[tracked]
    #[no_eq]
    pub data:
        (IndexMapUpdate<AstPtr<Expr>, ExprId<'db>>, IndexMapUpdate<AstPtr<Stmt>, StatementId<'db>>),
}

#[salsa::tracked]
impl<'db> ItemSourceMap<'db> {
    #[salsa::tracked]
    pub fn expr(self, db: &'db dyn BaseDb, ptr: AstPtr<Expr>) -> Option<ExprId<'db>> {
        self.data(db).0.get(&ptr).copied()
    }

    #[salsa::tracked]
    pub fn stmt(self, db: &'db dyn BaseDb, ptr: AstPtr<Stmt>) -> Option<StatementId<'db>> {
        self.data(db).1.get(&ptr).copied()
    }
}
