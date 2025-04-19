use std::collections::HashMap;
use rowan::ast::AstPtr;
use syntax::ast::nodes::{Expr, Stmt};
use crate::hir::ExprId;
use crate::hir::StatementId;
use crate::IndexMapUpdate;

#[derive(Clone, Hash, PartialEq, Eq, salsa::Update)]
pub struct ItemSourceMap<'db> {
    pub expr_map: IndexMapUpdate<AstPtr<Expr>, ExprId<'db>>,

    pub stmt_map: IndexMapUpdate<AstPtr<Stmt>, StatementId<'db>>
}

impl<'db> ItemSourceMap<'db> {
    pub fn new(exprs: IndexMapUpdate<AstPtr<Expr>, ExprId<'db>>, stmts: IndexMapUpdate<AstPtr<Stmt>, StatementId<'db>>) -> Self {
        ItemSourceMap {
            expr_map: exprs,
            stmt_map: stmts
        }
    }
}