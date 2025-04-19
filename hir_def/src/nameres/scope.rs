pub mod item;

pub use item::ItemScope;

use indexmap::IndexMap;
use salsa::{Database, Update};
use std::{hash::{Hash, Hasher}, ops::{Deref, DerefMut}};

use crate::hir::{ExprId, Ident, StatementId};

use super::body::{BodyScope, Definition};

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, salsa::Update)]
pub enum Scope<'db> {
    Item(ItemScope<'db>),
    Body(BodyScope<'db>),
    Expr(BodyScope<'db>, usize)
}

impl<'db> Scope<'db> {
    pub fn lookup(self, db: &'db dyn Database, name: Ident<'db>) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.lookup(db, name).iter().copied().next(),
            Scope::Body(body_scope) => body_scope.parent(db).lookup(db, name).iter().copied().next(),
            Scope::Expr(body_scope, i) => { return body_scope.lookup_in_scope(db, i, name) },
        }.map(Definition::Item)
    }

    pub fn lookup_in_expr(self, db: &'db dyn Database, expr: ExprId<'db>, name: Ident<'db>) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.lookup(db, name).iter().copied().next().map(Definition::Item),
            Scope::Body(expr_scope_root) | Scope::Expr(expr_scope_root, _) => expr_scope_root.lookup_in_expr(db, expr, name),
        }
    }

    pub fn lookup_in_stmt(self, db: &'db dyn Database, expr: ExprId<'db>, name: Ident<'db>) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.lookup(db, name).iter().copied().next().map(Definition::Item),
            Scope::Body(body_scope) | Scope::Expr(body_scope, _) => body_scope.lookup_in_expr(db, expr, name),
        }
    }

    pub fn for_stmt(self, db: &'db dyn Database, stmt: StatementId<'db>) -> Scope<'db> {
        match self {
            Scope::Item(item_scope) => Scope::Item(item_scope),
            Scope::Body(body_scope) | Scope::Expr(body_scope, _) => {
                if let Some(i) = body_scope.scope_by_stmt(db).get(&stmt) {
                    Scope::Expr(body_scope, *i)
                } else {
                    Scope::Body(body_scope)
                }
            },
        }
    }

    pub fn for_expr(self, db: &'db dyn Database, expr: ExprId<'db>) -> Scope<'db> {
        match self {
            Scope::Item(item_scope) => Scope::Item(item_scope),
            Scope::Body(body_scope) | Scope::Expr(body_scope, _) => {
                if let Some(i) = body_scope.scope_by_expr(db).get(&expr) {
                    Scope::Expr(body_scope, *i)
                } else {
                    Scope::Body(body_scope)
                }
            },
        }
    }
}