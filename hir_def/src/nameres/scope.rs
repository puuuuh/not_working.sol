pub mod item;
pub mod body;

use base_db::BaseDb;
pub use body::BodyScope;
pub use item::ItemScope;

use indexmap::IndexMap;
use salsa::{Database, Update};
use std::{hash::{Hash, Hasher}, ops::{Deref, DerefMut}};

use crate::hir::{ExprId, Ident, StatementId};

use super::scope::body::Definition;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, salsa::Update)]
pub enum Scope<'db> {
    Item(ItemScope<'db>),
    Body(BodyScope<'db>),
    Expr(BodyScope<'db>, usize)
}

impl<'db> Scope<'db> {
    pub fn lookup(self, db: &'db dyn BaseDb, name: Ident<'db>) -> Option<Definition<'db>> {
        let t = match self {
            Scope::Item(item_scope) => item_scope.name(db, name).next(),
            Scope::Body(body_scope) => body_scope.parent(db).name(db, name).next(),
            Scope::Expr(body_scope, i) => {
                return body_scope.lookup_in_scope(db, i, name).map(|a| a.1).next();
            }
        }.map(|(_, item)| Definition::Item(item));
        t
    }

    pub fn lookup_in_expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>, name: Ident<'db>) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.name(db, name).next().map(|(_, item)| Definition::Item(item)),
            Scope::Body(expr_scope_root) | Scope::Expr(expr_scope_root, _) => {
                expr_scope_root.lookup_in_expr(db, expr, name).next().map(|(_, item)| item)
            },
        }
    }

    pub fn for_stmt(self, db: &'db dyn BaseDb, stmt: StatementId<'db>) -> Scope<'db> {
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

    pub fn for_expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Scope<'db> {
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