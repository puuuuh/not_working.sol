use std::sync::Arc;

use base_db::BaseDb;
use vfs::File;

use crate::hir::{Ident, Item};

use super::scope::{self, body::Definition, Scope};

enum ScopeIterInner<'db> {
    Expr {
        items: &'db [(Ident<'db>, (File, Item<'db>))],
        pos: usize,
    },
}

impl<'db> ScopeIterInner<'db> {
    fn expr(data: &'db [(Ident<'db>, (File, Item<'db>))]) -> Self {
        Self::Expr {
            items: data,
            pos: 0
        }
    }
}

impl<'db> Iterator for ScopeIterInner<'db> {
    type Item = (Ident<'db>, Definition<'db>);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ScopeIterInner::Expr { items, pos } => {
                if *pos < items.len() {
                    let item = items[*pos];
                    *pos += 1;
                    Some((item.0, Definition::Item(item.1)))
                } else {
                    None
                }
            } 
        }
    }
}

pub struct ScopeIter<'db> {
    db: &'db dyn BaseDb,
    expr_scopes: ScopeIterInner<'db>,
    current_scope: Scope<'db>
}

impl<'db> Iterator for ScopeIter<'db> {
    type Item = Definition<'db>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(t) = self.expr_scopes.next() {
                return Some(t.1);
            }
            match self.current_scope {
                Scope::Item(item_scope) => {
                    if let Some(parent) = item_scope.parent(self.db) {
                        self.current_scope = Scope::Item(parent);
                        self.expr_scopes = ScopeIterInner::expr(parent.items(self.db));
                    } else {
                        break;
                    }
                },
                Scope::Body(body_scope) => {
                    body_scope.expr_scopes(self.db);
                    //self.expr_scopes = Some(body_scope.expr_scopes(self.db));
                    self.current_scope = Scope::Item(body_scope.parent(self.db));
                },
                Scope::Expr(body_scope, i) => {
                    todo!()
                }
            };
        }
        None
    }
}

impl<'db> ScopeIter<'db> {
    pub fn new(db: &'db dyn BaseDb, scope: Scope<'db>) -> Self {
        todo!()
    }
}