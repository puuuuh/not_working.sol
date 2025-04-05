pub mod expr;
pub mod item;
pub mod resolver;

pub use item::ItemScope;
pub use expr::ExprScopeRoot;

use expr::{DefinitionSite};
use indexmap::IndexMap;
use salsa::{Database, Update};
use std::{hash::{Hash, Hasher}, ops::{Deref, DerefMut}};

use crate::hir::{expr::ExprId, ident::Ident};

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

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, PartialOrd, Ord, salsa::Supertype)]
pub enum Scope<'db> {
    Item(ItemScope<'db>),
    Expr(ExprScopeRoot<'db>)
}

#[salsa::tracked]
impl<'db> Scope<'db> {
    pub fn lookup(self, db: &'db dyn Database, name: Ident<'db>) -> Option<DefinitionSite<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.lookup(db, name).map(DefinitionSite::Item),
            Scope::Expr(expr_scope_root) => expr_scope_root.parent(db).lookup(db, name).map(DefinitionSite::Item)
        }
    }

    pub fn lookup_in_expr(self, db: &'db dyn Database, expr: ExprId<'db>, name: Ident<'db>) -> Option<DefinitionSite<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.lookup(db, name).map(DefinitionSite::Item),
            Scope::Expr(expr_scope_root) => expr_scope_root.lookup(db, expr, name),
        }
    }

    pub fn lookup_in_stmt(self, db: &'db dyn Database, expr: ExprId<'db>, name: Ident<'db>) -> Option<DefinitionSite<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.lookup(db, name).map(DefinitionSite::Item),
            Scope::Expr(expr_scope_root) => expr_scope_root.lookup(db, expr, name),
        }
    }
}
