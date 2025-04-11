use std::sync::Arc;

use crate::hir::Ident;
use crate::hir::Item;
use crate::scope::IndexMapUpdate;
use salsa::Database;

// Root scope
#[salsa::tracked]
pub struct ItemScope<'db> {
    pub parent: Option<ItemScope<'db>>,
    #[return_ref]
    pub items: Arc<IndexMapUpdate<Ident<'db>, Item<'db>>>
}

#[salsa::tracked]
impl<'db> ItemScope<'db> {
    #[salsa::tracked]
    pub fn lookup(self, db: &'db dyn Database, name: Ident<'db>) -> Option<Item<'db>> {
        if let Some(t) = self.items(db).0.get(&name).copied().map(Item::from) {
            return Some(t);
        };

        self.parent(db)?.lookup(db, name)
    }
}
