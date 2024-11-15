use crate::hir::ident::Ident;
use crate::item_tree::Item;
use crate::scope::IndexMapUpdate;
use salsa::Database;

#[salsa::tracked]
pub struct Scope<'db> {
    pub parent: Option<Scope<'db>>,
    #[return_ref]
    pub items: IndexMapUpdate<Ident<'db>, Item<'db>>,
}

#[salsa::tracked]
impl<'db> Scope<'db> {
    #[salsa::tracked]
    pub fn lookup(self, db: &'db dyn Database, name: Ident<'db>) -> Option<Item<'db>> {
        if let Some(t) = self.items(db).0.get(&name).copied().map(Item::from) {
            return Some(t);
        };

        self.parent(db)?.lookup(db, name)
    }
}
