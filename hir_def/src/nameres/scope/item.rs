use std::sync::Arc;

use crate::hir::Ident;
use crate::hir::Item;
use crate::IndexMapUpdate;
use salsa::Database;
use sorted_vec::SortedVec;
use vfs::File;

// Root/contract scope
#[salsa::tracked(debug)]
pub struct ItemScope<'db> {
    pub parent: Option<ItemScope<'db>>,
    // Keep this sorted, pls:)
    #[return_ref]
    pub items: Arc<Vec<(Ident<'db>, (File, Item<'db>))>>
}

#[salsa::tracked]
impl<'db> ItemScope<'db> {
    //#[salsa::tracked]
    pub fn lookup(self, db: &'db dyn Database, name: Ident<'db>) -> Box<[(File, Item<'db>)]> {
        let items = self.items(db);
        let Ok(t) = items.binary_search_by(|k| (k.0).cmp(&name)) else {
            if let Some(parent) = self.parent(db) {
                return parent.lookup(db, name);
            } else {
                return Default::default();
            }
        };

        items.iter()
            .skip(t).take_while(|a| a.0 == name)
            .map(|(name, i)| *i).collect()
    }
}
