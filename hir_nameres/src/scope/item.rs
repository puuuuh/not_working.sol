use std::sync::Arc;

use base_db::BaseDb;
use hir_def::{Ident, Item};
use salsa::Database;
use smallvec::SmallVec;
use sorted_vec::SortedVec;
use vfs::File;

pub struct ItemScopeIter<'db> {
    db: &'db dyn BaseDb,
    parent: Option<ItemScope<'db>>,
    items: &'db [(Ident<'db>, Item<'db>)],
    pos: usize,
    name: Option<Ident<'db>>,
}

impl<'db> ItemScopeIter<'db> {
    fn next_inner(&mut self) -> Option<(Ident<'db>, Item<'db>)> {
        loop {
            if self.pos < self.items.len() {
                let pos = self.pos;
                self.pos += 1;
                break Some(self.items[pos]);
            } else {
                if let Some(parent) = self.parent {
                    self.parent = parent.parent(self.db);
                    self.items = parent.items(self.db);
                    self.pos = 0;
                } else {
                    break None;
                }
            }
        }
    }
}

impl<'db> Iterator for ItemScopeIter<'db> {
    type Item = (Ident<'db>, Item<'db>);
    
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let t = self.next_inner()?;
            if let Some(name) = self.name {
                if t.0 == name {
                    return Some(t)
                }
            } else {
                return Some(t)
            }
        }
    }
    
}

impl<'db> ItemScopeIter<'db> {
    pub(crate) fn from_scope(db: &'db dyn BaseDb, scope: ItemScope<'db>, name: Option<Ident<'db>>) -> Self {
        Self {
            name,
            db,
            parent: scope.parent(db),
            items: scope.items(db),
            pos: 0
        }
    }
}

// Root/contract scope
#[salsa::tracked(debug)]
pub struct ItemScope<'db> {
    pub parent: Option<ItemScope<'db>>,
    // Keep this sorted, pls:)
    #[return_ref]
    pub items: Vec<(Ident<'db>, Item<'db>)>
}

#[salsa::tracked]
impl<'db> ItemScope<'db> {
    //#[salsa::tracked]
    pub fn name(self, db: &'db dyn BaseDb, name: Ident<'db>) -> ItemScopeIter<'db> {
        ItemScopeIter::from_scope(db, self, Some(name))
    }

    pub fn iter(self, db: &'db dyn BaseDb) -> ItemScopeIter<'db> {
        ItemScopeIter::from_scope(db, self, None)
    }
}