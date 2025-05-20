use std::{
    collections::{BTreeMap, btree_map},
    sync::Arc,
};

use base_db::BaseDb;
use hir_def::{Ident, Item};
use salsa::Database;
use smallvec::SmallVec;
use sorted_vec::SortedVec;
use vfs::File;

use super::body::Definition;

// Root/contract scope
#[salsa::tracked(debug)]
pub struct ItemScope<'db> {
    pub parent: Option<ItemScope<'db>>,
    #[returns(ref)]
    pub items: BTreeMap<Ident<'db>, SmallVec<[Definition<'db>; 1]>>,
}

#[salsa::tracked]
impl<'db> ItemScope<'db> {
    #[salsa::tracked]
    pub fn all_definitions(
        self,
        db: &'db dyn BaseDb,
    ) -> BTreeMap<Ident<'db>, SmallVec<[Definition<'db>; 1]>> {
        let mut res: BTreeMap<Ident<'_>, SmallVec<[Definition<'_>; 1]>> = Default::default();

        let mut t = Some(self);
        while let Some(scope) = t {
            for (name, defs) in scope.items(db) {
                if let btree_map::Entry::Vacant(v) = res.entry(*name) {
                    v.insert(defs.clone());
                }
            }
            t = scope.parent(db);
        }

        res
    }

    #[salsa::tracked]
    pub fn find_all(self, db: &'db dyn BaseDb, name: Ident<'db>) -> SmallVec<[Definition<'db>; 1]> {
        let mut res: SmallVec<[Definition<'db>; 1]> = Default::default();

        let mut t = Some(self);
        while let Some(scope) = t {
            if let Some(data) = scope.items(db).get(&name) {
                return data.clone();
            }
            t = scope.parent(db);
        }

        res
    }

    #[salsa::tracked]
    pub fn find(self, db: &'db dyn BaseDb, name: Ident<'db>) -> Option<Definition<'db>> {
        let mut t = Some(self);
        while let Some(scope) = t {
            if let Some(t) = scope.items(db).get(&name) {
                return t.first().copied();
            }

            t = scope.parent(db);
        }
        None
    }
}
