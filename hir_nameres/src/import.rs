use std::collections::{BTreeMap, btree_map};
use std::str::Matches;

use base_db::{BaseDb, Project};
use hir_def::{Ident, ImportId, ImportKind, Item, lower_file};
use indexmap::IndexSet;
use salsa::Database;
use salsa::{Accumulator, plumbing::AsId};
use smallvec::SmallVec;
use vfs::{AnchoredPath, File};

use crate::{NameresError, NameresErrorKind};

#[derive(Debug, salsa::Update)]
pub enum ImportResolutionError {
    FileNotFound { import_id: salsa::Id },
    ItemNotFound { import_id: salsa::Id, name: String },
    NameCollision { import_id: salsa::Id, name: String },
}

fn resolve_file_root_cycle_recovery<'db>(
    db: &'db dyn BaseDb,
    value: &BTreeMap<Ident<'db>, SmallVec<[Item<'db>; 1]>>,
    _count: u32,
    c: File,
) -> salsa::CycleRecoveryAction<BTreeMap<Ident<'db>, SmallVec<[Item<'db>; 1]>>> {
    salsa::CycleRecoveryAction::Iterate
}

fn resolve_file_root_cycle_initial<'db>(
    _db: &dyn BaseDb,
    c: File,
) -> BTreeMap<Ident<'db>, SmallVec<[Item<'db>; 1]>> {
    BTreeMap::new()
}

fn add_items<'db>(
    db: &'db dyn BaseDb,
    import_id: ImportId<'db>,
    data: &mut BTreeMap<Ident<'db>, SmallVec<[Item<'db>; 1]>>,
    items: impl IntoIterator<Item = (Ident<'db>, Item<'db>)>,
) {
    for (name, item) in items {
        match data.entry(name) {
            btree_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(SmallVec::from_iter([item]));
            }
            btree_map::Entry::Occupied(mut occupied_entry) => {
                let v = occupied_entry.get_mut();
                if !v.contains(&item) {
                    let item_is_function = matches!(item, Item::Function(_));
                    if !v.iter().all(|item| matches!(item, Item::Function(_))) {
                        NameresError {
                            kind: NameresErrorKind::Import(ImportResolutionError::NameCollision {
                                name: name.data(db).to_owned(),
                                import_id: import_id.as_id(),
                            }),
                        }
                        .accumulate(db);
                        continue;
                    }
                    v.push(item);
                }
            }
        }
    }
}

#[salsa::tracked(cycle_initial = resolve_file_root_cycle_initial, cycle_fn = resolve_file_root_cycle_recovery, returns(ref))]
pub fn resolve_file_root<'db>(
    db: &'db dyn BaseDb,
    file: File,
) -> BTreeMap<Ident<'db>, SmallVec<[Item<'db>; 1]>> {
    cov_mark::hit!(hir_nameres_resolve_file_root);
    let f = lower_file(db, file);
    let remappings = Project::get(db).remappings(db);
    let mut items: BTreeMap<Ident<'_>, SmallVec<[Item<'_>; 1]>> = BTreeMap::new();
    for item in f.items(db) {
        if let Some(name) = item.name(db) {
            items.entry(name).or_default().push(*item);
        }
    }

    for import in f.data(db).imports(db) {
        let Some(mut path) = import.path(db) else {
            NameresError {
                kind: NameresErrorKind::Import(ImportResolutionError::FileNotFound {
                    import_id: import.as_id(),
                }),
            }
            .accumulate(db);
            continue;
        };
        for (from, to) in remappings {
            if let Some(p) = path.strip_prefix(from) {
                path = format!("{to}{p}");
            }
        }
        let p = AnchoredPath::new(file, path);
        let Some(path) = db.resolve_path(&p) else {
            NameresError {
                kind: NameresErrorKind::Import(ImportResolutionError::FileNotFound {
                    import_id: import.as_id(),
                }),
            }
            .accumulate(db);
            continue;
        };
        let Some(file) = db.file(&path) else {
            NameresError {
                kind: NameresErrorKind::Import(ImportResolutionError::FileNotFound {
                    import_id: import.as_id(),
                }),
            }
            .accumulate(db);
            continue;
        };

        match import.kind(db) {
            ImportKind::Path { name, .. } => {
                if let Some(name) = name {
                    add_items(
                        db,
                        *import,
                        &mut items,
                        [(name, Item::Module(lower_file(db, file)))],
                    );
                } else {
                    add_items(
                        db,
                        *import,
                        &mut items,
                        resolve_file_root(db, file)
                            .iter()
                            .flat_map(|(name, i)| i.iter().map(|i| (*name, *i))),
                    );
                }
            }
            ImportKind::Aliases { symbol_aliases, .. } => {
                let root = resolve_file_root(db, file);
                for s in symbol_aliases {
                    let name = s.name;
                    let as_name = s.as_name.unwrap_or(s.name);
                    let Some(item) = root.get(&name) else {
                        NameresError {
                            kind: NameresErrorKind::Import(ImportResolutionError::ItemNotFound {
                                import_id: import.as_id(),
                                name: name.data(db).to_string(),
                            }),
                        }
                        .accumulate(db);
                        continue;
                    };
                    add_items(db, *import, &mut items, item.iter().map(|i| (as_name, *i)));
                }
            }
            ImportKind::Glob { as_name, path } => {
                let root = resolve_file_root(db, file);
                add_items(db, *import, &mut items, [(as_name, Item::Module(lower_file(db, file)))]);
            }
            ImportKind::Error => {}
        }
    }

    items
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use base_db::{BaseDb, Project, TestDatabase, TestFixture};
    use hir_def::{Ident, Item, lower_file};
    use salsa::{Database, Durability, Setter};
    use smallvec::smallvec;
    use tracing::level_filters::LevelFilter;

    #[test]
    fn recursive_imports() {
        let fixture = TestFixture::parse(
            r#"
            /main.sol
            import "./sec.sol";
            contract A1 {}
            /// ENDFILE
            /sec.sol
            import "./main.sol";
            contract B1 {}
            contract B2 {}
            "#,
        );
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        let db = &db;
        let main = lower_file(db, file);
        let sec = lower_file(db, db.file_unchecked(&vfs::VfsPath::from_virtual("/sec.sol".into())));
        let third =
            lower_file(db, db.file_unchecked(&vfs::VfsPath::from_virtual("/third.sol".into())));

        let expected: BTreeMap<_, smallvec::SmallVec<[_; 1]>> = BTreeMap::from_iter([
            (Ident::new(db, "B1"), smallvec![Item::Contract(sec.data(db).contract(db, "B1"))]),
            (Ident::new(db, "B2"), smallvec![Item::Contract(sec.data(db).contract(db, "B2"))]),
            (Ident::new(db, "A1"), smallvec![Item::Contract(main.data(db).contract(db, "A1"))]),
        ]);
        assert_eq!(super::resolve_file_root(db, file), &expected);
    }

    #[test]
    fn recursive_self_import() {
        let fixture = TestFixture::parse(
            r#"
            /main.sol
            import "main.sol";

            contract A1 {}
            /// ENDFILE
            /sec.sol
            contract B1 {}
            contract B2 {}
            /// ENDFILE
            /third.sol
            contract C1 {}
            contract C2 {}
            contract C3 {}
            contract C4 {}
            contract C5 {}
            "#,
        );
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        {
            let db = &db;
            let main = lower_file(db, file);
            let sec =
                lower_file(db, db.file_unchecked(&vfs::VfsPath::from_virtual("/sec.sol".into())));
            let third =
                lower_file(db, db.file_unchecked(&vfs::VfsPath::from_virtual("/third.sol".into())));

            let expected = BTreeMap::from_iter([(
                Ident::new(db, "A1"),
                smallvec![Item::Contract(main.data(db).contract(db, "A1"))],
            )]);
            assert_eq!(super::resolve_file_root(db, file), &expected);
        }
    }

    #[test]
    fn simple_imports() {
        let fixture = TestFixture::parse(
            r#"
            /main.sol
            import "sec.sol";
            import "third.sol" as third;
            import {C1,C2 as C2_RENAMED} from "third.sol";

            contract A1 {}
            /// ENDFILE
            /sec.sol
            contract B1 {}
            contract B2 {}
            /// ENDFILE
            /third.sol
            contract C1 {}
            contract C2 {}
            contract C3 {}
            contract C4 {}
            contract C5 {}
            "#,
        );
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        {
            let db = &db;
            let main = lower_file(db, file);
            let sec =
                lower_file(db, db.file_unchecked(&vfs::VfsPath::from_virtual("/sec.sol".into())));
            let third =
                lower_file(db, db.file_unchecked(&vfs::VfsPath::from_virtual("/third.sol".into())));

            let expected = BTreeMap::from_iter([
                (Ident::new(db, "third"), smallvec![Item::Module(third)]),
                (
                    Ident::new(db, "C1"),
                    smallvec![Item::Contract(third.data(db).contract(db, "C1"))],
                ),
                (
                    Ident::new(db, "C2_RENAMED"),
                    smallvec![Item::Contract(third.data(db).contract(db, "C2"))],
                ),
                (Ident::new(db, "A1"), smallvec![Item::Contract(main.data(db).contract(db, "A1"))]),
                (Ident::new(db, "B1"), smallvec![Item::Contract(sec.data(db).contract(db, "B1"))]),
                (Ident::new(db, "B2"), smallvec![Item::Contract(sec.data(db).contract(db, "B2"))]),
            ]);
            assert_eq!(super::resolve_file_root(db, file), &expected);
        }
    }

    #[test]
    fn import_cache_invalidation() {
        let fixture = TestFixture::parse(
            r#"
            /main.sol
            import "main.sol";

            contract A2 {}
            contract A3 {}
            contract A4 {}
            contract A5 {}
            contract A6 {}
            contract A7 {}
            "#,
        );
        let (mut db, file) = TestDatabase::from_fixture(fixture);
        let s = lower_file(&db, file);
        {
            {
                cov_mark::check_count!(hir_nameres_resolve_file_root, 2);
                super::resolve_file_root(&db, file);
            }
            file.set_content(&mut db).to(r#"



            import "main.sol";

            contract A2 {}
            contract A3 {}
            contract A4 {}
            contract A5 {}
            contract A6 {}
            contract A7 {}
            "#
            .into());
            let s = lower_file(&db, file);
            {
                cov_mark::check_count!(hir_nameres_resolve_file_root, 0);
                super::resolve_file_root(&db, file);
            }
        }
    }
}
