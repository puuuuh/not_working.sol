use std::{collections::{hash_map::Entry, BTreeSet, HashMap, HashSet}, sync::Arc};

use base_db::{BaseDb, Project, TestDatabase};
use indexmap::IndexMap;
use smallvec::SmallVec;
use vfs::{AnchoredPath, File, VfsPath};

use crate::{hir::{ident::Ident, import::{ImportId, ImportKind}, source_unit::{file_tree, Item, SourceUnit}}, scope::IndexMapUpdate};
use salsa::Database;

#[salsa::tracked]
pub struct ImportResolution<'db> {
    #[return_ref]
    pub items: Arc<IndexMapUpdate<Ident<'db>, Item<'db>>>,
    #[return_ref]
    pub errors: SmallVec<[String; 1]>,
}

#[salsa::tracked]
pub fn resolve_file<'db>(db: &'db dyn BaseDb, project: Project, file: File) -> ImportResolution<'db> {
    let mut ctx = ImportResolutionCtx {
        imported: Default::default(),
        modules: Default::default(),
        items: Default::default(),
        explicit_imports: Default::default(),
        queue: Default::default(),
        errors: SmallVec::new(),
        remappings: project.remappings(db),
    };
    ctx.queue.insert(file);
    while let Some(task) = ctx.queue.pop_first() {
        ctx.resolve_imports_inner(db, project, task, &None);
    }
    ctx.resolve_items_inner(db, project, file, None);
    ctx.resolve_items(db, project);
    ImportResolution::new(db, Arc::new(IndexMapUpdate(ctx.items)), ctx.errors)
}

struct ImportResolutionCtx<'db> {
    // Already resolved items
    imported: HashMap<VfsPath, Option<HashMap<Ident<'db>, Ident<'db>>>>,
    modules: IndexMap<Ident<'db>, SourceUnit<'db>>,
    items: IndexMap<Ident<'db>, Item<'db>>,
    explicit_imports: Vec<(Ident<'db>, ImportId<'db>)>,
    queue: BTreeSet<File>,
    errors: SmallVec<[String; 1]>,
    remappings: &'db Vec<(String, String)>,
}

impl<'db> ImportResolutionCtx<'db> {
    fn resolve_items(&mut self, db: &'db dyn BaseDb, project: Project) {
        for (path, filter) in std::mem::take(&mut self.imported) {
            if let Some(f) = db.file(&path) {
                self.resolve_items_inner(db, project, f, filter);
            } else {
                self.errors.push(format!("Couldn't open file: {}", path));
            }
        }
    }
    
    fn resolve_items_inner(&mut self, db: &'db dyn BaseDb, project: Project, f: File, remappings: Option<HashMap<Ident<'db>, Ident<'db>>>) {
        let tree = file_tree(db, f);
        if let Some(remappings) = remappings {
            for i in tree.named_items(db) {
                if let Some(rename) = remappings.get(&i.0) {
                    self.items.insert(*rename, i.1);
                }
            }
        } else {
            self.items.extend(tree.named_items(db));
        }
    }

    fn resolve_imports_inner(&mut self, db: &'db dyn BaseDb, project: Project, f: File, filter: &Option<HashSet<Ident<'db>>>) {
        let tree = file_tree(db, f);
        let imports = tree.data(db).imports(db);
        for (mut path, import) in imports.into_iter().flat_map(|i| i.path(db).map(|p| (p, i))) {
            for (from, to) in self.remappings {
                if let Some(p) = path.strip_prefix(from) {
                    path = format!("{to}{p}");
                }
            }
            let p = AnchoredPath::new(f, path);
            let Some(path) = db.resolve_path(project, &p) else {
                self.errors.push(format!("Couldn't resolve path: {}", p.path()));
                continue;
            };
            let Some(file) = db.file(&path) else {
                self.errors.push(format!("Couldn't resolve path: {}", path));
                return;
            };
            
            let mut root_changed = false;
            match import.kind(db) {
                ImportKind::Path { name: Some(as_name), .. } 
                    | ImportKind::Glob { as_name, .. } => {
                    self.modules.insert(as_name, file_tree(db, file));
                },
                ImportKind::Path { .. } => {
                    root_changed = self.imported.insert(path.clone(), None) != Some(None);
                },
                ImportKind::Aliases { symbol_aliases, .. } => {
                    if symbol_aliases.is_empty() {
                        continue;
                    }
                    match self.imported.entry(path.clone()) {
                        Entry::Occupied(mut e) => {
                            if let Some(e) =  e.get_mut() {
                                for a in symbol_aliases {
                                    root_changed |= e.insert(a.name, a.as_name.unwrap_or(a.name)).is_some();
                                }
                            } else {
                                // Already imported as glob
                                continue;
                            }
                        },
                        Entry::Vacant(e) => {
                            root_changed = true;
                            e.insert(Some(symbol_aliases.iter().map(|a| (a.name, a.as_name.unwrap_or(a.name))).collect()));
                        },
                    }
                },
                ImportKind::Error => { continue; },
            }

            if root_changed {
                self.queue.insert(file);
            }
        }
    }
}

#[test]
fn helloworld() {
    let db = TestDatabase::default();
    let path = VfsPath::from_path("Test.sol".into());
    let project = Project::new(
        &db,
        vec![
            VfsPath::from_path("\\src\\".into()),
            VfsPath::from_path("\\".into()),
        ],
        vec![
            ("@openzeppelin-contracts-upgradeable/".to_owned(),"dependencies/@openzeppelin-contracts-upgradeable-5.2.0/".to_owned()),
            ("@openzeppelin-contracts/".to_owned(),"dependencies/@openzeppelin-contracts-5.2.0/".to_owned()),
            ("@openzeppelin-mocks/".to_owned(),"dependencies/@openzeppelin-mocks-5.2.0/contracts/mocks/".to_owned()),
            ("@openzeppelin/contracts-upgradeable/".to_owned(),"dependencies/@openzeppelin-contracts-upgradeable-5.2.0/".to_owned()),
            ("@openzeppelin/contracts/".to_owned(),"dependencies/@openzeppelin-contracts-5.2.0/".to_owned()),
        ]
    );

    let file = db.file(&path).unwrap();
    let mut tmp = resolve_file(&db, project, file);
    db.attach(|_| {
        dbg!(tmp.errors(&db));
        dbg!(tmp.items(&db).0.keys());
    });
}