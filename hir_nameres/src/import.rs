use std::{
    collections::BTreeSet,
    iter::once,
    sync::Arc,
};

use base_db::{BaseDb, Project, TestDatabase};
use hir_def::{Ident, ImportId, ImportKind, Item, SourceUnit, lower_file};
use indexmap::{map::Entry, IndexMap, IndexSet};
use smallvec::SmallVec;
use vfs::{AnchoredPath, File, VfsPath};

use salsa::{tracked, Database};

#[salsa::tracked(return_ref)]
pub fn resolve_file_root<'db>(db: &'db dyn BaseDb, project: Project, file: File) -> Vec<(Ident<'db>, Item<'db>)> {
    let mut ctx = ImportResolutionCtx {
        imported: Default::default(),
        imported_modules: Default::default(),
        items: Default::default(),
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
    let mut items = ctx
        .items
        .into_iter()
        .flat_map(|(name, data)| data.into_iter().map(move |item| (name, item)))
        .collect::<Vec<_>>();
    items.sort_by_key(|item| item.0);

    items
}

struct ImportResolutionCtx<'db> {
    imported: IndexMap<File, Option<IndexMap<Ident<'db>, Ident<'db>>>>,
    imported_modules: IndexMap<Ident<'db>, SourceUnit<'db>>,
    items: IndexMap<Ident<'db>, IndexSet<Item<'db>>>,

    queue: BTreeSet<File>,
    errors: SmallVec<[(ImportId<'db>, String); 1]>,
    remappings: &'db Vec<(String, String)>,
}

impl<'db> ImportResolutionCtx<'db> {
    fn resolve_items(&mut self, db: &'db dyn BaseDb, project: Project) {
        for (f, filter) in std::mem::take(&mut self.imported) {
            self.resolve_items_inner(db, project, f, filter);
        }

        self.items.extend(
            std::mem::take(&mut self.imported_modules)
                .into_iter()
                .map(|(name, m)| (name, IndexSet::from([Item::Module(m)]))),
        );
    }

    fn resolve_items_inner(
        &mut self,
        db: &'db dyn BaseDb,
        project: Project,
        f: File,
        remappings: Option<IndexMap<Ident<'db>, Ident<'db>>>,
    ) {
        let tree = lower_file(db, f);
        if let Some(remappings) = remappings {
            for i in tree.named_items(db) {
                if let Some(rename) = remappings.get(i.0) {
                    match self.items.entry(*rename) {
                        indexmap::map::Entry::Occupied(mut occupied_entry) => {
                            occupied_entry.get_mut().insert((*i.1));
                        }
                        indexmap::map::Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(once(*i.1).collect());
                        }
                    }
                }
            }
        } else {
            for (name, item) in tree.named_items(db) {
                match self.items.entry(*name) {
                    indexmap::map::Entry::Occupied(mut occupied_entry) => {
                        occupied_entry.get_mut().insert(*item);
                    }
                    indexmap::map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(once(*item).collect());
                    }
                }
            }
        }
    }

    fn resolve_imports_inner(
        &mut self,
        db: &'db dyn BaseDb,
        project: Project,
        f: File,
        filter: &Option<IndexSet<Ident<'db>>>,
    ) {
        let tree = lower_file(db, f);
        let imports = tree.data(db).imports(db);
        for (mut path, import) in imports.into_iter().flat_map(|i| i.path(db).map(|p| (p, i))) {
            for (from, to) in self.remappings {
                if let Some(p) = path.strip_prefix(from) {
                    path = format!("{to}{p}");
                }
            }
            let p = AnchoredPath::new(f, path);
            let Some(path) = db.resolve_path(project, &p) else {
                self.errors.push((*import, format!("Couldn't resolve path: {}", p.path())));
                continue;
            };
            let Some(file) = db.file(&path) else {
                self.errors.push((*import, format!("File not found: {}", path)));
                return;
            };

            let mut root_changed = false;
            match import.kind(db) {
                ImportKind::Path { name: Some(as_name), .. } | ImportKind::Glob { as_name, .. } => {
                    self.imported_modules.insert(as_name, lower_file(db, file));
                }
                ImportKind::Path { .. } => {
                    root_changed = self.imported.insert(file, None) != Some(None);
                }
                ImportKind::Aliases { symbol_aliases, .. } => {
                    if symbol_aliases.is_empty() {
                        continue;
                    }
                    match self.imported.entry(file) {
                        Entry::Occupied(mut e) => {
                            if let Some(e) = e.get_mut() {
                                for a in symbol_aliases {
                                    root_changed |=
                                        e.insert(a.name, a.as_name.unwrap_or(a.name)).is_some();
                                }
                            } else {
                                // Already imported as glob
                                continue;
                            }
                        }
                        Entry::Vacant(e) => {
                            root_changed = true;
                            e.insert(Some(
                                symbol_aliases
                                    .iter()
                                    .map(|a| (a.name, a.as_name.unwrap_or(a.name)))
                                    .collect(),
                            ));
                        }
                    }
                }
                ImportKind::Error => {
                    continue;
                }
            }

            if root_changed {
                self.queue.insert(file);
            }
        }
    }
}
