use crate::hir::ident::Ident;
use crate::item_tree::{file_tree, DefSite, Item};
use crate::{lazy_field, FileAstPtr};
use base_db::{AnchoredPath, BaseDb, File, Project};
use salsa::tracked;
use syntax::ast::nodes;

#[derive(Debug, Clone, Eq, PartialEq, salsa::Update)]
pub enum ImportKind<'db> {
    Path { name: Option<Ident<'db>>, path: String },
    Aliases { symbol_aliases: Vec<SymbolAlias<'db>>, path: String },
    Glob { as_name: Ident<'db>, path: String },
    Error,
}

#[derive(Debug, Clone, Eq, PartialEq, salsa::Update)]
pub struct SymbolAlias<'db> {
    pub(crate) name: Ident<'db>,
    pub(crate) as_name: Option<Ident<'db>>,
}

#[tracked]
pub struct ImportId<'db> {
    pub kind: ImportKind<'db>,

    pub node: FileAstPtr<nodes::Import>,

}

lazy_field!(ImportId<'db>, def_site, set_def_site, DefSite<'db>);

#[salsa::tracked]
impl<'db> ImportId<'db> {
    #[salsa::tracked]
    pub fn items(
        self,
        db: &'db dyn BaseDb,
        project: Project,
        parent: File,
    ) -> Option<Vec<(Ident<'db>, Item<'db>)>> {
        match self.kind(db) {
            ImportKind::Path { name, path } => {
                let file = db.anchored_file(project, AnchoredPath::new(parent, path))?;
                let tree = file_tree(db, project, file);
                if let Some(name) = name {
                    Some(vec![(name, Item::ItemTree(tree))])
                } else {
                    Some(tree.named_top_items(db.as_dyn_database()).into_iter().collect())
                }
            }
            ImportKind::Aliases { symbol_aliases, path } => {
                let file = db.anchored_file(project, AnchoredPath::new(parent, path))?;
                let t = file_tree(db, project, file);
                let items = symbol_aliases
                    .iter()
                    .filter_map(|alias| {
                        let t = t.named_top_items(db.as_dyn_database());
                        let item = t.get(&alias.name);
                        item.map(|item| {
                            let name = alias.as_name.unwrap_or(alias.name);
                            (name, (*item))
                        })
                    })
                    .collect::<Vec<_>>();
                Some(items)
            }
            ImportKind::Glob { as_name, path } => {
                let file = db.anchored_file(project, AnchoredPath::new(parent, path))?;
                let t = file_tree(db, project, file);
                Some(vec![(as_name, Item::ItemTree(t))])
            }
            ImportKind::Error => None,
        }
    }
}
