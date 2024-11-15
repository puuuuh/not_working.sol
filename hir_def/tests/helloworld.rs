use base_db::{BaseDb, File, Project};
use hir_def::hir::ident::Ident;
use hir_def::hir::statement::Statement;
use hir_def::item_tree::print::HirPrint;
use hir_def::item_tree::{file_tree, Item};
use salsa::{Database, Event};
use std::path::PathBuf;
use std::sync::Arc;
use vfs::{AnchoredPath, Vfs, VfsPath};

#[salsa::db]
#[derive(Default, Clone)]
struct TestDatabase {
    storage: salsa::Storage<Self>,
    vfs: Arc<Vfs>,
}

#[salsa::db]
impl BaseDb for TestDatabase {
    fn resolve_path(&self, project: Project, path: AnchoredPath) -> Option<VfsPath> {
        self.vfs.resolve_path(self, &path, project.import_paths(self))
    }

    fn anchored_file(&self, project: Project, path: AnchoredPath) -> Option<File> {
        self.resolve_path(project, path)
            .and_then(|p| self.file(project, p))
    }

    fn file(&self, project: Project, path: VfsPath) -> Option<File> {
        self.vfs.file(self, path)
    }
}

#[salsa::db]
impl salsa::Database for TestDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> Event) {}
}

#[test]
fn helloworld() {
    let db = TestDatabase::default();
    let path = VfsPath::from_path("C:\\Users\\user\\Documents\\s-game\\contracts\\SGame.sol".into());
    let project = Project::new(
        &db,
        vec![
            VfsPath::from_path("C:\\Users\\user\\Documents\\s-game\\".into()),
            VfsPath::from_path("C:\\Users\\user\\Documents\\s-game\\node_modules\\".into())
        ],
    );

    let file = db.file(project, path).unwrap();
    let tree = file_tree(&db, project, file);
    dbg!(file_tree::accumulated::<hir_def::SyntaxError>(&db, project, file));
    /*let _id_map = ast_id_map(&db, file);
    let _span_map = span_map(&db, file);*/
    let tree_data = tree.data(&db);
    let contract = tree_data.contracts(&db)[0];
    // db.attach(|a| dbg!(contract.scope(&db).lookup(&db, Ident::new(&db, "Subscribe".to_owned()))));
    db.attach(|_a| dbg!(tree.scope(&db).lookup(&db, Ident::new(&db, "ReentrancyGuardUpgradeable"))));

    let init_func = contract.scope(&db).lookup(&db, Ident::new(&db, "createGame".to_owned()));
    if let Some(Item::Function(f)) = init_func {
        let scope = f.scope(&db);
        let Statement::Block { stmts, is_unchecked: _ } = f.body(&db).unwrap().kind(&db) else {
            panic!();
        };
        let l = *stmts.last().unwrap();
        let _s = scope.scope_by_stmt.0[&l];

        let mut buf = String::new();
        contract.write(&db, &mut buf, 0).unwrap();
        //println!("{buf}");
        /*
        db.attach(|a| dbg!(scope.lookup(&db, s, Ident::new(&db, "i".to_owned()))));
        db.attach(|a| dbg!(scope.expr_scopes.iter().map(|a| a.items.len()).collect::<Vec<_>>()));
        db.attach(|a| dbg!(scope.lookup(&db, s, Ident::new(&db, "game".to_owned()))));
        db.attach(|a| dbg!(scope.lookup(&db, s, Ident::new(&db, "id".to_owned()))));
         */
    }
}
