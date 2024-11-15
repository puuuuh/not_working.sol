use base_db::{AnchoredPath, BaseDb, File, Project, VfsPath};
use salsa::{Database, Event};
use std::sync::Arc;
use vfs::Vfs;
use crate::fixture::TestFixture;


#[salsa::db]
#[derive(Default, Clone)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
    vfs: Arc<Vfs>,
}

#[salsa::db]
impl Database for TestDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> Event) {}
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


impl TestDatabase {
    pub fn from_fixture(data: TestFixture) -> (Self, File) {
        let mut db = Self::default();
        let mut first_file = None;
        let f= data.files.into_iter().map(|(p, f)| {
            let f = File::new(&db, f);
            if first_file.is_none() {
                first_file = Some(f);
            }
            (VfsPath::from_virtual(p), f)
        });
        db.vfs = Arc::new(Vfs::with_files(f));
        (db, first_file.unwrap())
    }
}