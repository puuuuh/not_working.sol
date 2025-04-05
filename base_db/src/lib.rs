mod input;

use std::sync::Arc;

use rowan::TextSize;
use salsa::Event;
use vfs::Vfs;
pub use vfs::{AnchoredPath, File, VfsPath};
pub use crate::input::Project;

#[salsa::db]
pub trait BaseDb: salsa::Database {
    fn resolve_path(&self, project: Project, path: &AnchoredPath) -> Option<VfsPath>;
    fn anchored_file(&self, project: Project, path: &AnchoredPath) -> Option<File>;
    fn file(&self, path: &VfsPath) -> Option<File>;
    fn path(&self, file: File) -> VfsPath;
}

pub struct TestFixture {
    pub files: Vec<(String, Arc<str>)>,
    pub position: Option<TextSize>
}

impl TestFixture {
    pub fn parse(data: &str) -> Self {
        if let Some(t) = data.find("$0") {
            let data = data[..t].to_string() + &data[t+2..];
            Self {
                files: vec![("test_file.sol".to_owned(), Arc::from(data.as_str()))],
                position: Some(TextSize::new(t as _))
            }
        } else {
            Self {
                files: vec![("test_file.sol".to_string(), Arc::from(data))],
                position: None
            }
        }
    }
}

#[salsa::db]
#[derive(Default, Clone)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
    vfs: Arc<Vfs>,
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

#[salsa::db]
impl BaseDb for TestDatabase {
    fn resolve_path(&self, project: Project, path: &AnchoredPath) -> Option<VfsPath> {
        self.vfs.resolve_path(self, &path, project.import_paths(self))
    }

    fn anchored_file(&self, project: Project, path: &AnchoredPath) -> Option<File> {
        self.resolve_path(project, path)
            .and_then(|p| self.file(&p))
    }

    fn file(&self, path: &VfsPath) -> Option<File> {
        self.vfs.file(self, &path)
    }

    fn path(&self, file: File) -> VfsPath {
        self.vfs.path(file)
    }
}

#[salsa::db]
impl salsa::Database for TestDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> Event) {}
}