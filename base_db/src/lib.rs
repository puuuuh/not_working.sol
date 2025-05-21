mod input;

use std::sync::Arc;

pub use crate::input::Project;
use rowan::TextSize;
use vfs::Vfs;
pub use vfs::{AnchoredPath, File, VfsPath};

#[salsa::db]
pub trait BaseDb: salsa::Database {
    fn resolve_path(&self, path: &AnchoredPath) -> Option<VfsPath>;
    fn anchored_file(&self, path: &AnchoredPath) -> Option<File>;
    fn file(&self, path: &VfsPath) -> Option<File>;
    fn file_unchecked(&self, path: &VfsPath) -> File;
    fn path(&self, file: File) -> VfsPath;
}

pub struct TestFixture {
    pub files: Vec<(String, Arc<str>)>,
    pub position: Option<TextSize>,
}

impl TestFixture {
    pub fn parse(data: &str) -> Self {
        let pos = data.find("$0").map(|pos| TextSize::new(pos as _));
        let data = data.replacen("$0", "", 1);
        let files = data
            .split("/// ENDFILE")
            .map(|d| {
                let (name, data) = d.trim_start().split_once("\n").unwrap();
                (name.to_string(), Arc::from(data.to_string()))
            })
            .collect();

        Self { files, position: pos }
    }
}

#[salsa::db]
#[derive(Clone)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
    vfs: Arc<Vfs>,
}

impl Default for TestDatabase {
    fn default() -> Self {
        let db = Self { storage: Default::default(), vfs: Default::default() };
        Project::new(&db, VfsPath::from_virtual("".to_owned()));
        db
    }
}

impl TestDatabase {
    pub fn from_fixture(data: TestFixture) -> (Self, File) {
        let mut db = Self::default();
        let mut first_file = None;
        let f = data.files.into_iter().map(|(p, f)| {
            if first_file.is_none() {
                first_file = Some(VfsPath::from_virtual(p.clone()));
            }
            (VfsPath::from_virtual(p), f)
        });
        db.vfs = Arc::new(Vfs::with_files(&db, f));
        let first_file = db.vfs.file(&db, &first_file.unwrap());
        (db, first_file)
    }
}

#[salsa::db]
impl BaseDb for TestDatabase {
    fn resolve_path(&self, path: &AnchoredPath) -> Option<VfsPath> {
        self.vfs.resolve_path(self, &path, &*Project::get(self).import_paths(self))
    }

    fn anchored_file(&self, path: &AnchoredPath) -> Option<File> {
        self.resolve_path(path).and_then(|p| self.file(&p))
    }

    fn file(&self, path: &VfsPath) -> Option<File> {
        let f = self.file_unchecked(path);
        if f.exists(self) {
            Some(f)
        } else {
            None
        }
    }

    fn file_unchecked(&self, path: &VfsPath) -> File {
        self.vfs.file(self, path)
    }

    fn path(&self, file: File) -> VfsPath {
        self.vfs.path(file)
    }
}

#[salsa::db]
impl salsa::Database for TestDatabase {}
