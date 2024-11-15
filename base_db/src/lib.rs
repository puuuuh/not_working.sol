mod input;

pub use vfs::{AnchoredPath, File, VfsPath};
pub use crate::input::Project;

#[salsa::db]
pub trait BaseDb: salsa::Database {
    fn resolve_path(&self, project: Project, path: AnchoredPath) -> Option<VfsPath>;
    fn anchored_file(&self, project: Project, path: AnchoredPath) -> Option<File>;
    fn file(&self, project: Project, path: VfsPath) -> Option<File>;
}
