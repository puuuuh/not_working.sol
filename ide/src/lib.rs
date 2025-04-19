use std::sync::Arc;
use camino::Utf8PathBuf;
use change::FileChange;
use navigation_target::NavigationTarget;
use rowan::TextSize;
use base_db::{BaseDb, File, Project, TestDatabase};
use hir_def::{hir::FilePosition, FileExt};
use salsa::Setter;
use tracing::warn;
use vfs::VfsPath;
use salsa::Database;

mod goto_definition;
mod navigation_target;
pub mod change;

#[derive(Clone)]
pub struct AnalysisHost {
    db: TestDatabase,
    project: Project,
}

impl AnalysisHost {
    pub fn new() -> Self {
        let db = TestDatabase::default();
        let project = Project::new(&db, VfsPath::from_virtual(String::new())) ;
        Self {
            db,
            project
        }
    }

    pub fn reload_project(&mut self, root: Utf8PathBuf) {
        self.project.set_root(&mut self.db).to(VfsPath::from_path(root));
    }

    pub fn file(&self, path: Utf8PathBuf) -> Option<File> {
        self.db.file(&VfsPath::Path(path))
    }

    pub fn file_unchecked(&self, path: Utf8PathBuf) -> File {
        self.db.file_unchecked(&VfsPath::Path(path))
    }

    pub fn path(&self, f: File) -> Utf8PathBuf {
        match self.db.path(f)  {
            VfsPath::Path(utf8_path_buf) => utf8_path_buf,
            VfsPath::Virtual(virtual_path) => virtual_path.to_path_buf(),
        }
    }

    pub fn content(&self, file: File) -> Arc<str> {
        file.content(&self.db)
    }

    pub fn line_index(&self, file: File) -> Arc<line_index::LineIndex> {
        file.line_index(&self.db)
    }

    pub fn goto_definition(&self, file: File, pos: TextSize) -> Vec<NavigationTarget> {
        self.db.attach(|_| {
            goto_definition::goto_definition(&self.db, self.project, FilePosition { file, offset: pos }).unwrap_or(vec![])
        })
    }

    pub fn apply_changes(&mut self, file: File, change: FileChange) {
        match change {
            FileChange::SetContent { data } => {
                file.set_content(&mut self.db).to(data);
                file.set_exists(&mut self.db).to(true);
            },
            FileChange::Delete { } => {
                if !file.set_exists(&mut self.db).to(true) {
                    warn!("File already deleted")
                }
            }
            FileChange::Create { } => {
                if file.set_exists(&mut self.db).to(true) {
                    warn!("File already exists")
                }
            },
            FileChange::Rename { new_file } => {
                if new_file.exists(&mut self.db) {
                    warn!("Destination file already exists");
                }
                if !file.exists(&mut self.db) {
                    warn!("Source file not exists");
                }
                file.set_exists(&mut self.db).to(false);
                new_file.set_exists(&mut self.db).to(true);
                let content = file.content(&self.db);
                new_file.set_content(&mut self.db).to(content);
            },
        }
    }
}
