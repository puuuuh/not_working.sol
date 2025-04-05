use std::path::PathBuf;
use std::sync::Arc;
use camino::Utf8PathBuf;
use line_index::LineIndex;
use rowan::TextSize;
use base_db::{BaseDb, File, Project, TestDatabase};
use hir_def::FilePosition;
use salsa::Setter;
use vfs::VfsPath;

mod goto_definition;
mod navigation_target;

pub struct AnalysisHost {
    db: TestDatabase,
    project: Project,
}

impl AnalysisHost {
    pub fn new() -> Self {
        let db = TestDatabase::default();
        let project = Project::new(&db, vec![], vec![]) ;
        Self {
            db,
            project
        }
    }

    pub fn change_roots(&mut self, roots: Vec<VfsPath>) {
        self.project.set_import_paths(&mut self.db).to(roots);
    }

    pub fn line_index(&self, file: File) -> Arc<LineIndex> {
        hir_def::q_line_index(&self.db, file)
    }

    pub fn file(&self, file: PathBuf) -> Option<File> {
        self.db.file(&VfsPath::from_path(Utf8PathBuf::from_path_buf(file).unwrap()))
    }

    pub fn goto_definition(&self, file: File, pos: TextSize) {
        goto_definition::goto_definition(&self.db, self.project, FilePosition { file, position: pos });
    }
}
