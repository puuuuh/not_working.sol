use std::path::PathBuf;
use std::sync::Arc;
use line_index::LineIndex;
use rowan::{TextRange, TextSize};
use salsa::AsDynDatabase;
use base_db::{BaseDb, File, Project};
use hir_def::FilePosition;
use crate::db::TestDatabase;
use salsa::Setter;
use vfs::VfsPath;

mod db;
mod goto_definition;
mod fixture;

pub struct AnalysisHost {
    db: TestDatabase,
    project: Project,
}

impl AnalysisHost {
    pub fn new() -> Self {
        let db = TestDatabase::default();
        let project = Project::new(db.as_dyn_database(), vec![]) ;
        Self {
            db,
            project
        }
    }

    pub fn change_roots(&mut self, roots: ()) {
        self.project.set_import_paths(&mut self.db).to(vec![
            VfsPath::from_path("C:\\Users\\user\\Documents\\s-game\\".into()),
            VfsPath::from_path("C:\\Users\\user\\Documents\\s-game\\node_modules\\".into()),
        ]);
    }

    pub fn line_index(&self, file: File) -> Arc<LineIndex> {
        hir_def::q_line_index(&self.db, file)
    }

    pub fn goto_definition(&self, file: File, pos: TextSize) {
        goto_definition::goto_definition(&self.db, FilePosition { file, position: pos });
    }
}
