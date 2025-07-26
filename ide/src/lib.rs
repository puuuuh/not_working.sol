use async_lsp::ClientSocket;
use base_db::{BaseDb, File, Project, TestDatabase};
use camino::Utf8PathBuf;
use change::FileChange;
use completion::{completion, Completion};
use diagnostic::Diagnostic;
use hir_def::{hir::FilePosition, lower_file, FileExt, SyntaxError};
use hir_ty::error::TypeCheckError;
use hir_ty::resolver::resolve_file;
use hover::hover;
use navigation_target::NavigationTarget;
use rowan::TextRange;
use rowan::TextSize;
use salsa::Database;
use salsa::Setter;
use std::sync::Arc;
use tracing::warn;
use vfs::VfsPath;

pub mod change;
pub mod completion;
mod diagnostic;
mod goto_definition;
mod hover;
mod navigation_target;

#[derive(Clone)]
pub struct AnalysisHost {
    db: TestDatabase,
}

impl Default for AnalysisHost {
    fn default() -> Self {
        Self::new()
    }
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self { db: TestDatabase::default() }
    }

    pub fn reload_project(&mut self, root: Utf8PathBuf) {
        Project::get(&self.db).set_root(&mut self.db).to(VfsPath::from_path(root));
    }

    pub fn file(&self, path: Utf8PathBuf) -> Option<File> {
        self.db.file(&VfsPath::Path(path))
    }

    pub fn file_unchecked(&self, path: Utf8PathBuf) -> File {
        self.db.file_unchecked(&VfsPath::Path(path))
    }

    pub fn path(&self, f: File) -> Utf8PathBuf {
        match self.db.path(f) {
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
        self.db.attach(|db| {
            goto_definition::goto_definition(&self.db, FilePosition { file, offset: pos })
                .unwrap_or_default()
        })
    }

    pub fn completion(&self, file: File, pos: FilePosition) -> Option<Vec<Completion>> {
        completion::completion(&self.db, pos)
    }

    pub fn hover(&self, file: File, pos: FilePosition) -> Option<(TextRange, String)> {
        hover::hover(&self.db, pos)
    }

    pub fn apply_change(&mut self, file: File, change: FileChange) {
        match change {
            FileChange::SetContent { data } => {
                file.set_content(&mut self.db).to(data);
                file.set_exists(&mut self.db).to(true);
            }
            FileChange::Delete {} => {
                if !file.set_exists(&mut self.db).to(true) {
                    warn!("File already deleted")
                }
            }
            FileChange::Create {} => {
                if file.set_exists(&mut self.db).to(true) {
                    warn!("File already exists")
                }
            }
            FileChange::Rename { new_file } => {
                if new_file.exists(&self.db) {
                    warn!("Destination file already exists");
                }
                if !file.exists(&self.db) {
                    warn!("Source file not exists");
                }
                file.set_exists(&mut self.db).to(false);
                new_file.set_exists(&mut self.db).to(true);
                let content = file.content(&self.db);
                new_file.set_content(&mut self.db).to(content);
            }
        }
    }

    pub fn diagnostics(&self, file: File) -> Vec<Diagnostic> {
        let syntax: Vec<&SyntaxError> = lower_file::accumulated::<SyntaxError>(&self.db, file);
        self.db.attach(|_| {
            let typeres: Vec<&TypeCheckError> =
                resolve_file::accumulated::<TypeCheckError>(&self.db, file);
            typeres.len();
            let typeres = typeres.into_iter().map(Diagnostic::TypeCheck);
            syntax.into_iter().map(Diagnostic::Syntax).chain(typeres).collect()
        })
    }
}

#[cfg(test)]
mod tests {
    use salsa::{tracked, Database, DatabaseImpl, Durability};
    use tracing::Level;

    #[test]
    fn repro() {
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        struct BadHash(String);

        impl std::hash::Hash for BadHash {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                0.hash(state);
            }
        }

        #[salsa::input(debug)]
        struct Input {
            field: String
        }

        #[salsa::interned(debug)]
        struct Interned {
            field: String
        }

        #[salsa::tracked(debug)]
        struct Output<'db> {
            pub id: String,

            #[tracked]
            field: Interned<'db>
        }


        #[salsa::tracked]
        fn outer_query<'db>(db: &'db dyn Database, non_durable_input: Input, input: Input) -> Vec<Output<'db>> {
            let res: Vec<Output<'_>> = inner_query(db, input)
                .into_iter()
                .map(|i| Output::new(db, input.field(db), i))
                .collect();
            res
        }

        #[salsa::tracked ]
        fn intern_low_durable<'db>(db: &'db dyn Database, input: Input) -> Vec<Interned<'db>> {
            input.field(db);
            (0..10).map(|i| {
                Interned::new(db, format!("i1_{i}"))
            }).collect()
        }

        #[salsa::tracked]
        fn inner_query<'db>(db: &'db dyn Database, input: Input) -> Vec<Interned<'db>> {
            let res: Vec<Interned<'db>> = (0..10).map(|i| {
                Interned::new(db, format!("{}_{i}", input.field(db)))
            }).collect();
            res
        }


        tracing_subscriber::fmt()
            .with_max_level(Level::DEBUG)
            .with_ansi(false)
            .with_writer(std::io::stderr)
            .init();
        let mut db = DatabaseImpl::default();
        let i1 = Input::builder("i1".to_owned()).durability(Durability::HIGH).new(&db);
        let i2 = Input::builder("i2".to_owned()).durability(Durability::LOW).new(&db);
        let i3 = Input::builder("i3".to_owned()).durability(Durability::LOW).new(&db);
        let i4 = Input::builder("i4".to_owned()).durability(Durability::LOW).new(&db);
        let i5 = Input::builder("i5".to_owned()).durability(Durability::LOW).new(&db);
        let i6 = Input::builder("i6".to_owned()).durability(Durability::LOW).new(&db);
        let i7 = Input::builder("i7".to_owned()).durability(Durability::LOW).new(&db);

        let a = intern_low_durable(&db, i7);
        let b = outer_query(&db, i7, i1);

        outer_query(&db, i7, i2);
        db.synthetic_write(Durability::LOW);

        outer_query(&db, i7, i3);
        db.synthetic_write(Durability::LOW);

        outer_query(&db, i7, i4);
        db.synthetic_write(Durability::LOW);

        outer_query(&db, i7, i5);
        db.synthetic_write(Durability::LOW);
        outer_query(&db, i7, i6);
        for a in outer_query(&db, i7, i1) {
            dbg!(a.field(&db).field(&db));
        }
    }
}