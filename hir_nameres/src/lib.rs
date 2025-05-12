use base_db::BaseDb;
use hir_def::hir::Ident;
use import::ImportResolutionError;
use inheritance::LinearizationError;
use scope::body::Definition;
use vfs::File;
pub mod container;
pub mod inheritance;
pub mod nameres;
pub mod scope;
pub mod import;

pub enum NameresErrorKind {
    Linearization(LinearizationError),
    Import(ImportResolutionError)
}

#[salsa::accumulator]
pub struct NameresError {
    pub kind: NameresErrorKind,
}

pub trait HasDefs<'db> {
    fn defs(self, db: &'db dyn BaseDb) -> Vec<(Ident<'db>, Definition<'db>)>;
}
