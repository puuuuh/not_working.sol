use base_db::BaseDb;
use hir_def::hir::Ident;
use scope::body::Definition;
use vfs::File;
pub mod nameres;
pub mod scope;
pub mod container;
pub mod import;

pub trait HasDefs<'db> {
    fn defs(self, db: &'db dyn BaseDb) -> Vec<(Ident<'db>, Definition<'db>)>;
}
