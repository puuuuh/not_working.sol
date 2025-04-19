use std::{fmt::Debug};

use base_db::{BaseDb, File};
use hir_def::{hir::{ContractId, ElementaryTypeRef, EnumerationId, ErrorId, EventId, FunctionId, HasDefs, Ident, Item, ModifierId, SourceUnit, StructureId}, nameres::body::Definition};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum TyKind<'db> {
    Unknown,
    Function(Ty<'db>, Ty<'db>),
    Error(File, ErrorId<'db>),
    Modifier(Ty<'db>),
    Event(File, EventId<'db>),
    Struct(File, StructureId<'db>),
    Enum(File, EnumerationId<'db>),
    Contract(File, ContractId<'db>),
    Module(File, SourceUnit<'db>),
    Elementary(ElementaryTypeRef),
    Array(Ty<'db>, usize),
    Mapping(Ty<'db>, Ty<'db>),
    Tuple(Vec<Ty<'db>>)
}

#[salsa::interned(debug)]
pub struct Ty<'db> {
    pub kind: TyKind<'db>
}

impl<'db> HasDefs<'db> for Ty<'db> {
    fn defs(self, db: &'db dyn BaseDb, _: File) -> Vec<(Ident<'db>, Definition<'db>)> {
        match self.kind(db) {
            TyKind::Struct(file, structure_id) => {
                structure_id.defs(db, file)
            },
            TyKind::Enum(file, enumeration_id) => {
                enumeration_id.defs(db, file)
            },
            TyKind::Contract(file, contract_id) => {
                contract_id.defs(db, file)
            },
            TyKind::Module(file, source_unit) => {
                source_unit.defs(db, file)
            },
            TyKind::Function(file, function_id) => {
                vec![]
            },
            TyKind::Error(file, error_id) => {
                vec![]
            },
            TyKind::Modifier(_) => {
                vec![]
            },
            TyKind::Event(file, event_id) => {
                vec![]
            },
            TyKind::Elementary(elementary_type_ref) => {
                vec![]
            },
            TyKind::Array(ty, _) => {
                vec![]
            },
            TyKind::Mapping(ty, ty1) => {
                vec![]
            },
            TyKind::Tuple(items) => {
                vec![]
            },
            TyKind::Unknown => vec![],
        }
    }
}

#[salsa::tracked]
pub fn unknown<'db>(db: &'db dyn BaseDb) -> Ty<'db> {
    Ty::new(db, TyKind::Unknown)
}
