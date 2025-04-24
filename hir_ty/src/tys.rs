use base_db::{BaseDb, File};
use hir_def::hir::{ContractId, ElementaryTypeRef, EnumerationId, ErrorId, EventId, FunctionId, Ident, Item, ModifierId, SourceUnit, StructureId};
use hir_nameres::{container::Container, scope::body::Definition};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum TyKind<'db> {
    Unknown,
    Function(Ty<'db>, Ty<'db>),
    Error(ErrorId<'db>),
    Modifier(Ty<'db>),
    Event(EventId<'db>),
    Struct(StructureId<'db>),
    Enum(EnumerationId<'db>),
    Contract(ContractId<'db>),
    Module(SourceUnit<'db>),
    Elementary(ElementaryTypeRef),
    Array(Ty<'db>, usize),
    Mapping(Ty<'db>, Ty<'db>),
    Tuple(Vec<Ty<'db>>)
}

#[salsa::interned(debug)]
pub struct Ty<'db> {
    pub kind: TyKind<'db>
}

impl<'db> Ty<'db> {
    pub fn container(self, db: &'db dyn BaseDb) -> Option<Container<'db>> {
        Some(match self.kind(db) {
            TyKind::Struct(structure_id) => {
                Container::Structure(structure_id)
            },
            TyKind::Enum(enumeration_id) => {
                Container::Enum(enumeration_id)
            },
            TyKind::Contract(contract_id) => {
                Container::Contract(contract_id)
            },
            TyKind::Module(source_unit) => {
                Container::SourceUnit(source_unit)
            },
            _ => return None
        })
    }
}

#[salsa::tracked]
pub fn unknown<'db>(db: &'db dyn BaseDb) -> Ty<'db> {
    Ty::new(db, TyKind::Unknown)
}
