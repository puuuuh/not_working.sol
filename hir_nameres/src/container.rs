use std::convert::Infallible;

use base_db::BaseDb;
use hir_def::{ContractId, EnumerationId, Ident, Item, SourceUnit, StructureId};
use vfs::File;

use crate::scope::body::Definition;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Supertype, salsa::Update)]
pub enum Container<'db> {
    SourceUnit(SourceUnit<'db>),
    Contract(ContractId<'db>),
    Structure(StructureId<'db>),
    Enum(EnumerationId<'db>),
}

impl<'db> From<EnumerationId<'db>> for Container<'db> {
    fn from(value: EnumerationId<'db>) -> Self {
        Container::Enum(value)
    }
}

impl<'db> From<ContractId<'db>> for Container<'db> {
    fn from(value: ContractId<'db>) -> Self {
        Container::Contract(value)
    }
}

impl<'db> From<StructureId<'db>> for Container<'db> {
    fn from(value: StructureId<'db>) -> Self {
        Container::Structure(value)
    }
}

impl<'db> From<SourceUnit<'db>> for Container<'db> {
    fn from(value: SourceUnit<'db>) -> Self {
        Container::SourceUnit(value)
    }
}

impl<'db> TryFrom<Item<'db>> for Container<'db> {
    type Error = ();

    fn try_from(value: Item<'db>) -> Result<Self, Self::Error> {
        Ok(match value {
            Item::Contract(contract_id) => Self::Contract(contract_id),
            Item::Enum(enumeration_id) => Self::Enum(enumeration_id),
            Item::Struct(structure_id) => Self::Structure(structure_id),
            Item::Module(source_unit) => Self::SourceUnit(source_unit),
            _ => return Err(())
        })
    }
}

#[salsa::tracked]
impl<'db> Container<'db> {
    #[salsa::tracked]
    pub fn defs(self, db: &'db dyn BaseDb) -> Vec<(Ident<'db>, Definition<'db>)> {
        match self {
            Container::SourceUnit(source_unit) => {
                source_unit.items(db).iter()
                    .flat_map(|item| item.name(db)
                    .map(|name| (name, Definition::Item(Item::from(*item)))))
                    .collect()
            },
            Container::Contract(contract) => {
                contract.items(db).iter()
                    .flat_map(|item| item.name(db)
                    .map(|name| (name, Definition::Item(Item::from(*item)))))
                    .collect()
            },
            Container::Structure(structure_id) => {
                structure_id.fields(db)
                    .iter()
                    .map(|item| (item.name(db), Definition::Field(*item)))
                    .collect()
            },
            Container::Enum(enumeration_id) => {
                enumeration_id.fields(db)
                    .iter()
                    .map(|item| (item.name(db), Definition::EnumVariant(*item)))
                    .collect()
            },
        }
    }
}