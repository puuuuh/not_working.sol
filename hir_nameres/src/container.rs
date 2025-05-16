use std::convert::Infallible;

use base_db::BaseDb;
use hir_def::{ContractId, EnumerationId, Ident, Item, SourceUnit, StructureId};
use vfs::File;

use crate::scope::body::Definition;

#[derive(
    Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Supertype, salsa::Update,
)]
pub enum Container<'db> {
    Item(Item<'db>),
    Contract(ContractId<'db>),
    Structure(StructureId<'db>),
    Enum(EnumerationId<'db>),
}

impl<'db> TryFrom<Item<'db>> for Container<'db> {
    type Error = Infallible;

    fn try_from(value: Item<'db>) -> Result<Self, Self::Error> {
        Ok(match value {
            Item::Contract(contract_id) => Self::Contract(contract_id),
            Item::Enum(enumeration_id) => Self::Enum(enumeration_id),
            Item::Struct(structure_id) => Self::Structure(structure_id),
            item => Self::Item(value)
        })
    }
}


#[salsa::tracked]
impl<'db> Container<'db> {
    pub fn defs(self, db: &'db dyn BaseDb) -> Vec<(Ident<'db>, Definition<'db>)> {
        match self {
            Self::Item(item) => match item {
                Item::Module(source_unit) => source_unit
                    .items(db)
                    .iter()
                    .filter(|i| {
                        matches!(
                            i,
                            Item::Contract(_)
                                | Item::Enum(_)
                                | Item::UserDefinedValueType(_)
                                | Item::Error(_)
                                | Item::Event(_)
                                | Item::Function(_)
                                | Item::StateVariable(_)
                                | Item::Struct(_)
                                | Item::Constructor(_)
                                | Item::Modifier(_)
                                | Item::Module(_)
                        )
                    })
                    .flat_map(|item| {
                        item.name(db).map(|name| (name, Definition::Item(Item::from(*item))))
                    })
                    .collect(),
                Item::Contract(contract) => contract
                    .items(db)
                    .iter()
                    .filter(|i| {
                        matches!(
                            i,
                            hir_def::ContractItem::Constructor(_)
                                | hir_def::ContractItem::Function(_)
                                | hir_def::ContractItem::Modifier(_)
                                | hir_def::ContractItem::UserDefinedValueType(_)
                                | hir_def::ContractItem::Struct(_)
                                | hir_def::ContractItem::Enum(_)
                                | hir_def::ContractItem::Event(_)
                                | hir_def::ContractItem::StateVariable(_)
                                | hir_def::ContractItem::Error(_)
                        )
                    })
                    .flat_map(|item| {
                        item.name(db).map(|name| (name, Definition::Item(Item::from(*item))))
                    })
                    .collect(),
                _ => {
                    vec![]
                }
            },
            Container::Contract(contract_id) => contract_id
                .items(db)
                .iter()
                .filter(|i| {
                    matches!(
                        i,
                        hir_def::ContractItem::Function(_)
                            | hir_def::ContractItem::Modifier(_)
                            | hir_def::ContractItem::StateVariable(_)
                    )
                })
                .flat_map(|item| {
                    item.name(db).map(|name| (name, Definition::Item(Item::from(*item))))
                })
                .collect(),
            Container::Structure(structure_id) => structure_id
                .fields(db)
                .iter()
                .map(|item| (item.name(db), Definition::Field(*item)))
                .collect(),
            Container::Enum(enumeration_id) => enumeration_id
                .fields(db)
                .iter()
                .map(|item| (item.name(db), Definition::EnumVariant(*item)))
                .collect(),
        }
    }
}
