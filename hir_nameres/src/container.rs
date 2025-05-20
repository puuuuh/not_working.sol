use std::convert::Infallible;

use base_db::BaseDb;
use hir_def::{ContractId, ContractItem, EnumerationId, Ident, Item, SourceUnit, StructureId};
use vfs::File;

use crate::scope::body::Definition;

// Simple def container, exclude any overloadable entities
#[derive(
    Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Supertype, salsa::Update,
)]
pub enum Container<'db> {
    Item(Item<'db>),
}

impl<'db> TryFrom<Item<'db>> for Container<'db> {
    type Error = Infallible;

    fn try_from(value: Item<'db>) -> Result<Self, Self::Error> {
        Ok(match value {
            item => Self::Item(value),
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
                            ContractItem::Modifier(_)
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
        }
    }
}
