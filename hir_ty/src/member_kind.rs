use hir_def::{EnumerationVariantId, Item, StructureFieldId, VariableDeclaration};

use crate::tys::{Ty, TyKindInterned};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum MemberKind<'db> {
    Item((Item<'db>)),
    Field(StructureFieldId<'db>),
    EnumVariant(EnumerationVariantId<'db>),
    SynteticItem(Ty<'db>),
}