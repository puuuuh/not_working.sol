use hir_def::{EnumerationVariantId, FunctionId, Item, StructureFieldId, VariableDeclaration};

use crate::{callable::Callable, tys::{Ty, TyKindInterned}};

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub enum MemberKind<'db> {
    Item((Item<'db>)),
    ExtensionFunction(FunctionId<'db>, Callable<'db>),
    Field(StructureFieldId<'db>),
    EnumVariant(EnumerationVariantId<'db>),
    SynteticItem(Ty<'db>),
}
