use crate::hir::constructor::ConstructorId;
use crate::hir::contract::ContractId;
use crate::hir::enumeration::EnumerationId;
use crate::hir::error::ErrorId;
use crate::hir::event::EventId;
use crate::hir::expr::{Expr, ExprId};
use crate::hir::function::FunctionId;
use crate::hir::import::ImportId;
use crate::hir::modifier::ModifierId;
use crate::hir::state_variable::StateVariableId;
use crate::hir::statement::StatementId;
use crate::hir::structure::StructureId;
use crate::hir::user_defined_value_type::UserDefinedValueTypeId;
use crate::hir::using::UsingId;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, salsa::Update)]
pub enum ChildSource<'db> {
    Constructor(ConstructorId<'db>),
    Contract(ContractId<'db>),
    Enum(EnumerationId<'db>),
    Error(ErrorId<'db>),
    Event(EventId<'db>),
    Function(FunctionId<'db>),
    Import(ImportId<'db>),
    Modifier(ModifierId<'db>),
    StateVariable(StateVariableId<'db>),
    Expr(ExprId<'db>),
    Statement(StatementId<'db>),
    Struct(StructureId<'db>),
    Using(UsingId<'db>),
    UserDefinedValueType(UserDefinedValueTypeId<'db>)
}