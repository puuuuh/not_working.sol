use hir_def::{hir::{constructor::ConstructorId, contract::ContractId, enumeration::EnumerationId, error::ErrorId, event::EventId, function::FunctionId, modifier::ModifierId, source_unit::{Item, SourceUnit}, state_variable::StateVariableId, statement::StatementId, structure::StructureId, user_defined_value_type::UserDefinedValueTypeId}, scope::expr::DefinitionSite};

#[derive(Debug, Clone, Copy)]
pub enum Definition<'db> {
    Module(SourceUnit<'db>),
    Contract(ContractId<'db>),
    Function(FunctionId<'db>),
    Modifier(ModifierId<'db>),
    Constructor(ConstructorId<'db>),
    StateVariable(StateVariableId<'db>),
    Local(StatementId<'db>),
    Enum(EnumerationId<'db>),
    Event(EventId<'db>),
    Error(ErrorId<'db>),
    Struct(StructureId<'db>),
    UserType(UserDefinedValueTypeId<'db>),
}

pub trait ToDef<'db> {
    fn to_def(&self) -> Option<Definition<'db>>;
}

impl<'db> ToDef<'db> for DefinitionSite<'db> {
    fn to_def(&self) -> Option<Definition<'db>> {
        Some(match self {
            DefinitionSite::Item(item) => item.to_def()?,
            DefinitionSite::Statement(statement_id) => Definition::Local(*statement_id),
            DefinitionSite::Argument(argument_id) => ,
        })
    }
}

impl<'db> ToDef<'db> for Item<'db> {
    fn to_def(&self) -> Option<Definition<'db>> {
        Some(match self {
            Item::Contract(contract_id) |
            Item::Library(contract_id) |
            Item::Interface(contract_id) => Definition::Contract(*contract_id),
            Item::Enum(enumeration_id) => Definition::Enum(*enumeration_id),
            Item::UserDefinedValueType(user_defined_value_type_id) => Definition::UserType(*user_defined_value_type_id),
            Item::Error(error_id) => Definition::Error(*error_id),
            Item::Event(event_id) => Definition::Event(*event_id),
            Item::Function(function_id) => Definition::Function(*function_id),
            Item::StateVariable(state_variable_id) => Definition::StateVariable(*state_variable_id),
            Item::Struct(structure_id) => Definition::Struct(*structure_id),
            Item::Constructor(constructor_id) => Definition::Constructor(*constructor_id),
            Item::Modifier(modifier_id) => Definition::Modifier(*modifier_id),
            Item::Module(source_unit) => Definition::Module(*source_unit),
            _ => return None,
        })
    }
}