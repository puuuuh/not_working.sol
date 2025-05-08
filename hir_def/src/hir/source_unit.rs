use std::collections::BTreeSet;

use crate::hir::constructor::ConstructorId;
use crate::hir::contract::{ContractId, ContractItem};
use crate::hir::enumeration::EnumerationId;
use crate::hir::error::ErrorId;
use crate::hir::event::EventId;
use crate::hir::function::FunctionId;
use crate::hir::ident::Ident;
use crate::hir::import::ImportId;
use crate::hir::modifier::ModifierId;
use crate::hir::pragma::PragmaId;
use crate::hir::state_variable::StateVariableId;
use crate::hir::structure::StructureId;
use crate::hir::user_defined_value_type::UserDefinedValueTypeId;
use crate::hir::using::UsingId;
use crate::lower::LowerCtx;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::source_map::span_map::SpanMap;
use crate::{impl_major_item, lazy_field, parse_file, FileExt};
use base_db::{BaseDb, File, Project};
use indexmap::IndexMap;
use rowan::TextRange;
use salsa::Database;

use super::statement::StatementId;
use super::{user_defined_value_type};

#[salsa::tracked(debug)]
pub struct SourceUnit<'db> {
    #[id]
    pub file: File,

    #[return_ref]
    #[tracked]
    pub items: BTreeSet<Item<'db>>,

    #[return_ref]
    #[tracked]
    pub source_map: SpanMap<Item<'db>>,
}

#[salsa::tracked]
pub fn lower_file<'db>(db: &'db dyn BaseDb, file: File) -> SourceUnit<'db> {
    let input = parse_file(db, file);
    let mut lower = LowerCtx::new(db, file);
    let items = lower.lower_source(input.node());

    let item_tree =
        SourceUnit::new(db, file, items, SpanMap::new(core::mem::take(&mut lower.spans)));

    item_tree
}

#[salsa::tracked]
impl<'db> SourceUnit<'db> {
    #[salsa::tracked(return_ref)]
    pub fn named_items(self, db: &'db dyn BaseDb) -> IndexMap<Ident<'db>, Item<'db>> {
        let top_items = self.items(db);
        top_items.iter().filter_map(|a| a.name(db).map(|name| (name, *a))).collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn data(self, db: &'db dyn BaseDb) -> ItemTreeData<'db> {
        let mut imports = vec![];
        let mut pragmas = vec![];
        let mut usings = vec![];
        let mut contracts = vec![];
        let mut enums = vec![];
        let mut user_type_definitions = vec![];
        let mut errors = vec![];
        let mut events = vec![];
        let mut functions = vec![];
        let mut state_variables = vec![];
        let mut structs = vec![];
        for i in self.items(db) {
            match i {
                Item::Import(i) => imports.push(*i),
                Item::Pragma(p) => pragmas.push(*p),
                Item::Using(u) => usings.push(*u),
                Item::Contract(c) => contracts.push(*c),
                Item::Enum(e) => enums.push(*e),
                Item::Struct(s) => structs.push(*s),
                Item::UserDefinedValueType(t) => user_type_definitions.push(*t),
                Item::Error(e) => errors.push(*e),
                Item::Event(e) => events.push(*e),
                Item::Function(f) => functions.push(*f),
                Item::StateVariable(s) => state_variables.push(*s),
                // Invalid items
                Item::Constructor(constructor_id) => {}
                Item::Modifier(modifier_id) => {}
                Item::Module(_) => {}
            }
        }

        ItemTreeData::new(
            db,
            imports,
            pragmas,
            usings,
            contracts,
            enums,
            structs,
            user_type_definitions,
            errors,
            events,
            functions,
            state_variables,
        )
    }
}

#[salsa::tracked(debug)]
pub struct ItemTreeData<'db> {
    #[tracked]
    #[return_ref]
    pub imports: Vec<ImportId<'db>>,
    #[return_ref]
    pub pragmas: Vec<PragmaId<'db>>,
    #[return_ref]
    pub usings: Vec<UsingId<'db>>,
    #[return_ref]
    pub contracts: Vec<ContractId<'db>>,

    #[return_ref]
    pub enums: Vec<EnumerationId<'db>>,
    #[return_ref]
    pub structs: Vec<StructureId<'db>>,
    #[return_ref]
    pub user_type_definitions: Vec<UserDefinedValueTypeId<'db>>,
    #[return_ref]
    pub errors: Vec<ErrorId<'db>>,
    #[return_ref]
    pub events: Vec<EventId<'db>>,
    #[return_ref]
    pub functions: Vec<FunctionId<'db>>,
    #[return_ref]
    pub state_variables: Vec<StateVariableId<'db>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Update)]
pub enum NamedItem<'db> {
    Contract(ContractId<'db>),

    Enum(EnumerationId<'db>),
    UserDefinedValueType(UserDefinedValueTypeId<'db>),
    Error(ErrorId<'db>),
    Event(EventId<'db>),
    Function(FunctionId<'db>),

    StateVariable(StateVariableId<'db>),
    Struct(StructureId<'db>),

    Modifier(ModifierId<'db>),
}

#[derive(
    Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Supertype, salsa::Update,
)]
pub enum Item<'db> {
    Import(ImportId<'db>),
    Pragma(PragmaId<'db>),
    Using(UsingId<'db>),

    Contract(ContractId<'db>),

    Enum(EnumerationId<'db>),
    UserDefinedValueType(UserDefinedValueTypeId<'db>),
    Error(ErrorId<'db>),
    Event(EventId<'db>),
    Function(FunctionId<'db>),

    StateVariable(StateVariableId<'db>),
    Struct(StructureId<'db>),

    Constructor(ConstructorId<'db>),
    Modifier(ModifierId<'db>),

    Module(SourceUnit<'db>),
}

impl<'db> Item<'db> {
    pub fn file(self, db: &'db dyn BaseDb) -> File {
        match self {
            Item::Import(import_id) => import_id.file(db),
            Item::Pragma(pragma_id) => pragma_id.file(db),
            Item::Using(using_id) => using_id.file(db),
            Item::Contract(contract_id) => contract_id.file(db),
            Item::Enum(enumeration_id) => enumeration_id.file(db),
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                user_defined_value_type_id.file(db)
            }
            Item::Error(error_id) => error_id.file(db),
            Item::Event(event_id) => event_id.file(db),
            Item::Function(function_id) => function_id.file(db),
            Item::StateVariable(state_variable_id) => state_variable_id.file(db),
            Item::Struct(structure_id) => structure_id.file(db),
            Item::Constructor(constructor_id) => constructor_id.file(db),
            Item::Modifier(modifier_id) => modifier_id.file(db),
            Item::Module(source_unit) => source_unit.file(db),
        }
    }

    pub fn name(self, db: &'db dyn Database) -> Option<Ident<'db>> {
        match self {
            Item::Import(import_id) => None,
            Item::Pragma(pragma_id) => None,
            Item::Using(using_id) => None,
            Item::Contract(contract_id) => Some(contract_id.name(db)),
            Item::Enum(enumeration_id) => Some(enumeration_id.name(db)),
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                Some(user_defined_value_type_id.name(db))
            }
            Item::Error(error_id) => Some(error_id.name(db)),
            Item::Event(event_id) => Some(event_id.name(db)),
            Item::Function(function_id) => function_id.name(db),
            Item::StateVariable(state_variable_id) => Some(state_variable_id.name(db)),
            Item::Struct(structure_id) => Some(structure_id.name(db)),
            Item::Constructor(constructor_id) => None,
            Item::Modifier(modifier_id) => Some(modifier_id.name(db)),
            Item::Module(source_unit) => None,
        }
    }

    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        match self {
            Item::Import(import_id) => None,
            Item::Pragma(pragma_id) => None,
            Item::Using(using_id) => None,
            Item::Contract(contract_id) => None,
            Item::Enum(enumeration_id) => None,
            Item::UserDefinedValueType(user_defined_value_type_id) => None,
            Item::Error(error_id) => None,
            Item::Event(event_id) => None,
            Item::Function(function_id) => function_id.body(db),
            Item::StateVariable(state_variable_id) => None,
            Item::Struct(structure_id) => None,
            Item::Constructor(constructor_id) => None,
            Item::Modifier(modifier_id) => modifier_id.body(db),
            Item::Module(source_unit) => None,
        }
    }
}

impl<'db> From<ContractItem<'db>> for Item<'db> {
    fn from(value: ContractItem<'db>) -> Self {
        match value {
            ContractItem::Constructor(i) => Self::Constructor(i),
            ContractItem::Function(i) => Self::Function(i),
            ContractItem::Modifier(i) => Self::Modifier(i),
            ContractItem::UserDefinedValueType(i) => Self::UserDefinedValueType(i),
            ContractItem::StateVariable(i) => Self::StateVariable(i),
            ContractItem::Struct(i) => Self::Struct(i),
            ContractItem::Enum(i) => Self::Enum(i),
            ContractItem::Event(i) => Self::Event(i),
            ContractItem::Error(i) => Self::Error(i),
            ContractItem::Using(i) => Self::Using(i),
        }
    }
}
