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
use crate::resolution::resolve_file;
use crate::scope::expr::ExprScopeRoot;
use crate::scope::{IndexMapUpdate, ItemScope, Scope};
use crate::source_map::item_source_map::ItemSourceMap;
use crate::source_map::span_map::SpanMap;
use crate::{impl_has_origin, impl_major_item, lazy_field, FileExt};
use base_db::{BaseDb, File, Project};
use indexmap::IndexMap;
use salsa::Database;

use super::statement::StatementId;
use super::{user_defined_value_type, HasOrigin};

#[salsa::tracked]
pub struct SourceUnit<'db> {
    pub file: File,

    #[return_ref]
    pub items: Vec<Item<'db>>,

    #[return_ref]
    pub span_map: SpanMap<Item<'db>>
}

#[salsa::tracked]
pub fn file_tree<'db>(db: &'db dyn BaseDb, file: File) -> SourceUnit<'db> {
    let input = file.tree(db);
    let mut lower = LowerCtx::new(db, file);
    let items = lower.lower_source(input);

    let item_tree = SourceUnit::new(db, file, items, SpanMap::new(core::mem::take(&mut lower.spans)));
    for c in item_tree.items(db) {
        c.set_origin(db, ItemOrigin::Root(item_tree))
    }
    item_tree
}

#[salsa::tracked]
impl<'db> SourceUnit<'db> {
    pub fn named_items(self, db: &'db dyn Database) -> IndexMap<Ident<'db>, Item<'db>> {
        let top_items = self.items(db);
        top_items.iter().filter_map(|a| a.name(db).map(|name| (name, (*a).into()))).collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn data(self, db: &'db dyn Database) -> ItemTreeData<'db> {
        let mut imports = vec![];
        let mut pragmas = vec![];
        let mut usings = vec![];
        let mut contracts = vec![];
        let mut libraries = vec![];
        let mut interfaces = vec![];
        let mut enums = vec![];
        let mut user_type_definitions = vec![];
        let mut errors = vec![];
        let mut events = vec![];
        let mut functions = vec![];
        let mut state_variables = vec![];
        let mut structs = vec![];
        for i in self.items(db) {
            match i {
                Item::Import(i)=>imports.push(*i),
                Item::Pragma(p)=>pragmas.push(*p),
                Item::Using(u)=>usings.push(*u),
                Item::Contract(c)=>contracts.push(*c),
                Item::Library(l)=>libraries.push(*l),
                Item::Interface(i)=>interfaces.push(*i),
                Item::Enum(e)=>enums.push(*e),
                Item::Struct(s)=>structs.push(*s),
                Item::UserDefinedValueType(t)=>user_type_definitions.push(*t),
                Item::Error(e)=>errors.push(*e),
                Item::Event(e)=>events.push(*e),
                Item::Function(f)=>functions.push(*f),
                Item::StateVariable(s)=>state_variables.push(*s),
                // Invalid items
                Item::Constructor(constructor_id) => {},
                Item::Modifier(modifier_id) => {},
                Item::Module(_) => {},
            }
        }

        ItemTreeData::new(
            db,
            imports,
            pragmas,
            usings,
            contracts,
            libraries,
            interfaces,
            enums,
            structs,
            user_type_definitions,
            errors,
            events,
            functions,
            state_variables,
        )
    }

    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let mut items = resolve_file(db, project, self.file(db));

        ItemScope::new(db, None, items.items(db).clone())
    }
}

#[salsa::tracked]
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
    pub libraries: Vec<ContractId<'db>>,
    #[return_ref]
    pub interfaces: Vec<ContractId<'db>>,

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
pub enum Item<'db> {
    Import(ImportId<'db>),
    Pragma(PragmaId<'db>),
    Using(UsingId<'db>),

    Contract(ContractId<'db>),
    Library(ContractId<'db>),
    Interface(ContractId<'db>),

    Enum(EnumerationId<'db>),
    UserDefinedValueType(UserDefinedValueTypeId<'db>),
    Error(ErrorId<'db>),
    Event(EventId<'db>),
    Function(FunctionId<'db>),

    StateVariable(StateVariableId<'db>),
    Struct(StructureId<'db>),

    Constructor(ConstructorId<'db>),
    Modifier(ModifierId<'db>),

    Module(SourceUnit<'db>)
}


impl<'db> Item<'db> {
    pub fn set_origin(self, db: &'db dyn Database, origin: ItemOrigin) {
        match self {
            Item::Import(import_id) => import_id.set_origin(db, origin),
            Item::Pragma(pragma_id) => pragma_id.set_origin(db, origin),
            Item::Using(using_id) => using_id.set_origin(db, origin),
            Item::Contract(contract_id)
                                | Item::Library(contract_id)
                                | Item::Interface(contract_id) => contract_id.set_origin(db, origin),
            Item::Enum(enumeration_id) => enumeration_id.set_origin(db, origin),
            Item::UserDefinedValueType(user_defined_value_type_id) => user_defined_value_type_id.set_origin(db, origin),
            Item::Error(error_id) => error_id.set_origin(db, origin),
            Item::Event(event_id) => event_id.set_origin(db, origin),
            Item::Function(function_id) => function_id.set_origin(db, origin),
            Item::StateVariable(state_variable_id) => state_variable_id.set_origin(db, origin),
            Item::Struct(structure_id) => structure_id.set_origin(db, origin),
            Item::Constructor(constructor_id) => constructor_id.set_origin(db, origin),
            Item::Modifier(modifier_id) => modifier_id.set_origin(db, origin),
            Item::Module(source_unit) => {},
        }
    }

    pub fn origin(self, db: &'db dyn Database) -> ItemOrigin<'db> {
        match self {
            Item::Import(import_id) => import_id.origin(db),
            Item::Pragma(pragma_id) => pragma_id.origin(db),
            Item::Using(using_id) => using_id.origin(db),
            Item::Contract(contract_id)
                                | Item::Library(contract_id)
                                | Item::Interface(contract_id) => contract_id.origin(db),
            Item::Enum(enumeration_id) => enumeration_id.origin(db),
            Item::UserDefinedValueType(user_defined_value_type_id) => user_defined_value_type_id.origin(db),
            Item::Error(error_id) => error_id.origin(db),
            Item::Event(event_id) => event_id.origin(db),
            Item::Function(function_id) => function_id.origin(db),
            Item::StateVariable(state_variable_id) => state_variable_id.origin(db),
            Item::Struct(structure_id) => structure_id.origin(db),
            Item::Constructor(constructor_id) => constructor_id.origin(db),
            Item::Modifier(modifier_id) => modifier_id.origin(db),
            Item::Module(source_unit) => { todo!() },
        }
    }

    pub fn name(self, db: &'db dyn Database) -> Option<Ident<'db>> {
        match self {
            Item::Import(import_id) => None,
            Item::Pragma(pragma_id) => None,
            Item::Using(using_id) => None,
            Item::Contract(contract_id)
                | Item::Library(contract_id)
                | Item::Interface(contract_id) => Some(contract_id.name(db)),
            Item::Enum(enumeration_id) => Some(enumeration_id.name(db)),
            Item::UserDefinedValueType(user_defined_value_type_id) => Some(user_defined_value_type_id.name(db)),
            Item::Error(error_id) => Some(error_id.name(db)),
            Item::Event(event_id) => Some(event_id.name(db)),
            Item::Function(function_id) => function_id.name(db),
            Item::StateVariable(state_variable_id) => Some(state_variable_id.name(db)),
            Item::Struct(structure_id) => Some(structure_id.name(db)),
            Item::Constructor(constructor_id) => None,
            Item::Modifier(modifier_id) => Some(modifier_id.name(db)),
            Item::Module(source_unit) => None
        }
    }

    pub fn scope(self, db: &'db dyn BaseDb, project: Project) -> Option<Scope<'db>> {
        Some(Scope::Item(match self {
            Item::Import(import_id) => import_id.item_origin(db).scope(db, project),
            Item::Pragma(pragma_id) => pragma_id.item_origin(db).scope(db, project),
            Item::Using(using_id) => using_id.item_origin(db).scope(db, project),
            Item::Contract(contract_id) |
            Item::Library(contract_id) |
            Item::Interface(contract_id) => contract_id.scope(db, project),
            Item::Enum(enumeration_id) => enumeration_id.item_origin(db).scope(db, project),
            Item::UserDefinedValueType(user_defined_value_type_id) => user_defined_value_type_id.item_origin(db).scope(db, project),
            Item::Error(error_id) => error_id.item_origin(db).scope(db, project),
            Item::Event(event_id) => event_id.item_origin(db).scope(db, project),
            Item::Function(function_id) => function_id.item_origin(db).scope(db, project),
            Item::StateVariable(state_variable_id) => state_variable_id.item_origin(db).scope(db, project),
            Item::Struct(structure_id) => structure_id.item_origin(db).scope(db, project),
            Item::Constructor(constructor_id) => constructor_id.item_origin(db).scope(db, project),
            Item::Modifier(modifier_id) => modifier_id.item_origin(db).scope(db, project),
            Item::Module(source_unit) => source_unit.scope(db, project),
        }))
    }

    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        match self {
            Item::Import(import_id) => None,
            Item::Pragma(pragma_id) => None,
            Item::Using(using_id) => None,
            Item::Contract(contract_id) => None,
            Item::Library(contract_id) => None,
            Item::Interface(contract_id) => None,
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

#[derive(Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Update)]
pub enum ItemOrigin<'db> {
    Root(SourceUnit<'db>),
    Contract(ContractId<'db>),
}

impl<'db> ItemOrigin<'db> {
    pub fn scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        match self {
            ItemOrigin::Contract(c) => c.scope(db, project),
            ItemOrigin::Root(c) => c.scope(db, project)
        }
    }
}
