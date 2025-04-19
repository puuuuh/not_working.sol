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
use crate::nameres::body::Definition;
use crate::nameres::scope::{ItemScope, Scope};
use crate::nameres::ImportResolution;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::source_map::span_map::SpanMap;
use crate::{impl_major_item, lazy_field, FileExt};
use base_db::{BaseDb, File, Project};
use indexmap::IndexMap;
use rowan::TextRange;
use salsa::Database;

use super::statement::StatementId;
use super::{user_defined_value_type, ExprId, HasDefs, HasSourceUnit};

#[salsa::tracked(debug)]
pub struct SourceUnit<'db> {
    #[return_ref]
    pub items: Vec<Item<'db>>,

    #[return_ref]
    pub item_map: SpanMap<Item<'db>>
}

#[salsa::tracked]
fn lower_file<'db>(db: &'db dyn BaseDb, file: File) -> SourceUnit<'db> {
    let input = file.node(db);
    let mut lower = LowerCtx::new(db, file);
    let items = lower.lower_source(input);

    let item_tree = SourceUnit::new(db, items, SpanMap::new(core::mem::take(&mut lower.spans)));

    item_tree
}

impl<'db> HasSourceUnit<'db> for File {
    fn source_unit(self, db: &'db dyn BaseDb) -> SourceUnit<'db> {
        lower_file(db, self)
    }
}

#[salsa::tracked]
impl<'db> SourceUnit<'db> {
    #[salsa::tracked]
    pub fn scope_by_stmt(self, db: &'db dyn BaseDb, project: Project, module: File, stmt: StatementId<'db>) -> Scope<'db> {
        if let Some(node) = stmt.node(db) {
            return self.item_scope_by_range(db, project, module, node.syntax_node_ptr().text_range());
        }

        Scope::Item(self.scope(db, project, module))
    }

    #[salsa::tracked]
    pub fn scope_by_expr(self, db: &'db dyn BaseDb, project: Project, module: File, expr: ExprId<'db>) -> Scope<'db> {
        if let Some(node) = expr.node(db) {
            return self.item_scope_by_range(db, project, module, node.syntax_node_ptr().text_range());
        }

        Scope::Item(self.scope(db, project, module))
    }

    pub fn item_scope_by_range(self, db: &'db dyn BaseDb, project: Project, module: File, range: TextRange) -> Scope<'db> {
        if let Some(item) = self.item_map(db).find(range) {
            return item.scope(db, project, module);
        }

        Scope::Item(self.scope(db, project, module))
    }
    
    #[salsa::tracked(return_ref)]
    pub fn named_items(self, db: &'db dyn BaseDb) -> IndexMap<Ident<'db>, Item<'db>> {
        let top_items = self.items(db);
        top_items.iter().filter_map(|a| a.name(db).map(|name| (name, *a)))
            .collect()
    }

    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> ItemScope<'db> {
        return ItemScope::new(db, None, self.resolve_imports(db, project, module).items(db).clone())
    }

    #[salsa::tracked]
    pub fn resolve_imports(self, db: &'db dyn BaseDb, project: Project, module: File) -> ImportResolution<'db> {
        ImportResolution::from_file(db, project, module)
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
                Item::Import(i)=>imports.push(*i),
                Item::Pragma(p)=>pragmas.push(*p),
                Item::Using(u)=>usings.push(*u),
                Item::Contract(c)=>contracts.push(*c),
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

#[salsa::tracked]
impl<'db> HasDefs<'db> for SourceUnit<'db> {
    #[salsa::tracked]
    fn defs(self, db: &'db dyn BaseDb, module: File) -> Vec<(Ident<'db>, Definition<'db>)> {
        self.items(db)
            .iter()
            .filter_map(|item| item.name(db).map(|name| (name, Definition::Item((module, *item)))))
            .collect()
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord, salsa::Update)]
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

    Module(SourceUnit<'db>)
}

impl<'db> HasDefs<'db> for Item<'db> {
    fn defs(self, db: &'db dyn BaseDb, module: File) -> Vec<(Ident<'db>, Definition<'db>)> {
        match self {
            Item::Contract(contract_id) => {
                contract_id.defs(db, module)
            },
            Item::Enum(enumeration_id) => {
                enumeration_id.defs(db, module)
            },
            Item::Struct(structure_id) => {
                structure_id.defs(db, module)
            },
            Item::Module(source_unit) => {
                source_unit.defs(db, module)
            },
            _ => vec![]
        }
    }
}

impl<'db> Item<'db> {
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> Scope<'db> {
        match self {
            Item::StateVariable(s) => Scope::Item(s.scope(db, project, module)),
            Item::Struct(s) => Scope::Item(s.scope(db, project, module)),
            Item::Import(s) => Scope::Item(s.scope(db, project, module)),
            Item::Enum(s) => Scope::Item(s.scope(db, project, module)),
            Item::UserDefinedValueType(s) => Scope::Item(s.scope(db, project, module)),
            Item::Error(s) => Scope::Item(s.scope(db, project, module)),
            Item::Event(s) => Scope::Item(s.scope(db, project, module)),
            Item::Pragma(s) => Scope::Item(s.scope(db, project, module)),
            Item::Using(s) => Scope::Item(module.source_unit(db).scope(db, project, module)),
            Item::Contract(contract_id) => Scope::Item(contract_id.scope(db, project, module)),
            Item::Function(function_id) => Scope::Body(function_id.scope(db, project, module)),
            Item::Constructor(constructor_id) => Scope::Body(constructor_id.scope(db, project, module)),
            Item::Modifier(modifier_id) => Scope::Body(modifier_id.scope(db, project, module)),
            Item::Module(source_unit) => Scope::Item(source_unit.scope(db, project, module)),
        }
    }
    pub fn name(self, db: &'db dyn Database) -> Option<Ident<'db>> {
        match self {
            Item::Import(import_id) => None,
            Item::Pragma(pragma_id) => None,
            Item::Using(using_id) => None,
            Item::Contract(contract_id) => Some(contract_id.name(db)),
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

    pub fn body(self, db: &'db dyn BaseDb, module: File) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        match self {
            Item::Import(import_id) => None,
            Item::Pragma(pragma_id) => None,
            Item::Using(using_id) => None,
            Item::Contract(contract_id) => None,
            Item::Enum(enumeration_id) => None,
            Item::UserDefinedValueType(user_defined_value_type_id) => None,
            Item::Error(error_id) => None,
            Item::Event(event_id) => None,
            Item::Function(function_id) => function_id.body(db, module),
            Item::StateVariable(state_variable_id) => None,
            Item::Struct(structure_id) => None,
            Item::Constructor(constructor_id) => None,
            Item::Modifier(modifier_id) => modifier_id.body(db, module),
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