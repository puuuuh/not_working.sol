pub mod print;

use crate::hir::constructor::ConstructorId;
use crate::hir::contract::{ContractId, ContractItem};
use crate::hir::enumeration::EnumerationId;
use crate::hir::error::ErrorId;
use crate::hir::event::EventId;
use crate::hir::function::FunctionId;
use crate::hir::ident::Ident;
use crate::hir::import::ImportId;
use crate::hir::modifier::ModifierId;
use crate::hir::state_variable::StateVariableId;
use crate::hir::structure::StructureId;
use crate::hir::user_defined_value_type::UserDefinedValueTypeId;
use crate::hir::using::UsingId;
use crate::lower::Ctx;
use crate::scope::IndexMapUpdate;
use crate::{lazy_field, parse};
use base_db::{BaseDb, File, Project};
use indexmap::IndexMap;
use salsa::Database;
use crate::semantics::span_map::SpanMap;

#[salsa::tracked]
pub struct SourceUnit<'db> {
    pub file: File,
    pub project: Project,

    #[return_ref]
    pub items: Vec<TopItem<'db>>,

    #[return_ref]
    pub span_map: SpanMap<'db>
}

#[salsa::tracked]
pub fn file_tree<'db>(db: &'db dyn BaseDb, project: Project, file: File) -> SourceUnit<'db> {
    let input = parse(db, file).tree();
    let mut lower = Ctx::new(db, file);
    let items = lower.lower_source(input);

    let item_tree = SourceUnit::new(db, file, project, items, SpanMap::new(core::mem::take(&mut lower.spans)));
    for c in item_tree.data(db.as_dyn_database()).contracts(db) {
        c.set_def_site(db.as_dyn_database(), DefSite::Root(item_tree))
    }
    item_tree
}

#[salsa::tracked]
impl<'db> SourceUnit<'db> {
    pub fn named_top_items(self, db: &'db dyn Database) -> IndexMap<Ident<'db>, Item<'db>> {
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
                TopItem::Import(i) => imports.push(*i),
                TopItem::Pragma(p) => pragmas.push(*p),
                TopItem::Using(u) => usings.push(*u),
                TopItem::Contract(c) => contracts.push(*c),
                TopItem::Library(l) => libraries.push(*l),
                TopItem::Interface(i) => interfaces.push(*i),
                TopItem::Enum(e) => enums.push(*e),
                TopItem::Struct(s) => structs.push(*s),
                TopItem::UserDefinedValueType(t) => user_type_definitions.push(*t),
                TopItem::Error(e) => errors.push(*e),
                TopItem::Event(e) => events.push(*e),
                TopItem::Function(f) => functions.push(*f),
                TopItem::StateVariable(s) => state_variables.push(*s),
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
    pub fn scope(self, db: &'db dyn BaseDb) -> crate::scope::item::Scope<'db> {
        let mut items = self.named_top_items(db.as_dyn_database());

        let imports = self.data(db.as_dyn_database()).imports(db);
        for i in imports {
            if let Some(i) = i.items(db, self.project(db), self.file(db)) {
                items.extend(i.into_iter())
            }
        }

        crate::scope::item::Scope::new(db, None, IndexMapUpdate(items))
    }
}

#[salsa::tracked]
pub struct ItemTreeData<'db> {
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

#[salsa::tracked]
pub struct PragmaId<'db> {
    #[return_ref]
    pub data: String,
}

lazy_field!(PragmaId<'db>, def_site, set_def_site, DefSite<'db>);

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, PartialOrd, Ord, salsa::Update)]
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

    ItemTree(SourceUnit<'db>),
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

impl<'db> From<TopItem<'db>> for Item<'db> {
    fn from(value: TopItem<'db>) -> Self {
        match value {
            TopItem::Import(i) => Self::Import(i),
            TopItem::Pragma(i) => Self::Pragma(i),
            TopItem::Using(i) => Self::Using(i),
            TopItem::Contract(i) => Self::Contract(i),
            TopItem::Library(i) => Self::Library(i),
            TopItem::Interface(i) => Self::Interface(i),
            TopItem::Enum(i) => Self::Enum(i),
            TopItem::UserDefinedValueType(i) => Self::UserDefinedValueType(i),
            TopItem::Error(i) => Self::Error(i),
            TopItem::Event(i) => Self::Event(i),
            TopItem::Function(i) => Self::Function(i),
            TopItem::StateVariable(i) => Self::StateVariable(i),
            TopItem::Struct(i) => Self::Struct(i),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, PartialOrd, Ord, salsa::Update)]
pub enum TopItem<'db> {
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
}

impl<'db> TopItem<'db> {
    pub fn name(&self, db: &'db dyn Database) -> Option<Ident<'db>> {
        match self {
            TopItem::Import(_) => None,
            TopItem::Pragma(_) => None,
            TopItem::Using(_) => None,
            TopItem::Contract(i) => Some(i.name(db)),
            TopItem::Library(i) => Some(i.name(db)),
            TopItem::Interface(i) => Some(i.name(db)),
            TopItem::Enum(i) => Some(i.name(db)),
            TopItem::UserDefinedValueType(i) => Some(i.name(db)),
            TopItem::Error(i) => Some(i.name(db)),
            TopItem::Event(i) => Some(i.name(db)),
            TopItem::Function(i) => i.name(db),
            TopItem::StateVariable(i) => Some(i.name(db)),
            TopItem::Struct(i) => Some(i.name(db)),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, PartialOrd, Ord, salsa::Update)]
pub enum DefWithBody<'db> {
    Function(FunctionId<'db>),
    Constructor(ConstructorId<'db>),
    Modifier(ModifierId<'db>),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, PartialOrd, Ord, salsa::Update)]
pub enum DefSite<'db> {
    Root(SourceUnit<'db>),
    Contract(ContractId<'db>),
}

impl<'db> DefSite<'db> {
    pub fn scope(self, db: &'db dyn BaseDb) -> crate::scope::item::Scope<'db> {
        match self {
            DefSite::Contract(c) => c.scope(db),
            DefSite::Root(c) => c.scope(db),
        }
    }
}
