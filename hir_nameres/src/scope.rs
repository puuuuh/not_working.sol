pub mod body;
pub mod item;

use base_db::{BaseDb, Project};
pub use body::BodyScope;
use hir_def::{
    Constructor, ConstructorId, ContractId, EnumerationId, ErrorId, EventId, ExprId, FunctionId,
    Ident, IdentPath, ImportId, Item, ModifierId, PragmaId, SourceUnit, StateVariableId,
    StatementId, StructureId, UserDefinedValueTypeId, UsingId, lower_file,
};
pub use item::ItemScope;

use indexmap::IndexMap;
use salsa::{Database, Update};
use smallvec::SmallVec;
use std::{
    collections::{BTreeMap, btree_map},
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
};
use vfs::File;

use crate::{
    HasDefs, container::Container, import::resolve_file_root, inheritance::inheritance_chain,
};

use super::scope::body::Definition;
use salsa::plumbing::AsId;
mod prelude;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, salsa::Update)]
pub enum Scope<'db> {
    Item(ItemScope<'db>),
    Body(BodyScope<'db>),
    Expr(BodyScope<'db>, usize),
}

impl<'db> From<ItemScope<'db>> for Scope<'db> {
    fn from(value: ItemScope<'db>) -> Self {
        Scope::Item(value)
    }
}

impl<'db> From<BodyScope<'db>> for Scope<'db> {
    fn from(value: BodyScope<'db>) -> Self {
        Scope::Body(value)
    }
}

impl<'db> Scope<'db> {
    pub fn all_definitions(
        self,
        db: &'db dyn BaseDb,
    ) -> BTreeMap<Ident<'db>, SmallVec<[Definition<'db>; 1]>> {
        match self {
            Scope::Item(item_scope) => item_scope.all_definitions(db),
            Scope::Body(body_scope) => body_scope.parent(db).all_definitions(db),
            Scope::Expr(body_scope, i) => body_scope.all_definitions(db, i),
        }
    }

    pub fn find_all(self, db: &'db dyn BaseDb, name: Ident<'db>) -> SmallVec<[Definition<'db>; 1]> {
        match self {
            Scope::Item(item_scope) => item_scope.find_all(db, name),
            Scope::Body(body_scope) => body_scope.parent(db).find_all(db, name),
            Scope::Expr(body_scope, i) => body_scope.find_all(db, i, name),
        }
    }

    pub fn find(self, db: &'db dyn BaseDb, name: Ident<'db>) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.find(db, name),
            Scope::Body(body_scope) => body_scope.parent(db).find(db, name),
            Scope::Expr(body_scope, i) => body_scope.find(db, i, name),
        }
    }

    pub fn find_in_expr(
        self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        name: Ident<'db>,
    ) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.find(db, name),
            Scope::Body(expr_scope_root) | Scope::Expr(expr_scope_root, _) => {
                expr_scope_root.find_in_expr(db, expr, name)
            }
        }
    }

    pub fn lookup_path(self, db: &'db dyn BaseDb, path: &[Ident<'db>]) -> Option<Definition<'db>> {
        let mut path = path.into_iter();
        let start = path.next()?;
        if let mut def @ Definition::Item(item) = self.find(db, *start)? {
            let mut file = item.file(db);
            'ident_loop: for ident in path {
                let container = Container::try_from(item).ok()?;
                for (name, new_def) in container.defs(db) {
                    if *ident == name {
                        def = new_def;
                        if let Definition::Item(item) = new_def {
                            file = item.file(db);
                        }
                        continue 'ident_loop;
                    }
                }
                return None;
            }

            Some(def)
        } else {
            None
        }
    }

    pub fn for_stmt(self, db: &'db dyn BaseDb, stmt: StatementId<'db>) -> Scope<'db> {
        match self {
            Scope::Item(item_scope) => Scope::Item(item_scope),
            Scope::Body(body_scope) | Scope::Expr(body_scope, _) => {
                if let Some(i) = body_scope.scope_by_salsa_id(db).get(&stmt.as_id()) {
                    Scope::Expr(body_scope, *i)
                } else {
                    Scope::Body(body_scope)
                }
            }
        }
    }

    pub fn for_expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Scope<'db> {
        match self {
            Scope::Item(item_scope) => Scope::Item(item_scope),
            Scope::Body(body_scope) | Scope::Expr(body_scope, _) => {
                if let Some(i) = body_scope.scope_by_salsa_id(db).get(&expr.as_id()) {
                    Scope::Expr(body_scope, *i)
                } else {
                    Scope::Body(body_scope)
                }
            }
        }
    }
}

pub trait HasScope<'db> {
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db>;
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db>;
}

#[salsa::tracked]
impl<'db> HasScope<'db> for ConstructorId<'db> {
    #[salsa::tracked]
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        self.origin(db)
            .map(|c| c.item_scope(db, project))
            .unwrap_or_else(|| lower_file(db, self.file(db)).item_scope(db, project))
    }

    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        BodyScope::from_body(
            db,
            project,
            self.item_scope(db, project),
            Item::Constructor(self),
            self.info(db).args.iter().copied().collect(),
            self.body(db).map(|a| a.0),
        )
        .into()
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for ContractId<'db> {
    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        Scope::Item(self.item_scope(db, project))
    }

    #[salsa::tracked]
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let chain = inheritance_chain(db, project, self);

        let mut items: BTreeMap<Ident<'_>, smallvec::SmallVec<[Definition<'_>; 1]>> =
            BTreeMap::new();
        for c in chain.into_iter() {
            let named_items = c
                .items(db)
                .iter()
                .filter_map(|a| Some((a.name(db)?, Definition::Item((*a).into()))));
            for (name, def) in named_items {
                match items.entry(name) {
                    btree_map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert([def].into_iter().collect());
                    }
                    btree_map::Entry::Occupied(mut occupied_entry) => {
                        occupied_entry.get_mut().push(def);
                    }
                }
            }
        }

        ItemScope::new(
            db,
            Some(
                self.origin(db)
                    .map(|c| c.item_scope(db, project))
                    .unwrap_or_else(|| lower_file(db, self.file(db)).item_scope(db, project)),
            ),
            items,
        )
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for FunctionId<'db> {
    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        let map = lower_file(db, self.file(db));
        let info = self.info(db);
        BodyScope::from_body(
            db,
            project,
            self.item_scope(db, project),
            Item::Function(self),
            info.args.iter().chain(info.returns.iter().flatten()).copied().collect(),
            self.body(db).map(|a| a.0),
        )
        .into()
    }

    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        self.origin(db)
            .map(|c| c.item_scope(db, project))
            .unwrap_or_else(|| lower_file(db, self.file(db)).item_scope(db, project))
    }
}

macro_rules! impl_has_scope {
    ($($t:ty),+) => {
        $(
            #[salsa::tracked]
            impl<'db> HasScope<'db> for $t {
                #[salsa::tracked]
                fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
                    self.origin(db)
                        .map(|c| c.item_scope(db, project))
                        .unwrap_or_else(|| lower_file(db, self.file(db)).item_scope(db, project))
                        .into()
                }

                fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
                    Scope::Item(self.item_scope(db, project))
                }
            }
        )+
    };
}

impl_has_scope!(
    EnumerationId<'db>,
    EventId<'db>,
    StateVariableId<'db>,
    StructureId<'db>,
    UserDefinedValueTypeId<'db>,
    UsingId<'db>,
    ErrorId<'db>,
    ModifierId<'db>
);

#[salsa::tracked]
impl<'db> HasScope<'db> for SourceUnit<'db> {
    #[salsa::tracked]
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let imports = resolve_file_root(db, project, self.file(db));

        ItemScope::new(
            db,
            Some(prelude::prelude(db)),
            imports
                .iter()
                .map(|(name, defs)| (*name, defs.iter().map(|t| Definition::Item(*t)).collect()))
                .collect(),
        )
    }

    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        Scope::Item(self.item_scope(db, project))
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for ImportId<'db> {
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let file = self.file(db);
        lower_file(db, file).item_scope(db, project)
    }

    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        Scope::Item(self.item_scope(db, project))
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for PragmaId<'db> {
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let file = self.file(db);
        lower_file(db, file).item_scope(db, project)
    }

    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        Scope::Item(self.item_scope(db, project))
    }
}

impl<'db> HasScope<'db> for Item<'db> {
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        match self {
            Item::Import(import_id) => import_id.scope(db, project),
            Item::Pragma(pragma_id) => pragma_id.scope(db, project),
            Item::Using(using_id) => using_id.scope(db, project),
            Item::Contract(contract_id) => contract_id.scope(db, project),
            Item::Enum(enumeration_id) => enumeration_id.scope(db, project),
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                user_defined_value_type_id.scope(db, project)
            }
            Item::Error(error_id) => error_id.scope(db, project),
            Item::Event(event_id) => event_id.scope(db, project),
            Item::Function(function_id) => function_id.scope(db, project),
            Item::StateVariable(state_variable_id) => state_variable_id.scope(db, project),
            Item::Struct(structure_id) => structure_id.scope(db, project),
            Item::Constructor(constructor_id) => constructor_id.scope(db, project),
            Item::Modifier(modifier_id) => modifier_id.scope(db, project),
            Item::Module(source_unit) => source_unit.scope(db, project),
        }
    }

    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        match self {
            Item::Import(import_id) => import_id.item_scope(db, project),
            Item::Pragma(pragma_id) => pragma_id.item_scope(db, project),
            Item::Using(using_id) => using_id.item_scope(db, project),
            Item::Contract(contract_id) => contract_id.item_scope(db, project),
            Item::Enum(enumeration_id) => enumeration_id.item_scope(db, project),
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                user_defined_value_type_id.item_scope(db, project)
            }
            Item::Error(error_id) => error_id.item_scope(db, project),
            Item::Event(event_id) => event_id.item_scope(db, project),
            Item::Function(function_id) => function_id.item_scope(db, project),
            Item::StateVariable(state_variable_id) => state_variable_id.item_scope(db, project),
            Item::Struct(structure_id) => structure_id.item_scope(db, project),
            Item::Constructor(constructor_id) => constructor_id.item_scope(db, project),
            Item::Modifier(modifier_id) => modifier_id.item_scope(db, project),
            Item::Module(source_unit) => source_unit.item_scope(db, project),
        }
    }
}
