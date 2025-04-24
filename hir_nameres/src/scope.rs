pub mod item;
pub mod body;

use base_db::{BaseDb, Project};
pub use body::BodyScope;
use hir_def::{Constructor, ConstructorId, ContractId, EnumerationId, ErrorId, EventId, ExprId, FunctionId, HasSourceUnit, Ident, IdentPath, ImportId, Item, ModifierId, PragmaId, SourceUnit, StateVariableId, StatementId, StructureId, UserDefinedValueTypeId, UsingId};
pub use item::ItemScope;

use indexmap::IndexMap;
use salsa::{Database, Update};
use vfs::File;
use std::{hash::{Hash, Hasher}, ops::{Deref, DerefMut}};

use crate::{container::Container, import::ImportResolution, inheritance::linearization, HasDefs};

use super::scope::body::Definition;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd, Ord, salsa::Update)]
pub enum Scope<'db> {
    Item(ItemScope<'db>),
    Body(BodyScope<'db>),
    Expr(BodyScope<'db>, usize)
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
    pub fn lookup(self, db: &'db dyn BaseDb, name: Ident<'db>) -> Option<Definition<'db>> {
        let t = match self {
            Scope::Item(item_scope) => item_scope.name(db, name).next(),
            Scope::Body(body_scope) => body_scope.parent(db).name(db, name).next(),
            Scope::Expr(body_scope, i) => {
                return body_scope.lookup_in_scope(db, i, name).map(|a| a.1).next();
            }
        }.map(|(_, item)| Definition::Item(item));
        t
    }

    pub fn lookup_in_expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>, name: Ident<'db>) -> Option<Definition<'db>> {
        match self {
            Scope::Item(item_scope) => item_scope.name(db, name).next().map(|(_, item)| Definition::Item(item)),
            Scope::Body(expr_scope_root) | Scope::Expr(expr_scope_root, _) => {
                expr_scope_root.lookup_in_expr(db, expr, name).next().map(|(_, item)| item)
            },
        }
    }

    pub fn lookup_path(self, db: &'db dyn BaseDb, path: &[Ident<'db>]) -> Option<Definition<'db>> {
        let mut path = path.into_iter();
        let start = path.next()?;
        if let mut def @ Definition::Item(item) = self.lookup(db, *start)? {
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
                if let Some(i) = body_scope.scope_by_stmt(db).get(&stmt) {
                    Scope::Expr(body_scope, *i)
                } else {
                    Scope::Body(body_scope)
                }
            },
        }
    }

    pub fn for_expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Scope<'db> {
        match self {
            Scope::Item(item_scope) => Scope::Item(item_scope),
            Scope::Body(body_scope) | Scope::Expr(body_scope, _) => {
                if let Some(i) = body_scope.scope_by_expr(db).get(&expr) {
                    Scope::Expr(body_scope, *i)
                } else {
                    Scope::Body(body_scope)
                }
            },
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
                .unwrap_or_else(|| self.file(db).source_unit(db).item_scope(db, project))
    }

    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        let map = self.file(db).source_unit(db);
        BodyScope::from_body(
            db, 
            project, 
            self.item_scope(db, project), 
            Item::Constructor(self),
            self.info(db).args.iter().copied(), 
            self.body(db).map(|a| a.0)
        ).into()
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for ContractId<'db> {
    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        Scope::Item(self.item_scope(db, project))
    }
    
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let inheritance_chain = linearization(db, project, self).unwrap_or(vec![self]);

        let items: Vec<_> = inheritance_chain.into_iter().rev().flat_map(|c| {
            c
                .items(db)
                .iter()
                .filter_map(|a| a.name(db).map(|name| (name, (*a).into())))
        })
        .collect();

        ItemScope::new(
            db,
            Some(self.origin(db)
                .map(|c| c.item_scope(db, project))
                .unwrap_or_else(|| self.file(db).source_unit(db).item_scope(db, project))),
            items,
        )
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for FunctionId<'db> {
    #[salsa::tracked]
    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        let map = self.file(db).source_unit(db);
        BodyScope::from_body(
            db, 
            project, 
            self.item_scope(db, project),
            Item::Function(self),
            self.info(db).args.iter().copied(), 
            self.body(db).map(|a| a.0)
        ).into()
    }
    
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        self.origin(db)
            .map(|c| c.item_scope(db, project))
            .unwrap_or_else(|| self.file(db).source_unit(db).item_scope(db, project))
    }
}

macro_rules! impl_has_scope {
    ($($t:ty),+) => {
        $(
            #[salsa::tracked]
            impl<'db> HasScope<'db> for $t {
                fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
                    self.origin(db)
                        .map(|c| c.item_scope(db, project))
                        .unwrap_or_else(|| self.file(db).source_unit(db).item_scope(db, project))
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
        let imports = ImportResolution::from_file(db, project, self.file(db));
        ItemScope::new(db, None, imports.items(db).clone())
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
        file.source_unit(db).item_scope(db, project)
    }

    fn scope(self, db: &'db dyn BaseDb, project: Project) -> Scope<'db> {
        Scope::Item(self.item_scope(db, project))
    }
}

#[salsa::tracked]
impl<'db> HasScope<'db> for PragmaId<'db> {
    fn item_scope(self, db: &'db dyn BaseDb, project: Project) -> ItemScope<'db> {
        let file = self.file(db);
        file.source_unit(db).item_scope(db, project)
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
            Item::UserDefinedValueType(user_defined_value_type_id) => user_defined_value_type_id.scope(db, project),
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
            Item::UserDefinedValueType(user_defined_value_type_id) => user_defined_value_type_id.item_scope(db, project),
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