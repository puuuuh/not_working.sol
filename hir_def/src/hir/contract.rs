use crate::hir::constructor::ConstructorId;
use crate::hir::enumeration::EnumerationId;
use crate::hir::error::ErrorId;
use crate::hir::event::EventId;
use crate::hir::expr::ExprId;
use crate::hir::function::FunctionId;
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::modifier::ModifierId;
use crate::hir::state_variable::StateVariableId;
use crate::hir::structure::StructureId;
use crate::hir::user_defined_value_type::UserDefinedValueTypeId;
use crate::hir::using::UsingId;
use crate::item_tree::print::HirPrint;
use crate::item_tree::DefSite;
use crate::scope::IndexMapUpdate;
use crate::{lazy_field, FileAstPtr};
use base_db::BaseDb;
use salsa::Database;
use std::fmt::Write;
use syntax::ast::nodes;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, salsa::Update)]
pub enum ContractItem<'db> {
    Constructor(ConstructorId<'db>),
    Function(FunctionId<'db>),
    Modifier(ModifierId<'db>),
    UserDefinedValueType(UserDefinedValueTypeId<'db>),
    StateVariable(StateVariableId<'db>),
    Struct(StructureId<'db>),
    Enum(EnumerationId<'db>),
    Event(EventId<'db>),
    Error(ErrorId<'db>),
    Using(UsingId<'db>),
}

impl<'db> ContractItem<'db> {
    pub fn set_def_site(self, db: &'db dyn BaseDb, contract: ContractId<'db>) {
        let db = db.as_dyn_database();
        match self {
            ContractItem::Constructor(i) => i.set_def_site(db, DefSite::Contract(contract)),
            ContractItem::Function(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::Modifier(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::UserDefinedValueType(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::StateVariable(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::Struct(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::Enum(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::Event(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::Error(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
            ContractItem::Using(i) => {
                i.set_def_site(db, DefSite::Contract(contract));
            }
        }
    }
    pub fn name(self, db: &'db dyn Database) -> Option<Ident<'db>> {
        match self {
            ContractItem::Using(_i) => None,
            ContractItem::Constructor(_i) => None,
            ContractItem::Function(i) => i.name(db),
            ContractItem::Modifier(i) => Some(i.name(db)),
            ContractItem::UserDefinedValueType(i) => Some(i.name(db)),
            ContractItem::StateVariable(i) => Some(i.name(db)),
            ContractItem::Struct(i) => Some(i.name(db)),
            ContractItem::Enum(i) => Some(i.name(db)),
            ContractItem::Event(i) => Some(i.name(db)),
            ContractItem::Error(i) => Some(i.name(db)),
        }
    }
}

impl HirPrint for ContractItem<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        let ident = ident + 1;
        match self {
            ContractItem::Constructor(c) => c.write(db, w, ident),
            ContractItem::Function(c) => c.write(db, w, ident),
            ContractItem::Modifier(c) => c.write(db, w, ident),
            ContractItem::UserDefinedValueType(c) => c.write(db, w, ident),
            ContractItem::StateVariable(c) => c.write(db, w, ident),
            ContractItem::Struct(c) => c.write(db, w, ident),
            ContractItem::Enum(c) => c.write(db, w, ident),
            ContractItem::Event(c) => c.write(db, w, ident),
            ContractItem::Error(c) => c.write(db, w, ident),
            ContractItem::Using(c) => c.write(db, w, ident),
        }
    }
}

#[salsa::tracked]
pub struct ContractId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub is_abstract: bool,
    #[return_ref]
    pub inheritance_chain: Vec<InheritanceSpecifier<'db>>,

    #[return_ref]
    pub body: Vec<ContractItem<'db>>,

    pub node: FileAstPtr<nodes::Contract>,
}

lazy_field!(ContractId<'db>, def_site, set_def_site, DefSite<'db>);

pub enum ContractType {
    Interface,
    Contract,
    Library,
}

#[salsa::tracked]
impl<'db> ContractId<'db> {
    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb) -> crate::scope::item::Scope<'db> {
        let items = self
            .body(db)
            .iter()
            .filter_map(|a| a.name(db.as_dyn_database()).map(|name| (name, (*a).into())))
            .collect();
        crate::scope::item::Scope::new(
            db,
            Some(self.def_site(db.as_dyn_database()).scope(db)),
            IndexMapUpdate(items),
        )
    }
}

impl HirPrint for ContractId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        let my_ident_str = "\t".repeat(ident);
        let ident_str = "\t".repeat(ident + 1);

        w.write_str("contract ")?;
        self.name(db).write(db, w, ident)?;
        if self.is_abstract(db) {
            w.write_str(" abstract")?;
        }
        if !self.inheritance_chain(db).is_empty() {
            w.write_str(" is ")?;
            for (i, s) in self.inheritance_chain(db).iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?
                }
                s.write(db, w, ident)?;
            }
        }

        w.write_str(" {\n")?;

        for i in self.body(db) {
            w.write_str(&ident_str)?;
            i.write(db, w, ident)?;
            w.write_str("\n")?;
        }

        w.write_str(&my_ident_str)?;
        w.write_str("}\n")?;
        Ok(())
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Hash, salsa::Update)]
pub struct InheritanceSpecifier<'db> {
    pub path: IdentPath<'db>,
    pub args: Option<Vec<(Option<Ident<'db>>, ExprId<'db>)>>,
}

impl HirPrint for InheritanceSpecifier<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.path.write(db, w, ident)?;
        if let Some(a) = &self.args {
            let named = matches!(a.first(), Some((Some(_), _)));
            if named {
                w.write_str("({")?;
            } else {
                w.write_str("(")?;
            }
            for (i, a) in a.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?;
                }
                if let Some(a) = a.0 {
                    a.write(db, w, ident)?;
                    w.write_str(": ")?;
                }
                a.1.write(db, w, ident)?;
            }
            if named {
                w.write_str("})")?;
            } else {
                w.write_str(")")?;
            }
        }
        Ok(())
    }
}
