use crate::hir::argument::ArgumentId;
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::statement::StatementId;
use crate::hir::{ExprId, StateMutability, Visibility};
use crate::item_tree::print::HirPrint;
use crate::item_tree::DefSite;
use crate::scope::expr::ExprScopeRoot;
use crate::scope::resolver::Resolver;
use crate::{lazy_field, FileAstPtr};
use base_db::BaseDb;
use salsa::Database;
use std::fmt::Write;
use syntax::ast::nodes;

#[salsa::tracked]
pub struct FunctionId<'db> {
    #[id]
    pub name: Option<Ident<'db>>,
    pub info: Function<'db>,
    pub body: Option<StatementId<'db>>,

    pub node: FileAstPtr<nodes::FunctionDefinition>,
}

lazy_field!(FunctionId<'db>, def_site, set_def_site, DefSite<'db>);

impl HirPrint for FunctionId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("function ")?;
        if let Some(name) = self.name(db) {
            name.write(db, w, ident)?
        };

        self.info(db).write(db, w, ident)?;
        if let Some(b) = self.body(db) {
            b.write(db, w, ident)
        } else {
            w.write_char(';')
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Hash, salsa::Update)]
pub struct Function<'db> {
    pub args: Vec<ArgumentId<'db>>,
    pub returns: Option<Vec<ArgumentId<'db>>>,
    pub modifiers: Vec<ModifierInvocation<'db>>,
    pub overrides: Option<Vec<IdentPath<'db>>>,
    pub vis: Visibility,
    pub mutability: StateMutability,
    pub is_virtual: bool,
}

#[derive(Eq, PartialEq, Debug, Clone, Hash, salsa::Update)]
pub struct ModifierInvocation<'db> {
    pub path: IdentPath<'db>,
    pub args: Option<Vec<(Option<Ident<'db>>, ExprId<'db>)>>,
}

impl HirPrint for ModifierInvocation<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.path.write(db, w, ident)?;
        if let Some(args) = &self.args {
            w.write_str("(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?;
                }
                a.1.write(db, w, ident)?;
            }
            w.write_str(")")?;
        }
        Ok(())
    }
}

#[salsa::tracked]
impl<'db> FunctionId<'db> {
    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb) -> ExprScopeRoot<'db> {
        Resolver::function_scopes(db, self)
    }
}

impl HirPrint for Function<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("(")?;
        for (i, a) in self.args.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            a.write(db, w, ident)?
        }
        w.write_str(")")?;
        if self.is_virtual {
            w.write_str(" virtual")?;
        }
        write!(w, " {} {} ", self.vis, self.mutability)?;
        if let Some(o) = &self.overrides {
            w.write_str(" override(")?;
            for (i, a) in o.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?
                }
                a.write(db, w, ident)?
            }
            w.write_str(")")?;
        }
        for (i, a) in self.modifiers.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            a.write(db, w, ident)?;
        }
        Ok(())
    }
}
