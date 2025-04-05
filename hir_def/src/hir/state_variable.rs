use crate::hir::ident::{Ident, IdentPath};
use crate::hir::source_unit::ItemOrigin;
use crate::hir::type_name::TypeRef;
use crate::hir::Visibility;
use crate::items::HirPrint;
use crate::{impl_has_origin, lazy_field, FileAstPtr};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::{Display, Formatter, Write};
use syntax::ast::nodes;

#[tracked]
pub struct StateVariableId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub ty: TypeRef<'db>,
    pub info: StateVariableInfo<'db>,

    pub init: Option<AstPtr<nodes::Expr>>,
    pub node: AstPtr<nodes::StateVariableDeclaration>,
}

lazy_field!(StateVariableId<'db>, origin, set_origin, ItemOrigin<'db>);
impl_has_origin!(StateVariableId<'db>);

impl HirPrint for StateVariableId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.ty(db).write(db, w, ident)?;
        w.write_char(' ')?;
        self.info(db).write(db, w, ident)?;
        w.write_str(";")
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum StateVariableMutability {
    Const,
    Immutable,
    Default,
}

impl Display for StateVariableMutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            StateVariableMutability::Const => "const",
            StateVariableMutability::Immutable => "immutable",
            StateVariableMutability::Default => "",
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub struct StateVariableInfo<'db> {
    pub mutability: StateVariableMutability,
    pub vis: Visibility,
    pub overrides: Vec<IdentPath<'db>>,
}

impl HirPrint for StateVariableInfo<'_> {
    fn write<T: Write>(&self, _db: &dyn Database, w: &mut T, _ident: usize) -> std::fmt::Result {
        write!(w, "{} {} ", self.vis, self.mutability)?;
        Ok(())
    }
}
