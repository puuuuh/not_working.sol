use crate::hir::ident::IdentPath;
use crate::hir::op::UserDefineableOp;
use crate::hir::{ContractId, TypeRefId};
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;
use vfs::File;

#[tracked(debug)]
pub struct UsingId<'db> {
    #[id]
    pub file: File,

    #[tracked]
    pub data: UsingData<'db>,

    #[tracked]
    pub node: AstPtr<nodes::Using>,
}

lazy_field!(UsingId<'db>, origin, set_origin, Option<ContractId<'db>>);

impl HirPrint for UsingId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("using ")?;
        self.data(db).write(db, w, ident)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct UsingData<'db> {
    pub items: Vec<UsingAlias<'db>>,
    pub type_name: Option<TypeRefId<'db>>,
    pub is_global: bool,
}

impl HirPrint for UsingData<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("{")?;
        for (i, a) in self.items.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?;
            }
            a.write(db, w, ident)?;
        }
        w.write_str("} for ")?;
        if let Some(ty) = &self.type_name {
            ty.write(db, w, ident)?;
        } else {
            w.write_char('*')?;
        }
        if self.is_global {
            w.write_str(" global;")?;
        } else {
            w.write_char(';')?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct UsingAlias<'db> {
    pub path: IdentPath<'db>,
    pub as_name: Option<UserDefineableOp>,
}

impl HirPrint for UsingAlias<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.path.write(db, w, ident)?;
        if let Some(as_name) = &self.as_name {
            w.write_str(" as ")?;
            write!(w, " as {as_name}")?;
        }
        Ok(())
    }
}
