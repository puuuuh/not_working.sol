use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::item_tree::print::HirPrint;
use crate::item_tree::DefSite;
use crate::{lazy_field, FileAstPtr};
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked]
pub struct ErrorId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub parameters: Vec<ErrorParameterId<'db>>,

    pub node: FileAstPtr<nodes::ErrorDefinition>,
}

lazy_field!(ErrorId<'db>, def_site, set_def_site, DefSite<'db>);

impl HirPrint for ErrorId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("error ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str("(")?;
        for (i, p) in self.parameters(db).iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?;
            }
            p.write(db, w, ident)?;
        }
        w.write_str(");")
    }
}

#[tracked]
pub struct ErrorParameterId<'db> {
    pub info: ErrorParameter<'db>,
}

impl HirPrint for ErrorParameterId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub struct ErrorParameter<'db> {
    pub name: Option<Ident<'db>>,
    pub ty: TypeRef<'db>,
}

impl HirPrint for ErrorParameter<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.ty.write(db, w, ident)?;
        if let Some(name) = self.name {
            w.write_str(" ")?;
            name.write(db, w, ident)?;
        }
        Ok(())
    }
}
