use rowan::ast::AstNode;
use crate::hir::error::{ErrorId, ErrorParameter, ErrorParameterId};
use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_error_parameter(&mut self, e: nodes::ErrorParameter) -> ErrorParameterId<'db> {
        let info = ErrorParameter {
            name: e.name().map(|e| Ident::from_name(self.db.as_dyn_database(), Some(e))),
            ty: e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error),
        };
        ErrorParameterId::new(self.db, info)
    }

    pub fn lower_error(&mut self, e: nodes::ErrorDefinition) -> ErrorId<'db> {
        let res = ErrorId::new(
            self.db,
            Ident::from_name(self.db.as_dyn_database(), e.name()),
            e.error_parameters().map(|e| self.lower_error_parameter(e)).collect(),
            FileAstPtr::new(self.file, &e),
        );
        self.save_span(e.syntax().text_range(), ChildSource::Error(res));
        res
    }
}
