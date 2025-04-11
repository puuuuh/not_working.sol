use rowan::ast::{AstNode, AstPtr};
use crate::hir::{ErrorId, ErrorParameter, ErrorParameterId};
use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::TypeRef;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_error_parameter(&mut self, e: nodes::ErrorParameter) -> ErrorParameterId<'db> {
        let info = ErrorParameter {
            name: e.name().map(|e| Ident::from_name(self.db, Some(e))),
            ty: e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error),
        };
        ErrorParameterId::new(self.db, info)
    }

    pub fn lower_error(&mut self, e: nodes::ErrorDefinition) -> ErrorId<'db> {
        let res = ErrorId::new(
            self.db,
            Ident::from_name(self.db, e.name()),
            e.error_parameters().map(|e| self.lower_error_parameter(e)).collect(),
            AstPtr::new(&e),
        );
        self.save_span(e.syntax().text_range(), Item::Error(res));
        res
    }
}
