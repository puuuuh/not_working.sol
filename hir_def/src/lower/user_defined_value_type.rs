use rowan::ast::{AstNode, AstPtr};
use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::TypeRef;
use crate::hir::UserDefinedValueTypeId;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_user_defined_value_type(
        &mut self,
        e: nodes::UserDefinedValueTypeDefinition,
    ) -> UserDefinedValueTypeId<'db> {
        let name = Ident::from_name(self.db, e.name());
        let ty = e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error);
        let res = UserDefinedValueTypeId::new(self.db, self.file, name, ty, AstPtr::new(&e));
        self.save_span(e.syntax().text_range(), Item::UserDefinedValueType(res));
        res
    }
}
