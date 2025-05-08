use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::UserDefinedValueTypeId;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_user_defined_value_type(
        &mut self,
        e: nodes::UserDefinedValueTypeDefinition,
    ) -> UserDefinedValueTypeId<'db> {
        let name = Ident::from_name(self.db, e.name());
        let ty = self.lower_type_ref2(e.ty());
        let res = UserDefinedValueTypeId::new(self.db, self.file, name, ty, AstPtr::new(&e));
        self.save_span(e.syntax().text_range(), Item::UserDefinedValueType(res));
        res
    }
}
