use rowan::ast::AstNode;
use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::hir::user_defined_value_type::UserDefinedValueTypeId;
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_user_defined_value_type(
        &mut self,
        e: nodes::UserDefinedValueTypeDefinition,
    ) -> UserDefinedValueTypeId<'db> {
        let name = Ident::from_name(self.db.as_dyn_database(), e.name());
        let ty = e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error);
        let res = UserDefinedValueTypeId::new(self.db, name, ty, FileAstPtr::new(self.file, &e));
        self.save_span(e.syntax().text_range(), ChildSource::UserDefinedValueType(res));
        res
    }
}
