use rowan::ast::{AstNode, AstPtr};
use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::{StructureFieldId, StructureId};
use crate::hir::TypeRef;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_structure_field(&mut self, e: nodes::StructMember) -> StructureFieldId<'db> {
        StructureFieldId::new(
            self.db,
            Ident::from_name(self.db, e.name()),
            e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error),
        )
    }

    // TODO: use function attrs maybe?
    pub fn lower_structure(&mut self, e: nodes::StructDefinition) -> StructureId<'db> {
        let name = Ident::from_name(self.db, e.name());
        let fields = e.struct_members().map(|e| self.lower_structure_field(e)).collect::<Vec<_>>();
        let s = StructureId::new(self.db, name, fields.clone(), AstPtr::new(&e));
        for f in &fields {
            f.set_parent(self.db, s);
        }
        self.save_span(e.syntax().text_range(), Item::Struct(s));
        
        s
    }
}
