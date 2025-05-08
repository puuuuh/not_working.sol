use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::{StructureFieldId, StructureId};
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_structure_field(&mut self, e: nodes::StructMember) -> StructureFieldId<'db> {
        StructureFieldId::new(
            self.db,
            Ident::from_name(self.db, e.name()),
            self.lower_type_ref2(e.ty()),
        )
    }

    // TODO: use function attrs maybe?
    pub fn lower_structure(&mut self, e: nodes::StructDefinition) -> StructureId<'db> {
        let name = Ident::from_name(self.db, e.name());
        let fields = e.struct_members().map(|e| self.lower_structure_field(e)).collect::<Vec<_>>();
        let s = StructureId::new(self.db, self.file, name, fields.clone(), AstPtr::new(&e));
        for f in &fields {
            f.set_parent(self.db, s);
        }
        self.save_span(e.syntax().text_range(), Item::Struct(s));

        s
    }
}
