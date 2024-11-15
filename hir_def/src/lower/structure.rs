use rowan::ast::AstNode;
use crate::hir::ident::Ident;
use crate::hir::structure::{StructureFieldId, StructureId};
use crate::hir::type_name::TypeRef;
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_structure_field(&mut self, e: nodes::StructMember) -> StructureFieldId<'db> {
        StructureFieldId::new(
            self.db,
            Ident::from_name(self.db.as_dyn_database(), e.name()),
            e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error),
        )
    }

    // TODO: use function attrs maybe?
    pub fn lower_structure(&mut self, e: nodes::StructDefinition) -> StructureId<'db> {
        let name = Ident::from_name(self.db.as_dyn_database(), e.name());
        let fields = e.struct_members().map(|e| self.lower_structure_field(e)).collect::<Vec<_>>();
        let s = StructureId::new(self.db, name, fields.clone(), FileAstPtr::new(self.file, &e));
        for f in &fields {
            f.set_parent(self.db.as_dyn_database(), s);
        }
        self.save_span(e.syntax().text_range(), ChildSource::Struct(s));
        
        s
    }
}
