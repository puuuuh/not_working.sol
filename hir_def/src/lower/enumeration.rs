use rowan::ast::AstNode;
use crate::hir::enumeration::{EnumerationId, EnumerationVariantId};
use crate::hir::ident::Ident;
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_enumeration_variant(&mut self, e: nodes::EnumMember) -> EnumerationVariantId<'db> {
        EnumerationVariantId::new(self.db, Ident::from_name(self.db.as_dyn_database(), e.name()))
    }

    pub fn lower_enumeration(&mut self, e: nodes::EnumDefinition) -> EnumerationId<'db> {
        let name = Ident::from_name(self.db.as_dyn_database(), e.name());
        let fields =
            e.enum_members().map(|e| self.lower_enumeration_variant(e)).collect::<Vec<_>>();
        let s = EnumerationId::new(
            self.db.as_dyn_database(),
            name,
            fields.clone(),
            FileAstPtr::new(self.file, &e),
        );
        self.save_span(e.syntax().text_range(), ChildSource::Enum(s));
        for f in &fields {
            f.set_parent(self.db.as_dyn_database(), s);
        }
        s
    }
}
