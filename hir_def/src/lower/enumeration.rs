use rowan::ast::{AstNode, AstPtr};
use crate::hir::{EnumerationId, EnumerationVariantId};
use crate::hir::Ident;
use crate::hir::Item;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_enumeration_variant(&mut self, e: nodes::EnumMember) -> EnumerationVariantId<'db> {
        EnumerationVariantId::new(self.db, Ident::from_name(self.db, e.name()))
    }

    pub fn lower_enumeration(&mut self, e: nodes::EnumDefinition) -> EnumerationId<'db> {
        let name = Ident::from_name(self.db, e.name());
        let fields =
            e.enum_members().map(|e| self.lower_enumeration_variant(e)).collect::<Vec<_>>();
        let s = EnumerationId::new(
            self.db,
            name,
            fields.clone(),
            AstPtr::new(&e),
        );
        self.save_span(e.syntax().text_range(), Item::Enum(s));
        for f in &fields {
            f.set_parent(self.db, s);
        }
        s
    }
}
