use rowan::ast::{AstNode, AstPtr};
use crate::hir::{EventId, EventParameter, EventParameterId};
use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::TypeRef;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_event_parameter(&mut self, e: nodes::EventParameter) -> EventParameterId<'db> {
        let info = EventParameter {
            name: e.name().map(|e| Ident::from_name(self.db, Some(e))),
            is_indexed: e.indexed_token().is_some(),
            ty: e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error),
        };
        EventParameterId::new(self.db, info)
    }

    pub fn lower_event(&mut self, e: nodes::EventDefinition) -> EventId<'db> {
        let res = EventId::new(
            self.db,
            self.file,
            Ident::from_name(self.db, e.name()),
            e.anonymous_token().is_some(),
            e.event_parameters().map(|e| self.lower_event_parameter(e)).collect(),
            AstPtr::new(&e),
        );
        self.save_span(e.syntax().text_range(), Item::Event(res));
        
        res
    }
}
