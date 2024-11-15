use rowan::ast::AstNode;
use crate::hir::event::{EventId, EventParameter, EventParameterId};
use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_event_parameter(&mut self, e: nodes::EventParameter) -> EventParameterId<'db> {
        let info = EventParameter {
            name: e.name().map(|e| Ident::from_name(self.db.as_dyn_database(), Some(e))),
            is_indexed: e.indexed_token().is_some(),
            ty: e.ty().map(|e| self.lower_type_ref(e)).unwrap_or(TypeRef::Error),
        };
        EventParameterId::new(self.db, info)
    }

    pub fn lower_event(&mut self, e: nodes::EventDefinition) -> EventId<'db> {
        let res = EventId::new(
            self.db,
            Ident::from_name(self.db.as_dyn_database(), e.name()),
            e.anonymous_token().is_some(),
            e.event_parameters().map(|e| self.lower_event_parameter(e)).collect(),
            FileAstPtr::new(self.file, &e),
        );
        self.save_span(e.syntax().text_range(), ChildSource::Event(res));
        
        res
    }
}
