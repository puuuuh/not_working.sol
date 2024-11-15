use rowan::ast::AstNode;
use crate::hir::constructor::{Constructor, ConstructorId};
use crate::item_tree::DefWithBody;
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_constructor_definition(
        &mut self,
        e: nodes::ConstructorDefinition,
    ) -> ConstructorId<'db> {
        let (vis, mu, mods, _over, _virt) = self.lower_function_attrs(e.function_attributes());
        let body = e.block().map(|a| self.lower_stmt(nodes::Stmt::Block(a)));
        let c = ConstructorId::new(
            self.db,
            Constructor {
                args: e
                    .parameter_list()
                    .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect())
                    .unwrap_or_default(),
                modifiers: mods,
                vis,
                mutability: mu,
            },
            body,
            FileAstPtr::new(self.file, &e),
        );
        self.save_span(e.syntax().text_range(), ChildSource::Constructor(c));
        if let Some(s) = body {
            s.set_owner_recursive(self.db.as_dyn_database(), DefWithBody::Constructor(c));
        }
        c
    }
}
