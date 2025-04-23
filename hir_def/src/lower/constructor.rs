use rowan::ast::{AstNode, AstPtr};
use crate::hir::{Constructor, ConstructorId, VariableDeclaration};
use crate::hir::Item;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_constructor_definition(
        &mut self,
        e: nodes::ConstructorDefinition,
    ) -> ConstructorId<'db> {
        let prev = self.spans.last();
        let (vis, mu, mods, _over, _virt) = self.lower_function_attrs(e.function_attributes());
        let body = e.block().map(|a| AstPtr::new(&a));
        let args: Vec<VariableDeclaration<'_>> = e
                    .parameter_list()
                    .map(|a| a.variable_declarations().map(|p| self.lower_parameter(p)).collect())
                    .unwrap_or_default();
        let c = ConstructorId::new(
            self.db,
            Constructor {
                args: args.clone(),
                modifiers: mods,
                vis,
                mutability: mu,
            },
            body,
            AstPtr::new(&e),
        );

        self.save_span(e.syntax().text_range(), Item::Constructor(c));
        c
    }
}
