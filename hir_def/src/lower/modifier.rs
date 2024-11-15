use crate::hir::ident::{Ident, IdentPath};
use crate::hir::modifier::{Modifier, ModifierId};
use crate::lower::Ctx;
use crate::FileAstPtr;
use rowan::ast::AstNode;
use syntax::ast::nodes::ModifierAttribute;
use syntax::ast::{nodes, AstChildren};
use syntax::T;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    // TODO: use function attrs maybe?
    pub fn lower_modifier_attrs(
        &mut self,
        e: AstChildren<ModifierAttribute>,
    ) -> (Option<Vec<IdentPath<'db>>>, bool) {
        let mut overrides = None;
        let mut virt = false;

        for m in e {
            if let Some(i) = m.override_specifier() {
                overrides = Some(
                    i.ident_paths()
                        .map(|p| IdentPath::from(self.db.as_dyn_database(), p))
                        .collect::<Vec<_>>(),
                );
            } else {
                for t in m.syntax().children_with_tokens() {
                    if t.kind() == T![virtual] {
                        virt = true;
                    }
                }
            }
        }
        (overrides, virt)
    }

    pub fn lower_modifier_definition(
        &mut self,
        e: nodes::ModifierDefinition,
    ) -> ModifierId<'db> {
        let (over, virt) = self.lower_modifier_attrs(e.modifier_attributes());
        let name = Ident::from_name(self.db.as_dyn_database(), e.name());
        let res = ModifierId::new(
            self.db,
            name,
            Modifier {
                args: e
                    .parameter_list()
                    .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect())
                    .unwrap_or_default(),
                overrides: over,
                is_virtual: virt,
            },
            self.lower_stmt2(e.block().map(nodes::Stmt::Block)),
            FileAstPtr::new(self.file, &e),
        );
        self.save_span(e.syntax().text_range(), ChildSource::Modifier(res));
        res
    }
}
