use crate::hir::{Ident, IdentPath};
use crate::hir::Item;
use crate::hir::{StateVariableId, StateVariableInfo, StateVariableMutability};
use crate::hir::TypeRef;
use crate::hir::Visibility;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes::StateVariableAttribute;
use syntax::ast::{nodes, AstChildren};
use syntax::T;

impl<'db> LowerCtx<'db> {
    pub fn lower_state_variable_attrs(
        &mut self,
        e: AstChildren<StateVariableAttribute>,
    ) -> (Visibility, StateVariableMutability, Vec<IdentPath<'db>>) {
        let mut visibility = Visibility::Unknown;
        let mut mutability = StateVariableMutability::Default;
        let mut overrides = vec![];

        for m in e {
            if let Some(i) = m.override_specifier() {
                overrides = i
                    .ident_paths()
                    .map(|p| IdentPath::from(self.db, p))
                    .collect::<Vec<_>>();
            } else {
                for t in m.syntax().children_with_tokens() {
                    match t.kind() {
                        T![external] => visibility = Visibility::External,
                        T![public] => visibility = Visibility::Public,
                        T![private] => visibility = Visibility::Private,
                        T![internal] => visibility = Visibility::Internal,
                        T![constant] => mutability = StateVariableMutability::Const,
                        T![immutable] => mutability = StateVariableMutability::Immutable,
                        _ => {}
                    }
                }
            }
        }
        (visibility, mutability, overrides)
    }

    pub fn lower_state_variable(
        &mut self,
        s: nodes::StateVariableDeclaration,
    ) -> StateVariableId<'db> {
        let (vis, mutability, overrides) =
            self.lower_state_variable_attrs(s.state_variable_attributes());
        let info = StateVariableInfo { mutability, vis, overrides };
        let res = StateVariableId::new(
            self.db,
            Ident::from_name(self.db, s.name()),
            s.ty().map(|t| self.lower_type_ref(t)).unwrap_or(TypeRef::Error),
            info,

            s.expr().map(|e| AstPtr::new(&e)),
            AstPtr::new(&s),
        );
        self.save_span(s.syntax().text_range(), Item::StateVariable(res));
        
        res
    }
}
