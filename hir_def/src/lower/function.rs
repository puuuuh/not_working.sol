use crate::hir::CallOption;
use crate::hir::ExprId;
use crate::hir::Item;
use crate::hir::StateMutability;
use crate::hir::VariableDeclaration;
use crate::hir::Visibility;
use crate::hir::{FunctionId, FunctionSig, ModifierInvocation};
use crate::hir::{Ident, IdentPath};
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes::FunctionDefinition;
use syntax::ast::{nodes, AstChildren};
use syntax::T;

impl<'db> LowerCtx<'db> {
    pub fn lower_modifier_invocation(
        &mut self,
        e: nodes::ModifierInvocation,
    ) -> Option<ModifierInvocation<'db>> {
        Some(ModifierInvocation {
            path: IdentPath::from(self.db, e.ident_path()?),
            args: e.call_argument_list().map(|a| self.lower_call_argument_list(a)),
        })
    }

    pub fn lower_function_attrs(
        &mut self,
        e: impl Iterator<Item = nodes::FunctionAttribute>,
    ) -> (
        Visibility,
        StateMutability,
        Vec<ModifierInvocation<'db>>,
        Option<Vec<IdentPath<'db>>>,
        bool,
    ) {
        let mut visibility = Visibility::Unknown;
        let mut mutability = StateMutability::NonPayable;
        let mut modifiers = vec![];
        let mut overrides = None;
        let mut virt = false;

        for m in e {
            if let Some(i) = m.override_specifier() {
                overrides =
                    Some(i.ident_paths().map(|p| IdentPath::from(self.db, p)).collect::<Vec<_>>());
            } else if let Some(i) = m.modifier_invocation() {
                if let Some(m) = self.lower_modifier_invocation(i) {
                    modifiers.push(m)
                }
            } else {
                for t in m.syntax().children_with_tokens() {
                    match t.kind() {
                        T![virtual] => {
                            virt = true;
                        }
                        T![external] => visibility = Visibility::External,
                        T![public] => visibility = Visibility::Public,
                        T![private] => visibility = Visibility::Private,
                        T![internal] => visibility = Visibility::Internal,
                        T![pure] => mutability = StateMutability::Pure,
                        T![view] => mutability = StateMutability::View,
                        T![payable] => mutability = StateMutability::Payable,
                        _ => {}
                    }
                }
            }
        }
        (visibility, mutability, modifiers, overrides, virt)
    }

    pub fn lower_parameter(&mut self, p: nodes::VariableDeclaration) -> VariableDeclaration<'db> {
        VariableDeclaration::new(
            self.db,
            self.lower_type_ref2(p.ty()),
            p.location().map(Into::into),
            Ident::from_name_opt(self.db, p.name()),
            FileAstPtr::new(self.file, &p),
        )
    }

    pub fn collect_call_options(&mut self, expr: nodes::CallOptions) -> Vec<CallOption<'db>> {
        expr.call_options()
            .map(|o| CallOption {
                name: Ident::from_name_ref(self.db, o.name_ref()),
                val: self.lower_expr2(o.expr()),
            })
            .collect::<Vec<_>>()
    }

    pub fn collect_argument_list(
        &mut self,
        expr: nodes::CallArgumentList,
    ) -> Vec<(Option<Ident<'db>>, ExprId<'db>)> {
        match expr {
            nodes::CallArgumentList::CallArguments(args) => {
                args.exprs().map(|expr| (None, self.lower_expr(expr))).collect()
            }
            nodes::CallArgumentList::NamedCallArguments(args) => args
                .named_call_arguments()
                .map(|arg| {
                    let name = Ident::from_name_ref(self.db, arg.name_ref());
                    let expr = self.lower_expr2(arg.expr());
                    (Some(name), expr)
                })
                .collect(),
        }
    }

    fn lower_function_definition_inner(
        &mut self,
        attrs: impl Iterator<Item = nodes::FunctionAttribute>,
        name: Option<nodes::Name>,
        body: Option<nodes::Block>,
        parameters: Option<nodes::ParameterList>,
        returns: Option<nodes::ParameterList>,
        node: nodes::FunctionDefinition,
    ) -> FunctionId<'db> {
        let range = node.syntax().text_range();
        let (vis, mu, mods, over, virt) = self.lower_function_attrs(attrs);
        let name = Ident::from_name(self.db, name);
        let body = body.map(|a| AstPtr::new(&a));

        let args: Vec<_> = parameters
            .map(|a| a.variable_declarations().map(|p| self.lower_parameter(p)).collect())
            .unwrap_or_default();
        let returns: Option<Vec<_>> =
            returns.map(|a| a.variable_declarations().map(|p| self.lower_parameter(p)).collect());

        let f = FunctionId::new(
            self.db,
            self.file,
            Some(name),
            FunctionSig {
                args: args.clone(),
                returns: returns.clone(),
                modifiers: mods,
                overrides: over,
                vis,
                mutability: mu,
                is_virtual: virt,
            },
            body,
            AstPtr::new(&node.clone()),
        );

        self.save_span(range, Item::Function(f));

        f
    }

    pub fn lower_fallback_function_definition(
        &mut self,
        e: nodes::FallbackFunctionDefinition,
    ) -> FunctionId<'db> {
        self.lower_function_definition_inner(
            e.function_attributes(),
            None,
            e.block(),
            e.parameter_list(),
            e.returns().and_then(|e| e.parameter_list()),
            nodes::FunctionDefinition::FallbackFunctionDefinition(e),
        )
    }

    pub fn lower_receive_function_definition(
        &mut self,
        e: nodes::ReceiveFunctionDefinition,
    ) -> FunctionId<'db> {
        self.lower_function_definition_inner(
            e.function_attributes(),
            None,
            e.block(),
            None,
            None,
            nodes::FunctionDefinition::ReceiveFunctionDefinition(e),
        )
    }
    pub fn lower_named_function_definition(
        &mut self,
        e: nodes::NamedFunctionDefinition,
    ) -> FunctionId<'db> {
        self.lower_function_definition_inner(
            e.function_attributes(),
            e.name(),
            e.block(),
            e.parameter_list(),
            e.returns().and_then(|e| e.parameter_list()),
            nodes::FunctionDefinition::NamedFunctionDefinition(e),
        )
    }
}
