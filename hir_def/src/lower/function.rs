use crate::hir::argument::ArgumentId;
use crate::hir::expr::ExprId;
use crate::hir::function::{Function, FunctionId, ModifierInvocation};
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::source_unit::Item;
use crate::hir::type_name::TypeRef;
use crate::hir::{CallOption, StateMutability, Visibility};
use crate::lower::LowerCtx;
use rowan::ast::{AstNode, AstPtr};
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
        e: AstChildren<nodes::FunctionAttribute>,
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
                overrides = Some(
                    i.ident_paths()
                        .map(|p| IdentPath::from(self.db, p))
                        .collect::<Vec<_>>(),
                );
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

    pub fn lower_parameter(&mut self, p: nodes::Parameter) -> ArgumentId<'db> {
        ArgumentId::new(
            self.db,
            p.ty().map(|ty| self.lower_type_ref(ty)).unwrap_or(TypeRef::Error),
            p.location().map(Into::into),
            Ident::from_name_opt(self.db, p.name()),
            AstPtr::new(&p)
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

    pub fn lower_fallback_function_definition(
        &mut self,
        e: nodes::FallbackFunctionDefinition,
    ) -> FunctionId<'db> {
        let range = e.syntax().text_range();
        let (vis, mu, mods, over, virt) = self.lower_function_attrs(e.function_attributes());
        let name = None;
        let body = e.block().map(|b| AstPtr::new(&b)); // .map(|a| self.lower_stmt(nodes::Stmt::Block(a)));
        let f = FunctionId::new(
            self.db,
            name,
            Function {
                args: e
                    .parameter_list()
                    .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect())
                    .unwrap_or_default(),
                returns: e
                    .returns()
                    .and_then(|a| a.parameter_list())
                    .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect()),
                modifiers: mods,
                overrides: over,
                vis,
                mutability: mu,
                is_virtual: virt,
            },
            body,
            AstPtr::new(&nodes::FunctionDefinition::FallbackFunctionDefinition(e)),
        );
        self.save_span(range, Item::Function(f));
        f
    }

    pub fn lower_receive_function_definition(
        &mut self,
        e: nodes::ReceiveFunctionDefinition,
    ) -> FunctionId<'db> {
        let range = e.syntax().text_range();
        let (vis, mu, mods, over, virt) = self.lower_function_attrs(e.function_attributes());
        let name = None;
        let body = e.block().map(|a| AstPtr::new(&a));
        let f = FunctionId::new(
            self.db,
            name,
            Function {
                args: vec![],
                returns: None,
                modifiers: mods,
                overrides: over,
                vis,
                mutability: mu,
                is_virtual: virt,
            },
            body,
            AstPtr::new(&nodes::FunctionDefinition::ReceiveFunctionDefinition(e)),
        );
        self.save_span(range, Item::Function(f));
        f
    }
    pub fn lower_named_function_definition(
        &mut self,
        e: nodes::NamedFunctionDefinition,
    ) -> FunctionId<'db> {
        let range = e.syntax().text_range();
        let (vis, mu, mods, over, virt) = self.lower_function_attrs(e.function_attributes());
        let name = Ident::from_name(self.db, e.name());
        let body = e.block().map(|a| AstPtr::new(&a));
        let f = FunctionId::new(
            self.db,
            Some(name),
            Function {
                args: e
                    .parameter_list()
                    .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect())
                    .unwrap_or_default(),
                returns: e
                    .returns()
                    .and_then(|a| a.parameter_list())
                    .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect()),
                modifiers: mods,
                overrides: over,
                vis,
                mutability: mu,
                is_virtual: virt,
            },
            body,
            AstPtr::new(&e.clone().into()),
        );
        self.save_span(range, Item::Function(f));

        f
    }
}
