use crate::hir::argument::ArgumentId;
use crate::hir::expr::ExprId;
use crate::hir::function::{Function, FunctionId, ModifierInvocation};
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::type_name::TypeRef;
use crate::hir::{CallOption, StateMutability, Visibility};
use crate::item_tree::DefWithBody;
use crate::lower::Ctx;
use crate::FileAstPtr;
use rowan::ast::AstNode;
use syntax::ast::{nodes, AstChildren};
use syntax::T;
use crate::semantics::child_container::ChildSource;

impl<'db> Ctx<'db> {
    pub fn lower_modifier_invocation(
        &mut self,
        e: nodes::ModifierInvocation,
    ) -> Option<ModifierInvocation<'db>> {
        Some(ModifierInvocation {
            path: IdentPath::from(self.db.as_dyn_database(), e.ident_path()?),
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
                        .map(|p| IdentPath::from(self.db.as_dyn_database(), p))
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
            Ident::from_name_opt(self.db.as_dyn_database(), p.name()),
        )
    }

    pub fn collect_call_options(&mut self, expr: nodes::CallOptions) -> Vec<CallOption<'db>> {
        expr.call_options()
            .map(|o| CallOption {
                name: Ident::from_name_ref(self.db.as_dyn_database(), o.name_ref()),
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
                    let name = Ident::from_name_ref(self.db.as_dyn_database(), arg.name_ref());
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
        let body = e.block().map(|a| self.lower_stmt(nodes::Stmt::Block(a)));
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
            FileAstPtr::new(self.file, &nodes::FunctionDefinition::FallbackFunctionDefinition(e)),
        );
        self.save_span(range, ChildSource::Function(f));
        if let Some(body) = body {
            body.set_owner_recursive(self.db.as_dyn_database(), DefWithBody::Function(f));
        }
        f
    }

    pub fn lower_receive_function_definition(
        &mut self,
        e: nodes::ReceiveFunctionDefinition,
    ) -> FunctionId<'db> {
        let range = e.syntax().text_range();
        let (vis, mu, mods, over, virt) = self.lower_function_attrs(e.function_attributes());
        let name = None;
        let body = e.block().map(|a| self.lower_stmt(nodes::Stmt::Block(a)));
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
            FileAstPtr::new(self.file, &nodes::FunctionDefinition::ReceiveFunctionDefinition(e)),
        );
        self.save_span(range, ChildSource::Function(f));
        if let Some(body) = body {
            body.set_owner_recursive(self.db.as_dyn_database(), DefWithBody::Function(f));
        }
        f
    }
    pub fn lower_named_function_definition(
        &mut self,
        e: nodes::NamedFunctionDefinition,
    ) -> FunctionId<'db> {
        let range = e.syntax().text_range();
        let (vis, mu, mods, over, virt) = self.lower_function_attrs(e.function_attributes());
        let name = Ident::from_name(self.db.as_dyn_database(), e.name());
        let body = e.block().map(|a| self.lower_stmt(nodes::Stmt::Block(a)));
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
            FileAstPtr::new(self.file, &e.clone().into()),
        );
        self.save_span(range, ChildSource::Function(f));

        if let Some(body) = body {
            body.set_owner_recursive(self.db.as_dyn_database(), DefWithBody::Function(f));
        }
        f
    }
}
