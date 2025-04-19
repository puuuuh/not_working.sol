mod constructor;
mod contract;
mod enumeration;
mod error;
mod event;
pub mod expr;
mod function;
mod import;
mod literal;
mod modifier;
mod state_varible;
mod stmt;
mod structure;
pub mod types;
mod user_defined_value_type;

use crate::hir::ContractType;
use crate::hir::ExprId;
use crate::hir::{Ident, IdentPath};
use crate::hir::UserDefineableOp;
use crate::hir::PragmaId;
use crate::hir::Item;
use crate::hir::StatementId;
use crate::hir::{UsingAlias, UsingData, UsingId};
use crate::source_map::span_map::SpanMapRange;
use crate::FileAstPtr;
use crate::IndexMapUpdate;
use base_db::{BaseDb, File};
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes::{self, Expr, Stmt};
use syntax::TextRange;

pub(crate) struct LowerCtx<'a> {
    db: &'a dyn BaseDb,

    pub(crate) spans: Vec<(SpanMapRange, Item<'a>)>,
    pub(crate) exprs: IndexMapUpdate<AstPtr<Expr>, ExprId<'a>>,
    pub(crate) stmts: IndexMapUpdate<AstPtr<Stmt>, StatementId<'a>>,
}

impl<'a> LowerCtx<'a> {
    pub fn new(db: &'a dyn BaseDb, file: File) -> Self {
        Self { db, spans: Vec::new(), exprs: Default::default(), stmts: Default::default() }
    }

    pub fn save_span(&mut self, s: TextRange, data: Item<'a>) {
        self.spans.push((SpanMapRange(s.start(), s.end()), data));
    }

    pub fn save_expr(&mut self, s: nodes::Expr, data: ExprId<'a>) {
        self.exprs.0.insert(AstPtr::new(&s), data);
    }

    pub fn save_stmt(&mut self, s: nodes::Stmt, data: StatementId<'a>) {
        self.stmts.0.insert(AstPtr::new(&s), data);
    }

    pub fn lower_source(&mut self, src: nodes::UnitSource) -> Vec<Item<'a>> {
        src.items().map(|i| self.lower_item(i)).collect()
    }

    pub fn lower_item(&mut self, i: nodes::Item) -> Item<'a> {
        match i {
            nodes::Item::Pragma(pragma) => Item::Pragma(self.lower_pragma(pragma)),
            nodes::Item::Import(import) => Item::Import(self.lower_import(import)),
            nodes::Item::Using(using) => Item::Using(self.lower_using(using)),
            nodes::Item::Contract(contract) => match self.lower_contract(contract) {
                (_, c) => Item::Contract(c),
            },
            nodes::Item::NamedFunctionDefinition(f) =>
                Item::Function(self.lower_named_function_definition(f)),
            nodes::Item::StateVariableDeclaration(v) =>
                Item::StateVariable(self.lower_state_variable(v)),
            nodes::Item::StructDefinition(s) => Item::Struct(self.lower_structure(s)),
            nodes::Item::EnumDefinition(e) => Item::Enum(self.lower_enumeration(e)),
            nodes::Item::UserDefinedValueTypeDefinition(t) =>
                Item::UserDefinedValueType(self.lower_user_defined_value_type(t)),
            nodes::Item::ErrorDefinition(e) => Item::Error(self.lower_error(e)),
            nodes::Item::EventDefinition(e) => Item::Event(self.lower_event(e)),
        }
    }

    pub fn lower_pragma(&mut self, s: nodes::Pragma) -> PragmaId<'a> {
        let data = s.data_string();
        PragmaId::new(self.db, data, AstPtr::new(&s))
    }

    pub fn lower_using(&mut self, s: nodes::Using) -> UsingId<'a> {
        let items = match s.using_item() {
            Some(nodes::UsingItem::IdentPath(p)) => {
                vec![UsingAlias {
                    path: IdentPath::from(self.db, p),
                    as_name: None,
                }]
            }
            Some(nodes::UsingItem::UsingAliases(a)) => {
                a.using_aliases().map(|a| self.lower_using_alias(a)).collect::<Vec<_>>()
            }
            None => {
                vec![]
            }
        };
        let ty_name = s.using_target().and_then(|t| {
            if t.star_token().is_some() {
                None
            } else {
                t.ty().map(|ty| self.lower_type_ref(ty))
            }
        });

        let using = UsingData { items, type_name: ty_name, is_global: s.global_token().is_some() };
        UsingId::new(self.db, using, AstPtr::new(&s))
    }

    pub fn lower_using_alias(&mut self, s: nodes::UsingAlias) -> UsingAlias<'a> {
        UsingAlias {
            path: IdentPath::from_opt(self.db, s.ident_path()),
            as_name: s
                .user_defineable_operator()
                .and_then(|t| t.syntax().first_token())
                .map(UserDefineableOp::from),
        }
    }
    pub fn lower_call_argument_list(
        &mut self,
        expr: nodes::CallArgumentList,
    ) -> Vec<(Option<Ident<'a>>, ExprId<'a>)> {
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
}
