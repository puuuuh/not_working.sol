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

use crate::hir::contract::ContractType;
use crate::hir::expr::ExprId;
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::op::UserDefineableOp;
use crate::hir::using::{UsingAlias, UsingData, UsingId};
use crate::item_tree::*;
use crate::FileAstPtr;
use base_db::{BaseDb, File};
use rowan::ast::AstNode;
use syntax::ast::nodes;
use syntax::TextRange;
use crate::semantics::child_container::ChildSource;
use crate::semantics::span_map::SpanMapRange;

pub(crate) struct Ctx<'a> {
    file: File,
    db: &'a dyn BaseDb,

    pub(crate) spans: Vec<(SpanMapRange, ChildSource<'a>)>,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn BaseDb, file: File) -> Self {
        Self { file, db, spans: Vec::new() }
    }

    pub fn save_span(&mut self, s: TextRange, data: ChildSource<'a>) {
        self.spans.push((SpanMapRange(s.start(), s.end()), data));
    }

    pub fn lower_source(&mut self, src: nodes::UnitSource) -> Vec<TopItem<'a>> {
        src.items().map(|i| self.lower_item(i)).collect()
    }

    pub fn lower_item(&mut self, i: nodes::Item) -> TopItem<'a> {
        match i {
            nodes::Item::Pragma(pragma) => TopItem::Pragma(self.lower_pragma(pragma)),
            nodes::Item::Import(import) => TopItem::Import(self.lower_import(import)),
            nodes::Item::Using(using) => TopItem::Using(self.lower_using(using)),
            nodes::Item::Contract(contract) => match self.lower_contract(contract) {
                (ContractType::Contract, c) => TopItem::Contract(c),
                (ContractType::Library, c) => TopItem::Library(c),
                (ContractType::Interface, c) => TopItem::Interface(c),
            },
            nodes::Item::NamedFunctionDefinition(f) => {
                TopItem::Function(self.lower_named_function_definition(f))
            }
            nodes::Item::StateVariableDeclaration(v) => {
                TopItem::StateVariable(self.lower_state_variable(v))
            }
            nodes::Item::StructDefinition(s) => TopItem::Struct(self.lower_structure(s)),
            nodes::Item::EnumDefinition(e) => TopItem::Enum(self.lower_enumeration(e)),
            nodes::Item::UserDefinedValueTypeDefinition(t) => {
                TopItem::UserDefinedValueType(self.lower_user_defined_value_type(t))
            }
            nodes::Item::ErrorDefinition(e) => TopItem::Error(self.lower_error(e)),
            nodes::Item::EventDefinition(e) => TopItem::Event(self.lower_event(e)),
        }
    }

    pub fn lower_pragma(&mut self, s: nodes::Pragma) -> PragmaId<'a> {
        let data = s.data_string();
        PragmaId::new(self.db, data)
    }

    pub fn lower_using(&mut self, s: nodes::Using) -> UsingId<'a> {
        let items = match s.using_item() {
            Some(nodes::UsingItem::IdentPath(p)) => {
                vec![UsingAlias {
                    path: IdentPath::from(self.db.as_dyn_database(), p),
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
        UsingId::new(self.db, using, FileAstPtr::new(self.file, &s))
    }

    pub fn lower_using_alias(&mut self, s: nodes::UsingAlias) -> UsingAlias<'a> {
        UsingAlias {
            path: IdentPath::from_opt(self.db.as_dyn_database(), s.ident_path()),
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
                    let name = Ident::from_name_ref(self.db.as_dyn_database(), arg.name_ref());
                    let expr = self.lower_expr2(arg.expr());
                    (Some(name), expr)
                })
                .collect(),
        }
    }
}
