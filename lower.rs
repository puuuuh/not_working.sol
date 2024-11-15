use crate::item_tree::*;
use crate::{ast_id_map, HirDefDb};
use base_db::File;
use span::AstIdMap;
use std::sync::Arc;
use syntax::ast::AstNode;
use crate::hir::contract::{Contract, ContractId, ContractItem, InheritanceSpecifier};
use crate::hir::Expr;
use crate::hir::ident::IdentPath;
use crate::hir::import::{ImportKind, SymbolAlias};
use crate::hir::using::UsingAlias;
use crate::lower::expr::lower_expr;
use crate::lower::types::lower_type_ref;

pub(crate) struct Ctx<'a> {
    db: &'a dyn HirDefDb,
    ast_ids: Arc<AstIdMap>,
}

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn HirDefDb, file: File) -> Self {
        Self { db, ast_ids: ast_id_map(db, file) }
    }

    pub fn lower_source(&self, src: nodes::UnitSource) -> Vec<TopItem<'a>> {
        src.items().filter_map(|i| self.lower_item(i)).collect()
    }

    pub fn lower_contract_item(&self, i: nodes::ContractItem) -> Option<ContractItem<'a>> {
        Some(match i {
            nodes::ContractItem::ConstructorDefinition(i) => {

            }
            nodes::ContractItem::FunctionDefinition(i) => {}
            nodes::ContractItem::ModifierDefinition(i) => {}
            nodes::ContractItem::FallbackFunctionDefinition(i) => {}
            nodes::ContractItem::ReceiveFunctionDefinition(i) => {}
            nodes::ContractItem::StructDefinition(i) => {}
            nodes::ContractItem::EnumDefinition(i) => {}
            nodes::ContractItem::UserDefinedValueTypeDefinition(i) => {}
            nodes::ContractItem::StateVariableDeclaration(i) => {}
            nodes::ContractItem::EventDefinition(i) => {}
            nodes::ContractItem::ErrorDefinition(i) => {}
            nodes::ContractItem::Using(i) => {}
        })
    }

    pub fn lower_item(&self, i: nodes::Item) -> Option<TopItem<'a>> {
        Some(match i {
            nodes::Item::Pragma(pragma) => TopItem::Pragma(self.lower_pragma(pragma)?),
            nodes::Item::Import(import) => TopItem::Import(self.lower_import(import)?),
            nodes::Item::Using(using) => TopItem::Using(self.lower_using(using)?),
            nodes::Item::Contract(contract) => TopItem::Contract(self.lower_contract(contract)?),
            nodes::Item::Interface(i) => TopItem::Interface(self.lower_interface(i)?),
            nodes::Item::Library(l) => TopItem::Library(self.lower_library(l)?),
            nodes::Item::FunctionDefinition(f) => TopItem::Function(self.lower_function(f)?),
            nodes::Item::StateVariableDeclaration(v) => {
                TopItem::StateVariable(self.lower_state_variable(v)?)
            }
            nodes::Item::StructDefinition(s) => TopItem::Struct(self.lower_struct(s)?),
            nodes::Item::EnumDefinition(e) => TopItem::Enum(self.lower_enum(e)?),
            nodes::Item::UserDefinedValueTypeDefinition(t) => {
                TopItem::UserDefinedValueType(self.lower_value_type(t)?)
            }
            nodes::Item::ErrorDefinition(e) => TopItem::Error(self.lower_error(e)?),
            nodes::Item::EventDefinition(e) => TopItem::Event(self.lower_event(e)?),
        })
    }

    pub fn lower_pragma(&self, s: nodes::Pragma) -> Option<Pragma<'a>> {
        let data = s.data_string();
        Some(Pragma::new(self.db, data, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_import(&self, s: nodes::Import) -> Option<Import<'a>> {
        let kind = match s.import_item()? {
            nodes::ImportItem::ImportPath(path) => {
                ImportKind::Path {
                    path: path.path()?.string_token()?.text().to_string(),
                    name: path.name().map(Name::from),
                }
            }
            nodes::ImportItem::ImportSymbols(data) => {
                if let Some(alias) = data.symbol_alias() {
                    ImportKind::Glob {
                        as_name: Name::from(alias.name()?),
                        path: data.path()?.string_token()?.text().to_owned()
                    }
                } else if let Some(aliases) = data.symbol_aliases() {
                    ImportKind::Aliases {
                        symbol_aliases: aliases.symbol_aliases().filter_map(|a| {
                            Some(SymbolAlias {
                                name: Name(a.symbol()?.ident_token()?.text().to_owned()),
                                as_name: a.name().map(Name::from)
                            })
                        }).collect(),
                        path: data.path()?.string_token()?.text().to_owned()
                    }
                } else {
                    return None
                }
            }
        };
        Some(Import::new(self.db, kind, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_using(&self, s: nodes::Using) -> Option<Using<'a>> {
        let items = match s.using_item()? {
            nodes::UsingItem::IdentPath(p) => {
                vec![UsingAlias {
                    path: p.into(),
                    as_name: None,
                }]
            }
            nodes::UsingItem::UsingAliases(a) => {
                a.using_aliases().filter_map(|a| self.lower_using_alias(a)).collect::<Vec<_>>()
            }
        };
        let t = s.using_target()?;
        let ty_name = if t.star_token().is_some() {
            None
        } else {
            Some(lower_type_ref(self.db, t.ty()?))
        };

        let using = UsingData {
            items,
            type_name: ty_name,
            is_global: s.global_token().is_some(),
        };
        Some(Using::new(self.db, using, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_using_alias(&self, s: nodes::UsingAlias) -> Option<UsingAlias> {
        Some(UsingAlias {
            path: s.ident_path()?.into(),
            as_name: Some(s.user_defineable_operator()?.syntax().first_token()?.into())
        })
    }

    pub fn lower_contract(&self, s: nodes::Contract) -> Option<ContractId<'a>> {
        let contract = Contract {
            name: s.name().into(),
            is_abstract: s.abstract_token().is_some(),
            inheritance_chain: s.inheritance().and_then(|s| self.lower_inheritance(s)).unwrap_or_default(),
        };

        let body = s.contract_items()
            .filter_map(|t| self.lower_contract_item(t))
            .collect();

        Some(ContractId::new(self.db, contract, body))
    }

    pub fn lower_interface(&self, s: nodes::Interface) -> Option<ContractId<'a>> {
        let contract = Contract {
            name: s.name().into(),
            is_abstract: false,
            inheritance_chain: s.inheritance().and_then(|s| self.lower_inheritance(s)).unwrap_or_default(),
        };
        Some(ContractId::new(self.db, contract, vec![]))
    }

    pub fn lower_library(&self, s: nodes::Library) -> Option<ContractId<'a>> {
        let name = s.name().into();
        let contract = Contract {
            name,
            is_abstract: false,
            inheritance_chain: s.inheritance().and_then(|s| self.lower_inheritance(s)).unwrap_or_default(),
        };
        Some(ContractId::new(self.db, contract, vec![]))
    }

    pub fn lower_function(&self, s: nodes::FunctionDefinition) -> Option<Function<'a>> {
        let name = s.name()?.ident_token()?.to_string();
        Some(Function::new(self.db, name, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_state_variable(
        &self,
        s: nodes::StateVariableDeclaration,
    ) -> Option<StateVariable<'a>> {
        let name = s.name()?.ident_token()?.to_string();
        Some(StateVariable::new(self.db, name, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_struct(&self, s: nodes::StructDefinition) -> Option<Struct<'a>> {
        let name = s.name()?.ident_token()?.text().to_owned();
        Some(Struct::new(self.db, name, self.ast_ids.ast_id(&s)))
    }
    pub fn lower_enum(&self, s: nodes::EnumDefinition) -> Option<Enum<'a>> {
        let name = s.name()?.ident_token()?.to_string();
        Some(Enum::new(self.db, name, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_value_type(
        &self,
        s: nodes::UserDefinedValueTypeDefinition,
    ) -> Option<UserDefinedValueType<'a>> {
        let name = s.name()?.ident_token()?.to_string();
        Some(UserDefinedValueType::new(self.db, name, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_event(&self, s: nodes::EventDefinition) -> Option<Event<'a>> {
        let name = s.name()?.ident_token()?.to_string();
        Some(Event::new(self.db, name, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_error(&self, s: nodes::ErrorDefinition) -> Option<Error<'a>> {
        let name = s.name()?.ident_token()?.to_string();
        Some(Error::new(self.db, name, self.ast_ids.ast_id(&s)))
    }

    pub fn lower_inheritance(&self, s: nodes::Inheritance) -> Option<Vec<InheritanceSpecifier>> {
        Some(s.inheritance_specifiers()
            .filter_map(|s| self.lower_inheritance_specifier(s))
            .collect())
    }

    pub fn lower_inheritance_specifier(&self, s: nodes::InheritanceSpecifier) -> Option<InheritanceSpecifier> {
        let path = IdentPath::from(s.ident_path()?);
        let args = s.call_argument_list().map(|list| self.lower_argument_list(list));
        Some(
            InheritanceSpecifier {
                path,
                args,
            }
        )
    }

    pub fn lower_argument_list(
        &self,
        expr: nodes::CallArgumentList,
    ) -> Vec<(Option<Name>, Expr)> {
        match expr {
            nodes::CallArgumentList::CallArguments(args) => {
                args.exprs().map(|expr| (None, lower_expr(self.db, expr))).collect()
            }
            nodes::CallArgumentList::NamedCallArguments(args) => args
                .named_call_arguments()
                .map(|arg| {
                    let name = arg.name().map(Name::from).unwrap_or_else(Name::missing);
                    let expr = arg.expr().map(|e| lower_expr(self.db, e)).unwrap_or(Expr::Missing);
                    (Some(name), expr)
                })
                .collect(),
        }
    }
}
