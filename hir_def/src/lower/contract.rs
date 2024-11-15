use rowan::ast::AstNode;
use crate::hir::contract::{ContractId, ContractItem, ContractType, InheritanceSpecifier};
use crate::hir::ident::{Ident, IdentPath};
use crate::lower::Ctx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'a> Ctx<'a> {
    pub fn lower_contract_item(&mut self, i: nodes::ContractItem) -> Option<ContractItem<'a>> {
        Some(match i {
            nodes::ContractItem::ConstructorDefinition(i) => {
                ContractItem::Constructor(self.lower_constructor_definition(i))
            }
            nodes::ContractItem::NamedFunctionDefinition(i) => {
                ContractItem::Function(self.lower_named_function_definition(i))
            }
            nodes::ContractItem::ModifierDefinition(i) => {
                ContractItem::Modifier(self.lower_modifier_definition(i))
            }
            nodes::ContractItem::UserDefinedValueTypeDefinition(i) => {
                ContractItem::UserDefinedValueType(self.lower_user_defined_value_type(i))
            }
            nodes::ContractItem::StateVariableDeclaration(i) => {
                ContractItem::StateVariable(self.lower_state_variable(i))
            }
            nodes::ContractItem::StructDefinition(i) => {
                ContractItem::Struct(self.lower_structure(i))
            }
            nodes::ContractItem::EnumDefinition(i) => ContractItem::Enum(self.lower_enumeration(i)),
            nodes::ContractItem::EventDefinition(i) => ContractItem::Event(self.lower_event(i)),
            nodes::ContractItem::ErrorDefinition(i) => ContractItem::Error(self.lower_error(i)),
            nodes::ContractItem::Using(i) => ContractItem::Using(self.lower_using(i)),
            nodes::ContractItem::FallbackFunctionDefinition(i) => {
                ContractItem::Function(self.lower_fallback_function_definition(i))
            }
            nodes::ContractItem::ReceiveFunctionDefinition(i) => {
                ContractItem::Function(self.lower_receive_function_definition(i))
            }
        })
    }

    pub fn lower_contract(&mut self, s: nodes::Contract) -> (ContractType, ContractId<'a>) {
        let mut t = ContractType::Contract;
        if s.interface_token().is_some() {
            t = ContractType::Interface;
        }
        if s.library_token().is_some() {
            t = ContractType::Library;
        }
        let name = Ident::from_name(self.db.as_dyn_database(), s.name());
        let is_abstract = s.abstract_token().is_some();
        let inheritance_chain =
            s.inheritance().and_then(|s| self.lower_inheritance(s)).unwrap_or_default();

        let body = s.contract_items().filter_map(|t| self.lower_contract_item(t)).collect();

        let contract = ContractId::new(
            self.db,
            name,
            is_abstract,
            inheritance_chain,
            body,
            FileAstPtr::new(self.file, &s),
        );
        
        self.save_span(s.syntax().text_range(), ChildSource::Contract(contract));
        for i in contract.body(self.db) {
            i.set_def_site(self.db, contract);
        }
        (t, contract)
    }

    pub fn lower_inheritance(
        &mut self,
        s: nodes::Inheritance,
    ) -> Option<Vec<InheritanceSpecifier<'a>>> {
        Some(
            s.inheritance_specifiers()
                .filter_map(|s| self.lower_inheritance_specifier(s))
                .collect(),
        )
    }

    pub fn lower_inheritance_specifier(
        &mut self,
        s: nodes::InheritanceSpecifier,
    ) -> Option<InheritanceSpecifier<'a>> {
        let path = IdentPath::from_opt(self.db.as_dyn_database(), s.ident_path());
        let args = s.call_argument_list().map(|list| self.lower_call_argument_list(list));
        Some(InheritanceSpecifier { path, args })
    }
}
