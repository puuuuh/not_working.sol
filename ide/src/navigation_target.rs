use base_db::BaseDb;
use hir_def::{hir::{EnumerationVariantId, HasFile, HasSyntax, Item, StructureFieldId, VariableDeclaration}, FileExt, InFile};
use syntax::{ast::nodes::{self, ModifierDefinition, Name}, TextRange};
use rowan::ast::AstNode;
use vfs::File;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct NavigationTarget {
    pub file: File,

    pub full_range: TextRange,
    pub focus_range: TextRange,
}

impl NavigationTarget {
    pub fn from_variant(db: &'_ dyn BaseDb, d: EnumerationVariantId<'_>) -> Option<NavigationTarget> {
        let e= d.parent(db);
        let node = e.syntax(db);
        let name = d.name(db).data(db);
        let variable = node.enum_members()
            .find(|e| e.name().and_then(|n| n.ident_token()).map(|n| n.text() == name) == Some(true))?;
        let full_range = node.syntax().text_range();
        let focus_range = variable.syntax().text_range();
        Some(NavigationTarget {
            file: e.file(db),
            full_range,
            focus_range
        })
    }

    pub fn from_field(db: &'_ dyn BaseDb, d: StructureFieldId<'_>) -> Option<NavigationTarget> {
        let s = d.parent(db);
        let node = s.syntax(db);
        let name = d.name(db).data(db);
        let variable = node.struct_members()
            .find(|e| e.name().and_then(|n| n.ident_token()).map(|n| n.text() == name) == Some(true))?;
        let full_range = node.syntax().text_range();
        let focus_range = variable.syntax().text_range();
        Some(NavigationTarget {
            file: s.file(db),
            full_range,
            focus_range
        })
    }

    pub fn from_local(db: &'_ dyn BaseDb, d: InFile<VariableDeclaration<'_>>) -> Option<NavigationTarget> {
        let node = d.syntax(db);
        let full_range = node.syntax().text_range();
        Some(NavigationTarget {
            file: d.file,
            full_range,
            focus_range: node.name().map(|n| n.syntax().text_range()).unwrap_or(full_range),
        })
    }

    pub fn from_item(db: &'_ dyn BaseDb, d: Item<'_>) -> Option<NavigationTarget> {
        let name_range = |n: Name| n.syntax().text_range();
        let (full_range, focus_range) = match d {
            Item::Import(import_id) => {
                let node = import_id.syntax(db);
                (node.syntax().text_range(), None)
            },
            Item::Pragma(pragma_id) => {
                let node = pragma_id.syntax(db);
                (node.syntax().text_range(), None)
            },
            Item::Using(using_id) => {
                let node = using_id.syntax(db);
                (node.syntax().text_range(), None)
            },
            Item::Contract(contract_id) => {
                let node = contract_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Enum(enumeration_id) => {
                let node = enumeration_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                let node = user_defined_value_type_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Error(error_id) => {
                let node = error_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Event(event_id) => {
                let node = event_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Function(function_id) => {
                let node = function_id.syntax(db);
                let focus = match &node {
                    nodes::FunctionDefinition::NamedFunctionDefinition(named_function_definition) => 
                        named_function_definition.name().map(name_range),
                    nodes::FunctionDefinition::FallbackFunctionDefinition(fallback_function_definition) => 
                        fallback_function_definition.fallback_token().map(|t| t.text_range()),
                    nodes::FunctionDefinition::ReceiveFunctionDefinition(receive_function_definition) => 
                        receive_function_definition.receive_token().map(|t| t.text_range()),
                };
                (node.syntax().text_range(), focus)
            },
            Item::StateVariable(state_variable_id) => {
                let node = state_variable_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Struct(structure_id) => {
                let node = structure_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Constructor(constructor_id) => {
                let node = constructor_id.syntax(db);
                (node.syntax().text_range(), node.constructor_token().map(|t| t.text_range()))
            },
            Item::Modifier(modifier_id) => {
                let node = modifier_id.syntax(db);
                (node.syntax().text_range(), node.name().map(name_range))
            },
            Item::Module(source_unit) => {
                let node = source_unit.file(db).node(db);
                (node.syntax().text_range(), None)
            },
        };
        Some(NavigationTarget {
            file: d.file(db),
            full_range,
            focus_range: focus_range.unwrap_or(full_range),
        })
    }
}