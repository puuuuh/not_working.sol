use std::env::args;

use rowan::ast::{AstNode, AstPtr};
use base_db::{BaseDb, Project};
use hir_def::{hir::{expr::Expr, ident::Ident, source_unit::{file_tree, Item}, HasFile as _}, parse, scope::expr::DefinitionSite, FilePosition};
use syntax::{ast::nodes, SyntaxKind};

use crate::navigation_target::NavigationTarget;

pub fn goto_definition(db: &dyn BaseDb, project: Project, pos: FilePosition) -> Option<NavigationTarget> {
    let t = hir_def::parse(db, pos.file);
    let parsed = file_tree(db, pos.file);
    let token = t.tree().syntax().token_at_offset(pos.position).next()?;
    if token.kind() != SyntaxKind::IDENT {
        return None;
    }
    let item = parsed.span_map(db).find(token.text_range())?;
    let scope = item.scope(db, project)?;
    let Some((_, map)) = item.body(db) else {
        if let Some(defsite) = scope.lookup(db, Ident::new(db, token.text())) {
            let ptr = match defsite {
                DefinitionSite::Item(item) => {
                    match item {
                        Item::Contract(contract_id) |
                            Item::Library(contract_id) |
                            Item::Interface(contract_id) => {
                            let node = contract_id.node(db);
                            let f = parse(db, contract_id.file(db));
                            let ptr = contract_id.node(db).to_node(&f.syntax_node());
                            ptr.name()?.syntax().text_range();
                        },
                        /* 
                        Item::Enum(enumeration_id) => {

                        },
                        Item::UserDefinedValueType(user_defined_value_type_id) => user_defined_value_type_id.node(db),
                        Item::Error(error_id) => error_id.node(db),
                        Item::Event(event_id) => event_id.node(db),
                        Item::Function(function_id) => function_id.node(db),
                        Item::StateVariable(state_variable_id) => state_variable_id.node(db),
                        Item::Struct(structure_id) => structure_id.node(db),
                        Item::Modifier(modifier_id) => modifier_id.node(db),
                        Item::Import(_) |
                        Item::Pragma(_) |
                        Item::Using(_) |
                        Item::Constructor(_) => todo!(),
                        Item::Module(_) => {return None}
                        */
                        _ => {todo!()}
                    }
                },
                _ => todo!()
            };

        }
        return None;
    };

    for ancestor in token.parent_ancestors() {
        if nodes::Expr::can_cast(ancestor.kind()) {
            let t = map.expr_map.0.get(&AstPtr::new(&nodes::Expr::cast(ancestor)?))?;
            let expr = match t.kind(db) {
                Expr::MemberAccess { owner, member_name } => todo!(),
                Expr::Call { callee, args } => todo!(),
                Expr::Ident { name_ref } => {
                    name_ref
                },
                Expr::Literal { data } => todo!(),
                _ => {
                    return None;
                }
            };

            scope.lookup_in_expr(db, *t, *expr);
        } else if let Some(a) = nodes::Stmt::cast(ancestor) {
            todo!();
        }
    }

    let ident_path = token.parent_ancestors().filter_map(nodes::Expr::cast).next()?;

    match item {
        Item::Constructor(_) => {}
        Item::Contract(_) => {}
        Item::Enum(_) => {}
        Item::Error(_) => {}
        Item::Event(e) => {
        }
        Item::Function(f) => {
            let scope = f.scope(db, project);
            let (body, map) = f.body(db)?;
            let t = map.expr_map.0.get(&AstPtr::new(&ident_path))?;
            
            scope.lookup(db, *t, f.name(db).unwrap());
        }
        Item::Import(_) => {}
        Item::Modifier(_) => {}
        Item::Struct(_) => {}
        Item::Using(_) => {}
        Item::UserDefinedValueType(_) => {}
        _ => {}
    }

    None
}

fn goto_definition_expr(db: &dyn BaseDb) {

}

#[cfg(test)]
mod tests {
    use salsa::Database;
    use base_db::{Project, TestDatabase, TestFixture};
    use hir_def::FilePosition;
    use crate::goto_definition::goto_definition;

    #[test]
    fn basic_goto() {
        let fixture = TestFixture::parse(r"
            contract ReentrancyGuardUpgradeable {}
            
            contract Test is
                ReentrancyGu$0ardUpgradeable,
                ERC2771ContextUpgradeable(address(0))
            {
                function helloWorld(IERC20 tmp) {
                    uint256 help = tmp.ReentrancyGuardUpgradeable.Hello(address(0));
                }
            }
        ");
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        db.attach(|db| {
            goto_definition(db, Project::new(db, vec![], vec![]), FilePosition {position: pos, file} );
        })
    }
}