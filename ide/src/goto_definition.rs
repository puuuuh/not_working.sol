use rowan::ast::{AstNode, AstPtr};
use base_db::{BaseDb, Project};
use hir_def::{hir::{FilePosition, HasSourceUnit, Ident, Item}, nameres::body::Definition, FileExt};
use syntax::{ast::nodes::{self}, SyntaxKind};

use crate::navigation_target::NavigationTarget;

pub fn goto_definition(db: &dyn BaseDb, project: Project, pos: FilePosition) -> Option<Vec<NavigationTarget>> {
    let t = pos.file.node(db);
    let parsed = pos.file.source_unit(db);
    let token = t.syntax().token_at_offset(pos.offset).find(|t| t.kind() == SyntaxKind::IDENT)?;
    let expr = token.parent_ancestors().filter_map(|t| nodes::Expr::cast(t)).next();
    let item = parsed.item_map(db).find(token.text_range())?;
    let type_inference = hir_ty::resolver::resolve_item(db, project, item, pos.file);
    let body = match item {
        Item::Function(function_id) => Some((function_id.body(db, pos.file), function_id.scope(db, project, pos.file))),
        Item::Constructor(constructor_id) => Some((constructor_id.body(db, pos.file), constructor_id.scope(db, project, pos.file))),
        Item::Modifier(modifier_id) => Some((modifier_id.body(db, pos.file), modifier_id.scope(db, project, pos.file))),
        _ => None,
    };

    if let Some((Some((_, source_map)), scopes)) = body {
        if let Some(expr) = expr {
            if let Some(e) = source_map.expr_map.get(&AstPtr::new(&expr)) {
                dbg!(type_inference.expr_map(db).get(e));
                return Some(scopes.lookup_in_expr(db, *e, Ident::new(db, token.text()))
                    .into_iter()
                    .flat_map(|defsite| match defsite {
                        Definition::Item((module, item)) => NavigationTarget::from_item(db, item, module),
                        Definition::Local(variable_declaration) => NavigationTarget::from_local(db, variable_declaration.1, pos.file),
                        Definition::Field(f) => f.parent(db);
                        _ => None
                    })
                    .collect());
            }
        }
    }

    let scope = item.scope(db, project, pos.file);

    return Some(scope.lookup(db, Ident::new(db, token.text()))
        .into_iter()
        .flat_map(|defsite| match defsite {
            Definition::Item((module, item)) => NavigationTarget::from_item(db, item, module),
            Definition::Local(variable_declaration) => NavigationTarget::from_local(db, variable_declaration.1, pos.file),
            _ => None
        })
        .collect());
}

#[cfg(test)]
mod tests {
    use hir_def::hir::FilePosition;
    use salsa::Database;
    use base_db::{Project, TestDatabase, TestFixture};
    use crate::goto_definition::goto_definition;

    #[test]
    fn basic_goto() {
        let fixture = TestFixture::parse(r"
            contract ReentrancyGuardUpgradeable {}
            contract IERC20 {
                struct Test {}

                function help() {}
                function help(uint256) {}
            }
            
            contract Test is
                ReentrancyGuardUpgradeable,
                ERC2771ContextUpgradeable(address(0))
            {

                struct TestStruct {
                    uint256 field;
                    uint128 field2;
                    uint256 field3;
                    uint256 field4;
                    uint256 field5;
                }
                uint64 testvar = 1;

                function helloWorld(IERC20 memory tmp) {
                    tmp.he$0lp(0);
                    TestStruct test = 1;
                    test.field;
                }
            }
        ");
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        db.attach(|db| {
            goto_definition(db, Project::new(db, vfs::VfsPath::from_virtual("".to_owned())), FilePosition {offset: pos, file} );
        })
    }
}