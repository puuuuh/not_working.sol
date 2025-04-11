use rowan::ast::{AstNode, AstPtr};
use base_db::{BaseDb, Project};
use hir_def::{hir::{file_tree, Ident, Item}, scope::expr::DefinitionSite, source_map::span_map, FileExt, FilePosition};
use syntax::{ast::nodes::{self, FunctionDefinition}, SyntaxKind};

use crate::navigation_target::NavigationTarget;

pub fn goto_definition(db: &dyn BaseDb, project: Project, pos: FilePosition) -> Option<Vec<NavigationTarget>> {
    let t = pos.file.tree(db);
    let parsed = file_tree(db, pos.file);
    let token = t.syntax().token_at_offset(pos.position).find(|t| t.kind() == SyntaxKind::IDENT)?;
    let expr = token.parent_ancestors().filter_map(|t| nodes::Expr::cast(t)).next();
    let item = parsed.span_map(db).find(token.text_range())?;
    let scope = item.scope(db, project)?;
    let body = match item {
        Item::Function(function_id) => Some((function_id.body(db), function_id.scope(db, project))),
        Item::Constructor(constructor_id) => Some((constructor_id.body(db), constructor_id.scope(db, project))),
        Item::Modifier(modifier_id) => Some((modifier_id.body(db), modifier_id.scope(db, project))),
        _ => None,
    };

    if let Some((Some((_, source_map)), scopes))= body {
        if let Some(expr) = expr {
            if let Some(e) = source_map.expr_map.get(&AstPtr::new(&expr)) {
                return Some(scopes.lookup(db, *e, Ident::new(db, token.text()))
                    .into_iter()
                    .flat_map(|defsite| match defsite {
                        DefinitionSite::Item(item) => NavigationTarget::from_item(db, item),
                        DefinitionSite::Local(variable_declaration) => NavigationTarget::from_local(db, variable_declaration, pos.file),
                    })
                    .collect());
            }
        }
    }

    return Some(scope.lookup(db, Ident::new(db, token.text()))
        .into_iter()
        .flat_map(|defsite| match defsite {
            DefinitionSite::Item(item) => NavigationTarget::from_item(db, item),
            DefinitionSite::Local(variable_declaration) => NavigationTarget::from_local(db, variable_declaration, pos.file),
        })
        .collect());
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
                ReentrancyGuardUpgradeable,
                ERC2771ContextUpgradeable(address(0))
            {
                modifier temp(IERC20 memory tmp) {
                    return tm$0p;
                }

                function helloWorld(IERC20 memory tmp) {
                    uint256 test = 1;
                    uint256 help = test.ReentrancyGuardUpgradeable.Hello(address(0));
                }
            }
        ");
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        db.attach(|db| {
            goto_definition(db, Project::new(db, vfs::VfsPath::from_virtual("".to_owned())), FilePosition {position: pos, file} );
        })
    }
}