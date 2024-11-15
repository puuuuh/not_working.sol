use rowan::ast::AstNode;
use salsa::Database;
use base_db::BaseDb;
use hir_def::FilePosition;

pub fn goto_definition(db: &dyn BaseDb, pos: FilePosition) -> Option<()> {
    let t = hir_def::parse(db, pos.file);
    let token = t.tree().syntax().token_at_offset(pos.position).next();
    let ident_path = token?.parent_ancestors().filter_map(syntax::ast::nodes::NameRef::cast).next();

    dbg!(ident_path.unwrap().syntax().parent());
    Some(())
}

#[cfg(test)]
mod tests {
    use salsa::Database;
    use hir_def::{parse, FilePosition};
    use crate::db::TestDatabase;
    use crate::goto_definition::goto_definition;

    #[test]
    fn basic_goto() {
        let fixture = crate::fixture::TestFixture::parse(r"
            contract ReentrancyGuardUpgradeable {}
            
            contract SGame is
                test.ReentrancyGuardUpgradeable,
                ERC2771ContextUpgradeable(address(0))
            {
                function helloWorld() {
                    uint256 help = tmp.Reentran$0cyGuardUpgradeable(address(0));
                }
            }
        ");
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        goto_definition(&db, FilePosition {position: pos, file} );
    }
}