use base_db::{BaseDb, Project};
use hir_def::{
    hir::{FilePosition, Ident, Item},
    lower_file, FileExt, InFile,
};
use hir_nameres::scope::{body::Definition, HasScope};
use hir_ty::tys::TyKind;
use rowan::ast::{AstNode, AstPtr};
use syntax::{
    ast::nodes::{self},
    SyntaxKind,
};

use crate::navigation_target::NavigationTarget;

pub fn goto_definition(
    db: &dyn BaseDb,
    project: Project,
    pos: FilePosition,
) -> Option<Vec<NavigationTarget>> {
    let t = pos.file.node(db);
    let parsed = lower_file(db, pos.file);
    let token = t.syntax().token_at_offset(pos.offset).find(|t| t.kind() == SyntaxKind::IDENT)?;
    let expr = token.parent_ancestors().filter_map(|t| nodes::Expr::cast(t)).next();
    let item = parsed.source_map(db).find(token.text_range())?;
    let type_inference = hir_ty::resolver::resolve_item(db, project, item);
    let body = match item {
        Item::Function(function_id) => Some((function_id.body(db), function_id.scope(db, project))),
        Item::Constructor(constructor_id) => {
            Some((constructor_id.body(db), constructor_id.scope(db, project)))
        }
        Item::Modifier(modifier_id) => Some((modifier_id.body(db), modifier_id.scope(db, project))),
        _ => None,
    };

    if let Some((Some((_, source_map)), scopes)) = body {
        if let Some(expr) = expr {
            if let Some(e) = source_map.expr_map.get(&AstPtr::new(&expr)) {
                let defs = match e.kind(db) {
                    hir_def::hir::Expr::MemberAccess { owner, member_name } => {
                        let expr_map = type_inference.expr_map(db);
                        let owner_ty = dbg!(expr_map.get(owner)?);
                        let container = owner_ty.container(db)?;
                        container
                            .defs(db)
                            .iter()
                            .find(|(name, _)| name == member_name)
                            .and_then(|a| match a.1 {
                                Definition::Item(item) => NavigationTarget::from_item(db, item),
                                Definition::Local(variable_declaration) => {
                                    NavigationTarget::from_local(
                                        db,
                                        InFile { file: pos.file, data: variable_declaration.1 },
                                    )
                                }
                                Definition::Field(structure_field_id) => {
                                    NavigationTarget::from_field(db, structure_field_id)
                                }
                                Definition::EnumVariant(enumeration_variant_id) => {
                                    NavigationTarget::from_variant(db, enumeration_variant_id)
                                }
                            })
                            .into_iter()
                            .collect()
                    }
                    hir_def::hir::Expr::Ident { name_ref } => scopes
                        .lookup_in_expr(db, *e, *name_ref)
                        .into_iter()
                        .flat_map(|defsite| match defsite.1 {
                            Definition::Item(item) => NavigationTarget::from_item(db, item),
                            Definition::Local(variable_declaration) => {
                                NavigationTarget::from_local(
                                    db,
                                    InFile { file: pos.file, data: variable_declaration.1 },
                                )
                            }
                            _ => None,
                        })
                        .collect(),
                    _ => {
                        return None;
                    }
                };

                return Some(defs);
            }
        }
    }

    let scope = item.scope(db, project);

    return Some(
        scope
            .lookup(db, Ident::new(db, token.text()))
            .into_iter()
            .flat_map(|defsite| match defsite.1 {
                Definition::Item(item) => NavigationTarget::from_item(db, item),
                Definition::Local(variable_declaration) => NavigationTarget::from_local(
                    db,
                    InFile { file: pos.file, data: variable_declaration.1 },
                ),
                _ => None,
            })
            .collect(),
    );
}

#[cfg(test)]
mod tests {
    use crate::goto_definition::goto_definition;
    use base_db::{Project, TestDatabase, TestFixture};
    use hir_def::{hir::FilePosition, FileExt};
    use salsa::Database;

    #[test]
    fn basic_goto() {
        let fixture = TestFixture::parse(
            r#"
            import * as Test from "hello.sol";
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
                IERC20 test;

                struct TestStruct {
                    uint256 field;
                    uint128 field2;
                    uint256 field3;
                    uint256 field4;
                    uint256 field5;
                }
                uint64 testvar = 1;

                function helloWorld(IERC20 memory tmp) {
                    tmp.help(0);
                    TestStruct test22 = 1;
                    if (te$0st22 == 2) {
                    }
                    test22.field;
                }
            }
        "#,
        );
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        db.attach(|db| {
            dbg!(goto_definition(
                db,
                Project::new(db, vfs::VfsPath::from_virtual("".to_owned())),
                FilePosition { offset: pos, file }
            ));
        })
    }
}
