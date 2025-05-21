use std::sync::Arc;

use base_db::{BaseDb};
use hir_def::source_map::item_source_map::ItemSourceMap;
use hir_def::{
    lower_file, Expr, ExprId, FileExt, FilePosition, Ident, IdentPath, Item, StatementId,
};
use hir_nameres::container::Container;
use hir_nameres::scope::{HasScope, Scope};
use hir_ty::extensions::Extensions;
use hir_ty::tys::Ty;
use rowan::ast::{AstNode, AstPtr};
use rowan::{TextRange, TextSize};
use syntax::ast::nodes;
use syntax::{SyntaxKind, SyntaxToken};

use super::item::Completion;

#[derive(Debug, Clone)]
enum CompletionKind<'db> {
    DotAccess { receiver: ExprId<'db>, receiver_ty: Ty<'db> },
    Path { receiver: Vec<Ident<'db>> },
    Name,
    Unknown,
}

pub struct CompletionCtx<'db> {
    pub db: &'db dyn BaseDb,
    pub item: Item<'db>,
    pub pos: TextRange,
    pub token: SyntaxToken,
}

fn find_expr<'db>(
    db: &'db dyn BaseDb,
    token: SyntaxToken,
    body: StatementId<'db>,
    source_map: ItemSourceMap<'db>,
    mut scope: Scope<'db>,
) -> Option<ExprId<'db>> {
    let e = token.parent_ancestors().find_map(|p| nodes::Expr::cast(p))?;

    source_map.expr(db, AstPtr::new(&e))
}

fn prev_token(token: &SyntaxToken) -> Option<SyntaxToken> {
    let mut prev = token.prev_token();
    while prev.as_ref().map(|t| t.kind()) == Some(SyntaxKind::WHITESPACE) {
        prev = prev.unwrap().prev_token();
    }
    prev
}

impl<'db> CompletionCtx<'db> {
    pub fn new(db: &'db dyn BaseDb, pos: FilePosition) -> Option<Self> {
        let source_unit = lower_file(db, pos.file);
        let n = pos.file.node(db);
        let token = n.syntax().token_at_offset(pos.offset).next()?;
        let map = source_unit.source_map(db);
        let item = map.find_pos(pos.offset)?;
        let pos = if token.kind() == SyntaxKind::IDENT {
            token.text_range()
        } else {
            TextRange::at(pos.offset, 0.into())
        };

        Some(Self { db, item, pos, token })
    }

    pub fn completions(&'db self) -> Option<Vec<Completion>> {
        Some(match self.kind()? {
            CompletionKind::DotAccess { receiver, receiver_ty } => {
                let c = receiver_ty.members(self.db);
                c
                    .into_iter()
                    .map(|a| Completion {
                        label: a.0.data(self.db).clone(),
                        src_range: self.pos,
                        text: a.0.data(self.db).clone(),
                        kind: super::CompletionKind::Item,
                    })
                    .collect()
            }
            CompletionKind::Path { receiver } => {
                let s = self.item.scope(self.db).lookup_path(self.db, &receiver)?;
                let c = Container::try_from(self.item).ok()?;
                c.defs(self.db)
                    .into_iter()
                    .map(|a| Completion {
                        label: a.0.data(self.db).clone(),
                        src_range: self.pos,
                        text: a.0.data(self.db).clone(),
                        kind: super::CompletionKind::Item,
                    })
                    .collect()
            }
            CompletionKind::Name => {
                let mut s = self.item.scope(self.db);
                if let Some((_, source_map)) = self.item.body(self.db) {
                    for ancestor in self.token.parent_ancestors() {
                        if nodes::Expr::can_cast(ancestor.kind()) {
                            if let Some(expr) = source_map
                                .expr(self.db, AstPtr::new(&nodes::Expr::cast(ancestor).unwrap()))
                            {
                                s = s.for_expr(self.db, expr);
                            }
                            break;
                        } else if let Some(stmt) = nodes::Stmt::cast(ancestor) {
                            if let Some(stmt) = source_map.stmt(self.db, AstPtr::new(&stmt)) {
                                s = s.for_stmt(self.db, stmt);
                            }
                            break;
                        }
                    }
                }

                s.all_definitions(self.db)
                    .into_iter()
                    .map(|a| Completion {
                        label: a.0.data(self.db).clone(),
                        src_range: self.pos,
                        text: a.0.data(self.db).clone(),
                        kind: super::CompletionKind::Item,
                    })
                    .collect()
            }
            CompletionKind::Unknown => return None,
        })
    }

    fn kind(&'db self) -> Option<CompletionKind<'db>> {
        if let Some(expr) = self.receiver_expr() {
            let type_inference = hir_ty::resolver::resolve_item(self.db, self.item);
            let ty = type_inference.expr(self.db, expr);
            if !ty.is_unknown(self.db) {
                return Some(CompletionKind::DotAccess { receiver: expr, receiver_ty: ty });
            }
        }
        if let Some(path) = self.parent_ident_path() {
            return Some(CompletionKind::Path { receiver: path });
        }

        return Some(CompletionKind::Name);
    }

    fn parent_ident_path(&self) -> Option<Vec<Ident<'db>>> {
        let mut res = vec![];
        let mut token = self.token.clone();
        if token.kind() != SyntaxKind::DOT {
            token = prev_token(&token)?;
        }
        if token.kind() != SyntaxKind::DOT {
            return None;
        }
        token = prev_token(&token)?;
        loop {
            if token.kind() != SyntaxKind::IDENT {
                break;
            }

            res.push(Ident::from_str(self.db, Some(token.text())));

            if let Some(p) = prev_token(&token) {
                token = p;
            } else {
                break;
            }

            if token.kind() != SyntaxKind::DOT {
                break;
            }

            if let Some(p) = prev_token(&token) {
                token = p;
            } else {
                break;
            }
        }
        res.reverse();

        Some(res)
    }

    fn receiver_expr(&self) -> Option<ExprId<'db>> {
        let mut token = self.token.clone();
        if token.kind() != SyntaxKind::DOT {
            token = prev_token(&token)?;
        }
        if token.kind() != SyntaxKind::DOT {
            return None;
        }
        token = prev_token(&token)?;

        let e = token.parent_ancestors().find_map(|t| nodes::Expr::cast(t))?;
        let (stmt, map) = self.item.body(self.db)?;

        return map.expr(self.db, AstPtr::new(&e));
    }
}
