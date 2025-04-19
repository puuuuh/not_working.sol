use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::expr::ExprId;
use crate::hir::ident::Ident;
use crate::hir::source_unit::Item;
use crate::hir::HasSourceUnit;
use crate::items::HirPrint;
use crate::{impl_has_syntax, impl_major_item, lazy_field, FileAstPtr, FileExt, InFile};
use base_db::BaseDb;
use rowan::ast::AstPtr;
use salsa::Database;
use vfs::File;
use std::fmt::Write;
use syntax::ast::nodes;
use rowan::ast::AstNode;

use super::{DataLocation, HasFile, HasSyntax, TypeRef};

#[salsa::tracked(debug)]
pub struct StatementId<'db> {
    #[return_ref]
    pub kind: Statement<'db>,

    pub node: Option<AstPtr<nodes::Stmt>>,
}

#[salsa::tracked]
impl<'db> StatementId<'db> {
    #[salsa::tracked]
    pub fn owner(self, db: &'db dyn BaseDb, module: File) -> Item<'db> {
        let node = self.node(db).unwrap();
        module.source_unit(db).item_map(db).find(node.syntax_node_ptr().text_range()).unwrap()
    }
}

#[salsa::tracked(debug)]
pub struct CatchClause<'db> {
    pub name: Option<Ident<'db>>,
    #[return_ref]
    pub args: Option<Vec<VariableDeclaration<'db>>>,
    pub body: StatementId<'db>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub enum Statement<'db> {
    Missing,
    VarDecl {
        items: Vec<Option<VariableDeclaration<'db>>>,
        init_expr: Option<ExprId<'db>>,
    },
    Expr {
        expr: ExprId<'db>,
    },
    Block {
        stmts: Vec<StatementId<'db>>,
        is_unchecked: bool,
    },
    If {
        cond: ExprId<'db>,
        body: StatementId<'db>,
        else_body: Option<StatementId<'db>>,
    },
    ForLoop {
        init: Option<StatementId<'db>>,
        cond: Option<ExprId<'db>>,
        finish_action: Option<ExprId<'db>>,
        body: StatementId<'db>,
    },
    WhileLoop {
        cond: ExprId<'db>,
        body: StatementId<'db>,
    },
    DoWhileLoop {
        cond: ExprId<'db>,
        body: StatementId<'db>,
    },
    Try {
        expr: ExprId<'db>,
        returns: Option<Vec<VariableDeclaration<'db>>>,
        body: StatementId<'db>,
        catch: Vec<CatchClause<'db>>,
    },
    Return {
        expr: Option<ExprId<'db>>,
    },
    Emit {
        event: ExprId<'db>,
        args: Vec<(Option<Ident<'db>>, ExprId<'db>)>,
    },
    Revert {
        event: ExprId<'db>,
        args: Vec<(Option<Ident<'db>>, ExprId<'db>)>,
    },
    Assembly {},
    Continue {},
    Break {},
}

impl HirPrint for StatementId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.kind(db).write(db, w, ident)
    }
}

impl HirPrint for CatchClause<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str(" catch ")?;
        if let Some(name) = self.name(db) {
            name.write(db, w, ident)?;
        }
        if let Some(args) = self.args(db) {
            w.write_str("(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?;
                }
                a.write(db, w, ident)?;
            }
            w.write_str(")")?;
        }
        self.body(db).write(db, w, ident)?;

        Ok(())
    }
}
impl HirPrint for Statement<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        match self {
            Statement::Missing => {
                w.write_str("<missing>; ")?;
            }
            Statement::VarDecl { items, init_expr } => {
                let is_single = items.len() == 1 && items[0].is_some();

                if !is_single {
                    w.write_str("(")?;
                }
                for (i, a) in items.iter().enumerate() {
                    if i > 0 {
                        w.write_str(", ")?;
                    }
                    if let Some(a) = a {
                        a.write(db, w, ident)?;
                    }
                }
                if !is_single {
                    w.write_str(")")?;
                }
                if let Some(e) = init_expr {
                    w.write_str(" = ")?;
                    e.write(db, w, ident)?;
                }
                w.write_str(";")?;
            }
            Statement::Expr { expr } => {
                expr.write(db, w, ident)?;
                w.write_str(";")?;
            }
            Statement::Block { stmts, is_unchecked } => {
                let my_ident_str = "\t".repeat(ident);
                let ident_str = "\t".repeat(ident + 1);
                if *is_unchecked {
                    w.write_str("unchecked ")?;
                }
                w.write_str(" {\n")?;
                for s in stmts {
                    write!(w, "{ident_str}")?;
                    s.write(db, w, ident + 1)?;
                    w.write_str("\n")?
                }
                write!(w, "{my_ident_str}}}")?;
            }
            Statement::If { cond, body, else_body } => {
                w.write_str("if (")?;
                cond.write(db, w, ident)?;
                w.write_str(")")?;

                body.write(db, w, ident)?;
                if let Some(s) = else_body {
                    w.write_str(" else")?;
                    s.write(db, w, ident)?;
                }
            }
            Statement::ForLoop { init, cond, finish_action, body } => {
                w.write_str("for (")?;
                if let Some(init) = init {
                    init.write(db, w, ident)?;
                } else {
                    w.write_str("; ")?;
                }
                if let Some(cond) = cond {
                    cond.write(db, w, ident)?;
                }
                w.write_str("; ")?;
                if let Some(finish_action) = finish_action {
                    finish_action.write(db, w, ident)?;
                }
                w.write_str(") ")?;
                body.write(db, w, ident)?;
            }
            Statement::WhileLoop { cond, body } => {
                w.write_str("while (")?;
                cond.write(db, w, ident)?;
                w.write_str(")")?;
                body.write(db, w, ident)?;
            }
            Statement::DoWhileLoop { cond, body } => {
                w.write_str("do")?;
                body.write(db, w, ident)?;
                w.write_str("while (")?;
                cond.write(db, w, ident)?;
                w.write_str(");")?;
            }
            Statement::Try { expr, returns, body, catch } => {
                w.write_str("try ")?;
                expr.write(db, w, ident)?;
                if let Some(ret) = returns {
                    w.write_str(" returns(")?;
                    for (i, a) in ret.iter().enumerate() {
                        if i > 0 {
                            w.write_str(", ")?;
                        }

                        a.write(db, w, ident)?;
                    }
                    w.write_str(")")?;
                }
                body.write(db, w, ident)?;
                for c in catch {
                    c.write(db, w, ident)?;
                }
            }
            Statement::Return { expr } => {
                w.write_str("return ")?;
                if let Some(expr) = expr {
                    expr.write(db, w, ident)?;
                }
                w.write_str(";")?;
            }
            Statement::Emit { event, args: _ } => {
                w.write_str("emit ")?;
                event.write(db, w, ident)?;
                w.write_str(";")?;
            }
            Statement::Revert { event, args: _ } => {
                w.write_str("revert ")?;
                event.write(db, w, ident)?;
                w.write_str(";")?;
            }
            Statement::Assembly {} => {
                w.write_str("assembly <unimplemented>;")?;
            }
            Statement::Continue {} => {
                w.write_str("continue;")?;
            }
            Statement::Break {} => {
                w.write_str("break;")?;
            }
        }
        Ok(())
    }
}
