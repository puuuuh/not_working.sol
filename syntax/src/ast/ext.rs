use crate::ast::nodes::{ArrayType, Expr, ForStmt, IfStmt, Stmt, TernaryExpr};
use crate::ast::{nodes, support, AstNode};
use crate::{SyntaxKind, SyntaxToken, T};

impl nodes::IndexExpr {
    pub fn base(&self) -> Option<Expr> {
        support::children(self.syntax()).next()
    }
    pub fn index(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }
}

impl nodes::SliceExpr {
    pub fn from(&self) -> Option<Expr> {
        if let Some((i, _)) =
            self.syntax().children_with_tokens().enumerate().find(|(_, e)| T![:] == e.kind())
        {
            return self
                .syntax
                .children_with_tokens()
                .take(i)
                .find_map(|e| Expr::cast(e.into_node()?));
        }
        None
    }
    pub fn to(&self) -> Option<Expr> {
        if let Some((i, _)) =
            self.syntax().children_with_tokens().enumerate().find(|(_, e)| T![:] == e.kind())
        {
            return self
                .syntax
                .children_with_tokens()
                .skip(i)
                .find_map(|e| Expr::cast(e.into_node()?));
        }
        None
    }
}

impl nodes::PrefixExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::children(self.syntax()).next()
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| match c
            .kind()
        {
            T![-] | T![--] | T![++] | T![!] | T![~] | T![new] | T![delete] => Some(c),

            _ => None,
        })
    }
}

impl nodes::PostfixExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::children(self.syntax()).next()
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| match c
            .kind()
        {
            T![--] | T![++] => Some(c),

            _ => None,
        })
    }
}

impl nodes::InfixExpr {
    pub fn lhs(&self) -> Option<Expr> {
        support::children(self.syntax()).next()
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn op(&self) -> Option<SyntaxToken> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|c| match c
            .kind()
        {
            T![||]
            | T![&&]
            | T![=]
            | T![==]
            | T![!=]
            | T![<=]
            | T![>=]
            | T![<]
            | T![>]
            | T![|]
            | T![&]
            | T![^]
            | T![<<]
            | T![>>]
            | T![>>>]
            | T![+]
            | T![-]
            | T![*]
            | T![/]
            | T![%]
            | T![**]
            | T![+=]
            | T![-=]
            | T![*=]
            | T![/=]
            | T![%=]
            | T![|=]
            | T![^=]
            | T![&=]
            | T![<<=]
            | T![>>=]
            | T![>>>=] => Some(c),

            _ => None,
        })
    }
}

impl TernaryExpr {
    pub fn cond(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(0)
    }

    pub fn if_true(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn if_false(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(2)
    }
}

impl IfStmt {
    pub fn then_body(&self) -> Option<Stmt> {
        support::children(self.syntax()).nth(0)
    }

    pub fn else_body(&self) -> Option<Stmt> {
        support::children(self.syntax()).nth(1)
    }
}

impl ForStmt {
    pub fn iterator(&self) -> (Option<Stmt>, Option<Expr>, Option<Expr>) {
        let iter = self.syntax.children_with_tokens();
        let mut args = iter.skip_while(|e| {
            e.kind() != SyntaxKind::SEMICOLON
                && e.kind() != SyntaxKind::EXPR_STMT
                && e.kind() != SyntaxKind::VARIABLE_DECLARATION_STMT
        });
        let init = args.next().and_then(|i| i.into_node()).and_then(Stmt::cast);
        let cond = args
            .find(|a| a.kind() == SyntaxKind::SEMICOLON || Stmt::can_cast(a.kind()))
            .and_then(|s| s.into_node())
            .and_then(|s| support::children(&s).next());

        let iter = args
            .find(|a| a.kind() == SyntaxKind::SEMICOLON || Expr::can_cast(a.kind()))
            .into_iter()
            .filter_map(|s| s.into_node())
            .find_map(Expr::cast);

        (init, cond, iter)
    }

    pub fn body(&self) -> Option<Stmt> {
        self.syntax
            .children_with_tokens()
            .skip_while(|a| a.kind() != SyntaxKind::R_PAREN)
            .filter_map(|a| a.into_node())
            .find_map(Stmt::cast)
    }
}

impl ArrayType {
    pub fn len(&self) -> Option<Expr> {
        self.syntax
            .children_with_tokens()
            .skip_while(|t| t.kind() != SyntaxKind::L_BRACK)
            .filter_map(|a| a.into_node())
            .find_map(Expr::cast)
    }
}

impl nodes::Pragma {
    pub fn data_string(&self) -> String {
        if let Some(data) = self.data().next() {
            data.to_string()
        } else {
            String::new()
        }
    }
}
