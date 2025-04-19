use super::{expr::ExprId, ident::Ident};


#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct CallOption<'db> {
    pub name: Ident<'db>,
    pub val: ExprId<'db>,
}