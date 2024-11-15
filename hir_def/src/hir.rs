pub mod argument;
pub mod constructor;
pub mod contract;
pub mod data_location;
pub mod enumeration;
pub mod error;
pub mod event;
pub mod expr;
pub mod function;
pub mod helpers;
pub mod ident;
pub mod import;
pub mod literal;
pub mod modifier;
pub mod op;
pub mod state_variable;
pub mod statement;
pub mod structure;
pub mod type_name;
pub mod user_defined_value_type;
pub mod using;
pub mod visibility;

use crate::hir::expr::ExprId;
use crate::hir::ident::Ident;
pub use crate::hir::op::{BinaryOp, PostfixOp, PrefixOp};
use crate::hir::type_name::TypeRef;
pub use crate::hir::visibility::Visibility;
use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub struct CallOption<'db> {
    pub name: Ident<'db>,
    pub val: ExprId<'db>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum StateMutability {
    Pure,
    View,
    Payable,
    NonPayable,
}

impl Display for StateMutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            StateMutability::Pure => " pure",
            StateMutability::View => " view",
            StateMutability::Payable => " payable",
            StateMutability::NonPayable => "",
        })
    }
}
