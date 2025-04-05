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
pub mod pragma;
pub mod source_unit;

use base_db::BaseDb;
use source_unit::ItemOrigin;
use vfs::File;

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

pub trait HasOrigin<'db> {
    fn item_origin(self, db: &'db dyn BaseDb) -> ItemOrigin<'db>;
}

#[macro_export]
macro_rules! impl_has_origin {
    ($t:ty) => {
        impl<'db> crate::hir::HasOrigin<'db> for $t {
            fn item_origin(self, db: &'db dyn base_db::BaseDb) -> ItemOrigin<'db> {
                self.origin(db)
            }
        }
    };
}

pub trait HasFile<'db> {
    fn file(self, db: &'db dyn BaseDb) -> File;
}

impl<'db, T: HasOrigin<'db>> HasFile<'db> for T {
    fn file(self, db: &'db dyn BaseDb) -> File {
        let mut o = self.item_origin(db);
        loop {
            match o {
                ItemOrigin::Root(source_unit) => return source_unit.file(db),
                ItemOrigin::Contract(contract_id) => {
                    o = contract_id.origin(db);
                },
            }
        }
    }
}