mod call_options;
mod constructor;
mod contract;
mod data_location;
mod enumeration;
mod error;
mod event;
mod expr;
mod function;
mod helpers;
mod ident;
mod import;
mod literal;
mod modifier;
mod op;
mod pragma;
mod source_unit;
mod state_mutability;
mod state_variable;
mod statement;
mod structure;
mod type_name;
mod user_defined_value_type;
mod using;
mod variable_declaration;
mod visibility;

use base_db::BaseDb;
use rowan::ast::AstNode;
use rowan::TextSize;
use syntax::ast::nodes;
use vfs::File;

use std::fmt::{Display, Formatter};

pub use call_options::*;
pub use constructor::*;
pub use contract::*;
pub use data_location::*;
pub use enumeration::*;
pub use error::*;
pub use event::*;
pub use expr::*;
pub use function::*;
pub use helpers::*;
pub use ident::*;
pub use import::*;
pub use literal::*;
pub use modifier::*;
pub use op::*;
pub use pragma::*;
pub use source_unit::*;
pub use state_mutability::*;
pub use state_variable::*;
pub use statement::*;
pub use structure::*;
pub use type_name::*;
pub use user_defined_value_type::*;
pub use using::*;
pub use variable_declaration::*;
pub use visibility::*;

use crate::{source_map::item_source_map::ItemSourceMap, FileExt, InFile};

pub struct FilePosition {
    pub file: File,
    pub offset: TextSize,
}

#[macro_export]
macro_rules! impl_major_item {
    ($($t:ty: $t1:ty),+) => {
        $(
            $crate::impl_has_syntax!($t, $t1);
        )+
    };
}

pub trait HasSyntax<'db> {
    type Node;

    fn syntax(self, db: &'db dyn BaseDb) -> Self::Node;
}

#[macro_export]
macro_rules! impl_has_syntax {
    ($t:ty, $node_ty:ty) => {
        impl<'db> $crate::hir::HasSyntax<'db> for $t {
            type Node = $node_ty;

            fn syntax(self, db: &'db dyn base_db::BaseDb) -> Self::Node {
                let file = self.file(db);
                let f = <base_db::File as $crate::FileExt>::node(file, db);
                let node = self.node(db).to_node(&<_ as $crate::AstNode>::syntax(&f));
                node
            }
        }
    };
}

pub trait HasFile<'db> {
    fn file(self, db: &'db dyn BaseDb) -> File;
}

impl_major_item!(
    ConstructorId<'db>: nodes::ConstructorDefinition,
    ContractId<'db>: nodes::Contract,
    EnumerationId<'db>: nodes::EnumDefinition,
    ErrorId<'db>: nodes::ErrorDefinition,
    EventId<'db>: nodes::EventDefinition,
    FunctionId<'db>: nodes::FunctionDefinition,
    ImportId<'db>: nodes::Import,
    ModifierId<'db>: nodes::ModifierDefinition,
    PragmaId<'db>: nodes::Pragma,
    StateVariableId<'db>: nodes::StateVariableDeclaration,
    StructureId<'db>: nodes::StructDefinition,
    UserDefinedValueTypeId<'db>: nodes::UserDefinedValueTypeDefinition,
    UsingId<'db>: nodes::Using
);

impl<'db> HasSyntax<'db> for VariableDeclaration<'db> {
    type Node = nodes::VariableDeclaration;

    fn syntax(self, db: &'db dyn BaseDb) -> Self::Node {
        self.node(db).to_node(db)
    }
}

pub trait HasBody<'db> {
    fn body(
        self,
        db: &'db dyn BaseDb,
        file: File,
    ) -> Option<(StatementId<'db>, ItemSourceMap<'db>)>;
}
