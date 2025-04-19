mod variable_declaration;
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
mod state_variable;
mod statement;
mod structure;
mod type_name;
mod user_defined_value_type;
mod using;
mod visibility;
mod pragma;
mod source_unit;
mod call_options;
mod state_mutability;

use base_db::BaseDb;
use rowan::TextSize;
use syntax::ast::nodes;
use vfs::File;

use std::fmt::{Display, Formatter};

pub use variable_declaration::*;
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
pub use state_variable::*;
pub use statement::*;
pub use structure::*;
pub use type_name::*;
pub use user_defined_value_type::*;
pub use using::*;
pub use visibility::*;
pub use pragma::*;
pub use source_unit::*;
pub use call_options::*;
pub use state_mutability::*;

use crate::{nameres::body::Definition, source_map::item_source_map::ItemSourceMap};

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

    fn syntax(self, db: &'db dyn BaseDb, module: vfs::File) -> Self::Node;
}

#[macro_export]
macro_rules! impl_has_syntax {
    ($t:ty, $node_ty:ty) => {
        impl<'db> crate::hir::HasSyntax<'db> for $t {
            type Node = $node_ty;

            fn syntax(self, db: &'db dyn base_db::BaseDb, module: vfs::File) -> Self::Node {
                let f = <base_db::File as $crate::FileExt>::node(module, db);
                let node = self.node(db).to_node(&<_ as $crate::AstNode>::syntax(&f));
                node
            }
        }
    }
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
    UsingId<'db>: nodes::Using,
    VariableDeclaration<'db>: nodes::VariableDeclaration
);

pub trait HasBody<'db> {
    fn body(self, db: &'db dyn BaseDb, file: File) -> Option<(StatementId<'db>, ItemSourceMap<'db>)>;
}

pub trait HasSourceUnit<'db> {
    fn source_unit(self, db: &'db dyn BaseDb) -> SourceUnit<'db>;
}

pub trait HasDefs<'db> {
    fn defs(self, db: &'db dyn BaseDb, module: File) -> Vec<(Ident<'db>, Definition<'db>)>;
}