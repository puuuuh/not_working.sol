use crate::hir::ident::Ident;
use crate::hir::variable_declaration::VariableDeclaration;
use crate::items::HirPrint;
use crate::lazy_field;
use crate::{hir::expr::ExprId, FileAstPtr};
use rowan::ast::AstPtr;
use salsa::Database;
use std::fmt::{Debug, Display, Formatter, Write};
use syntax::ast::nodes;

use super::{state_mutability::StateMutability, visibility::Visibility};

#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Debug, Hash, salsa::Update)]
pub enum ElementaryTypeRef {
    Address { payable: bool },
    Bool,
    String,
    Bytes,
    Integer { signed: bool, size: u16 },
    FixedBytes { size: u16 },
    Fixed { signed: bool, size: u16, decimal_points: u8 },
    Unknown,
}

impl Display for ElementaryTypeRef {
    fn fmt(&self, w: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ElementaryTypeRef::Address { payable } => {
                if *payable {
                    write!(w, "address payable")
                } else {
                    write!(w, "address")
                }
            }
            ElementaryTypeRef::Bool => {
                write!(w, "bool")
            }
            ElementaryTypeRef::String => {
                write!(w, "string")
            }
            ElementaryTypeRef::Bytes => {
                write!(w, "bytes")
            }
            ElementaryTypeRef::Integer { signed, size } => {
                let i = if *signed { "int" } else { "uint" };
                write!(w, "{i}{size}")
            }
            ElementaryTypeRef::FixedBytes { size } => {
                write!(w, "bytes{size}")
            }
            ElementaryTypeRef::Fixed { signed, size, decimal_points } => {
                let i = if *signed { "fixed" } else { "ufixed" };
                write!(w, "{i}{size}x{decimal_points}")
            }
            ElementaryTypeRef::Unknown => {
                write!(w, "<error>")
            }
        }
    }
}

impl HirPrint for ElementaryTypeRef {
    fn write<T: Write>(&self, _db: &dyn Database, w: &mut T, _ident: usize) -> std::fmt::Result {
        write!(w, "{self}")
    }
}

#[salsa::tracked(debug)]
pub struct TypeRefId<'db> {
    pub kind: TypeRefKind<'db>,
    #[tracked]
    pub node: Option<FileAstPtr<nodes::Type>>,
}

pub fn missing<'db>(db: &'db dyn Database) -> TypeRefId<'db> {
    TypeRefId::new(db, TypeRefKind::Error, None)
}

impl<'db> TypeRefId<'db> {
    pub fn missing(db: &'db dyn Database) -> Self {
        missing(db)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, salsa::Update)]
pub enum TypeRefKind<'db> {
    Elementary(ElementaryTypeRef),
    Function {
        arguments: Vec<VariableDeclaration<'db>>,
        visibility: Visibility,
        mutability: StateMutability,
        returns: Vec<VariableDeclaration<'db>>,
    },
    Mapping {
        key_type: TypeRefId<'db>,
        key_name: Option<Ident<'db>>,
        value_type: TypeRefId<'db>,
        value_name: Option<Ident<'db>>,
    },
    Path(Vec<Ident<'db>>),
    Array {
        ty: TypeRefId<'db>,
        len: Option<ExprId<'db>>,
    },
    Error,
}

pub fn walk_type_ref<'db>(
    t: TypeRefId<'db>,
    db: &'db dyn Database,
    expr_fn: &mut impl FnMut(ExprId<'db>),
) {
    match t.kind(db) {
        TypeRefKind::Elementary(_) => {}
        TypeRefKind::Function { arguments, visibility: _, mutability: _, returns } => {
            for a in arguments {
                walk_type_ref(a.ty(db), db, expr_fn);
            }
            for a in returns {
                walk_type_ref(a.ty(db), db, expr_fn);
            }
        }
        TypeRefKind::Mapping { key_type, key_name: _, value_type, value_name: _ } => {
            walk_type_ref(key_type, db, expr_fn);
            walk_type_ref(value_type, db, expr_fn);
        }
        TypeRefKind::Path(_) => {}
        TypeRefKind::Array { ty, len } => {
            walk_type_ref(ty, db, expr_fn);
            if let Some(len) = len {
                len.walk(db, expr_fn);
            }
        }
        TypeRefKind::Error => {}
    }
}

impl HirPrint for TypeRefId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        match self.kind(db) {
            TypeRefKind::Elementary(t) => {
                t.write(db, w, ident)?;
            }
            TypeRefKind::Function { arguments, visibility, mutability, returns } => {
                write!(w, "function(")?;
                for (i, a) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    a.write(db, w, ident)?;
                }
                write!(w, ")")?;
                write!(w, "{visibility}{mutability} returns(")?;

                for (i, a) in returns.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    a.write(db, w, ident)?;
                }
            }
            TypeRefKind::Mapping { key_type, key_name, value_type, value_name } => {
                write!(w, "mapping(")?;
                key_type.write(db, w, ident)?;
                if let Some(n) = key_name {
                    write!(w, " {}", n.data(db))?
                }
                write!(w, " => ")?;
                value_type.write(db, w, ident)?;
                if let Some(n) = value_name {
                    write!(w, " {}", n.data(db))?
                }
                write!(w, ")")?;
            }
            TypeRefKind::Path(p) => {
                for (i, p) in p.iter().enumerate() {
                    if i > 0 {
                        write!(w, ".")?;
                    }

                    write!(w, "{}", p.data(db))?
                }
            }
            TypeRefKind::Array { ty, len } => {
                ty.write(db, w, ident)?;
                write!(w, "[")?;
                if let Some(len) = len {
                    len.write(db, w, ident)?;
                }
                write!(w, "]")?;
            }
            TypeRefKind::Error => {
                write!(w, "<error>")?;
            }
        }
        Ok(())
    }
}
