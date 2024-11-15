use crate::hir::argument::ArgumentId;
use crate::hir::expr::ExprId;
use crate::hir::ident::Ident;
use crate::hir::{StateMutability, Visibility};
use crate::item_tree::print::HirPrint;
use salsa::Database;
use std::fmt::{Debug, Display, Formatter, Write};

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
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
                    write!(w, "address")
                } else {
                    write!(w, "address payable")
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

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum TypeRef<'db> {
    Elementary(ElementaryTypeRef),
    Function {
        arguments: Vec<ArgumentId<'db>>,
        visibility: Visibility,
        mutability: StateMutability,
        returns: Vec<ArgumentId<'db>>,
    },
    Mapping {
        key_type: Box<TypeRef<'db>>,
        key_name: Option<Ident<'db>>,
        value_type: Box<TypeRef<'db>>,
        value_name: Option<Ident<'db>>,
    },
    Path(Vec<Ident<'db>>),
    Array {
        ty: Box<TypeRef<'db>>,
        len: Option<ExprId<'db>>,
    },
    Error,
}

impl HirPrint for TypeRef<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        match self {
            TypeRef::Elementary(t) => {
                t.write(db, w, ident)?;
            }
            TypeRef::Function { arguments, visibility, mutability, returns } => {
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
            TypeRef::Mapping { key_type, key_name, value_type, value_name } => {
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
            TypeRef::Path(p) => {
                for (i, p) in p.iter().enumerate() {
                    if i > 0 {
                        write!(w, ".")?;
                    }

                    write!(w, "{}", p.data(db))?
                }
            }
            TypeRef::Array { ty, len } => {
                ty.write(db, w, ident)?;
                write!(w, "[")?;
                if let Some(len) = len {
                    len.write(db, w, ident)?;
                }
                write!(w, "]")?;
            }
            TypeRef::Error => {
                write!(w, "<error>")?;
            }
        }
        Ok(())
    }
}
