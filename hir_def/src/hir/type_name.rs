use crate::hir::argument::ArgumentId;
use crate::hir::expr::ExprId;
use crate::hir::ident::Ident;
use crate::hir::{StateMutability, Visibility};
use crate::items::HirPrint;
use salsa::Database;
use std::fmt::{Debug, Display, Formatter, Write};
use crate::lazy_field;

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

pub fn walk_type_ref<'db>(
    t: &TypeRef<'db>,
    db: &'db dyn Database,
    expr_fn: &mut impl FnMut(ExprId<'db>),
) {
    match t {
        TypeRef::Elementary(_) => {}
        TypeRef::Function { arguments, visibility: _, mutability: _, returns } => {
            for a in arguments {
                walk_type_ref(&a.ty(db), db, expr_fn);
            }
            for a in returns {
                walk_type_ref(&a.ty(db), db, expr_fn);
            }
        }
        TypeRef::Mapping { key_type, key_name: _, value_type, value_name: _ } => {
            walk_type_ref(key_type, db, expr_fn);
            walk_type_ref(value_type, db, expr_fn);
        }
        TypeRef::Path(_) => {}
        TypeRef::Array { ty, len } => {
            walk_type_ref(ty, db, expr_fn);
            if let Some(len) = len {
                len.walk(db, expr_fn);
            }
        }
        TypeRef::Error => {}
    }
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