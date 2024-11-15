use crate::hir::ident::Ident;
use crate::hir::type_name::{ElementaryTypeRef, TypeRef};
use crate::hir::{StateMutability, Visibility};
use crate::lower::Ctx;
use rowan::ast::AstNode;
use std::str::FromStr;
use syntax::ast::nodes;
use syntax::ast::nodes::{ElementaryType, FunctionType};
use syntax::T;

impl<'db> Ctx<'db> {
    pub fn lower_type_ref(&mut self, ty: nodes::Type) -> TypeRef<'db> {
        match ty {
            nodes::Type::ElementaryType(t) => {
                self.lower_elementary_name(t).map(TypeRef::Elementary).unwrap_or(TypeRef::Error)
            }
            nodes::Type::FunctionType(t) => self.lower_function_name(t),
            nodes::Type::MappingType(t) => self.lower_mapping_name(t),
            nodes::Type::IdentPathType(t) => self.lower_ident_type_name(t),
            nodes::Type::ArrayType(t) => self.lower_array_name(t),
        }
    }

    fn lower_array_name(&mut self, p: nodes::ArrayType) -> TypeRef<'db> {
        let ty = p.ty().map(|ty| self.lower_type_ref(ty)).unwrap_or(TypeRef::Error);
        let expr = p.len().map(|e| self.lower_expr(e));
        TypeRef::Array { ty: Box::new(ty), len: expr }
    }

    fn lower_ident_type_name(&mut self, p: nodes::IdentPathType) -> TypeRef<'db> {
        let mut res: Vec<Ident> =
            p.segment().map(|a| Ident::from_name_ref(self.db.as_dyn_database(), Some(a))).collect();
        res.shrink_to_fit();
        TypeRef::Path(res)
    }

    fn lower_mapping_name(&mut self, p: nodes::MappingType) -> TypeRef<'db> {
        let key = p
            .key()
            .map(|k| {
                let ty = k.ty().map(|ty| self.lower_type_ref(ty)).unwrap_or(TypeRef::Error);
                (ty, Ident::from_name_opt(self.db.as_dyn_database(), k.name()))
            })
            .unwrap_or((TypeRef::Error, None));
        let val = p
            .val()
            .map(|k| {
                let ty = k.ty().map(|ty| self.lower_type_ref(ty)).unwrap_or(TypeRef::Error);
                (ty, Ident::from_name_opt(self.db.as_dyn_database(), k.name()))
            })
            .unwrap_or((TypeRef::Error, None));

        TypeRef::Mapping {
            key_type: Box::new(key.0),
            key_name: key.1,
            value_type: Box::new(val.0),
            value_name: val.1,
        }
    }
    fn lower_function_name(&mut self, ty: FunctionType) -> TypeRef<'db> {
        let args = ty
            .parameter_list()
            .map(|list| list.parameters().map(|a| self.lower_parameter(a)).collect())
            .unwrap_or_default();
        let returns = ty
            .returns()
            .and_then(|r| r.parameter_list())
            .map(|list| list.parameters().map(|a| self.lower_parameter(a)).collect())
            .unwrap_or_default();
        let mut visibility = Visibility::Unknown;
        let mut mutability = StateMutability::NonPayable;
        for m in ty.function_modifiers() {
            for t in m.syntax().children_with_tokens() {
                match t.kind() {
                    T![external] => visibility = Visibility::External,
                    T![public] => visibility = Visibility::Public,
                    T![private] => visibility = Visibility::Private,
                    T![internal] => visibility = Visibility::Internal,
                    T![pure] => mutability = StateMutability::Pure,
                    T![view] => mutability = StateMutability::View,
                    T![payable] => mutability = StateMutability::Payable,
                    _ => {}
                }
            }
        }

        TypeRef::Function { arguments: args, visibility, mutability, returns }
    }

    pub fn lower_elementary_name(&mut self, ty: ElementaryType) -> Option<ElementaryTypeRef> {
        ty.elementary_type_ident_token().and_then(|t| {
            let t = t.text();
            let simple_type = match t {
                "bool" => Some(ElementaryTypeRef::Bool),
                "string" => Some(ElementaryTypeRef::String),
                "bytes" => Some(ElementaryTypeRef::Bytes),
                "int" => Some(ElementaryTypeRef::Integer { signed: true, size: 256 }),
                "uint" => Some(ElementaryTypeRef::Integer { signed: false, size: 256 }),
                "fixed" => {
                    Some(ElementaryTypeRef::Fixed { signed: false, size: 128, decimal_points: 18 })
                }
                "ufixed" => {
                    Some(ElementaryTypeRef::Fixed { signed: true, size: 128, decimal_points: 18 })
                }
                "address" => {
                    Some(ElementaryTypeRef::Address { payable: ty.payable_token().is_some() })
                }
                _ => None,
            };
            if simple_type.is_some() {
                return simple_type;
            }

            if let Some(suffix) = t.strip_prefix("bytes") {
                return u16::from_str(suffix)
                    .ok()
                    .map(|size| ElementaryTypeRef::FixedBytes { size });
            }
            let (signed, t) = t.strip_prefix("u").map(|tail| (false, tail)).unwrap_or((true, t));
            if let Some(suffix) = t.strip_prefix("int") {
                return Some(ElementaryTypeRef::Integer {
                    signed,
                    size: u16::from_str(suffix).ok()?,
                });
            }
            if let Some(suffix) = t.strip_prefix("fixed") {
                let (m, n) = suffix.split_once("x")?;
                return Some(ElementaryTypeRef::Fixed {
                    signed,
                    size: u16::from_str(m).ok()?,
                    decimal_points: u8::from_str(n).ok()?,
                });
            }
            None
        })
    }
}
