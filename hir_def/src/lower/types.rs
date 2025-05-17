use crate::hir::ElementaryTypeRef;
use crate::hir::Ident;
use crate::hir::StateMutability;
use crate::hir::Visibility;
use crate::lower::LowerCtx;
use crate::TypeRefId;
use crate::TypeRefKind;
use rowan::ast::AstNode;
use rowan::ast::AstPtr;
use std::str::FromStr;
use syntax::ast::nodes;
use syntax::ast::nodes::{ElementaryType, FunctionType};
use syntax::T;

impl<'db> LowerCtx<'db> {
    pub fn lower_type_ref2(&mut self, ty: Option<nodes::Type>) -> TypeRefId<'db> {
        ty.map(|ty| self.lower_type_ref(ty)).unwrap_or(self.missing_typeref)
    }

    pub fn lower_type_ref(&mut self, ty: nodes::Type) -> TypeRefId<'db> {
        let node = crate::FileAstPtr::new(self.file, &ty);
        TypeRefId::new(
            self.db,
            match ty {
                nodes::Type::ElementaryType(t) => {
                    if let Some(e) =
                        self.lower_elementary_name(t).map(|t| TypeRefKind::Elementary(t))
                    {
                        e
                    } else {
                        return self.missing_typeref;
                    }
                }
                nodes::Type::FunctionType(t) => self.lower_function_name(t),
                nodes::Type::MappingType(t) => self.lower_mapping_name(t),
                nodes::Type::IdentPathType(t) => self.lower_ident_type_name(t),
                nodes::Type::ArrayType(t) => self.lower_array_name(t),
            },
            Some(node),
        )
    }

    fn lower_array_name(&mut self, p: nodes::ArrayType) -> TypeRefKind<'db> {
        let ty = self.lower_type_ref2(p.ty());
        let expr = p.len().map(|e| self.lower_expr(e));
        TypeRefKind::Array { ty: ty, len: expr }
    }

    fn lower_ident_type_name(&mut self, p: nodes::IdentPathType) -> TypeRefKind<'db> {
        let mut res: Vec<Ident> =
            p.segment().map(|a| Ident::from_name_ref(self.db, Some(a))).collect();
        res.shrink_to_fit();
        TypeRefKind::Path(res)
    }

    fn lower_mapping_name(&mut self, p: nodes::MappingType) -> TypeRefKind<'db> {
        let key = p
            .key()
            .map(|k| {
                let ty = self.lower_type_ref2(k.ty());
                (ty, Ident::from_name_opt(self.db, k.name()))
            })
            .unwrap_or((self.missing_typeref, None));
        let val = p
            .val()
            .map(|k| {
                let ty = self.lower_type_ref2(k.ty());
                (ty, Ident::from_name_opt(self.db, k.name()))
            })
            .unwrap_or((self.missing_typeref, None));

        TypeRefKind::Mapping {
            key_type: key.0,
            key_name: key.1,
            value_type: val.0,
            value_name: val.1,
        }
    }
    fn lower_function_name(&mut self, ty: FunctionType) -> TypeRefKind<'db> {
        let args = ty
            .parameter_list()
            .map(|list| list.variable_declarations().map(|a| self.lower_parameter(a)).collect())
            .unwrap_or_default();
        let returns = ty
            .returns()
            .and_then(|r| r.parameter_list())
            .map(|list| list.variable_declarations().map(|a| self.lower_parameter(a)).collect())
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

        TypeRefKind::Function { arguments: args, visibility, mutability, returns }
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
