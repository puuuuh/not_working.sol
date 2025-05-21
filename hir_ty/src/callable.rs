use base_db::BaseDb;
use hir_def::Item;

use crate::{
    resolver::resolve_item_signature,
    tys::{Ty, TyKind, TyKindInterned},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct Callable<'db> {
    pub args: TyKindInterned<'db>,
    pub returns: Ty<'db>,
}

impl<'db> Callable<'db> {
    pub fn try_from_ty(db: &'db dyn BaseDb, t: Ty<'db>) -> Option<Self> {
        match t.kind(db) {
            TyKind::Function(callable) => Some(callable),
            TyKind::Modifier(callable) => Some(callable),
            TyKind::Type(item) => Self::try_from_item(db, item),
            _ => None,
        }
    }
    pub fn try_from_item(db: &'db dyn BaseDb, item: Item<'db>) -> Option<Self> {
        Some(match item {
            Item::Contract(contract_id) => Self {
                args: TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Address { payable: false }),
                ),
                returns: Ty::new_intern(db, TyKind::Contract(contract_id)),
            },
            Item::Enum(enumeration_id) => Self {
                args: TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 256,
                    }),
                ),
                returns: Ty::new_intern(db, TyKind::Enum(enumeration_id)),
            },
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                let type_res = resolve_item_signature(db, item);
                let basic_ty = type_res.type_ref(db, user_defined_value_type_id.ty(db));
                Self {
                    args: basic_ty,
                    returns: Ty::new_intern(
                        db,
                        TyKind::UserDefinedValueType(user_defined_value_type_id),
                    ),
                }
            }
            Item::Error(error_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = error_id
                    .parameters(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.info(db).ty)))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: Ty::new_intern(db, TyKind::Error),
                }
            }
            Item::Event(event_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = event_id
                    .parameters(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.info(db).ty)))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: Ty::new_intern(db, TyKind::Event),
                }
            }
            Item::Function(function_id) => {
                let type_res = resolve_item_signature(db, item);
                let info = function_id.info(db);
                let params = info
                    .args
                    .into_iter()
                    .map(|p| Ty::new_in(type_res.type_ref(db, p.ty(db)), p.location(db).into()))
                    .collect();

                let args = TyKindInterned::new(db, TyKind::Tuple(params));
                if let Some(r) = &info.returns {
                    if r.len() == 1 {
                        return Some(Self {
                            args,
                            returns: Ty::new_in(
                                type_res.type_ref(db, r[0].ty(db)),
                                r[0].location(db).into(),
                            ),
                        });
                    }
                }
                let returns = info
                    .returns
                    .into_iter()
                    .flatten()
                    .map(|p| Ty::new_in(type_res.type_ref(db, p.ty(db)), p.location(db).into()))
                    .collect();

                Self { args, returns: Ty::new_intern(db, TyKind::Tuple(returns)) }
            }
            Item::Struct(structure_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = structure_id
                    .fields(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: Ty::new_intern(db, TyKind::Struct(structure_id)),
                }
            }
            Item::Constructor(constructor_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = constructor_id
                    .info(db)
                    .args
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();

                let contract = match constructor_id.origin(db) {
                    Some(c) => TyKind::Contract(c),
                    None => TyKind::Unknown,
                };
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: Ty::new_intern(db, contract),
                }
            }
            Item::Modifier(modifier_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = modifier_id
                    .info(db)
                    .args
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();

                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: Ty::new_intern(db, TyKind::Unknown),
                }
            }
            _ => return None,
        })
    }
}
