use base_db::{BaseDb, Project};
use hir_def::Item;

use crate::{
    resolver::resolve_item_signature,
    tys::{Ty, TyKind, TyKindInterned},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct Callable<'db> {
    pub args: TyKindInterned<'db>,
    pub returns: TyKindInterned<'db>,
}

impl<'db> Callable<'db> {
    pub fn try_from_ty(db: &'db dyn BaseDb, project: Project, t: Ty<'db>) -> Option<Self> {
        match t.kind(db) {
            TyKind::Callable(callable) => Some(callable),
            TyKind::Function(function_id) => {
                Self::try_from_item(db, project, Item::Function(function_id))
            }
            TyKind::Modifier(modifier_id) => {
                Self::try_from_item(db, project, Item::Modifier(modifier_id))
            }
            TyKind::ItemRef(item) => Self::try_from_item(db, project, item),
            _ => None,
        }
    }
    pub fn try_from_item(db: &'db dyn BaseDb, project: Project, item: Item<'db>) -> Option<Self> {
        Some(match item {
            Item::Contract(contract_id) => Self {
                args: TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Address { payable: false }),
                ),
                returns: TyKindInterned::new(db, TyKind::Contract(contract_id)),
            },
            Item::Enum(enumeration_id) => Self {
                args: TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 256,
                    }),
                ),
                returns: TyKindInterned::new(db, TyKind::Enum(enumeration_id)),
            },
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                let type_res = resolve_item_signature(db, project, item);
                let basic_ty = type_res.type_ref(db, user_defined_value_type_id.ty(db));
                Self {
                    args: basic_ty,
                    returns: TyKindInterned::new(
                        db,
                        TyKind::UserDefinedValueType(user_defined_value_type_id),
                    ),
                }
            }
            Item::Error(error_id) => {
                let type_res = resolve_item_signature(db, project, item);
                let params = error_id
                    .parameters(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.info(db).ty)))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: TyKindInterned::new(db, TyKind::Error(error_id)),
                }
            }
            Item::Event(event_id) => {
                let type_res = resolve_item_signature(db, project, item);
                let params = event_id
                    .parameters(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.info(db).ty)))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: TyKindInterned::new(db, TyKind::Event(event_id)),
                }
            }
            Item::Function(function_id) => {
                let type_res = resolve_item_signature(db, project, item);
                let info = function_id.info(db);
                let params = info
                    .args
                    .into_iter()
                    .map(|p| Ty::new_in(type_res.type_ref(db, p.ty(db)), p.location(db).into()))
                    .collect();
                let returns = info
                    .returns
                    .into_iter()
                    .flatten()
                    .map(|p| Ty::new_in(type_res.type_ref(db, p.ty(db)), p.location(db).into()))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: TyKindInterned::new(db, TyKind::Tuple(returns)),
                }
            }
            Item::Struct(structure_id) => {
                let type_res = resolve_item_signature(db, project, item);
                let params = structure_id
                    .fields(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();
                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: TyKindInterned::new(db, TyKind::Struct(structure_id)),
                }
            }
            Item::Constructor(constructor_id) => {
                let type_res = resolve_item_signature(db, project, item);
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
                    returns: TyKindInterned::new(db, contract),
                }
            }
            Item::Modifier(modifier_id) => {
                let type_res = resolve_item_signature(db, project, item);
                let params = modifier_id
                    .info(db)
                    .args
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();

                Self {
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: TyKindInterned::new(db, TyKind::Unknown),
                }
            }
            _ => return None,
        })
    }
}
