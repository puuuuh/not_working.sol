use base_db::BaseDb;
use hir_def::Item;
use hir_nameres::scope::body::MagicDefinitionKind;

use crate::{
    resolver::resolve_item_signature,
    tys::{Ty, TyKind, TyKindInterned},
};

#[derive(Debug, Clone, PartialEq, Ord, PartialOrd, Eq, Hash, salsa::Update)]
pub struct Callable<'db> {
    pub args: TyKindInterned<'db>,
    // last arg can be repeated many times
    pub variadic: bool,
    pub returns: Ty<'db>,
}

impl<'db> Callable<'db> {
    pub fn try_from_magic(db: &'db dyn BaseDb, t: MagicDefinitionKind) -> Option<Self> {
        let (args, ret) = match t {
            MagicDefinitionKind::Keccak256 => {
                (
                    TyKindInterned::tuple(db, vec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bytes))]),
                    Ty::new(TyKindInterned::new(db, TyKind::Elementary(hir_def::ElementaryTypeRef::FixedBytes { size: 32 })))
                )
            },
            MagicDefinitionKind::Sha256 => {
                (
                    TyKindInterned::tuple(db, vec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bytes))]),
                    Ty::new(TyKindInterned::new(db, TyKind::Elementary(hir_def::ElementaryTypeRef::FixedBytes { size: 32 })))
                )
            },
            MagicDefinitionKind::Gasleft => {
                (
                    TyKindInterned::tuple(db, vec![]),
                    Ty::new(TyKindInterned::new(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Integer { signed: false, size: 256 })))
                )
            },
            MagicDefinitionKind::Assert => {
                (
                    TyKindInterned::tuple(db, vec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bool))]),
                    Ty::new(TyKindInterned::new(db, TyKind::Tuple(vec![])))
                )
            },
            MagicDefinitionKind::Require => {
                (
                    TyKindInterned::tuple(db, vec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bool))]),
                    Ty::new(TyKindInterned::new(db, TyKind::Tuple(vec![])))
                )
            },
            MagicDefinitionKind::RequireWithMessage =>  {
                (
                    TyKindInterned::tuple(db, vec![
                        Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bool)),
                        Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::String))
                    ]),
                    Ty::new(TyKindInterned::new(db, TyKind::Tuple(vec![])))
                )
            },
            MagicDefinitionKind::Revert => {
                (
                    TyKindInterned::tuple(db, vec![]),
                    Ty::new(TyKindInterned::new(db, TyKind::Tuple(vec![])))
                )
            },
            MagicDefinitionKind::RevertWithMessage => {
                (
                    TyKindInterned::tuple(db, vec![
                        Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::String)),
                    ]),
                    Ty::new(TyKindInterned::new(db, TyKind::Tuple(vec![])))
                )
            },
            MagicDefinitionKind::AddMod => {
                let uint = Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Integer { signed: false, size: 256 }));
                (
                    TyKindInterned::tuple(db, vec![
                        uint,
                        uint,
                        uint
                    ]),
                    uint
                )
            },
            MagicDefinitionKind::MulMod => {
                let uint = Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Integer { signed: false, size: 256 }));
                (
                    TyKindInterned::tuple(db, vec![
                        uint,
                        uint,
                        uint
                    ]),
                    uint
                )
            },
            MagicDefinitionKind::Ripemd160 => {
                (
                    TyKindInterned::tuple(db, vec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bytes))]),
                    Ty::new(TyKindInterned::new(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Integer { signed: false, size: 160 })))
                )
            },
            MagicDefinitionKind::Ecrecover =>  {
                let bytes32 = Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::FixedBytes { size: 32 }));
                let uint8 = Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Integer { signed: false, size: 8 }));
                (
                    TyKindInterned::tuple(db, vec![bytes32, uint8, bytes32, bytes32]),
                    Ty::new(TyKindInterned::new(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Address { payable: false })))
                )
            }
            _ => return None
        };

        return Some(Callable {
            args,
            variadic: false,
            returns: ret,
        })
    }
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
                variadic: false,
                args: TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Address { payable: false }),
                ),
                returns: Ty::new_intern(db, TyKind::Contract(contract_id)),
            },
            Item::Enum(enumeration_id) => Self {
                variadic: false,
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
                    variadic: false,
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
                    variadic: false,
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
                    variadic: false,
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
                            variadic: false,
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

                Self { variadic: false, args, returns: Ty::new_intern(db, TyKind::Tuple(returns)) }
            }
            Item::Struct(structure_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = structure_id
                    .fields(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();
                Self {
                    variadic: false, 
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
                    variadic: false, 
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
                    variadic: false, 
                    args: TyKindInterned::new(db, TyKind::Tuple(params)),
                    returns: Ty::new_intern(db, TyKind::Unknown),
                }
            }
            _ => return None,
        })
    }
}
