use base_db::BaseDb;
use hir_def::{Ident, Item};
use hir_nameres::scope::body::MagicDefinitionKind;
use smallvec::{smallvec, SmallVec};

use crate::{
    resolver::resolve_item_signature,
    tys::{Ty, TyKind, TyKindInterned},
};

#[derive(Debug, Clone, PartialEq, Ord, PartialOrd, Eq, Hash, salsa::Update)]
pub enum CallableType<'db> {
    ExtensionFn { this: Ty<'db> },
    Plain,
}

#[derive(Debug, Clone, PartialEq, Ord, PartialOrd, Eq, Hash, salsa::Update)]
pub struct Callable<'db> {
    pub args: SmallVec<[Ty<'db>; 4]>,
    // Function can accept any arguments, for abi module
    pub any_args: bool,
    pub returns: SmallVec<[Ty<'db>; 2]>,
}

impl<'db> Callable<'db> {
    pub fn return_ty(self, db: &'db dyn BaseDb) -> Ty<'db> {
        if self.returns.len() == 1 {
            return self.returns[0];
        }

        Ty::new_intern(db, TyKind::Tuple(self.returns))
    }

    pub fn try_from_magic(db: &'db dyn BaseDb, t: MagicDefinitionKind) -> Option<Self> {
        let (args, returns) = match t {
            MagicDefinitionKind::Keccak256 => (
                smallvec![Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Bytes)
                )],
                smallvec![Ty::new(TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::FixedBytes { size: 32 })
                ))],
            ),
            MagicDefinitionKind::Sha256 => (
                smallvec![Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Bytes)
                )],
                smallvec![Ty::new(TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::FixedBytes { size: 32 })
                ))],
            ),
            MagicDefinitionKind::Gasleft => (
                smallvec![],
                smallvec![Ty::new(TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 256
                    })
                ))],
            ),
            MagicDefinitionKind::Assert => (
                smallvec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bool))],
                smallvec![],
            ),
            MagicDefinitionKind::Require => (
                smallvec![Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bool))],
                smallvec![],
            ),
            MagicDefinitionKind::RequireWithMessage => (
                smallvec![
                    Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::Bool)),
                    Ty::new_intern(db, TyKind::Elementary(hir_def::ElementaryTypeRef::String))
                ],
                smallvec![],
            ),
            MagicDefinitionKind::Revert => (smallvec![], smallvec![]),
            MagicDefinitionKind::RevertWithMessage => (
                smallvec![Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::String)
                ),],
                smallvec![],
            ),
            MagicDefinitionKind::AddMod => {
                let uint = Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 256,
                    }),
                );
                (smallvec![uint, uint, uint], smallvec![uint])
            }
            MagicDefinitionKind::MulMod => {
                let uint = Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 256,
                    }),
                );
                (smallvec![uint, uint, uint], smallvec![uint])
            }
            MagicDefinitionKind::Ripemd160 => (
                smallvec![Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Bytes)
                )],
                smallvec![Ty::new(TyKindInterned::new(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 160
                    })
                ))],
            ),
            MagicDefinitionKind::Ecrecover => {
                let bytes32 = Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::FixedBytes { size: 32 }),
                );
                let uint8 = Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 8,
                    }),
                );
                (
                    smallvec![bytes32, uint8, bytes32, bytes32],
                    smallvec![Ty::new(TyKindInterned::new(
                        db,
                        TyKind::Elementary(hir_def::ElementaryTypeRef::Address { payable: false })
                    ))],
                )
            }
            _ => return None,
        };

        return Some(Callable { args, any_args: false, returns });
    }

    pub fn try_from_ty(db: &'db dyn BaseDb, t: Ty<'db>) -> Option<Self> {
        match t.kind(db) {
            TyKind::Callable(callable) => Some(callable),
            TyKind::Modifier(callable) => Some(callable),
            TyKind::ItemRef(item) => Self::try_from_item(db, item),
            _ => None,
        }
    }

    pub fn try_from_item(db: &'db dyn BaseDb, item: Item<'db>) -> Option<Self> {
        Some(match item {
            Item::Contract(contract_id) => Self {
                any_args: false,
                args: smallvec![Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Address { payable: false })
                )],
                returns: smallvec![Ty::new_intern(db, TyKind::Contract(contract_id),)],
            },
            Item::Enum(enumeration_id) => Self {
                any_args: false,
                args: smallvec![Ty::new_intern(
                    db,
                    TyKind::Elementary(hir_def::ElementaryTypeRef::Integer {
                        signed: false,
                        size: 256,
                    })
                )],
                returns: smallvec![Ty::new_intern(db, TyKind::Enum(enumeration_id),)],
            },
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                let type_res = resolve_item_signature(db, item);
                let basic_ty = type_res.type_ref(db, user_defined_value_type_id.ty(db));
                Self {
                    any_args: false,
                    args: smallvec![Ty::new(basic_ty)],
                    returns: smallvec![Ty::new_intern(
                        db,
                        TyKind::UserDefinedValueType(user_defined_value_type_id),
                    )],
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
                    any_args: false,
                    args: params,
                    returns: smallvec![Ty::new_intern(db, TyKind::Error)],
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
                    any_args: false,
                    args: params,
                    returns: smallvec![Ty::new_intern(db, TyKind::Event)],
                }
            }
            Item::Function(function_id) => {
                let type_res = resolve_item_signature(db, item);
                let info = function_id.info(db);
                let args = info
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

                Self { any_args: false, args, returns }
            }
            Item::Struct(structure_id) => {
                let type_res = resolve_item_signature(db, item);
                let params = structure_id
                    .fields(db)
                    .into_iter()
                    .map(|p| Ty::new(type_res.type_ref(db, p.ty(db))))
                    .collect();
                Self {
                    any_args: false,
                    args: params,
                    returns: smallvec![Ty::new_intern(db, TyKind::Struct(structure_id))],
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
                    any_args: false,
                    args: params,
                    returns: smallvec![Ty::new_intern(db, contract)],
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
                    any_args: false,
                    args: params,
                    returns: smallvec![Ty::new_intern(db, TyKind::Unknown)],
                }
            }
            _ => return None,
        })
    }
}
