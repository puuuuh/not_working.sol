use core::error;

use base_db::{BaseDb, File};

use hir_def::{
    hir::{
        ContractId, ElementaryTypeRef, EnumerationId, ErrorId, EventId, FunctionId, Ident, Item,
        ModifierId, SourceUnit, StructureId,
    },
    DataLocation,
};

use hir_nameres::{container::Container, scope::body::Definition};

use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum TyKind<'db> {
    Unknown,
    Function(TyKindInterned<'db>, TyKindInterned<'db>),
    Modifier(ModifierId<'db>),
    Struct(StructureId<'db>),
    Event(EventId<'db>),
    Error(ErrorId<'db>),
    Enum(EnumerationId<'db>),
    Contract(ContractId<'db>),
    Elementary(ElementaryTypeRef),
    Array(TyKindInterned<'db>, usize),
    Mapping(TyKindInterned<'db>, TyKindInterned<'db>),
    Tuple(Vec<Ty<'db>>),
    // Reference to an item without value (ContractName.Struct, for example)
    ItemRef(Item<'db>),
}

#[salsa::interned(debug)]
pub struct TyKindInterned<'db> {
    pub data: TyKind<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct Ty<'db> {
    pub ty_kind: TyKindInterned<'db>,
    pub location: Option<DataLocation>,
}

impl<'db> TyKind<'db> {
    pub fn can_coerce(self, db: &'db dyn BaseDb, dst: TyKind<'db>) -> bool {
        match (self, dst) {
            (TyKind::Elementary(src), TyKind::Elementary(dst)) => match src {
                hir_def::ElementaryTypeRef::Address { payable: false } => {
                    return matches!(dst, hir_def::ElementaryTypeRef::Address { .. })
                }
                hir_def::ElementaryTypeRef::Address { payable: true } => {
                    return matches!(dst, hir_def::ElementaryTypeRef::Address { payable: true })
                }
                hir_def::ElementaryTypeRef::String => {
                    return matches!(
                        dst,
                        hir_def::ElementaryTypeRef::Bytes
                            | hir_def::ElementaryTypeRef::FixedBytes { .. }
                    )
                }
                hir_def::ElementaryTypeRef::Bytes => {
                    return matches!(
                        dst,
                        hir_def::ElementaryTypeRef::String
                            | hir_def::ElementaryTypeRef::FixedBytes { .. }
                    )
                }
                hir_def::ElementaryTypeRef::Integer { signed, size } => {
                    let hir_def::ElementaryTypeRef::Integer { signed: signed1, size: size1 } = dst
                    else {
                        return false;
                    };

                    return signed1 == signed && size1 >= size;
                }
                hir_def::ElementaryTypeRef::FixedBytes { size } => {
                    let hir_def::ElementaryTypeRef::FixedBytes { size: size1 } = dst else {
                        return false;
                    };

                    return size1 >= size;
                }
                hir_def::ElementaryTypeRef::Fixed { signed, size, decimal_points } => {
                    return false;
                }
                hir_def::ElementaryTypeRef::Bool => {
                    return false;
                }
                hir_def::ElementaryTypeRef::Unknown => {
                    return false;
                }
            },
            (TyKind::Tuple(src_tuple), TyKind::Tuple(dst_tuple)) => {
                (src_tuple.len() == dst_tuple.len())
                    && src_tuple.iter().zip(dst_tuple).all(|(src, dst)| src.can_coerce(db, dst))
            }
            _ => false,
        }
    }

    pub fn human_readable(self, db: &'db dyn BaseDb) -> String {
        let mut res = String::new();
        self.human_readable_to(db, &mut res);

        res
    }

    pub fn human_readable_to(self, db: &'db dyn BaseDb, res: &mut String) {
        match self {
            TyKind::Unknown => {
                *res += "{unknown}";
            }
            TyKind::Function(args, returns) => {
                let mut need_comma = false;
                *res += "function";
                args.data(db).human_readable_to(db, res);

                *res += " returns";
                returns.data(db).human_readable_to(db, res);
            }
            TyKind::Struct(structure_id) => {
                *res += structure_id.name(db).data(db);
            }
            TyKind::Enum(enumeration_id) => {
                *res += enumeration_id.name(db).data(db);
            }
            TyKind::Contract(contract_id) => {
                *res += contract_id.name(db).data(db);
            }
            TyKind::Error(error_id) => {
                *res += error_id.name(db).data(db);
            }
            TyKind::Modifier(modifier_id) => {
                *res += modifier_id.name(db).data(db);
            }
            TyKind::Event(event_id) => {
                *res += event_id.name(db).data(db);
            }
            TyKind::Elementary(elementary_type_ref) => match elementary_type_ref {
                ElementaryTypeRef::Address { payable } => {
                    *res += "address";
                    if payable {
                        *res += " payable"
                    }
                }
                ElementaryTypeRef::Bool => {
                    *res += "bool";
                }
                ElementaryTypeRef::String => {
                    *res += "string";
                }
                ElementaryTypeRef::Bytes => {
                    *res += "bytes";
                }
                ElementaryTypeRef::Integer { signed, size } => {
                    write!(res, "{}int{}", if signed { "" } else { "u" }, size);
                }
                ElementaryTypeRef::FixedBytes { size } => {
                    write!(res, "bytes{}", size);
                }
                ElementaryTypeRef::Fixed { signed, size, decimal_points } => {
                    write!(
                        res,
                        "{}fixed{}x{}",
                        if signed { "" } else { "u" },
                        size,
                        decimal_points
                    );
                }
                ElementaryTypeRef::Unknown => {
                    *res += "unknown";
                }
            },
            TyKind::Array(ty, _) => {
                ty.data(db).human_readable_to(db, res);
                *res += "[]";
            }
            TyKind::Mapping(ty, ty1) => {
                *res += "mapping(";
                ty.data(db).human_readable_to(db, res);
                *res += " => ";
                ty1.data(db).human_readable_to(db, res);
                *res += ")";
            }
            TyKind::Tuple(items) => {
                *res += "(";
                for i in items {
                    i.human_readable_to(db, res);
                    *res += ",";
                }
                *res += ")";
            }
            TyKind::ItemRef(item) => {
                *res += "ItemRef(";
                *res += item.name(db).map(|item| item.data(db).as_str()).unwrap_or("{unnamed}");
                *res += ")";
            }
        }
    }
}

impl<'db> Ty<'db> {
    pub fn new(db: &'db dyn BaseDb, kind: TyKind<'db>, location: Option<DataLocation>) -> Self {
        Self { ty_kind: TyKindInterned::new(db, kind), location }
    }

    pub fn new_interned(
        db: &'db dyn BaseDb,
        kind: TyKindInterned<'db>,
        location: Option<DataLocation>,
    ) -> Self {
        Self { ty_kind: kind, location }
    }

    pub fn kind(self, db: &'db dyn BaseDb) -> TyKind<'db> {
        self.ty_kind.data(db)
    }

    pub fn component_count(self, db: &'db dyn BaseDb) -> u32 {
        match self.kind(db) {
            TyKind::Tuple(items) => return items.len() as _,
            _ => 1,
        }
    }

    pub fn human_readable(self, db: &'db dyn BaseDb) -> String {
        let mut res = String::new();
        self.human_readable_to(db, &mut res);
        res
    }

    pub fn human_readable_to(self, db: &'db dyn BaseDb, res: &mut String) {
        self.kind(db).human_readable_to(db, res);

        if let Some(location) = self.location {
            write!(res, " {}", location);
        }
    }

    pub fn is_unknown(self, db: &'db dyn BaseDb) -> bool {
        self.ty_kind == unknown(db)
    }

    pub fn container(self, db: &'db dyn BaseDb) -> Option<Container<'db>> {
        Some(match self.kind(db) {
            TyKind::Struct(structure_id) => Container::Structure(structure_id),
            TyKind::Enum(enumeration_id) => Container::Enum(enumeration_id),
            TyKind::Contract(contract_id) => Container::Contract(contract_id),
            TyKind::ItemRef(item) => Container::Item(item),

            _ => return None,
        })
    }

    pub fn can_coerce(mut self, db: &'db dyn BaseDb, mut dst: Ty<'db>) -> bool {
        // Tuple unwrapping
        // TODO: Add limit or just convert any tuples with len == 1 to inner type somewhere else
        while let TyKind::Tuple(t) = self.kind(db) {
            if t.len() == 1 {
                self = t[0]
            } else {
                break;
            }
        }
        while let TyKind::Tuple(t) = dst.kind(db) {
            if t.len() == 1 {
                self = t[0]
            } else {
                break;
            }
        }

        if self == dst {
            return true;
        }

        self.kind(db).can_coerce(db, dst.kind(db))
    }
}

#[salsa::tracked]
pub fn unknown<'db>(db: &'db dyn BaseDb) -> TyKindInterned<'db> {
    TyKindInterned::new(db, TyKind::Unknown)
}

#[derive(Debug, PartialEq, Eq, salsa::Update, Clone, Copy)]
pub struct ElementaryTypes<'db> {
    pub address: Ty<'db>,
    pub payable_address: Ty<'db>,
    pub bool: Ty<'db>,
    pub string: Ty<'db>,
    pub bytes: Ty<'db>,
    pub int8: Ty<'db>,
    pub int16: Ty<'db>,
    pub int32: Ty<'db>,
    pub int64: Ty<'db>,
    pub int128: Ty<'db>,
    pub int256: Ty<'db>,
    pub uint8: Ty<'db>,
    pub uint16: Ty<'db>,
    pub uint32: Ty<'db>,
    pub uint64: Ty<'db>,
    pub uint128: Ty<'db>,
    pub uint256: Ty<'db>,
    pub unknown: Ty<'db>,
}

#[salsa::tracked]
pub fn common_types<'db>(db: &'db dyn BaseDb) -> ElementaryTypes<'db> {
    ElementaryTypes {
        address: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Address { payable: false }),
            None,
        ),
        payable_address: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Address { payable: true }),
            None,
        ),
        bool: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Bool), None),
        string: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::String), None),
        bytes: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Bytes), None),
        int8: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 8 }),
            None,
        ),
        int16: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 16 }),
            None,
        ),
        int32: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 32 }),
            None,
        ),
        int64: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 64 }),
            None,
        ),
        int128: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 128 }),
            None,
        ),
        int256: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 256 }),
            None,
        ),
        uint8: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 8 }),
            None,
        ),
        uint16: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 16 }),
            None,
        ),
        uint32: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 32 }),
            None,
        ),
        uint64: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 64 }),
            None,
        ),
        uint128: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 128 }),
            None,
        ),
        uint256: Ty::new(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
            None,
        ),
        unknown: Ty::new(db, TyKind::Unknown, None),
    }
}
