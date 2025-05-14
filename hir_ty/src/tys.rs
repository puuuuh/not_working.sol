use base_db::{BaseDb, File};

use hir_def::hir::{
    ContractId, ElementaryTypeRef, EnumerationId, ErrorId, EventId, FunctionId, Ident, Item,
    ModifierId, SourceUnit, StructureId,
};

use hir_nameres::{container::Container, scope::body::Definition};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum TyKind<'db> {
    Unknown,
    Function(Ty<'db>, Ty<'db>),
    Modifier(ModifierId<'db>),
    Struct(StructureId<'db>),
    Event(EventId<'db>),
    Error(ErrorId<'db>),
    Enum(EnumerationId<'db>),
    Contract(ContractId<'db>),
    Elementary(ElementaryTypeRef),
    Array(Ty<'db>, usize),
    Mapping(Ty<'db>, Ty<'db>),
    Tuple(Vec<Ty<'db>>),
    // Reference to an item without value (ContractName.Struct, for example)
    ItemRef(Item<'db>),
}

#[salsa::interned(debug)]
pub struct Ty<'db> {
    pub kind: TyKind<'db>,
}

impl<'db> Ty<'db> {
    pub fn component_count(self, db: &'db dyn BaseDb) -> u32 {
        match self.kind(db) {
            TyKind::Tuple(items) => return items.len() as _,
            _ => 1
        }
    }
    pub fn pretty_print(self, db: &'db dyn BaseDb) -> String {
        match self.kind(db) {
            TyKind::Unknown => "{unknown}".to_string(),
            TyKind::Function(ty, ty1) => {
                format!("function({}) -> ({})", ty.pretty_print(db), ty1.pretty_print(db))
            },
            TyKind::Struct(structure_id) => {
                format!("{}", structure_id.name(db).data(db))
            },
            TyKind::Enum(enumeration_id) => {
                format!("{}", enumeration_id.name(db).data(db))
            },
            TyKind::Contract(contract_id) => {
                format!("{}", contract_id.name(db).data(db))
            },
            TyKind::Error(error_id) => {
                format!("{}", error_id.name(db).data(db))
            },
            TyKind::Modifier(modifier_id) => {
                format!("{}", modifier_id.name(db).data(db))
            },
            TyKind::Event(event_id) => {
                format!("{}", event_id.name(db).data(db))
            },
            TyKind::Elementary(elementary_type_ref) => {
                match elementary_type_ref {
                    ElementaryTypeRef::Address { payable } => {
                        format!("address")
                    },
                    ElementaryTypeRef::Bool => {
                        format!("bool")
                    },
                    ElementaryTypeRef::String => {
                        format!("string")
                    },
                    ElementaryTypeRef::Bytes => {
                        format!("bytes")
                    },
                    ElementaryTypeRef::Integer { signed, size } => {
                        format!("{}int{}", if signed { "" } else { "u" }, size)
                    },
                    ElementaryTypeRef::FixedBytes { size } => {
                        format!("bytes{}", size)
                    },
                    ElementaryTypeRef::Fixed { signed, size, decimal_points } => {
                        format!("{}fixed{}x{}", if signed { "" } else { "u" }, size, decimal_points)
                    },
                    ElementaryTypeRef::Unknown => {
                        format!("unknown")
                    },
                }
            },
            TyKind::Array(ty, _) => {
                format!("{}[]", ty.pretty_print(db))
            },
            TyKind::Mapping(ty, ty1) => {
                format!("mapping({} => {})", ty.pretty_print(db), ty1.pretty_print(db))
            },
            TyKind::Tuple(items) => {
                format!("({})", items.iter().flat_map(|t| [t.pretty_print(db), ",".to_string()]).collect::<String>())
            },
            TyKind::ItemRef(item) => {
                let t = "";
                format!("ItemRef({})", item.name(db).map(|item| item.data(db).as_str()).unwrap_or(t))
            },
        }

    }


    pub fn is_unknown(self, db: &'db dyn BaseDb) -> bool {
        self == unknown(db)
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

    pub fn can_coerce(self, db: &'db dyn BaseDb, dst: Ty<'db>) -> bool {
        if self == dst {
            return true;
        }
        let src_kind = self.kind(db);
        let dst_kind = dst.kind(db);
        if let TyKind::Tuple(src) = &src_kind {
            if src.len() == 1 && src[0].can_coerce(db, dst) {
                return true;
            }
            if let TyKind::Tuple(dst) = &dst_kind {
                return dst.len() == src.len() && src.iter().zip(dst.iter()).all(|(src, dst)| src.can_coerce(db, *dst));
            }
        }
        if let TyKind::Tuple(dst) = &dst_kind {
            if dst.len() == 1 && self.can_coerce(db, dst[0]) {
                return true;
            }
        }
        if let TyKind::Elementary(src) = src_kind {
            if let TyKind::Elementary(dst) = dst_kind {
                match src {
                    hir_def::ElementaryTypeRef::Address { payable: false } => {
                        return matches!(dst, hir_def::ElementaryTypeRef::Address { .. })
                    },
                    hir_def::ElementaryTypeRef::Address { payable: true } => {
                        return matches!(dst, hir_def::ElementaryTypeRef::Address { payable: true })
                    },
                    hir_def::ElementaryTypeRef::String => {
                        return matches!(dst, hir_def::ElementaryTypeRef::Bytes | hir_def::ElementaryTypeRef::FixedBytes { .. })
                    },
                    hir_def::ElementaryTypeRef::Bytes => {
                        return matches!(dst, hir_def::ElementaryTypeRef::String | hir_def::ElementaryTypeRef::FixedBytes { .. })
                    },
                    hir_def::ElementaryTypeRef::Integer { signed, size } => {
                        let hir_def::ElementaryTypeRef::Integer { signed: signed1, size: size1 } = dst else {
                            return false
                        };

                        return signed1 == signed && size1 >= size;
                    },
                    hir_def::ElementaryTypeRef::FixedBytes { size } => {
                        let hir_def::ElementaryTypeRef::FixedBytes { size: size1 } = dst else {
                            return false;
                        };

                        return size1 >= size;
                    },
                    hir_def::ElementaryTypeRef::Fixed { signed, size, decimal_points } => {
                        return false;
                    },
                    hir_def::ElementaryTypeRef::Bool => {
                        return false;
                    },
                    hir_def::ElementaryTypeRef::Unknown => {
                        return false;
                    },
                }
            }
        }

        false
    }
}

#[salsa::tracked]
pub fn unknown<'db>(db: &'db dyn BaseDb) -> Ty<'db> {
    Ty::new(db, TyKind::Unknown)
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
        address: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Address { payable: false })), 
        payable_address: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Address { payable: true })), 
        bool: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Bool)), 
        string: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::String)), 
        bytes: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Bytes)), 
        int8: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 8 })), 
        int16: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 16 })), 
        int32: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 32 })), 
        int64: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 64 })), 
        int128: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 128 })), 
        int256: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 256 })), 
        uint8: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 8 })), 
        uint16: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 16 })), 
        uint32: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 32 })), 
        uint64: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 64 })), 
        uint128: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 128 })), 
        uint256: Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 })), 
        unknown: Ty::new(db, TyKind::Unknown)
    }
}