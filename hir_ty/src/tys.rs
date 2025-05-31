use core::error;

use base_db::{BaseDb, File};

use hir_def::{
    hir::{
        ContractId, ElementaryTypeRef, EnumerationId, ErrorId, EventId, FunctionId, Ident, Item,
        ModifierId, SourceUnit, StructureId,
    },
    DataLocation, UserDefinedValueTypeId,
};

use hir_nameres::{
    container::Container,
    inheritance::inheritance_chain,
    scope::body::{Declaration, MagicDefinitionKind},
};
use smallvec::{smallvec, SmallVec};

use std::{
    collections::{btree_map::Entry, BTreeMap},
    fmt::{Display, Write},
};

use crate::{
    callable::{Callable, CallableType},
    extensions::Extensions,
    member_kind::MemberKind,
    resolver::resolve_item,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum TyKind<'db> {
    Unknown,
    // For functions like abi.encode
    Any,
    // Callable
    Callable(Callable<'db>),
    Modifier(Callable<'db>),

    Error,
    Struct(StructureId<'db>),
    Event,
    Contract(ContractId<'db>),
    Elementary(ElementaryTypeRef),
    UserDefinedValueType(UserDefinedValueTypeId<'db>),

    Enum(EnumerationId<'db>),
    Array(TyKindInterned<'db>, usize),
    Mapping(TyKindInterned<'db>, TyKindInterned<'db>),
    Tuple(SmallVec<[Ty<'db>; 2]>),
    // Reference to an item, not an instance (ContractName.Struct, for example)
    Type(Item<'db>),

    Magic(MagicDefinitionKind),
}

#[salsa::interned(debug)]
#[derive(PartialOrd, Ord)]
pub struct TyKindInterned<'db> {
    pub data: TyKind<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub enum TypeModifier {
    Memory,
    StorageRef,
    StoragePointer,
    Calldata,
}

impl Display for TypeModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TypeModifier::Memory => "memory",
            TypeModifier::StorageRef => "storage ref",
            TypeModifier::StoragePointer => "storage ptr",
            TypeModifier::Calldata => "calldata",
        })
    }
}

impl<T: Into<TypeModifier>> From<Option<T>> for TypeModifier {
    fn from(value: Option<T>) -> Self {
        let Some(value) = value else {
            return TypeModifier::Memory;
        };
        value.into()
    }
}

impl From<DataLocation> for TypeModifier {
    fn from(value: DataLocation) -> Self {
        match value {
            DataLocation::Memory => Self::Memory,
            DataLocation::Storage => Self::StoragePointer,
            DataLocation::Calldata => Self::Calldata,
        }
    }
}

impl<'db> TyKind<'db> {
    pub fn is_complex(self) -> bool {
        match self {
            TyKind::Elementary(ElementaryTypeRef::String | ElementaryTypeRef::Bytes) => true,
            TyKind::Unknown
            | TyKind::Elementary(_)
            | TyKind::Callable(_)
            | TyKind::Modifier(_)
            | TyKind::Tuple(_)
            | TyKind::Type(_) => false,
            _ => true,
        }
    }
    pub fn can_coerce(&self, db: &'db dyn BaseDb, dst: TyKind<'db>) -> bool {
        match (self, dst) {
            (_, TyKind::Any) => true,
            (a, b) if *a == b => true,
            (TyKind::Elementary(src), TyKind::Elementary(dst)) => match src {
                hir_def::ElementaryTypeRef::Address { payable: false } => {
                    matches!(dst, hir_def::ElementaryTypeRef::Address { .. })
                }
                hir_def::ElementaryTypeRef::Address { payable: true } => false,
                hir_def::ElementaryTypeRef::String => {
                    matches!(
                        dst,
                        hir_def::ElementaryTypeRef::Bytes
                            | hir_def::ElementaryTypeRef::FixedBytes { .. }
                    )
                }
                hir_def::ElementaryTypeRef::Bytes => {
                    matches!(
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

                    signed1 == *signed && size1 >= *size
                }
                hir_def::ElementaryTypeRef::FixedBytes { size } => {
                    let hir_def::ElementaryTypeRef::FixedBytes { size: size1 } = dst else {
                        return false;
                    };

                    size1 >= *size
                }
                hir_def::ElementaryTypeRef::Fixed { signed, size, decimal_points } => false,
                hir_def::ElementaryTypeRef::Bool => false,
                hir_def::ElementaryTypeRef::Unknown => false,
            },
            (TyKind::Tuple(src_tuple), TyKind::Tuple(dst_tuple)) => {
                (src_tuple.len() == dst_tuple.len())
                    && src_tuple.iter().zip(dst_tuple).all(|(src, dst)| src.can_coerce(db, dst))
            }
            (TyKind::Contract(a), TyKind::Contract(b)) => {
                *a == b || inheritance_chain(db, *a).contains(&b)
            }
            (a, b) => false,
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
            TyKind::Any => {
                *res += "{any}";
            }
            TyKind::Callable(callable) => {
                *res += "function";
                // FIXME
                *res += "(";
                for (i, a) in callable.args.iter().enumerate() {
                    if i > 0 {
                        *res += ", ";
                    }
                    a.human_readable_to(db, res);
                }
                *res += ")";
                *res += " returns(";
                for (i, a) in callable.returns.iter().enumerate() {
                    if i > 0 {
                        *res += ", ";
                    }
                    a.human_readable_to(db, res);
                }
                *res += ")";
            }
            TyKind::Modifier(callable) => {
                *res += "modifier(";
                for (i, a) in callable.args.iter().enumerate() {
                    if i > 0 {
                        *res += ", ";
                    }
                    a.human_readable_to(db, res);
                }
                *res += ")";
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
            TyKind::Error => {
                *res += "error";
            }
            TyKind::Event => {
                *res += "event";
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
            TyKind::Type(item) => {
                *res += "type(";
                *res += item.name(db).map(|item| item.data(db).as_str()).unwrap_or("{unnamed}");
                *res += ")";
            }
            TyKind::UserDefinedValueType(user_defined_value_type_id) => {
                *res += "UserType(";
                *res += user_defined_value_type_id.name(db).data(db).as_str();
                *res += ")";
            }
            TyKind::Magic(kind) => *res += "magic",
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
pub struct Ty<'db> {
    pub ty_kind: TyKindInterned<'db>,
    pub modifier: TypeModifier,
}

#[salsa::tracked(returns(ref))]
pub fn members<'db>(
    db: &'db dyn BaseDb,
    ty: TyKindInterned<'db>,
    modifier: TypeModifier,
    ext: &'db Extensions<'db>,
) -> BTreeMap<Ident<'db>, SmallVec<[MemberKind<'db>; 1]>> {
    let mut res = BTreeMap::new();
    let modifier = match modifier {
        TypeModifier::StoragePointer => TypeModifier::StorageRef,
        a => a,
    };
    match ty.data(db) {
        TyKind::Type(Item::Error(_)) | TyKind::Type(Item::Event(_)) | TyKind::Callable(_) => {
            res.insert(
                Ident::new(db, "selector".to_owned()),
                smallvec![MemberKind::SynteticItem(Ty::new_intern(
                    db,
                    TyKind::Elementary(ElementaryTypeRef::FixedBytes { size: 32 }),
                ))],
            );
        }
        TyKind::Struct(structure_id) => {
            for f in structure_id.fields(db) {
                res.insert(f.name(db), smallvec![MemberKind::Field(f)]);
            }
        }
        TyKind::Contract(contract_id) => {
            let chain = inheritance_chain(db, contract_id);
            for contract_id in chain {
                if let Some(t) =
                    ext.local.get(&TyKindInterned::new(db, TyKind::Contract(*contract_id)))
                {
                    for (name, data) in t {
                        for (c, f) in data {
                            if c.args.first().copied() == Some(Ty::new(ty)) {
                                let mut c = c.clone();
                                c.args.remove(0);
                                match res.entry(*name) {
                                    Entry::Vacant(vacant_entry) => {
                                        vacant_entry.insert(smallvec![
                                            MemberKind::ExtensionFunction(*f, c)
                                        ]);
                                    }
                                    Entry::Occupied(mut occupied_entry) => {
                                        occupied_entry
                                            .get_mut()
                                            .push(MemberKind::ExtensionFunction(*f, c));
                                    }
                                }
                            }
                        }
                    }
                }

                for (name, item) in contract_id
                    .items(db)
                    .iter()
                    .filter(|i| {
                        matches!(
                            i,
                            hir_def::ContractItem::Function(_)
                                | hir_def::ContractItem::StateVariable(_)
                        )
                    })
                    .flat_map(|item| {
                        item.name(db).map(|name| (name, MemberKind::Item(Item::from(*item))))
                    })
                {
                    match res.entry(name) {
                        Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert([item].into_iter().collect());
                        }
                        Entry::Occupied(mut occupied_entry) => {
                            occupied_entry.get_mut().push(item);
                        }
                    }
                }
            }
        }
        TyKind::Array(ty_kind_interned, _) => {
            res.insert(
                Ident::new(db, "length".to_owned()),
                smallvec![MemberKind::SynteticItem(Ty::new_intern(
                    db,
                    TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
                ))],
            );
        }
        TyKind::Type(item) => match item {
            Item::Module(source_unit) => {
                for (name, item) in source_unit
                    .items(db)
                    .iter()
                    .filter(|i| {
                        matches!(
                            i,
                            Item::Contract(_)
                                | Item::Enum(_)
                                | Item::UserDefinedValueType(_)
                                | Item::Error(_)
                                | Item::Event(_)
                                | Item::Function(_)
                                | Item::StateVariable(_)
                                | Item::Struct(_)
                                | Item::Constructor(_)
                                | Item::Modifier(_)
                                | Item::Module(_)
                        )
                    })
                    .flat_map(|item| item.name(db).map(|name| (name, MemberKind::Item(*item))))
                {
                    match res.entry(name) {
                        Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert([item].into_iter().collect());
                        }
                        Entry::Occupied(mut occupied_entry) => {
                            occupied_entry.get_mut().push(item);
                        }
                    }
                }
            }
            Item::Contract(contract) => {
                for (name, item) in contract
                    .items(db)
                    .iter()
                    .filter(|i| {
                        matches!(
                            i,
                            hir_def::ContractItem::Constructor(_)
                                | hir_def::ContractItem::Modifier(_)
                                | hir_def::ContractItem::UserDefinedValueType(_)
                                | hir_def::ContractItem::Struct(_)
                                | hir_def::ContractItem::Enum(_)
                                | hir_def::ContractItem::StateVariable(_)
                                | hir_def::ContractItem::Function(_)
                        )
                    })
                    .flat_map(|item| {
                        item.name(db).map(|name| (name, MemberKind::Item(Item::from(*item))))
                    })
                {
                    match res.entry(name) {
                        Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert([item].into_iter().collect());
                        }
                        Entry::Occupied(mut occupied_entry) => {
                            occupied_entry.get_mut().push(item);
                        }
                    }
                }
            }
            Item::Enum(enumeration_id) => {
                for f in enumeration_id.fields(db) {
                    res.insert(f.name(db), smallvec![MemberKind::EnumVariant(f)]);
                }
            }
            _ => Default::default(),
        },
        TyKind::Magic(kind) => {
            let fields = match kind {
                MagicDefinitionKind::Block => {
                    let uint = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
                    );
                    let address_payable = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::Address { payable: true }),
                    );
                    &[
                        ("basefee", uint),
                        ("blobbasefee", uint),
                        ("chainid", uint),
                        ("coinbase", address_payable),
                        ("difficulty", uint),
                        ("gaslimit", uint),
                        ("number", uint),
                        ("prevrandao", uint),
                        ("timestamp", uint),
                    ][..]
                }
                MagicDefinitionKind::Msg => {
                    let uint = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
                    );
                    let address = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::Address { payable: false }),
                    );
                    let bytes = Ty::new_intern(db, TyKind::Elementary(ElementaryTypeRef::Bytes));
                    let bytes4 = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::FixedBytes { size: 4 }),
                    );
                    &[("data", bytes), ("sender", address), ("sig", bytes4), ("value", uint)][..]
                }
                MagicDefinitionKind::Tx => {
                    let uint = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
                    );
                    let address = Ty::new_intern(
                        db,
                        TyKind::Elementary(ElementaryTypeRef::Address { payable: false }),
                    );
                    &[("gasprice", uint), ("origin", address)][..]
                }
                MagicDefinitionKind::Abi => {
                    let any = Ty::new_intern(db, TyKind::Any);
                    &[
                        (
                            "decode",
                            Ty::new_intern(
                                db,
                                TyKind::Callable(Callable {
                                    any_args: true,
                                    args: smallvec![any],
                                    returns: smallvec![any],
                                }),
                            ),
                        ),
                        (
                            "encode",
                            Ty::new_intern(
                                db,
                                TyKind::Callable(Callable {
                                    any_args: true,
                                    args: smallvec![any],
                                    returns: smallvec![Ty::new_intern(
                                        db,
                                        TyKind::Elementary(ElementaryTypeRef::Bytes)
                                    )],
                                }),
                            ),
                        ),
                        (
                            "encodePacked",
                            Ty::new_intern(
                                db,
                                TyKind::Callable(Callable {
                                    any_args: true,
                                    args: smallvec![any],
                                    returns: smallvec![Ty::new_intern(
                                        db,
                                        TyKind::Elementary(ElementaryTypeRef::Bytes)
                                    )],
                                }),
                            ),
                        ),
                        (
                            "encodeWithSelector",
                            Ty::new_intern(
                                db,
                                TyKind::Callable(Callable {
                                    any_args: true,
                                    args: smallvec![any],
                                    returns: smallvec![Ty::new_intern(
                                        db,
                                        TyKind::Elementary(ElementaryTypeRef::Bytes)
                                    )],
                                }),
                            ),
                        ),
                        (
                            "encodeCall",
                            Ty::new_intern(
                                db,
                                TyKind::Callable(Callable {
                                    any_args: true,
                                    args: smallvec![any],
                                    returns: smallvec![Ty::new_intern(
                                        db,
                                        TyKind::Elementary(ElementaryTypeRef::Bytes)
                                    )],
                                }),
                            ),
                        ),
                        (
                            "encodeWithSignature",
                            Ty::new_intern(
                                db,
                                TyKind::Callable(Callable {
                                    any_args: true,
                                    args: smallvec![
                                        Ty::new_intern(
                                            db,
                                            TyKind::Elementary(ElementaryTypeRef::FixedBytes {
                                                size: 4
                                            })
                                        ),
                                        any
                                    ],
                                    returns: smallvec![Ty::new_intern(
                                        db,
                                        TyKind::Elementary(ElementaryTypeRef::Bytes)
                                    )],
                                }),
                            ),
                        ),
                    ][..]
                }
                _ => return res,
            };
            res.extend(fields.iter().map(|(name, ty)| {
                (Ident::new(db, *name), SmallVec::from_elem(MemberKind::SynteticItem(*ty), 1))
            }))
        }
        //TyKind::Elementary(elementary_type_ref) => {
        //},
        _ => {}
    }

    res
}

impl<'db> Ty<'db> {
    pub fn new_intern(db: &'db dyn BaseDb, kind: TyKind<'db>) -> Self {
        Self::new_intern_in(db, kind, TypeModifier::Memory)
    }

    pub fn new_intern_in(db: &'db dyn BaseDb, kind: TyKind<'db>, modifier: TypeModifier) -> Self {
        Self::new_in(TyKindInterned::new(db, kind), modifier)
    }

    pub fn new(kind: TyKindInterned<'db>) -> Self {
        Self::new_in(kind, TypeModifier::Memory)
    }

    pub fn new_in(kind: TyKindInterned<'db>, modifier: TypeModifier) -> Self {
        Self { ty_kind: kind, modifier: modifier }
    }

    pub fn kind(self, db: &'db dyn BaseDb) -> TyKind<'db> {
        self.ty_kind.data(db)
    }

    pub fn component_count(self, db: &'db dyn BaseDb) -> u32 {
        match self.kind(db) {
            TyKind::Tuple(items) => items.len() as _,
            _ => 1,
        }
    }

    pub fn human_readable(self, db: &'db dyn BaseDb) -> String {
        let mut res = String::new();
        self.human_readable_to(db, &mut res);
        res
    }

    pub fn human_readable_to(self, db: &'db dyn BaseDb, res: &mut String) {
        let kind = self.kind(db);
        kind.human_readable_to(db, res);
        if self.kind(db).is_complex() {
            write!(res, " {}", self.modifier);
        }
    }

    pub fn is_unknown(self, db: &'db dyn BaseDb) -> bool {
        self.ty_kind == unknown(db)
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
                dst = t[0]
            } else {
                break;
            }
        }

        if self == dst {
            return true;
        }

        let locations_can_coerce = match (self.modifier, dst.modifier) {
            (a, b) if a == b => true,
            (_, TypeModifier::Memory) => true,
            (_, TypeModifier::StorageRef) => true,
            // FIXME
            (_, TypeModifier::Calldata) => true,
            (TypeModifier::StorageRef, TypeModifier::StoragePointer) => true,
            _ => false,
        };

        locations_can_coerce && self.kind(db).can_coerce(db, dst.kind(db))
    }

    pub fn members(
        self,
        db: &'db dyn BaseDb,
        mut ext: &'db Extensions<'db>,
    ) -> &'db BTreeMap<Ident<'db>, SmallVec<[MemberKind<'db>; 1]>> {
        if !matches!(self.kind(db), TyKind::Contract(_)) {
            ext = Extensions::empty();
        }
        members(db, self.ty_kind, self.modifier, ext)
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
        address: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Address { payable: false }),
        ),
        payable_address: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Address { payable: true }),
        ),
        bool: Ty::new_intern(db, TyKind::Elementary(ElementaryTypeRef::Bool)),
        string: Ty::new_intern(db, TyKind::Elementary(ElementaryTypeRef::String)),
        bytes: Ty::new_intern(db, TyKind::Elementary(ElementaryTypeRef::Bytes)),
        int8: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 8 }),
        ),
        int16: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 16 }),
        ),
        int32: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 32 }),
        ),
        int64: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 64 }),
        ),
        int128: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 128 }),
        ),
        int256: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: true, size: 256 }),
        ),
        uint8: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 8 }),
        ),
        uint16: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 16 }),
        ),
        uint32: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 32 }),
        ),
        uint64: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 64 }),
        ),
        uint128: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 128 }),
        ),
        uint256: Ty::new_intern(
            db,
            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
        ),
        unknown: Ty::new_intern(db, TyKind::Unknown),
    }
}
