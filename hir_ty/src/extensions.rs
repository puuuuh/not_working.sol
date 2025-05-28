use std::collections::BTreeMap;

use base_db::{BaseDb, File};
use hir_def::{lower_file, ContractItem, ContractType, FunctionId, Ident, Item, UsingId};
use hir_nameres::scope::{body::Declaration, HasScope, Scope};
use smallvec::SmallVec;

use crate::{
    callable::Callable, resolver::{resolve_item, TypeResolutionCtx}, tys::{TyKind, TyKindInterned}
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Default, salsa::Update)]
pub struct Extensions<'db> {
    pub local: BTreeMap<TyKindInterned<'db>, BTreeMap<Ident<'db>, SmallVec<[(Callable<'db>, FunctionId<'db>); 1]>>>,
    pub wildcard: BTreeMap<Ident<'db>, SmallVec<[(Callable<'db>, FunctionId<'db>); 1]>>,
}

impl<'db> Extensions<'db> {
    pub fn empty<'a>() -> &'a Extensions<'a> {
        const {
            &Extensions {
                local: BTreeMap::new(),
                wildcard: BTreeMap::new(),
            }
        }
    }

    pub fn for_item(db: &'db dyn BaseDb, item: Item<'db>) -> &'db Self {
        for_item(db, item)
    }
}

#[salsa::tracked(returns(ref))]
fn for_item<'db>(db: &'db dyn BaseDb, item: Item<'db>) -> Extensions<'db> {
    let mut res = for_file(db, item.file(db));
    let parent = match item {
        Item::Contract(contract_id) => Some(contract_id),
        Item::Enum(enumeration_id) => enumeration_id.origin(db),
        Item::UserDefinedValueType(user_defined_value_type_id) => {
            user_defined_value_type_id.origin(db)
        }
        Item::Error(error_id) => error_id.origin(db),
        Item::Event(event_id) => event_id.origin(db),
        Item::Function(function_id) => function_id.origin(db),
        Item::StateVariable(state_variable_id) => state_variable_id.origin(db),
        Item::Struct(structure_id) => structure_id.origin(db),
        Item::Constructor(constructor_id) => constructor_id.origin(db),
        Item::Modifier(modifier_id) => modifier_id.origin(db),
        _ => None,
    };
    if let Some(parent) = parent {
        collect_usings_to(
            db,
            parent.scope(db),
            parent.items(db).iter().filter_map(|i| {
                if let ContractItem::Using(u) = i {
                    Some(*u)
                } else {
                    None
                }
            }),
            &mut res,
        );
    }
    res
}

#[salsa::tracked]
fn for_file<'db>(db: &'db dyn BaseDb, f: File) -> Extensions<'db> {
    let s = lower_file(db, f);
    let scope = s.scope(db);
    let mut res: Extensions = Default::default();
    collect_usings_to(db, scope, s.data(db).usings(db).iter().copied(), &mut res);

    res
}

fn collect_usings_to<'db, 'a>(
    db: &'db dyn BaseDb,
    scope: Scope<'db>,
    usings: impl Iterator<Item = UsingId<'db>>,
    res: &mut Extensions<'db>,
) {
    for u in usings {
        let type_resolution = resolve_item(db, Item::Using(u));
        let using_data = u.data(db);

        let ty = if let Some(t) = using_data.type_name {
            let ty = type_resolution.type_ref(db, t);
            let ty = match ty.data(db) {
                TyKind::Contract(_) | TyKind::UserDefinedValueType(_) => ty,
                _ => {
                    continue;
                }
            };
            Some(ty)
        } else {
            None
        };
        let mut tmp =
            if let Some(ty) = ty { res.local.entry(ty).or_default() } else { &mut res.wildcard };

        for i in using_data.items {
            if i.as_name.is_some() {
                // FIXME
                continue;
            }
            if let Some(lib) = scope.lookup_path(db, &i.path.0) {
                match lib {
                    Item::Function(f) => {
                        if let Some(name) = f.name(db) {
                            if let Some(c) = Callable::try_from_item(db, Item::Function(f)) {
                                tmp.entry(name).or_default().push((c, f));
                            }
                        }
                    }
                    Item::Contract(c) => {
                        if c.kind(db) == ContractType::Library {
                            for f in c.items(db).iter().filter_map(|c| match c {
                                ContractItem::Function(f) => Some(f),
                                _ => None,
                            }) {
                                if let Some(name) = f.name(db) {
                                    if let Some(c) = Callable::try_from_item(db, Item::Function(*f)) {
                                        tmp.entry(name).or_default().push((c, *f));
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}
