use std::{cmp::Reverse, collections::BTreeMap};

use base_db::{BaseDb, File};
use hir_def::{
    hir::{BinaryOp, ElementaryTypeRef, Expr, ExprId, Ident, Item, Statement, StatementId},
    lower_file,
    walk::{walk_stmt, Visitor},
    ContractId, ContractItem, ContractType, DataLocation, FileAstPtr, FunctionId, InFile,
    IndexMapUpdate, TypeRefId, TypeRefKind,
};
use hir_nameres::{
    container::{self, Container},
    scope::{body::Declaration, Scope},
};
use hir_nameres::{inheritance::inheritance_chain, scope::HasScope};
use indexmap::IndexMap;
use salsa::{Accumulator, Database};
use smallvec::{smallvec, SmallVec};
use syntax::ast::nodes;

use crate::{
    callable::Callable,
    error::TypeCheckError,
    extensions::Extensions,
    member_kind::MemberKind,
    type_check::{self, check_item},
    tys::{unknown, Ty, TyKind, TyKindInterned, TypeModifier},
};

use hir_def::hir::HasSyntax;
use rowan::ast::AstNode;

#[salsa::tracked(debug)]
pub struct TypeResolution<'db> {
    #[returns_ref]
    #[tracked]
    pub expr_map: IndexMapUpdate<ExprId<'db>, Ty<'db>>,
    #[returns_ref]
    #[tracked]
    pub typeref_map: IndexMapUpdate<TypeRefId<'db>, TyKindInterned<'db>>,
}

#[salsa::tracked]
impl<'db> TypeResolution<'db> {
    #[salsa::tracked]
    pub fn expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Ty<'db> {
        self.expr_map(db).get(&expr).cloned().unwrap_or_else(|| Ty::new(unknown(db)))
    }

    #[salsa::tracked]
    pub fn type_ref(self, db: &'db dyn BaseDb, r: TypeRefId<'db>) -> TyKindInterned<'db> {
        self.typeref_map(db).get(&r).cloned().unwrap_or_else(|| unknown(db))
    }
}

pub(crate) struct TypeResolutionCtx<'db> {
    unknown: TyKindInterned<'db>,

    item: Item<'db>,
    extensions: &'db Extensions<'db>,
    scope: Scope<'db>,

    expr_map: IndexMap<ExprId<'db>, Ty<'db>>,
    typeref_map: IndexMap<TypeRefId<'db>, TyKindInterned<'db>>,
}

impl<'db> Visitor<'db> for &mut TypeResolutionCtx<'db> {
    type Ctx = ();
    type ExprCtx = ();

    fn stmt_end(&mut self, db: &'db dyn BaseDb, _: (), stmt: StatementId<'db>) {
        self.resolve_stmt(db, stmt);
    }

    fn expr_end(&mut self, db: &'db dyn BaseDb, ctx: (), expr: ExprId<'db>) {
        self.resolve_expr(db, expr);
    }

    fn stmt_start(&mut self, db: &'db dyn BaseDb, stmt: StatementId<'db>) -> (bool, Self::Ctx) {
        (true, ())
    }

    fn expr_start(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> (bool, Self::ExprCtx) {
        (true, ())
    }
}

fn resolve_all<'db>(db: &'db dyn BaseDb, items: impl Iterator<Item = Item<'db>>) {
    for i in items {
        if let Item::Contract(c) = i {
            resolve_all(db, c.items(db).iter().copied().map(Item::from));
        }
        resolve_item(db, i);
        check_item(db, i);
    }
}

#[salsa::tracked]
pub fn resolve_file<'db>(db: &'db dyn BaseDb, file: File) {
    let s = lower_file(db, file);
    resolve_all(db, s.items(db).iter().copied());
}

// Function for limited type resolution, must not resolve any other items
#[salsa::tracked(returns(ref))]
pub fn resolve_item_signature<'db>(db: &'db dyn BaseDb, item: Item<'db>) -> TypeResolution<'db> {
    let mut resolver = TypeResolutionCtx::new(db, item);
    resolver.resolve_signature(db);

    TypeResolution::new(db, IndexMapUpdate(resolver.expr_map), IndexMapUpdate(resolver.typeref_map))
}

#[salsa::tracked(returns(ref))]
pub fn resolve_item<'db>(db: &'db dyn BaseDb, item: Item<'db>) -> TypeResolution<'db> {
    let mut resolver = TypeResolutionCtx::new(db, item);
    resolver.resolve_signature(db);
    resolver.resolve_body(db);

    TypeResolution::new(db, IndexMapUpdate(resolver.expr_map), IndexMapUpdate(resolver.typeref_map))
}

impl<'db> TypeResolutionCtx<'db> {
    pub fn new(db: &'db dyn BaseDb, item: Item<'db>) -> Self {
        let mut ctx = TypeResolutionCtx {
            scope: item.scope(db),
            item,
            expr_map: Default::default(),
            typeref_map: Default::default(),

            extensions: Extensions::empty(),

            unknown: unknown(db),
        };

        let t = item.file(db);

        // Skip extensions for items without body
        match item {
            Item::Import(_) | Item::Pragma(_) | Item::Using(_) | Item::Module(_) => {
                return ctx;
            }
            _ => {}
        }

        ctx
    }

    pub fn resolve_signature(&mut self, db: &'db dyn BaseDb) {
        let scope = self.scope;
        match self.item {
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                self.resolve_type_ref(db, scope, user_defined_value_type_id.ty(db));
            }
            Item::Error(error_id) => {
                for p in error_id.parameters(db) {
                    let ty = p.info(db).ty;
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            Item::Event(event_id) => {
                for p in event_id.parameters(db) {
                    let ty = p.info(db).ty;
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            Item::StateVariable(state_variable_id) => {
                let ty = state_variable_id.ty(db);
                self.resolve_type_ref(db, scope, ty);
            }
            Item::Struct(structure_id) => {
                for f in structure_id.fields(db) {
                    let ty = f.ty(db);
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            Item::Function(function_id) => {
                let info = function_id.info(db);
                for a in info.args {
                    let ty = a.ty(db);
                    self.resolve_type_ref(db, scope, ty);
                }
                for a in info.returns.iter().flatten() {
                    let ty = a.ty(db);
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            Item::Constructor(constructor_id) => {
                let info = constructor_id.info(db);
                for f in info.args {
                    let ty = f.ty(db);
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            Item::Modifier(modifier_id) => {
                let info = modifier_id.info(db);
                for f in info.args {
                    let ty = f.ty(db);
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            Item::Using(using) => {
                if let Some(ty) = using.data(db).type_name {
                    self.resolve_type_ref(db, scope, ty);
                }
            }
            _ => {}
        }
    }

    pub fn resolve_body(&mut self, db: &'db dyn BaseDb) {
        cov_mark::hit!(hir_ty_resolve_item);
        self.extensions = Extensions::for_item(db, self.item);

        let item = self.item;
        let scope = self.scope;
        match item {
            Item::Function(function_id) => {
                if let Some((body, map)) = function_id.body(db) {
                    walk_stmt(db, body, self);
                }
            }
            Item::Constructor(constructor_id) => {
                if let Some((body, map)) = constructor_id.body(db) {
                    walk_stmt(db, body, self);
                }
            }
            Item::Modifier(modifier_id) => {
                if let Some((body, map)) = modifier_id.body(db) {
                    walk_stmt(db, body, self);
                }
            }
            _ => {}
        }
    }

    fn resolve_stmt(&mut self, db: &'db dyn BaseDb, stmt: StatementId<'db>) {
        match stmt.kind(db) {
            hir_def::hir::Statement::VarDecl { items, init_expr } => {
                for decl in items.iter() {
                    if let Some(decl) = decl {
                        self.resolve_type_ref(db, self.scope, decl.ty(db));
                    }
                }
            }
            hir_def::hir::Statement::Try { expr, returns, body, catch } => {
                // TODO: Implement
            }
            _ => {}
        }
    }

    fn resolve_method(
        &mut self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        args: SmallVec<[Ty<'db>; 2]>,
    ) -> Ty<'db> {
        self.expr_map.swap_remove(&expr);
        let t = self.resolve_method_inner(db, expr, args);
        self.expr_map.insert(expr, t);

        t
    }

    fn resolve_expr(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Ty<'db> {
        if let Some(t) = self.expr_map.get(&expr) {
            return *t;
        }
        let t = self.resolve_expr_inner(db, expr).unwrap_or(Ty::new(self::unknown(db)));

        self.expr_map.insert(expr, t);

        t
    }

    fn resolve_method_inner(
        &mut self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        args: SmallVec<[Ty<'db>; 2]>,
    ) -> Ty<'db> {
        match expr.kind(db) {
            Expr::Type { ty } => {
                if args.len() != 1 {
                    return Ty::new(self.unknown)
                }
                let res = self.resolve_type_ref(db, self.scope.for_expr(db, expr), *ty);
                Ty::new_intern(db, TyKind::Callable(Callable {
                    args: smallvec![Ty::new_intern(db, TyKind::Any)],
                    variadic: false,
                    returns: smallvec![Ty::new(res)],
                }))
            }
            Expr::ElementaryTypeName { data } => {
                if args.len() != 1 {
                    return Ty::new(self.unknown)
                }
                Ty::new_intern(db, TyKind::Callable(Callable {
                    args: smallvec![Ty::new_intern(db, TyKind::Any)],
                    variadic: false,
                    returns: smallvec![Ty::new_intern(db, TyKind::Elementary(*data))],
                }))
            }
            Expr::MemberAccess { owner, member_name } => {
                let owner_ty = self.resolve_expr(db, *owner);
                let members = owner_ty.members(db, self.extensions);
                for def in members.get(member_name).into_iter().flatten() {
                    let c = match def {
                        MemberKind::Item(item) => {
                            let Some(c) = Callable::try_from_item(db, *item) else {
                                continue;
                            };
                            c
                        }
                        MemberKind::SynteticItem(ty) => match ty.kind(db) {
                            TyKind::Callable(callable) => callable,
                            _ => continue,
                        },
                        MemberKind::ExtensionFunction(f, c) => c.clone(),
                        _ => continue,
                    };

                    if args.len() == c.args.len()
                        && args.iter().zip(&c.args).all(|(a, b)| a.can_coerce(db, *b))
                    {
                        return Ty::new_intern(db, TyKind::Callable(c));
                    }
                }
                Ty::new(self.unknown)
            }
            Expr::Ident { name_ref } => {
                let defs = self.scope.for_expr(db, expr).find_all(db, *name_ref);
                for def in defs.iter().copied() {
                    let c = match def {
                        Declaration::Item(item) => {
                            let Some(c) = Callable::try_from_item(db, item) else {
                                continue;
                            };
                            c
                        }
                        Declaration::Magic(magic_item) => {
                            let Some(c) = Callable::try_from_magic(db, magic_item) else {
                                continue;
                            };
                            c
                        }
                        _ => continue,
                    };

                    if args.len() == c.args.len()
                        && args.iter().zip(&c.args).all(|(a, b)| a.can_coerce(db, *b))
                    {
                        return Ty::new_intern(db, TyKind::Callable(c));
                    }
                }
                Ty::new(self.unknown)
            }
            _ => self.resolve_expr(db, expr),
        }
    }

    fn resolve_expr_inner(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Option<Ty<'db>> {
        let kind = match expr.kind(db) {
            Expr::Index { target, .. } => {
                let target_ty = self.resolve_expr(db, *target);
                let modifier = match target_ty.modifier {
                    TypeModifier::StoragePointer => TypeModifier::StorageRef,
                    a => a,
                };
                return match target_ty.kind(db) {
                    TyKind::Array(ty, _) => Some(Ty::new_in(ty, modifier)),
                    TyKind::Mapping(_k, v) => Some(Ty::new_in(v, modifier)),
                    TyKind::Elementary(ElementaryTypeRef::Bytes)
                    | TyKind::Elementary(ElementaryTypeRef::FixedBytes { .. }) => {
                        Some(Ty::new_intern_in(
                            db,
                            TyKind::Elementary(ElementaryTypeRef::Integer {
                                signed: false,
                                size: 8,
                            }),
                            modifier,
                        ))
                    }
                    _ => None,
                };
            }
            // TODO: fix
            Expr::Slice { base, start, end } => return Some(self.resolve_expr(db, *base)),
            Expr::MemberAccess { owner, member_name } => {
                let owner_ty = self.resolve_expr(db, *owner);
                let members = owner_ty.members(db, self.extensions);
                let def = members.get(member_name)?.first()?;

                let modifier = match owner_ty.modifier {
                    TypeModifier::StoragePointer => TypeModifier::StorageRef,
                    a => a,
                };

                match def {
                    MemberKind::Item(item) => {
                        return Some(match owner_ty.kind(db) {
                            TyKind::Type(owner) => Ty::new(self.resolve_item_ref(db, *item)),
                            _ => Ty::new(self.resolve_item_type(db, *item)),
                        })
                    }
                    MemberKind::EnumVariant(e) => TyKind::Enum(e.parent(db)),
                    MemberKind::Field(f) => {
                        if let TyKind::Struct(s) = owner_ty.kind(db) {
                            return Some(Ty::new_in(
                                self.resolve_type_ref(db, Item::Struct(s).scope(db), f.ty(db)),
                                modifier,
                            ));
                        }
                        return None;
                    }
                    MemberKind::SynteticItem(t) => return Some(*t),
                    MemberKind::ExtensionFunction(f, c) => {
                        let mut c = c.clone();
                        return Some(Ty::new_intern(db, TyKind::Callable(c)));
                    }
                }
            }
            Expr::CallOptions { base, options } => return Some(self.resolve_expr(db, *base)),
            Expr::Call { callee, args } => {
                let args_type = args.iter().map(|(_, a)| self.resolve_expr(db, *a)).collect();
                let receiver_type = self.resolve_method(db, *callee, args_type);
                let c = Callable::try_from_ty(db, receiver_type)?;
                return Some(c.return_ty(db));
            }
            Expr::PrefixOp { expr, op } => return Some(self.resolve_expr(db, *expr)),
            Expr::PostfixOp { expr, op } => return Some(self.resolve_expr(db, *expr)),
            Expr::Ternary { cond, lhs, rhs } => return Some(self.resolve_expr(db, *lhs)),
            Expr::Tuple { content } => TyKind::Tuple(
                content.iter().copied().map(|expr| self.resolve_expr(db, expr)).collect(),
            ),
            Expr::Array { content } => {
                let el_ty = content
                    .first()
                    .map(|expr| self.resolve_expr(db, *expr))
                    .unwrap_or(Ty::new(self.unknown));
                TyKind::Array(el_ty.ty_kind, 0)
            }
            Expr::BinaryOp { lhs, op, rhs } => {
                if let Some(op) = op {
                    return Some(match op {
                        BinaryOp::LogicOp(logic_op) => self.resolve_expr(db, *lhs),
                        BinaryOp::ArithOp(arith_op) => self.resolve_expr(db, *lhs),
                        BinaryOp::CmpOp(cmp_op) => {
                            Ty::new_intern(db, TyKind::Elementary(ElementaryTypeRef::Bool))
                        }
                        BinaryOp::Assignment { op } => self.resolve_expr(db, *rhs),
                    });
                };
                return None;
            }
            Expr::Ident { name_ref } => {
                return Some(match self.scope.find_in_expr(db, expr, *name_ref)? {
                    Declaration::Item(item @ Item::StateVariable(state_variable)) => {
                        Ty::new_in(self.resolve_item_type(db, item), TypeModifier::StorageRef)
                    }
                    Declaration::Item(item) => Ty::new(self.resolve_item_ref(db, item)),
                    Declaration::Local(item) => {
                        let ty_kind = self.resolve_type_ref(db, self.scope, item.ty(db));
                        Ty::new_in(ty_kind, item.location(db).into())
                    }
                    Declaration::Magic(magic) => Ty::new_intern(db, TyKind::Magic(magic)),
                })
            }
            Expr::Literal { data } => match data {
                hir_def::hir::Literal::String(items) => {
                    TyKind::Elementary(ElementaryTypeRef::String)
                }
                hir_def::hir::Literal::Number(big_int) => {
                    TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 })
                }
                hir_def::hir::Literal::Boolean(_) => TyKind::Elementary(ElementaryTypeRef::Bool),
                hir_def::hir::Literal::HexString(items) => {
                    TyKind::Elementary(ElementaryTypeRef::String)
                }
                hir_def::hir::Literal::UnicodeStringLiteral() => {
                    TyKind::Elementary(ElementaryTypeRef::String)
                }
                hir_def::hir::Literal::Error => return None,
            },
            Expr::ElementaryTypeName { data } => TyKind::Elementary(*data),
            Expr::New { ty } => {
                let res = self.resolve_type_ref(db, self.scope, *ty);
                let callable = match res.data(db) {
                    TyKind::Contract(contract_id) => {
                        let c = contract_id.constructor(db);
                        if let Some(c) = c {
                            Callable::try_from_item(db, Item::Constructor(c))
                        } else {
                            Some(Callable {
                                variadic: false,
                                args: SmallVec::new(),
                                returns: smallvec![Ty::new(res)],
                            })
                        }
                    }
                    TyKind::Array(ty_kind_interned, _) => Some(Callable {
                        variadic: false,
                        args: smallvec![Ty::new_intern(
                            db,
                            TyKind::Elementary(ElementaryTypeRef::Integer {
                                signed: false,
                                size: 256,
                            })
                        )],
                        returns: smallvec![Ty::new(res)],
                    }),
                    TyKind::Elementary(ElementaryTypeRef::Bytes) => Some(Callable {
                        variadic: false,
                        args: smallvec![Ty::new_intern(
                            db,
                            TyKind::Elementary(ElementaryTypeRef::Integer {
                                signed: false,
                                size: 256,
                            }),
                        )],
                        returns: smallvec![Ty::new(res)],
                    }),
                    _ => None,
                }?;
                return Some(Ty::new_intern(db, TyKind::Callable(callable)));
            }
            Expr::Type { ty } => {
                return None;
            }
            Expr::Missing => return None,
        };

        Some(Ty::new_intern(db, kind))
    }

    pub(crate) fn resolve_type_ref(
        &mut self,
        db: &'db dyn BaseDb,
        scope: Scope<'db>,
        t: TypeRefId<'db>,
    ) -> TyKindInterned<'db> {
        if let Some(t) = self.typeref_map.get(&t) {
            return *t;
        }
        let ty = self.resolve_type_ref_inner(db, scope, t);
        self.typeref_map.insert(t, ty);

        ty
    }

    fn resolve_type_ref_inner(
        &mut self,
        db: &'db dyn BaseDb,
        scope: Scope<'db>,
        t: TypeRefId<'db>,
    ) -> TyKindInterned<'db> {
        let kind = match t.kind(db) {
            TypeRefKind::Elementary(elementary_type_ref) => {
                return TyKindInterned::new(db, TyKind::Elementary(elementary_type_ref))
            }
            TypeRefKind::Function { arguments, visibility, mutability, returns } => {
                let args = arguments
                    .iter()
                    .map(|vardecl| {
                        Ty::new_in(
                            self.resolve_type_ref(db, scope, vardecl.ty(db)),
                            vardecl.location(db).into(),
                        )
                    })
                    .collect();

                let returns = returns
                    .iter()
                    .map(|vardecl| {
                        Ty::new_in(
                            self.resolve_type_ref(db, scope, vardecl.ty(db)),
                            vardecl.location(db).into(),
                        )
                    })
                    .collect();
                let args = args;
                return TyKindInterned::new(
                    db,
                    TyKind::Callable(Callable { variadic: false, args, returns }),
                );
            }
            TypeRefKind::Mapping { key_type, value_type, .. } => TyKind::Mapping(
                self.resolve_type_ref(db, scope, key_type),
                self.resolve_type_ref(db, scope, value_type),
            ),
            TypeRefKind::Array { ty, len } => {
                TyKind::Array(self.resolve_type_ref(db, scope, ty), 0)
            }
            TypeRefKind::Path(ref idents) => {
                if let Some(item) = scope.lookup_path(db, idents) {
                    let kind = match item {
                        Item::Contract(contract_id) => TyKind::Contract(contract_id),
                        Item::Enum(enumeration_id) => TyKind::Enum(enumeration_id),
                        Item::UserDefinedValueType(user_defined_value_type_id) => {
                            return self.resolve_type_ref(
                                db,
                                user_defined_value_type_id.scope(db),
                                user_defined_value_type_id.ty(db),
                            );
                        }
                        Item::Error(error_id) => TyKind::Type(item),
                        Item::Event(event_id) => TyKind::Type(item),
                        Item::Struct(structure_id) => TyKind::Struct(structure_id),
                        _ => {
                            emit_typeref_error(
                                db,
                                t,
                                "This item can't be used as type".to_string(),
                            );

                            TyKind::Unknown
                        }
                    };
                    return TyKindInterned::new(db, kind);
                } else {
                    emit_typeref_error(db, t, "Undeclared item".to_string());
                    TyKind::Unknown
                }
            }
            TypeRefKind::Error => TyKind::Unknown,
        };
        TyKindInterned::new(db, kind)
    }

    fn resolve_item_ref(&mut self, db: &'db dyn BaseDb, item: Item<'db>) -> TyKindInterned<'db> {
        match item {
            Item::Function(function_id) => self.resolve_item_type(db, item),
            Item::Modifier(modifier_id) => self.resolve_item_type(db, item),
            _ => TyKindInterned::new(db, TyKind::Type(item)),
        }
    }

    fn resolve_item_type(&mut self, db: &'db dyn BaseDb, item: Item<'db>) -> TyKindInterned<'db> {
        match item {
            Item::StateVariable(state_variable_id) => {
                self.resolve_type_ref_inner(db, item.scope(db), state_variable_id.ty(db))
            }
            Item::Modifier(_) => {
                let callable = Callable::try_from_item(db, item).unwrap();

                TyKindInterned::new(db, TyKind::Modifier(callable))
            }
            Item::Function(_) => {
                let callable = Callable::try_from_item(db, item).unwrap();

                TyKindInterned::new(db, TyKind::Callable(callable))
            }
            Item::UserDefinedValueType(user_defined_value_type_id) => self.resolve_type_ref(
                db,
                user_defined_value_type_id.scope(db),
                user_defined_value_type_id.ty(db),
            ),
            _ => self.unknown,
        }
    }
}

#[salsa::tracked]
fn emit_typeref_error<'db>(db: &'db dyn BaseDb, expr: TypeRefId<'db>, desc: String) {
    if let Some(node) = expr.node(db) {
        TypeCheckError {
            file: node.file,
            text: desc,
            range: node.ptr.syntax_node_ptr().text_range(),
        }
        .accumulate(db);
    }
}

#[salsa::tracked]
fn emit_expr_error<'db>(db: &'db dyn BaseDb, expr: ExprId<'db>, desc: String) {
    if let Some(node) = expr.node(db) {
        TypeCheckError {
            file: node.file,
            text: desc,
            range: node.ptr.syntax_node_ptr().text_range(),
        }
        .accumulate(db);
    }
}

#[cfg(test)]
mod tests {
    use std::hash::Hash;

    use base_db::{BaseDb, TestDatabase, TestFixture, VfsPath};
    use hir_def::{items::HirPrint, lower_file, FileExt, Item};
    use rowan::ast::AstNode;
    use salsa::{Database, Setter};
    use tracing_subscriber::filter::LevelFilter;

    use crate::resolver::{resolve_all, resolve_file};

    use super::resolve_item;

    #[track_caller]
    fn check_types(fixture: TestFixture, expected: &str) {
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        let item = lower_file(&db, file).source_map(&db).find_pos(pos);
        let types = resolve_item(&db, item.unwrap());
        let mut res = String::new();
        for (expr, ty) in types.expr_map(&db).0 {
            expr.write(&db, &mut res, 0).unwrap();
            res += ": ";
            res += &ty.human_readable(&db);
            res += "\n";
        }
        res += "\n";
        for (type_ref, ty) in types.typeref_map(&db).0 {
            type_ref.write(&db, &mut res, 0).unwrap();
            res += ": ";
            res += &ty.data(&db).human_readable(&db);
            res += "\n";
        }

        assert_eq!(expected, res, "expected: \n{}, found: \n{}", expected, res)
    }

    #[test]
    fn ident_path_in_mapping() {
        let fixture = TestFixture::parse(
            r#"
            /main.sol
            import * as hw from "./test.sol";

            contract Test
            {
                function t$0mp() {
                    hw.Helloworld tmp = "aa";
                    tmp.unknown;
                    mapping(hw.Helloworld => uint256) memory test;
                }
            }
            /// ENDFILE
            /test.sol
            contract Helloworld
            {
                struct Test {}
            }
        "#,
        );
        check_types(
            fixture,
            "\"aa\": string memory
tmp: Helloworld memory
tmp.unknown: {unknown}

hw.Helloworld: Helloworld
hw.Helloworld: Helloworld
uint256: uint256
mapping(hw.Helloworld => uint256): mapping(Helloworld => uint256)
",
        );
    }

    #[test]
    fn function_overloading() {
        let fixture = TestFixture::parse(
            r"
            /main.sol
            contract Test
            {
                Test testvar = 1;

                function test(uint256 a, bool b) returns(bool) {}

                function test(uint256 a, uint256 b) returns(uint256) {}

                function h$0elloWorld() {
                    testvar;
                    uint256 arg = 1;
                    test(arg, arg);
                    test(arg, true);
                }
            }
        ",
        );
        check_types(
            fixture,
            "testvar: Test storage ref
1: uint256
arg: uint256
arg: uint256
test: function(uint256, uint256) returns(uint256)
test(arg,arg): uint256
true: bool
arg: uint256
test: function(uint256, bool) returns(bool)
test(arg,true): bool

uint256: uint256
",
        );
    }

    #[test]
    fn basic_field_typeresolve() {
        let fixture = TestFixture::parse(
            r"
            /main.sol
            struct TestStruct {
                uint256 field;
                uint128 field2;
                uint256 field3;
                uint256 field4;
                uint256 field5;
            }

            contract Test
            {
                function h$0elloWorld() {
                    TestStruct memory test = 1;
                    test.field;
                }
            }
        ",
        );
        check_types(
            fixture,
            "1: uint256
test: TestStruct memory
test.field: uint256

TestStruct: TestStruct
uint256: uint256
",
        );
    }

    #[test]
    fn basic_invalidation_test() {
        let fixture = TestFixture::parse(
            r"
            /main.sol
            struct TestStruct {
                uint256 field;
                uint128 field2;
                uint256 field3;
                uint256 field4;
                uint256 field5;
            }

            contract Test
            {
                function helloWorld() {
                    TestStruct memory test = 1;
                    test.field;
                }
            }
        ",
        );

        let (mut db, file) = TestDatabase::from_fixture(fixture);
        {
            cov_mark::check_count!(hir_ty_resolve_item, 3);
            resolve_file(&db, file);
        }
        file.set_content(&mut db).to("
    struct TestStruct {
                uint256 field;
                uint128 field2;


                uint256 field3;
                uint256 field4;
                uint256 field5;
            }

            contract Test
            {
                function helloWorld() {

                    TestStruct memory test = 1;
                    test.field;
                }
    }
        "
        .into());
        {
            cov_mark::check_count!(hir_ty_resolve_item, 0);
            let item = lower_file(&db, file).data(&db).contract(&db, "Test");
            resolve_file(&db, file);
        }
    }

    #[test]
    fn extension_trait() {
        let fixture = TestFixture::parse(
            r"
            /main.sol

            using Extension for BaseContract;

            contract BaseContract {
            }

            library Extension {
                function extension(BaseContract, bool test) returns(uint256) {
                }
            }

            contract Test
            {
                BaseContract tmp;

                function hel$0loWorld() {
                    tmp.extension(true);
                }
            }
        ",
        );

        check_types(
            fixture,
            "tmp: BaseContract storage ref
true: bool
tmp.extension: function(bool) returns(uint256)
tmp.extension(true): uint256

",
        );
    }

    #[test]
    fn tuple_unwrapping_test() {
        let fixture = TestFixture::parse(
            r"
            /main.sol

            using GetterExt for First;

            contract First {}

            library GetterExt {
                function getInterface(First) returns(ITestInterface) {
                }
            }

            interface ITestInterface {
                function interfaceMember(address id) external view returns (address);
            }

            contract Test
            {
                First tmp;

                function hel$0loWorld(address help) {
                    First tmp;
                    tmp.getInterface().interfaceMember(help);
                }
            }
        ",
        );

        check_types(
            fixture,
            "tmp: First memory
tmp.getInterface: function() returns(ITestInterface memory)
tmp.getInterface(): ITestInterface memory
help: address
tmp.getInterface().interfaceMember: function(address) returns(address)
tmp.getInterface().interfaceMember(help): address

address: address
First: First
",
        );
    }

    #[test]
    fn new_test() {
        let fixture = TestFixture::parse(
            r"
            /main.sol

            contract First {}

            contract Test
            {
                function hel$0loWorld(address help) {
                    First tmp = new First();
                }
            }
        ",
        );

        check_types(
            fixture,
            "new First: function() returns(First memory)
new First(): First memory

address: address
First: First
First: First
",
        );
    }

    #[test]
    fn enum_variant() {
        let fixture = TestFixture::parse(
            r"
            /main.sol

            contract Test1 {
                enum SomeEnum {
                    Var1,
                    Var2
                }
            }

            contract Test
            {
                function hel$0loWorld() {
                    Test1.SomeEnum.Var1;
                }
            }
        ",
        );

        check_types(
            fixture,
            "Test1: type(Test1)
Test1.SomeEnum: type(SomeEnum)
Test1.SomeEnum.Var1: SomeEnum memory

",
        );
    }

    #[test]
    fn method_call_with_storage_args_test() {
        let fixture = TestFixture::parse(
            r"
            /main.sol

            struct Argument {
                uint256 t;
            }

            contract Test
            {
                Argument storageRef;

                function tmp(Argument storage t) {

                }

                function hel$0loWorld() {
                    tmp;
                    tmp(storageRef);
                }
            }
        ",
        );

        check_types(
            fixture,
            "tmp: function(Argument storage ptr) returns()
storageRef: Argument storage ref
tmp: function(Argument storage ptr) returns()
tmp(storageRef): ()

",
        );
    }

    #[test]
    fn type_cast() {
        let fixture = TestFixture::parse(
            r"
            /main.sol

            contract Test
            {
                address storageRef;

                function hel$0loWorld() {
                    uint256(123);
                }
            }
        ",
        );

        check_types(
            fixture,
            "123: uint256
uint256: {unknown}
uint256(123): {unknown}

",
        );
    }
}
