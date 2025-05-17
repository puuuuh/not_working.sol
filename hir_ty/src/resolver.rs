use std::{cmp::Reverse, collections::BTreeMap};

use base_db::{BaseDb, File, Project};
use hir_def::{
    hir::{BinaryOp, ElementaryTypeRef, Expr, ExprId, Ident, Item, Statement, StatementId},
    lower_file,
    walk::{walk_stmt, Visitor},
    DataLocation, FileAstPtr, InFile, IndexMapUpdate, TypeRefId, TypeRefKind,
};
use hir_nameres::scope::HasScope;
use hir_nameres::{
    container::{self, Container},
    scope::{body::Definition, Scope},
};
use indexmap::IndexMap;
use salsa::{Accumulator, Database};
use syntax::ast::nodes;

use crate::{
    error::TypeCheckError,
    type_check::{self, check_item},
    tys::{unknown, Ty, TyKind, TyKindInterned},
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
        self.expr_map(db)
            .get(&expr)
            .cloned()
            .unwrap_or_else(|| Ty { ty_kind: unknown(db), location: None })
    }

    #[salsa::tracked]
    pub fn type_ref(self, db: &'db dyn BaseDb, r: TypeRefId<'db>) -> TyKindInterned<'db> {
        self.typeref_map(db).get(&r).cloned().unwrap_or_else(|| unknown(db))
    }
}

struct TypeResolutionCtx<'db> {
    scope: Scope<'db>,
    project: Project,

    unknown: TyKindInterned<'db>,

    extensions: BTreeMap<Option<Ty<'db>>, ()>,

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

fn resolve_all<'db>(db: &'db dyn BaseDb, project: Project, items: impl Iterator<Item = Item<'db>>) {
    for i in items {
        if let Item::Contract(c) = i {
            resolve_all(db, project, c.items(db).iter().copied().map(Item::from));
        }
        resolve_item(db, project, i);
        //check_item(db, project, i);
    }
}

#[salsa::tracked]
pub fn resolve_file<'db>(db: &'db dyn BaseDb, project: Project, file: File) {
    let s = lower_file(db, file);
    resolve_all(db, project, s.items(db).iter().copied());
}

#[salsa::tracked]
pub fn resolve_item<'db>(
    db: &'db dyn BaseDb,
    project: Project,
    item: Item<'db>,
) -> TypeResolution<'db> {
    let resolution = TypeResolutionCtx::resolve_item(db, project, item);

    return TypeResolution::new(
        db,
        IndexMapUpdate(resolution.expr_map),
        IndexMapUpdate(resolution.typeref_map),
    );
}

impl<'db> TypeResolutionCtx<'db> {
    pub fn resolve_item(
        db: &'db dyn BaseDb,
        project: Project,
        item: Item<'db>,
    ) -> TypeResolutionCtx<'db> {
        cov_mark::hit!(hir_ty_resolve_item);
        let file = item.file(db);
        let mut ctx = TypeResolutionCtx {
            project,
            scope: item.scope(db, project),
            expr_map: Default::default(),
            typeref_map: Default::default(),

            extensions: Default::default(),

            unknown: unknown(db),
        };
        match item {
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                ctx.resolve_type_ref(db, ctx.scope, user_defined_value_type_id.ty(db));
            }
            Item::Error(error_id) => {
                for p in error_id.parameters(db) {
                    let ty = p.info(db).ty;
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
            }
            Item::Event(event_id) => {
                for p in event_id.parameters(db) {
                    let ty = p.info(db).ty;
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
            }
            Item::StateVariable(state_variable_id) => {
                let ty = state_variable_id.ty(db);
                ctx.resolve_type_ref(db, ctx.scope, ty);
            }
            Item::Struct(structure_id) => {
                for f in structure_id.fields(db) {
                    let ty = f.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
            }
            Item::Function(function_id) => {
                let info = function_id.info(db);
                for a in info.args {
                    let ty = a.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
                for a in info.returns.iter().flatten() {
                    let ty = a.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }

                if let Some((body, map)) = function_id.body(db) {
                    walk_stmt(db, body, &mut ctx);
                }
            }
            Item::Constructor(constructor_id) => {
                let info = constructor_id.info(db);
                for f in info.args {
                    let ty = f.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
                if let Some((body, map)) = constructor_id.body(db) {
                    walk_stmt(db, body, &mut ctx);
                }
            }
            Item::Modifier(modifier_id) => {
                let info = modifier_id.info(db);
                for f in info.args {
                    let ty = f.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
                if let Some((body, map)) = modifier_id.body(db) {
                    walk_stmt(db, body, &mut ctx);
                }
            }
            _ => {}
        }

        ctx
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
            hir_def::hir::Statement::ForLoop { init, cond, finish_action, body } => {
                if let Some(stmt) = init {
                    self.resolve_stmt(db, *stmt);
                }
            }
            hir_def::hir::Statement::Try { expr, returns, body, catch } => {
                // TODO: Implement
            }
            _ => {}
        }
    }

    fn resolve_expr_with_args(
        &mut self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        args: TyKindInterned<'db>,
    ) -> Ty<'db> {
        self.expr_map.swap_remove(&expr);
        let t = self.resolve_expr_with_args_inner(db, expr, args);
        self.expr_map.insert(expr, t);

        t
    }

    fn resolve_expr(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Ty<'db> {
        if let Some(t) = self.expr_map.get(&expr) {
            return *t;
        }
        let t = self
            .resolve_expr_inner(db, expr)
            .unwrap_or(Ty { ty_kind: self.unknown, location: None });

        self.expr_map.insert(expr, t);

        t
    }

    fn resolve_expr_with_args_inner(
        &mut self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        args_type: TyKindInterned<'db>,
    ) -> Ty<'db> {
        match expr.kind(db) {
            Expr::MemberAccess { owner, member_name } => {
                let owner_ty = self.resolve_expr(db, *owner);
                let Some(container) = owner_ty.container(db) else {
                    return Ty { ty_kind: self.unknown, location: None };
                };
                let defs = container.defs(db);
                let mut defs = defs.iter().filter(|(name, _)| *name == *member_name);
                loop {
                    let Some((_, def)) = defs.next() else {
                        return Ty { ty_kind: self.unknown, location: None };
                    };

                    let Definition::Item(item) = def else {
                        continue;
                    };

                    let ty = self.resolve_item_type(db, *item);
                    if let TyKind::Function(args, _) = ty.data(db) {
                        if args == args_type {
                            return Ty { ty_kind: ty, location: None };
                        }
                    }
                }
            }
            Expr::Ident { name_ref } => {
                let defs = self.scope.for_expr(db, expr);
                for def in defs.find_all(db, *name_ref) {
                    let Definition::Item(item) = def else {
                        continue;
                    };

                    let ty = self.resolve_item_type(db, item);
                    if let TyKind::Function(fn_args, _) = ty.data(db) {
                        if args_type.data(db).can_coerce(db, fn_args.data(db)) {
                            return Ty { ty_kind: ty, location: None };
                        }
                    }
                }
                return Ty { ty_kind: self.unknown, location: None };
            }
            _ => return self.resolve_expr(db, expr),
        };
    }

    fn resolve_expr_inner(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Option<Ty<'db>> {
        let kind = match expr.kind(db) {
            Expr::Index { target, .. } => {
                let target_ty = self.resolve_expr(db, *target);
                return match target_ty.kind(db) {
                    TyKind::Array(ty, _) => Some(Ty { ty_kind: ty, location: target_ty.location }),
                    TyKind::Mapping(_k, v) => Some(Ty { ty_kind: v, location: target_ty.location }),
                    _ => None,
                };
            }
            // TODO: fix
            Expr::Slice { base, start, end } => return Some(self.resolve_expr(db, *base)),
            Expr::MemberAccess { owner, member_name } => {
                let owner_ty = self.resolve_expr(db, *owner);
                let container = owner_ty.container(db)?;
                let def =
                    container.defs(db).iter().copied().find(|(name, _)| *name == *member_name)?;

                match def.1 {
                    Definition::Item(item) => {
                        return Some(match owner_ty.kind(db) {
                            TyKind::ItemRef(item) => {
                                Ty { ty_kind: self.resolve_item_ref(db, item), location: None }
                            }
                            _ => Ty { ty_kind: self.resolve_item_type(db, item), location: None },
                        })
                    }
                    Definition::EnumVariant(e) => {
                        TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 })
                    }
                    Definition::Field(f) => {
                        if let TyKind::Struct(s) = owner_ty.kind(db) {
                            return Some(Ty {
                                ty_kind: self.resolve_type_ref(
                                    db,
                                    Item::Struct(s).scope(db, self.project),
                                    f.ty(db),
                                ),
                                location: owner_ty.location,
                            });
                        }
                        return None;
                    }
                    Definition::Local((item)) => return None,
                }
            }
            Expr::CallOptions { base, options } => return Some(self.resolve_expr(db, *base)),
            Expr::Call { callee, args } => {
                let args_type = TyKindInterned::new(
                    db,
                    TyKind::Tuple(args.iter().map(|(_, a)| self.resolve_expr(db, *a)).collect()),
                );
                let receiver_type = self.resolve_expr_with_args(db, *callee, args_type);
                let result_type = match receiver_type.kind(db) {
                    TyKind::Function(_, result_type) => Ty { ty_kind: result_type, location: None },
                    TyKind::Error(_) | TyKind::Event(_) => receiver_type,
                    _ => return None,
                };
                return Some(result_type);
            }
            Expr::PrefixOp { expr, op } => return Some(self.resolve_expr(db, *expr)),
            Expr::PostfixOp { expr, op } => return Some(self.resolve_expr(db, *expr)),
            Expr::Ternary { cond, lhs, rhs } => return Some(self.resolve_expr(db, *lhs)),
            Expr::Tuple { content } => TyKind::Tuple(
                content.iter().copied().map(|expr| self.resolve_expr(db, expr)).collect(),
            ),
            Expr::Array { content } => {
                let el_ty = content
                    .get(0)
                    .map(|expr| self.resolve_expr(db, *expr))
                    .unwrap_or(Ty { ty_kind: self.unknown, location: None });
                TyKind::Array(el_ty.ty_kind, 0)
            }
            Expr::BinaryOp { lhs, op, rhs } => {
                if let Some(op) = op {
                    return Some(match op {
                        BinaryOp::LogicOp(logic_op) => self.resolve_expr(db, *lhs),
                        BinaryOp::ArithOp(arith_op) => self.resolve_expr(db, *lhs),
                        BinaryOp::CmpOp(cmp_op) => {
                            Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Bool), None)
                        }
                        BinaryOp::Assignment { op } => self.resolve_expr(db, *rhs),
                    });
                };
                return None;
            }
            Expr::Ident { name_ref } => {
                return Some(match self.scope.find_in_expr(db, expr, *name_ref)? {
                    Definition::Item(item @ Item::StateVariable(state_variable)) => {
                        Ty { ty_kind: self.resolve_item_type(db, item), location: None }
                    }
                    Definition::Item(item) => {
                        Ty { ty_kind: self.resolve_item_ref(db, item), location: None }
                    }
                    Definition::Local(item) => {
                        let ty_kind = self.resolve_type_ref(db, self.scope, item.ty(db));
                        Ty { ty_kind, location: item.location(db) }
                    }
                    Definition::EnumVariant(_) | Definition::Field(_) => return None,
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
                return Some(Ty {
                    ty_kind: self.resolve_type_ref(db, self.scope, ty.clone()),
                    location: None,
                })
            }
            Expr::Missing => return None,
        };

        Some(Ty::new(db, kind, None))
    }

    fn resolve_type_ref(
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
                    .map(|vardecl| Ty {
                        ty_kind: self.resolve_type_ref(db, scope, vardecl.ty(db)),
                        location: vardecl.location(db),
                    })
                    .collect();

                let returns = returns
                    .iter()
                    .map(|vardecl| Ty {
                        ty_kind: self.resolve_type_ref(db, scope, vardecl.ty(db)),
                        location: vardecl.location(db),
                    })
                    .collect();
                let args = TyKindInterned::new(db, TyKind::Tuple(args));
                let returns = TyKindInterned::new(db, TyKind::Tuple(returns));
                return TyKindInterned::new(db, TyKind::Function(args, returns));
            }
            TypeRefKind::Mapping { key_type, value_type, .. } => TyKind::Mapping(
                self.resolve_type_ref(db, scope, key_type),
                self.resolve_type_ref(db, scope, value_type),
            ),
            TypeRefKind::Array { ty, len } => {
                TyKind::Array(self.resolve_type_ref(db, scope, ty), 0)
            }
            TypeRefKind::Path(ref idents) => {
                if let Some(Definition::Item(item)) = scope.lookup_path(db, &idents) {
                    let kind = match item {
                        Item::Contract(contract_id) => TyKind::Contract(contract_id),
                        Item::Enum(enumeration_id) => TyKind::Enum(enumeration_id),
                        Item::UserDefinedValueType(user_defined_value_type_id) => {
                            return self.resolve_type_ref(
                                db,
                                user_defined_value_type_id.scope(db, self.project),
                                user_defined_value_type_id.ty(db),
                            );
                        }
                        Item::Error(error_id) => TyKind::Error(error_id),
                        Item::Event(event_id) => TyKind::Event(event_id),
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
            Item::Function(function_id) => {
                return self.resolve_item_type(db, item);
            }
            Item::Modifier(modifier_id) => return self.resolve_item_type(db, item),
            _ => return TyKindInterned::new(db, TyKind::ItemRef(item)),
        }
    }

    fn resolve_item_type(&mut self, db: &'db dyn BaseDb, item: Item<'db>) -> TyKindInterned<'db> {
        match item {
            Item::StateVariable(state_variable_id) => {
                return self.resolve_type_ref_inner(
                    db,
                    item.scope(db, self.project),
                    state_variable_id.ty(db),
                );
            }
            Item::Modifier(modifier_id) => {
                /*
                let scope = modifier_id.item_scope(db, self.project);

                let info = modifier_id.info(db);
                let args = info
                    .args
                    .iter()
                    .map(|vardecl| self.resolve_type_ref(db, Scope::Item(scope), vardecl.ty(db)))
                    .collect();
                 */
                TyKindInterned::new(db, TyKind::Modifier(modifier_id))
            }
            Item::Function(function_id) => {
                let scope = function_id.item_scope(db, self.project);
                let info = function_id.info(db);
                let args = info
                    .args
                    .iter()
                    .map(|vardecl| {
                        Ty::new_interned(
                            db,
                            self.resolve_type_ref(db, Scope::Item(scope), vardecl.ty(db)),
                            vardecl.location(db),
                        )
                    })
                    .collect();

                let returns = info
                    .returns
                    .iter()
                    .flatten()
                    .map(|vardecl| {
                        Ty::new_interned(
                            db,
                            self.resolve_type_ref(db, Scope::Item(scope), vardecl.ty(db)),
                            vardecl.location(db),
                        )
                    })
                    .collect();

                TyKindInterned::new(
                    db,
                    TyKind::Function(
                        TyKindInterned::new(db, TyKind::Tuple(args)),
                        TyKindInterned::new(db, TyKind::Tuple(returns)),
                    ),
                )
            }
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                return self.resolve_type_ref(
                    db,
                    user_defined_value_type_id.scope(db, self.project),
                    user_defined_value_type_id.ty(db),
                )
            }
            _ => {
                return self.unknown;
            }
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

    use base_db::{BaseDb, Project, TestDatabase, TestFixture, VfsPath};
    use hir_def::{items::HirPrint, lower_file, FileExt, Item};
    use rowan::ast::AstNode;
    use salsa::{Database, Setter};
    use tracing_subscriber::filter::LevelFilter;

    use crate::resolver::{resolve_all, resolve_file};

    use super::resolve_item;

    fn check_types(fixture: TestFixture, expected: &str) {
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        let item = lower_file(&db, file).source_map(&db).find_pos(pos);
        let types = resolve_item(
            &db,
            Project::new(&db, VfsPath::from_virtual("".to_owned())),
            item.unwrap(),
        );
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
            "String([[97, 97]]): string
tmp: Helloworld
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
                uint64 testvar = 1;

                function test(uint256 a, bool b) returns(bool) {}

                function test(uint256 a, uint256 b) returns(uint256) {}

                function h$0elloWorld() {
                    uint256 arg = 1;
                    test(arg, arg);
                    test(arg, true);
                }
            }
        ",
        );
        check_types(
            fixture,
            "Number(1): uint256
arg: uint256
arg: uint256
test: function(uint256,uint256,) returns(uint256,)
test(arg,arg): (uint256,)
Boolean(true): bool
arg: uint256
test: function(uint256,bool,) returns(bool,)
test(arg,Boolean(true)): (bool,)

uint256: uint256
uint256: uint256
bool: bool
bool: bool
uint256: uint256
uint256: uint256
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
            "Number(1): uint256
test: TestStruct memory
test.field: uint256 memory

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
        let project = Project::new(&db, VfsPath::from_virtual("".to_owned()));
        {
            cov_mark::check_count!(hir_ty_resolve_item, 3);
            resolve_file(&db, project, file);
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
            resolve_file(&db, project, file);
        }
    }
}
