use base_db::{BaseDb, File, Project};
use hir_def::{hir::{BinaryOp, ElementaryTypeRef, Expr, ExprId, HasDefs, HasSourceUnit, Ident, Item, Statement, StatementId, TypeRef}, nameres::scope::{body::Definition, Scope}, walk::{walk_stmt, Visitor}, IndexMapUpdate};
use indexmap::IndexMap;
use salsa::Database;

use crate::tys::{unknown, Ty, TyKind};

#[salsa::tracked(debug)]
pub struct TypeResolution<'db> {
    #[returns_ref]
    pub expr_map: IndexMapUpdate<ExprId<'db>, Ty<'db>>,
    #[returns_ref]
    pub typeref_map: IndexMapUpdate<TypeRef<'db>, Ty<'db>>
}

#[salsa::tracked]
impl<'db> TypeResolution<'db> {
    #[salsa::tracked]
    fn expr(self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Ty<'db> {
        self.expr_map(db).get(&expr).cloned().unwrap_or_else(|| Ty::new(db, TyKind::Unknown))
    }

    #[salsa::tracked]
    fn type_ref(self, db: &'db dyn BaseDb, r: TypeRef<'db>) -> Ty<'db> {
        self.typeref_map(db).get(&r).cloned().unwrap_or_else(|| Ty::new(db, TyKind::Unknown))
    }
}

pub struct TypeResolutionCtx<'db> {
    module: File,
    scope: Scope<'db>,
    project: Project,

    expr_map: IndexMap<ExprId<'db>, Ty<'db>>,
    typeref_map: IndexMap<TypeRef<'db>, Ty<'db>>
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
#[salsa::tracked]
pub fn resolve_item<'db>(db: &'db dyn BaseDb, project: Project, item: Item<'db>, module: File) -> TypeResolution<'db> {
    let resolution = TypeResolutionCtx::resolve_item(db, project, item, module);

    return TypeResolution::new(db, IndexMapUpdate(resolution.expr_map), IndexMapUpdate(resolution.typeref_map));
}

impl<'db> TypeResolutionCtx<'db> {
    pub fn resolve_item(db: &'db dyn BaseDb, project: Project, item: Item<'db>, module: File) -> TypeResolutionCtx<'db> {
        let mut ctx = TypeResolutionCtx { 
            module,
            project,
            scope: item.scope(db, project, module), 
            expr_map: Default::default(), 
            typeref_map: Default::default() 
        };
        match item {
            Item::UserDefinedValueType(user_defined_value_type_id) => {
                ctx.resolve_type_ref(db, ctx.scope, user_defined_value_type_id.ty(db));
            },
            Item::Error(error_id) => {
                for p in error_id.parameters(db) {
                    let ty = p.info(db).ty;
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
            },
            Item::Event(event_id) => {
                for p in event_id.parameters(db) {
                    let ty = p.info(db).ty;
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
            },
            Item::StateVariable(state_variable_id) => {
                let ty = state_variable_id.ty(db);
                ctx.resolve_type_ref(db, ctx.scope, ty);
            },
            Item::Struct(structure_id) => {
                for f in structure_id.fields(db) {
                    let ty = f.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
            },
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
                
                if let Some((body, map)) = function_id.body(db, module) {
                    walk_stmt(db, body, &mut ctx);
                }
            },
            Item::Constructor(constructor_id) => {
                let info = constructor_id.info(db);
                for f in info.args {
                    let ty = f.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
                if let Some((body, map)) = constructor_id.body(db, module) {
                    walk_stmt(db, body, &mut ctx);
                }
            },
            Item::Modifier(modifier_id) => {
                let info = modifier_id.info(db);
                for f in info.args {
                    let ty = f.ty(db);
                    ctx.resolve_type_ref(db, ctx.scope, ty);
                }
                if let Some((body, map)) = modifier_id.body(db, module) {
                    walk_stmt(db, body, &mut ctx);
                }
            },
            _ => {}
        }

        ctx
    }

    fn resolve_stmt(&mut self, db: &'db dyn BaseDb, stmt: StatementId<'db>) {
        match stmt.kind(db) {
            hir_def::hir::Statement::VarDecl { items, init_expr } => {
                for i in items.iter().flatten() {
                    let t = i.ty(db);
                    self.resolve_type_ref(db, self.scope, t);
                }
            },
            hir_def::hir::Statement::ForLoop { init, cond, finish_action, body } => {
                if let Some(stmt) = init {
                    self.resolve_stmt(db, *stmt);
                }
            },
            hir_def::hir::Statement::Try { expr, returns, body, catch } => {
                // TODO: Implement
            },
            _ => {}
        }
    }

    fn resolve_expr_with_args(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>, args: Ty<'db>) -> Ty<'db> {
        self.expr_map.swap_remove(&expr);
        let t = self.resolve_expr_with_args_inner(db, expr, args);

        self.expr_map.insert(expr, t);

        t
    }

    fn resolve_expr(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Ty<'db> {
        if let Some(t) =  self.expr_map.get(&expr) {
            return *t;
        }
        let t = self.resolve_expr_inner(db, expr);

        self.expr_map.insert(expr, t);

        t
    }

    fn resolve_expr_with_args_inner(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>, args_type: Ty<'db>) -> Ty<'db> {
        let kind = match expr.kind(db) {
            Expr::MemberAccess { owner, member_name } => {
                let owner_ty = self.resolve_expr(db, *owner);
                let defs = owner_ty.defs(db);
                let mut defs = defs.iter().filter(|(name, _)| *name == *member_name);
                loop {
                    let Some((_, def)) = defs.next() else {
                        break TyKind::Unknown;
                    };

                    let Definition::Item(item) = def else {
                        continue;
                    };

                    let ty = self.resolve_item_type(db, *item);
                    if let TyKind::Function(args, _) = ty.kind(db) {
                        if args == args_type {
                            return ty;
                        }
                    }
                }
            },
            Expr::Ident { name_ref } if false => {
                /*
                let defs = self.scope.for_expr(db, expr);
                let mut defs = defs.iter().filter(|(name, _)| *name == *member_name);
                loop {
                    let Some((_, def)) = defs.next() else {
                        break TyKind::Unknown;
                    };

                    let Definition::Item(item) = def else {
                        continue;
                    };

                    let ty = self.resolve_item_type(db, *item);
                    if let TyKind::Function(args, _) = ty.kind(db) {
                        if args == args_type {
                            return ty;
                        }
                    }
                }
                 */
                todo!()
            },
            _ => return self.resolve_expr(db, expr)
        };

        Ty::new(db, kind)
    }

    fn resolve_expr_inner(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> Ty<'db> {
        let kind = match expr.kind(db) {
            Expr::Index { target, .. } => {
                match self.resolve_expr(db, *target).kind(db) {
                    TyKind::Array(ty, _) => return ty,
                    TyKind::Mapping(_k, v) => return v,
                    _ => TyKind::Unknown
                }
            },
            Expr::Slice { base, start, end } => return self.resolve_expr(db, *base),
            Expr::MemberAccess { owner, member_name } => {
                let owner_ty = self.resolve_expr(db, *owner);
                if let Some(def) =  owner_ty.defs(db).iter().find(|(name, _)| *name == *member_name) {
                    match def.1 {
                        Definition::Item(item) => return self.resolve_item_type(db, item),
                        Definition::EnumVariant(e) => {
                            TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 })
                        },
                        Definition::Field(f) => {
                            if let TyKind::Struct(module, s) = owner_ty.kind(db) {
                                return self.resolve_type_ref(db, Item::Struct(s).scope(db, self.project, module), f.ty(db));
                            }
                            TyKind::Unknown
                        },
                        Definition::Local((origin, item)) => TyKind::Unknown,
                    }
                } else {
                    TyKind::Unknown
                }
            },
            Expr::CallOptions { base, options } => return self.resolve_expr(db, *base),
            Expr::Call { callee, args } => {
                let args_type = args.iter()
                    .map(|(_, a)| self.resolve_expr(db, *a))
                    .collect();
                return self.resolve_expr_with_args(db, *callee, Ty::new(db, TyKind::Tuple(args_type)));
            },
            Expr::PrefixOp { expr, op } => return self.resolve_expr(db, *expr),
            Expr::PostfixOp { expr, op } => return self.resolve_expr(db, *expr),
            Expr::Ternary { cond, lhs, rhs } => return self.resolve_expr(db, *lhs),
            Expr::Tuple { content } => TyKind::Tuple(content.iter().copied().map(|expr| self.resolve_expr(db, expr)).collect()),
            Expr::Array { content } => TyKind::Array(
                content.get(0)
                    .map(|expr| self.resolve_expr(db, *expr)).unwrap_or_else(|| Ty::new(db, TyKind::Unknown)), 0),
            Expr::BinaryOp { lhs, op, rhs } => {
                if let Some(op) = op {
                    return match op {
                        BinaryOp::LogicOp(logic_op) => self.resolve_expr(db, *lhs),
                        BinaryOp::ArithOp(arith_op) => self.resolve_expr(db, *lhs),
                        BinaryOp::CmpOp(cmp_op) => Ty::new(db, TyKind::Elementary(ElementaryTypeRef::Bool)),
                        BinaryOp::Assignment { op } => self.resolve_expr(db, *rhs),
                    }
                };
                TyKind::Unknown
            },
            Expr::Ident { name_ref } => {
                return match self.scope.lookup_in_expr(db, expr, *name_ref) {
                    Some(Definition::Item(item)) => 
                        self.resolve_item_type(db, item),
                    Some(Definition::Local((origin, item))) =>
                        self.resolve_type_ref(db, self.scope, item.ty(db)),
                    Some(Definition::EnumVariant(_)) |
                        Some(Definition::Field(_)) |
                        None => Ty::new(db, TyKind::Unknown),
                }
            },
            Expr::Literal { data } => match data {
                hir_def::hir::Literal::String(items) => TyKind::Elementary(ElementaryTypeRef::String),
                hir_def::hir::Literal::Number(big_int) => TyKind::Elementary(ElementaryTypeRef::Integer { signed: false, size: 256 }),
                hir_def::hir::Literal::Boolean(_) => TyKind::Elementary(ElementaryTypeRef::Bool),
                hir_def::hir::Literal::HexString(items) => TyKind::Elementary(ElementaryTypeRef::String),
                hir_def::hir::Literal::UnicodeStringLiteral() => TyKind::Elementary(ElementaryTypeRef::String),
                hir_def::hir::Literal::Error => TyKind::Unknown,
            },
            Expr::ElementaryTypeName { data } => TyKind::Elementary(*data),
            Expr::New { ty } => return self.resolve_type_ref(db, self.scope, ty.clone()),
            Expr::Missing => TyKind::Unknown,
        };

        Ty::new(db, kind)
    }

    fn resolve_type_ref(&mut self, db: &'db dyn BaseDb, scope: Scope<'db>, t: TypeRef<'db>) -> Ty<'db> {
        if let Some(t) = self.typeref_map.get(&t) {
            return *t;
        }
        let ty = self.resolve_type_ref_inner(db, scope, t.clone());
        self.typeref_map.insert(t, ty);

        ty
    }

    fn resolve_type_ref_inner(&mut self, db: &'db dyn BaseDb, scope: Scope<'db>, t: TypeRef<'db>) -> Ty<'db> {
        let ty = Ty::new(db, match &t { 
            TypeRef::Elementary(elementary_type_ref) => return Ty::new(db, TyKind::Elementary(*elementary_type_ref)),
            TypeRef::Function { arguments, visibility, mutability, returns } => {
                return unknown(db)
            },
            TypeRef::Mapping { key_type, value_type, .. } => {
                TyKind::Mapping(self.resolve_type_ref(db, scope,  *key_type.clone()), self.resolve_type_ref(db, scope, *value_type.clone()))
            },
            TypeRef::Array { ty, len } => {
                TyKind::Array(self.resolve_type_ref(db, scope, *ty.clone()), 0)
            },
            TypeRef::Path(ref idents) => {
                if let Some(Definition::Item(item)) = Self::resolve_path(db, scope, &*idents) {
                    return self.resolve_item_type(db, item)
                } else {
                    TyKind::Unknown
                }
            },
            TypeRef::Error => TyKind::Unknown,
        });

        ty
    }

    fn resolve_path(db: &'db dyn BaseDb, scope: Scope<'db>, path: &[Ident<'db>]) -> Option<Definition<'db>> {
        let mut path = path.into_iter();
        let start = path.next()?;
        if let mut def @ Definition::Item((mut module, _)) = scope.lookup(db, *start)? {
            'ident_loop: for ident in path {
                for (name, new_def) in def.defs(db,  module) {
                    if *ident == name {
                        def = new_def;
                        module = if let Definition::Item((new_module, _)) = new_def {
                            new_module
                        } else {
                            module
                        };
                        continue 'ident_loop;
                    }
                }
            }

            Some(def)
        } else {
            None
        }
    }

    fn resolve_item_type(&mut self, db: &'db dyn BaseDb, (module, i): (File, Item<'db>)) -> Ty<'db> {
        let kind = match i {
            Item::Contract(contract_id) => TyKind::Contract(module, contract_id),
            Item::Enum(enumeration_id) => TyKind::Enum(module, enumeration_id),
            Item::UserDefinedValueType(user_defined_value_type_id) => TyKind::Unknown,
            Item::StateVariable(state_variable_id) => {
                return self.resolve_type_ref_inner(db, i.scope(db, self.project, module), state_variable_id.ty(db));
            },
            Item::Struct(structure_id) => TyKind::Struct(module, structure_id),
            Item::Module(source_unit) => TyKind::Module(module, source_unit),
            Item::Modifier(modifier_id) => {
                let scope = modifier_id.origin(db)
                    .map(|c| c.scope(db, self.project, module))
                    .unwrap_or_else(|| module.source_unit(db).scope(db, self.project, module));
                let info = modifier_id.info(db);
                let args = info.args.iter()
                    .map(|vardecl| self.resolve_type_ref(db, Scope::Item(scope), vardecl.ty(db)) ).collect();

                TyKind::Modifier(Ty::new(db, TyKind::Tuple(args)))
            } 
            Item::Error(error_id) => TyKind::Error(module, error_id),
            Item::Event(event_id) => TyKind::Event(module, event_id),
            Item::Function(function_id) => {
                let scope = function_id.origin(db)
                    .map(|c| c.scope(db, self.project, module))
                    .unwrap_or_else(|| module.source_unit(db).scope(db, self.project, module));
                let info = function_id.info(db);
                let args = info.args.iter()
                    .map(|vardecl| self.resolve_type_ref(db, Scope::Item(scope), vardecl.ty(db)) ).collect();

                let returns = info.returns.iter().flatten()
                    .map(|vardecl| self.resolve_type_ref(db, Scope::Item(scope), vardecl.ty(db)) ).collect();

                TyKind::Function(Ty::new(db, TyKind::Tuple(args)), Ty::new(db, TyKind::Tuple(returns)))
            } 
            _ => TyKind::Unknown,
        };

        Ty::new(db, kind)
    }
}

#[cfg(test)]
mod tests {
    use base_db::{Project, TestDatabase, TestFixture, VfsPath};
    use hir_def::{hir::HasSourceUnit, items::HirPrint};
    use salsa::Database;

    use super::resolve_item;

    #[test]
    fn basic_goto() {
        let fixture = TestFixture::parse(r"
            struct TestStruct {
                uint256 field;
                uint128 field2;
                uint256 field3;
                uint256 field4;
                uint256 field5;
            }

            contract Test is
                ReentrancyGuardUpgradeable,
                ERC2771ContextUpgradeable(address(0))
            {
                uint64 testvar = 1;

                function h$0elloWorld(IERC20 memory tmp) {
                    TestStruct memory test = 1;
                    test.field;
                }
            }
        ");
        let pos = fixture.position.unwrap();
        let (db, file) = TestDatabase::from_fixture(fixture);
        let item = file.source_unit(&db).source_map(&db).find_pos(pos);
        let types = resolve_item(&db, Project::new(&db, VfsPath::from_virtual("".to_owned())), item.unwrap(), file);
        for (expr, ty) in types.expr_map(&db).0 {
            let mut t = String::new();
            expr.write(&db, &mut t, 0).unwrap();
        }
    }
}