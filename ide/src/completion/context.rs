use std::sync::Arc;

use base_db::BaseDb;
use hir_def::source_map::item_source_map::ItemSourceMap;
use hir_def::{
    lower_file, Expr, ExprId, FileExt, FilePosition, Ident, IdentPath, Item, StatementId,
};
use hir_nameres::container::Container;
use hir_nameres::scope::body::{Declaration, MagicDefinitionKind};
use hir_nameres::scope::{HasScope, Scope};
use hir_ty::extensions::Extensions;
use hir_ty::member_kind::MemberKind;
use hir_ty::tys::Ty;
use rowan::ast::{AstNode, AstPtr};
use rowan::{TextRange, TextSize};
use syntax::ast::nodes;
use syntax::{SyntaxKind, SyntaxToken};

use super::item::Completion;

#[derive(Debug, Clone)]
enum CompletionKind<'db> {
    DotAccess { receiver: ExprId<'db>, receiver_ty: Ty<'db> },
    Path { receiver: Vec<Ident<'db>> },
    Name,
    Unknown,
}

pub struct CompletionCtx<'db> {
    pub db: &'db dyn BaseDb,
    pub item: Item<'db>,
    pub pos: TextRange,
    pub token: SyntaxToken,
}

fn find_expr<'db>(
    db: &'db dyn BaseDb,
    token: SyntaxToken,
    body: StatementId<'db>,
    source_map: ItemSourceMap<'db>,
    mut scope: Scope<'db>,
) -> Option<ExprId<'db>> {
    let e = token.parent_ancestors().find_map(nodes::Expr::cast)?;

    source_map.expr(db, AstPtr::new(&e))
}

fn prev_token(token: &SyntaxToken) -> Option<SyntaxToken> {
    let mut prev = token.prev_token();
    while prev.as_ref().map(|t| t.kind()) == Some(SyntaxKind::WHITESPACE) {
        prev = prev.unwrap().prev_token();
    }
    prev
}

fn classify_item(item: &Item) -> super::CompletionKind {
    match item {
        Item::Contract(contract_id) => super::CompletionKind::Class,
        Item::Enum(enumeration_id) => super::CompletionKind::Enum,
        Item::UserDefinedValueType(user_defined_value_type_id) => super::CompletionKind::Item,
        Item::Error(error_id) => super::CompletionKind::Class,
        Item::Event(event_id) => super::CompletionKind::Event,
        Item::Function(function_id) => super::CompletionKind::Function,
        Item::StateVariable(state_variable_id) => super::CompletionKind::Variable,
        Item::Struct(structure_id) => super::CompletionKind::Struct,
        Item::Constructor(constructor_id) => super::CompletionKind::Constructor,
        Item::Modifier(modifier_id) => super::CompletionKind::Method,
        Item::Module(source_unit) => super::CompletionKind::Module,
        _ => super::CompletionKind::Value
    }
}

fn classify_declaration(decl: &Declaration) -> super::CompletionKind {
    match decl {
        Declaration::Item(item) => classify_item(item),
        Declaration::Local(variable_declaration) => super::CompletionKind::Variable,
        Declaration::Magic(magic_definition_kind) => classify_magic_definition(magic_definition_kind),
    }
}

fn classify_magic_definition(def: &MagicDefinitionKind) -> super::CompletionKind {
    match def {
        MagicDefinitionKind::Block => super::CompletionKind::Value,
        MagicDefinitionKind::Msg => super::CompletionKind::Value,
        MagicDefinitionKind::Tx => super::CompletionKind::Value,
        MagicDefinitionKind::Abi => super::CompletionKind::Module,
        MagicDefinitionKind::Keccak256 => super::CompletionKind::Function,
        MagicDefinitionKind::Sha256 => super::CompletionKind::Function,
        MagicDefinitionKind::Gasleft => super::CompletionKind::Function,
        MagicDefinitionKind::Assert => super::CompletionKind::Function,
        MagicDefinitionKind::Require => super::CompletionKind::Function,
        MagicDefinitionKind::RequireWithMessage => super::CompletionKind::Function,
        MagicDefinitionKind::Revert => super::CompletionKind::Function,
        MagicDefinitionKind::RevertWithMessage =>super::CompletionKind::Function,
        MagicDefinitionKind::AddMod =>super::CompletionKind::Function,
        MagicDefinitionKind::MulMod => super::CompletionKind::Function,
        MagicDefinitionKind::Ripemd160 => super::CompletionKind::Function,
        MagicDefinitionKind::Ecrecover => super::CompletionKind::Function,
    }
}

fn classify_member_kind<'db>(db: &'db dyn BaseDb, data: &MemberKind) -> super::CompletionKind {
    match data {
        MemberKind::Item(item) => {
            classify_item(item)
        },
        MemberKind::ExtensionFunction(function_id, callable) => {
            super::CompletionKind::Function
        },
        MemberKind::Field(structure_field_id) => {
            super::CompletionKind::Field
        },
        MemberKind::EnumVariant(enumeration_variant_id) => {
            super::CompletionKind::EnumMember
        },
        MemberKind::SynteticItem(ty) => {
            match ty.kind(db) {
                hir_ty::tys::TyKind::Unknown => super::CompletionKind::Value,
                hir_ty::tys::TyKind::Any => super::CompletionKind::Value,
                hir_ty::tys::TyKind::Callable(callable) => super::CompletionKind::Function,
                hir_ty::tys::TyKind::Modifier(callable) => super::CompletionKind::Function,
                hir_ty::tys::TyKind::Error => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Struct(structure_id) => super::CompletionKind::Struct,
                hir_ty::tys::TyKind::Event => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Contract(contract_id) => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Elementary(elementary_type_ref) => super::CompletionKind::Class,
                hir_ty::tys::TyKind::UserDefinedValueType(user_defined_value_type_id) => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Enum(enumeration_id) => super::CompletionKind::Enum,
                hir_ty::tys::TyKind::Array(ty_kind_interned, _) => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Mapping(ty_kind_interned, ty_kind_interned1) => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Tuple(small_vec) => super::CompletionKind::Class,
                hir_ty::tys::TyKind::Type(item) => classify_item(&item),
                hir_ty::tys::TyKind::Magic(magic_definition_kind) => classify_magic_definition(&magic_definition_kind),
            }
        },
    }
}

impl<'db> CompletionCtx<'db> {
    pub fn new(db: &'db dyn BaseDb, pos: FilePosition) -> Option<Self> {
        let source_unit = lower_file(db, pos.file);
        let n = pos.file.node(db);
        let token = n.syntax().token_at_offset(pos.offset).next()?;
        let map = source_unit.source_map(db);
        let item = map.find_pos(pos.offset)?;
        let pos = if token.kind() == SyntaxKind::IDENT {
            token.text_range()
        } else {
            TextRange::at(pos.offset, 0.into())
        };

        Some(Self { db, item, pos, token })
    }

    pub fn completions(&'db self) -> Option<Vec<Completion>> {
        Some(match self.kind()? {
            CompletionKind::DotAccess { receiver, receiver_ty } => {
                let c = receiver_ty.members(self.db, Extensions::empty());
                c.iter()
                    .flat_map(|(ident, data)| {
                        data.iter().map(|elem| (*ident, elem))
                    })
                    .map(|(name, elem)| {
                        Completion {
                            label: name.data(self.db).clone(),
                            src_range: self.pos,
                            text: name.data(self.db).clone(),
                            kind: classify_member_kind(self.db, elem)
                        }
                })
                    .collect()
            }
            CompletionKind::Path { receiver } => {
                let s = self.item.scope(self.db).lookup_path(self.db, &receiver)?;
                let c = Container::try_from(self.item).ok()?;
                c.defs(self.db)
                    .into_iter()
                    .map(|a| Completion {
                        label: a.0.data(self.db).clone(),
                        src_range: self.pos,
                        text: a.0.data(self.db).clone(),
                        kind: classify_item(&a.1)
                    })
                    .collect()
            }
            CompletionKind::Name => {
                let mut s = self.item.scope(self.db);
                if let Some((_, source_map)) = self.item.body(self.db) {
                    for ancestor in self.token.parent_ancestors() {
                        if nodes::Expr::can_cast(ancestor.kind()) {
                            if let Some(expr) = source_map
                                .expr(self.db, AstPtr::new(&nodes::Expr::cast(ancestor).unwrap()))
                            {
                                s = s.for_expr(self.db, expr);
                            }
                            break;
                        } else if let Some(stmt) = nodes::Stmt::cast(ancestor) {
                            if let Some(stmt) = source_map.stmt(self.db, AstPtr::new(&stmt)) {
                                s = s.for_stmt(self.db, stmt);
                            }
                            break;
                        }
                    }
                }

                s.all_definitions(self.db)
                    .into_iter()
                    .map(|a| Completion {
                        label: a.0.data(self.db).clone(),
                        src_range: self.pos,
                        text: a.0.data(self.db).clone(),
                        kind: classify_declaration(a.1.first().unwrap()),
                    })
                    .collect()
            }
            CompletionKind::Unknown => return None,
        })
    }

    fn kind(&'db self) -> Option<CompletionKind<'db>> {
        if let Some(expr) = self.receiver_expr() {
            let type_inference = hir_ty::resolver::resolve_item(self.db, self.item);
            let ty = type_inference.expr(self.db, expr);
            if !ty.is_unknown(self.db) {
                return Some(CompletionKind::DotAccess { receiver: expr, receiver_ty: ty });
            }
        }
        if let Some(path) = self.parent_ident_path() {
            return Some(CompletionKind::Path { receiver: path });
        }

        Some(CompletionKind::Name)
    }

    fn parent_ident_path(&self) -> Option<Vec<Ident<'db>>> {
        let mut res = vec![];
        let mut token = self.token.clone();
        if token.kind() != SyntaxKind::DOT {
            token = prev_token(&token)?;
        }
        if token.kind() != SyntaxKind::DOT {
            return None;
        }
        token = prev_token(&token)?;
        loop {
            if token.kind() != SyntaxKind::IDENT {
                break;
            }

            res.push(Ident::from_str(self.db, Some(token.text())));

            if let Some(p) = prev_token(&token) {
                token = p;
            } else {
                break;
            }

            if token.kind() != SyntaxKind::DOT {
                break;
            }

            if let Some(p) = prev_token(&token) {
                token = p;
            } else {
                break;
            }
        }
        res.reverse();

        Some(res)
    }

    fn receiver_expr(&self) -> Option<ExprId<'db>> {
        let mut token = self.token.clone();
        if token.kind() != SyntaxKind::DOT {
            token = prev_token(&token)?;
        }
        if token.kind() != SyntaxKind::DOT {
            return None;
        }
        token = prev_token(&token)?;

        let e = token.parent_ancestors().find_map(nodes::Expr::cast)?;
        let (stmt, map) = self.item.body(self.db)?;

        map.expr(self.db, AstPtr::new(&e))
    }
}
