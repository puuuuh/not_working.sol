use base_db::BaseDb;
use hir_def::Ident;

use super::{ItemScope, body::Declaration};

pub fn prelude<'db>(db: &'db dyn BaseDb) -> ItemScope<'db> {
    ItemScope::new(
        db,
        None,
        [
            (
                Ident::new(db, "block"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Block)].into_iter().collect(),
            ),
            (
                Ident::new(db, "tx"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Tx)].into_iter().collect(),
            ),
            (
                Ident::new(db, "msg"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Msg)].into_iter().collect(),
            ),
            (
                Ident::new(db, "abi"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Abi)].into_iter().collect(),
            ),
            (
                Ident::new(db, "keccak256"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Keccak256)].into_iter().collect(),
            ),
            (
                Ident::new(db, "sha256"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Sha256)].into_iter().collect(),
            ),
            (
                Ident::new(db, "gasleft"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Gasleft)].into_iter().collect(),
            ),
            (
                Ident::new(db, "assert"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Assert)].into_iter().collect(),
            ),
            (
                Ident::new(db, "require"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Require), Declaration::Magic(super::body::MagicDefinitionKind::RequireWithMessage)].into_iter().collect(),
            ),
            (
                Ident::new(db, "revert"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Revert), Declaration::Magic(super::body::MagicDefinitionKind::RevertWithMessage)].into_iter().collect(),
            ),
            (
                Ident::new(db, "addmod"),
                [Declaration::Magic(super::body::MagicDefinitionKind::AddMod)].into_iter().collect(),
            ),
            (
                Ident::new(db, "mulmod"),
                [Declaration::Magic(super::body::MagicDefinitionKind::MulMod)].into_iter().collect(),
            ),
            (
                Ident::new(db, "ripemd160"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Ripemd160)].into_iter().collect(),
            ),
            (
                Ident::new(db, "ecrecover"),
                [Declaration::Magic(super::body::MagicDefinitionKind::Ecrecover)].into_iter().collect(),
            ),
        ]
        .into_iter()
        .collect(),
    )
}