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
        ]
        .into_iter()
        .collect(),
    )
}
