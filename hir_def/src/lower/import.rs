use crate::hir::ident::Ident;
use crate::hir::import::{ImportId, ImportKind, SymbolAlias};
use crate::lower::literal::decode_string_literal;
use crate::lower::Ctx;
use crate::FileAstPtr;
use std::path::PathBuf;
use rowan::ast::AstNode;
use syntax::ast::nodes;
use crate::semantics::child_container::ChildSource;

impl<'a> Ctx<'a> {
    pub fn lower_path(&mut self, s: nodes::Path) -> String {
        s.string_token()
            .map(|s| {
                String::from_utf8(decode_string_literal(s.text())).unwrap()
            })
            .unwrap_or_default()
    }

    pub fn lower_import(&mut self, s: nodes::Import) -> ImportId<'a> {
        let Some(item) = s.import_item() else {
            return ImportId::new(self.db, ImportKind::Error, FileAstPtr::new(self.file, &s));
        };
        let kind = match item {
            nodes::ImportItem::ImportPath(path) => ImportKind::Path {
                path: path
                    .path()
                    .map(|p| self.lower_path(p))
                    .unwrap_or_default(),
                name: path.name().map(|a| Ident::from_name(self.db.as_dyn_database(), Some(a))),
            },
            nodes::ImportItem::ImportSymbols(data) => {
                if let Some(alias) = data.symbol_alias() {
                    ImportKind::Glob {
                        as_name: Ident::from_name(self.db.as_dyn_database(), alias.name()),
                        path: data
                            .path()
                            .map(|p| self.lower_path(p))
                            .unwrap_or_default(),
                    }
                } else if let Some(aliases) = data.symbol_aliases() {
                    ImportKind::Aliases {
                        symbol_aliases: aliases
                            .symbol_aliases()
                            .map(|a| SymbolAlias {
                                name: Ident::from_symbol(self.db.as_dyn_database(), a.symbol()),
                                as_name: Ident::from_name_opt(self.db.as_dyn_database(), a.name()),
                            })
                            .collect(),
                        path: data
                            .path()
                            .map(|p| self.lower_path(p))
                            .unwrap_or_default(),
                    }
                } else {
                    ImportKind::Error
                }
            }
        };
        let res = ImportId::new(self.db, kind, FileAstPtr::new(self.file, &s));
        self.save_span(s.syntax().text_range(), ChildSource::Import(res));
        res
    }
}
