use crate::hir::ident::Ident;
use crate::hir::import::{ImportId, ImportKind, SymbolAlias};
use crate::hir::source_unit::Item;
use crate::lower::literal::decode_string_literal;
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use std::path::PathBuf;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes;

impl<'a> LowerCtx<'a> {
    pub fn lower_path(&mut self, s: nodes::Path) -> String {
        s.string_token()
            .map(|s| {
                String::from_utf8(decode_string_literal(s.text())).unwrap()
            })
            .unwrap_or_default()
    }

    pub fn lower_import(&mut self, s: nodes::Import) -> ImportId<'a> {
        let Some(item) = s.import_item() else {
            return ImportId::new(self.db, ImportKind::Error, AstPtr::new(&s));
        };
        let kind = match item {
            nodes::ImportItem::ImportPath(path) => ImportKind::Path {
                path: path
                    .path()
                    .map(|p| self.lower_path(p))
                    .unwrap_or_default(),
                name: path.name().map(|a| Ident::from_name(self.db, Some(a))),
            },
            nodes::ImportItem::ImportSymbols(data) => {
                if let Some(alias) = data.symbol_alias() {
                    ImportKind::Glob {
                        as_name: Ident::from_name(self.db, alias.name()),
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
                                name: Ident::from_symbol(self.db, a.symbol()),
                                as_name: Ident::from_name_opt(self.db, a.name()),
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
        let res = ImportId::new(self.db, kind, AstPtr::new(&s));
        self.save_span(s.syntax().text_range(), Item::Import(res));
        res
    }
}
