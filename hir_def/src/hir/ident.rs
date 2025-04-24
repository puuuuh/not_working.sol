use salsa::Database;
use std::fmt::Write;
use syntax::ast::nodes;
use syntax::SyntaxToken;

use crate::items::HirPrint;

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct IdentPath<'db>(pub Vec<Ident<'db>>);

impl HirPrint for IdentPath<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        for (i, a) in self.0.iter().enumerate() {
            if i > 0 {
                w.write_str(".")?;
            }
            a.write(db, w, ident)?;
        }
        Ok(())
    }
}

impl<'db> IdentPath<'db> {
    pub fn from(db: &'db dyn Database, data: nodes::IdentPath) -> Self {
        let segments = data.segment().map(|s| Ident::from_name_ref(db, Some(s))).collect();
        Self(segments)
    }

    pub fn from_opt(db: &'db dyn Database, data: Option<nodes::IdentPath>) -> Self {
        let segments = data
            .into_iter()
            .flat_map(|a| a.segment().map(|s| Ident::from_name_ref(db, Some(s))))
            .collect();
        Self(segments)
    }
}

#[salsa::interned(debug)]
pub struct Ident<'db> {
    #[return_ref]
    pub data: String,
}

impl HirPrint for Ident<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, _ident: usize) -> std::fmt::Result {
        w.write_str(self.data(db))
    }
}

impl<'db> Ident<'db> {
    pub fn missing(db: &'db dyn Database) -> Ident<'db> {
        Self::new(db, "<missing>")
    }
}

impl<'db> Ident<'db> {
    pub fn from_symbol(db: &'db dyn Database, data: Option<nodes::Symbol>) -> Self {
        Self::from_token(db, data.and_then(|n| n.ident_token()))
    }

    pub fn from_name_ref(db: &'db dyn Database, data: Option<nodes::NameRef>) -> Self {
        Self::from_token(db, data.and_then(|n| n.ident_token()))
    }

    pub fn from_name(db: &'db dyn Database, data: Option<nodes::Name>) -> Self {
        Self::from_token(db, data.and_then(|n| n.ident_token()))
    }
    
    pub fn from_name_opt(db: &'db dyn Database, data: Option<nodes::Name>) -> Option<Self> {
        if data.is_none() {
            None
        } else {
            Some(Self::from_token(db, data.and_then(|n| n.ident_token())))
        }
    }

    pub fn from_name_ref_opt(db: &'db dyn Database, data: Option<nodes::NameRef>) -> Option<Self> {
        if data.is_none() {
            None
        } else {
            Some(Self::from_token(db, data.and_then(|n| n.ident_token())))
        }
    }

    pub fn from_token(db: &'db dyn Database, data: Option<SyntaxToken>) -> Self {
        Self::from_str(db, data.as_ref().map(|t| t.text()))
    }

    pub fn from_str(db: &'db dyn Database, data: Option<&str>) -> Self {
        data.map(|t| Self::new(db, t)).unwrap_or(Self::missing(db))
    }
}
