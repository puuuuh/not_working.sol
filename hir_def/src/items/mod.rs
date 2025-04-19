use salsa::Database;
use std::fmt::Write;

pub trait HirPrint {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result;
}