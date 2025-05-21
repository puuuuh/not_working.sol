use std::env::var;

use base_db::{BaseDb, File};
use salsa::tracked;
use tys::{Ty, TyKind};

pub mod callable;
pub mod error;
pub mod resolver;
pub mod type_check;
pub mod tys;
pub mod member_kind;
pub mod extensions;