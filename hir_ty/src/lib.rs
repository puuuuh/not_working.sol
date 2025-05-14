use std::env::var;

use base_db::{BaseDb, File, Project};
use salsa::tracked;
use tys::{Ty, TyKind};

pub mod error;
pub mod resolver;
pub mod tys;

pub mod type_check;