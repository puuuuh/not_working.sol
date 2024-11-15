#[macro_export]
macro_rules! lazy_field {
    ($name:ty, $getter:ident, $setter:ident, Option<$ty:ty>) => {
        lazy_field!($name, $getter, $setter, Option<$ty>, None);
    };
    ($name:ty, $getter:ident, $setter:ident, $ty:ty) => {
        lazy_field!($name, $getter, $setter, $ty, panic!("undefined"));
    };
    ($name:ty, $getter:ident, $setter:ident, $ty:ty, $default:expr) => {
        const _: () = {
            #[salsa::tracked(specify)]
            fn getter<'db>(_a: &'db dyn salsa::Database, _b: $name) -> $ty {
                $default
            }

            impl<'db> $name {
                pub fn $getter(&self, db: &'db dyn salsa::Database) -> $ty {
                    getter(db, *self)
                }
                pub fn $setter(&self, db: &dyn salsa::Database, v: $ty) {
                    getter::specify(db, *self, v);
                }
            }
        };
    };
}
