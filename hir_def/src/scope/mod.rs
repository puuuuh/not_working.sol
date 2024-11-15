pub mod expr;
pub mod item;
pub mod resolver;

use indexmap::IndexMap;
use salsa::Update;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexMapUpdate<T: Hash + Eq, T1: PartialOrd>(pub IndexMap<T, T1>);

impl<T: Hash + Eq, T1: PartialOrd + Hash> Hash for IndexMapUpdate<T, T1> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in self.0.iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

unsafe impl<K, V> Update for IndexMapUpdate<K, V>
where
    K: Hash + Eq,
    V: PartialOrd<V>,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_map: &mut IndexMapUpdate<K, V> = unsafe { &mut *old_pointer };

        if old_map.0 != new_value.0 {
            *old_pointer = new_value;
            return true;
        }

        false
    }
}
