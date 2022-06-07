use std::hash::Hash;
use std::{collections::hash_map::DefaultHasher, hash::Hasher};

pub fn hash_id_value<T: Hash>(t: T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
