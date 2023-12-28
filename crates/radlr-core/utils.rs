//! Common Functions that are used throughout the library
use std::{
  collections::{btree_map::Entry, hash_map::DefaultHasher, BTreeMap},
  hash::{Hash, Hasher},
  vec,
};

#[inline]
pub fn hash_group_btree_iter<
  C: Extend<T> + Default,
  T: Sized,
  R: Hash + Sized + Ord + Eq,
  I: Iterator<Item = T>,
  Function: FnMut(usize, &T) -> R,
>(
  iter: I,
  mut hash_yielder: Function,
) -> BTreeMap<R, C> {
  let mut hash_groups = BTreeMap::<R, C>::new();

  for (i, val) in iter.enumerate() {
    match hash_groups.entry(hash_yielder(i, &val)) {
      Entry::Vacant(e) => {
        let mut new_container = C::default();
        new_container.extend(vec![val]);
        e.insert(new_container);
      }
      Entry::Occupied(mut e) => e.get_mut().extend(vec![val]),
    }
  }

  hash_groups
}

#[inline]
pub fn hash_group_btreemap<
  T: Sized,
  R: Hash + Sized + Ord + Eq,
  E: IntoIterator<Item = T> + Extend<T> + Default,
  Function: FnMut(usize, &T) -> R,
>(
  vector: E,
  mut hash_yielder: Function,
) -> BTreeMap<R, E> {
  let mut hash_groups = BTreeMap::<R, E>::new();

  for (i, val) in vector.into_iter().enumerate() {
    match hash_groups.entry(hash_yielder(i, &val)) {
      Entry::Vacant(e) => {
        let mut new_container = E::default();
        new_container.extend(vec![val]);
        e.insert(new_container);
      }
      Entry::Occupied(mut e) => e.get_mut().extend(vec![val]),
    }
  }

  hash_groups
}

pub fn create_u64_hash<T: Hash>(t: T) -> u64 {
  let mut s = DefaultHasher::new();

  t.hash(&mut s);

  s.finish()
}
