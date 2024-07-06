// Creating type aliases of common collections in the
// event we decide to use alternate implementations.

pub type Map<K, V> = ::std::collections::HashMap<K, V>;
pub type Set<K> = ::std::collections::HashSet<K>;
pub type OrderedMap<K, V> = ::std::collections::BTreeMap<K, V>;
pub type OrderedSet<K> = std::collections::BTreeSet<K>;
pub type Array<V> = ::std::vec::Vec<V>;
pub type Queue<V> = ::std::collections::VecDeque<V>;
pub type StandardHasher = ::std::collections::hash_map::DefaultHasher;

pub(crate) trait DeduplicateIterator<T: std::hash::Hash + Clone>: Iterator<Item = T> + Sized + Clone {
  fn dedup<U: FromIterator<T>>(self) -> U {
    let mut seen = Set::new();
    let mut out = Array::default();
    for err in self {
      if seen.insert(crate::utils::create_u64_hash(&err)) {
        out.push(err);
      }
    }

    U::from_iter(out.into_iter())
  }
}

impl<U: std::hash::Hash + Clone, T: Iterator<Item = U> + Sized + Clone> DeduplicateIterator<U> for T {}
