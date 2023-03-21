// Creating type aliases of common collections in the
// event we decide to use alternate implementations.

pub(crate) type Map<K, V> = ::std::collections::HashMap<K, V>;
pub(crate) type Set<K> = ::std::collections::HashSet<K>;
pub(crate) type OrderedMap<K, V> = ::std::collections::BTreeMap<K, V>;
pub(crate) type OrderedSet<K> = std::collections::BTreeSet<K>;
pub(crate) type Array<V> = ::std::vec::Vec<V>;
pub(crate) type Queue<V> = ::std::collections::VecDeque<V>;
