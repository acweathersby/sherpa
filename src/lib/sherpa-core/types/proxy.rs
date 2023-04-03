// Creating type aliases of common collections in the
// event we decide to use alternate implementations.

pub type Map<K, V> = ::std::collections::HashMap<K, V>;
pub type Set<K> = ::std::collections::HashSet<K>;
pub type OrderedMap<K, V> = ::std::collections::BTreeMap<K, V>;
pub type OrderedSet<K> = std::collections::BTreeSet<K>;
pub type Array<V> = ::std::vec::Vec<V>;
pub type Queue<V> = ::std::collections::VecDeque<V>;
