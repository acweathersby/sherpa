use sherpa_core::{proxy::OrderedMap, IString, IStringStore};
use std::{cmp::Ordering, collections::HashMap, fmt::Debug, hash::Hash};

#[derive(Default, Clone, Copy, Debug)]
pub enum Value<'scope> {
  Obj(&'scope dyn ValueObj),
  Num(f64),
  Int(isize),
  Str(IString),
  #[default]
  None,
}

pub trait ValueObj: Debug {
  #[allow(unused)]
  fn get_val<'scope>(&'scope self, key: &str, s_store: &IStringStore) -> Value<'scope> {
    Value::None
  }

  #[allow(unused)]
  fn get_index<'scope>(&'scope self, index: usize, s_store: &IStringStore) -> Value<'scope> {
    Value::None
  }

  #[allow(unused)]
  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {}

  fn get_type<'scope>(&'scope self) -> &str {
    "undefined"
  }

  fn get_keys<'scope>(&'scope self) -> &'static [&'static str] {
    &["type", "len"]
  }

  fn get_len<'scope>(&'scope self) -> usize {
    1
  }
}

impl<'a, K: From<String> + Eq + PartialEq + Hash + Ord + Debug, V: ValueObj> ValueObj for HashMap<K, V> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    let v = key.to_string().into();
    match self.get(&v) {
      None => Value::None,
      Some(val) => Value::Obj(val),
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "obj"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a, K: From<String> + Eq + PartialEq + Hash + Ord + Debug> ValueObj for HashMap<K, Value<'a>> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    let v = key.to_string().into();
    match self.get(&v) {
      None => Value::None,
      Some(val) => *val,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "obj"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a, K: From<String> + Debug + Ord> ValueObj for OrderedMap<K, Value<'a>> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    let v = key.to_string().into();
    match self.get(&v) {
      None => Value::None,
      Some(val) => *val,
    }
  }

  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    match self.iter().enumerate().find(|(i, _)| *i == index) {
      None => Value::None,
      Some((_, (_, val))) => *val,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "obj"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a, K: From<String> + Copy + Debug + Ord, V: ValueObj> ValueObj for OrderedMap<K, V> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    let v = key.to_string().into();
    match self.get(&v) {
      None => Value::None,
      Some(val) => Value::Obj(val),
    }
  }

  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    match self.iter().enumerate().find(|(i, _)| *i == index) {
      None => Value::None,
      Some((_, (_, val))) => Value::Obj(val),
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "obj"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a, V: ValueObj> ValueObj for Vec<V> {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if index < self.len() {
      Value::Obj(&self[index])
    } else {
      Value::None
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "list"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a, V: ValueObj> ValueObj for &[V] {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if index < self.len() {
      Value::Obj(&self[index])
    } else {
      Value::None
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "list"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a> ValueObj for Vec<Value<'a>> {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if index < self.len() {
      self[index]
    } else {
      Value::None
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "list"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

impl<'a> ValueObj for &[Value<'a>] {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if index < self.len() {
      self[index]
    } else {
      Value::None
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "list"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }
}

#[macro_export]
macro_rules! formatted_typed_vector {
  ($name: ident, $element_type:ty, $type_name:literal, $attr:meta) => {
    #[$attr]
    pub struct $name(pub(crate) Vec<$element_type>);

    impl std::ops::Deref for $name {
      type Target = Vec<$element_type>;

      fn deref(&self) -> &Vec<$element_type> {
        &self.0
      }
    }

    impl std::ops::DerefMut for $name {
      fn deref_mut(&mut self) -> &mut Vec<$element_type> {
        &mut self.0
      }
    }

    impl std::ops::Index<usize> for $name {
      type Output = $element_type;

      fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
      }
    }

    impl std::ops::IndexMut<usize> for $name {
      fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
      }
    }

    impl ValueObj for $name {
      fn get_index<'scope>(&'scope self, index: usize, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
        self.0.get_index(index, s_store)
      }

      fn get_len<'scope>(&'scope self) -> usize {
        self.0.get_len()
      }

      fn get_type<'scope>(&'scope self) -> &str {
        $type_name
      }

      fn get_val<'scope>(&'scope self, key: &str, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
        self.0.get_val(key, s_store)
      }
    }
  };
  ($name: ident, $element_type:ty, $type_name:literal) => {
    formatted_typed_vector!($name, $element_type, $type_name, derive(Debug, Default));
  };
}

#[macro_export]
macro_rules! formatted_typed_hash_map {
  ($name: ident, $key_type:ty, $val_type:ty, $type_name:literal) => {
    #[derive(Debug, Default)]
    pub struct $name(pub(crate) HashMap<$key_type, $val_type>);

    impl std::ops::Deref for $name {
      type Target = HashMap<$key_type, $val_type>;

      fn deref(&self) -> &HashMap<$key_type, $val_type> {
        &self.0
      }
    }

    impl std::ops::DerefMut for $name {
      fn deref_mut(&mut self) -> &mut HashMap<$key_type, $val_type> {
        &mut self.0
      }
    }

    impl ValueObj for $name {
      fn get_index<'scope>(&'scope self, index: usize, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
        self.0.get_index(index, s_store)
      }

      fn get_len<'scope>(&'scope self) -> usize {
        self.0.get_len()
      }

      fn get_type<'scope>(&'scope self) -> &str {
        $type_name
      }

      fn get_val<'scope>(&'scope self, key: &str, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
        self.0.get_val(key, s_store)
      }
    }
  };
}

#[macro_export]
macro_rules! formatted_typed_ordered_map {
  ($name: ident, $key_type:ty, $val_type:ty, $type_name:literal) => {
    #[derive(Debug, Default)]
    pub struct $name(pub(crate) OrderedMap<$key_type, $val_type>);

    impl std::ops::Deref for $name {
      type Target = OrderedMap<$key_type, $val_type>;

      fn deref(&self) -> &OrderedMap<$key_type, $val_type> {
        &self.0
      }
    }

    impl std::ops::DerefMut for $name {
      fn deref_mut(&mut self) -> &mut OrderedMap<$key_type, $val_type> {
        &mut self.0
      }
    }

    impl ValueObj for $name {
      fn get_index<'scope>(&'scope self, index: usize, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
        self.0.get_index(index, s_store)
      }

      fn get_len<'scope>(&'scope self) -> usize {
        self.0.get_len()
      }

      fn get_type<'scope>(&'scope self) -> &str {
        $type_name
      }

      fn get_val<'scope>(&'scope self, key: &str, s_store: &sherpa_core::IStringStore) -> Value<'scope> {
        self.0.get_val(key, s_store)
      }
    }
  };
}
