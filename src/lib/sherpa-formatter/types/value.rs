use std::{collections::HashMap, fmt::Debug};

use sherpa_core::{CachedString, IString, IStringStore};

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

  fn get_len<'scope>(&'scope self) -> usize {
    1
  }
}

impl<'a> ValueObj for HashMap<String, Value<'a>> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    match self.get(key) {
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

impl<'a> ValueObj for HashMap<IString, Value<'a>> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    match self.get(&key.to_token()) {
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

impl<'a> ValueObj for HashMap<&str, Value<'a>> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    match self.get(&key) {
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

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {
    for (key, val) in self.iter() {
      (*funct)(key.to_string(), *val)
    }
  }
}

impl<'a> ValueObj for HashMap<String, &'a dyn ValueObj> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    match self.get(key) {
      None => Value::None,
      Some(obj) => Value::Obj(*obj),
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "obj"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {
    for (key, val) in self.iter() {
      (*funct)(key.to_string(), Value::Obj(*val))
    }
  }
}

impl<'a> ValueObj for HashMap<&str, &'a dyn ValueObj> {
  fn get_val<'scope>(&'scope self, key: &str, _: &IStringStore) -> Value<'scope> {
    match self.get(key) {
      None => Value::None,
      Some(obj) => Value::Obj(*obj),
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "obj"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len()
  }

  fn vals<'scope>(&'scope self, funct: &mut dyn FnMut(String, Value<'scope>)) {
    for (key, val) in self.iter() {
      (*funct)(key.to_string(), Value::Obj(*val))
    }
  }
}

impl<'a, T: ValueObj> ValueObj for Vec<T> {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if let Some(obj) = self.get(index) {
      Value::Obj(obj)
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

impl<'a, T: ValueObj> ValueObj for &[T] {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if let Some(obj) = self.get(index) {
      Value::Obj(obj)
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

impl<'a> ValueObj for Vec<&'a dyn ValueObj> {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if let Some(obj) = self.get(index) {
      Value::Obj(*obj)
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

impl<'a> ValueObj for &[&'a dyn ValueObj] {
  fn get_index<'scope>(&'scope self, index: usize, _: &IStringStore) -> Value<'scope> {
    if let Some(obj) = self.get(index) {
      Value::Obj(*obj)
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
    if let Some(obj) = self.get(index) {
      *obj
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
    if let Some(obj) = self.get(index) {
      *obj
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
