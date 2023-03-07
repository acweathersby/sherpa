//! Functions for creating a variety of universally unique identifiers
//! for grammar primitives.
use std::{
  collections::hash_map::DefaultHasher,
  hash::{Hash, Hasher},
  path::PathBuf,
};

pub fn hash_values(values: &[&dyn Fn(&mut DefaultHasher)]) -> u64 {
  let mut s = DefaultHasher::new();
  for value_getter in values {
    value_getter(&mut s);
  }
  s.finish()
}

pub fn hash_id_value_u64<T: Hash>(t: T) -> u64 {
  let mut s = DefaultHasher::new();

  t.hash(&mut s);

  s.finish()
}

pub fn get_guid_grammar_name(uri: &PathBuf) -> String {
  match uri.file_stem() {
    Some(name) => {
      let file_name = String::from(name.to_str().unwrap());
      format!("{}_{:05X}", file_name, hash_id_value_u64(&uri))
    }
    None => "undefined".to_string(),
  }
}
