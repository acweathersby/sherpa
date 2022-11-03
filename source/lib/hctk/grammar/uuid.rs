//! Functions for creating a variety of universally unique identifiers
//! for grammar primitives.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;

use crate::types::ParseError;

pub fn hash_id_value_u64<T: Hash>(t: T) -> u64 {
  let mut s = DefaultHasher::new();

  t.hash(&mut s);

  s.finish()
}

pub fn hash_id_value_u128<T: Hash>(t: T) -> u128 {
  0
}

pub fn get_guid_grammar_name(uri: &PathBuf) -> Result<String, ParseError> {
  match uri.file_stem() {
    Some(name) => {
      let file_name = String::from(name.to_str().unwrap());

      Ok(unsafe { format!("{}_{:05X}", file_name, hash_id_value_u64(&uri)) })
    }
    None => Ok("undefined".to_string()),
  }
}
