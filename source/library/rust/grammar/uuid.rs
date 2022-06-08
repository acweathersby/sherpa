//! Functions for creating a variety of universally unique identifiers for
//! grammar primitives.

use std::hash::Hash;
use std::path::PathBuf;
use std::{collections::hash_map::DefaultHasher, hash::Hasher};

use super::parse::ParseError;

pub fn hash_id_value<T: Hash>(t: T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub fn get_uuid_grammar_name(uri: &PathBuf) -> Result<String, ParseError> {
    match uri.file_name() {
        Some(name) => {
            let file_name = String::from(name.to_str().unwrap());

            let hash = unsafe {
                format!("{:x}", hash_id_value(&uri))
                    .get_unchecked(0..5)
                    .to_owned()
            };

            Ok(file_name + &hash)
        }
        None => Err(ParseError::UNDEFINED),
    }
}
