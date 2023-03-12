use std::{
  path::PathBuf,
  sync::{Arc, LockResult, RwLock, RwLockReadGuard},
};

use crate::grammar::hash_id_value_u64;

use super::types::Map;

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]

pub(crate) struct StringToken(u64);

impl StringToken {
  pub fn new(string: &String) -> Self {
    Self(hash_id_value_u64(string))
  }
}

type InnerStringStore = Map<StringToken, String>;

#[derive(Default, Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) struct StringStore {
  _data: Arc<RwLock<InnerStringStore>>,
}


impl StringStore {
  fn intern(&self, string: String) -> StringToken {
    let token = StringToken::new(&string);

    match self._data.read() {
      LockResult::Ok( data) => {
        if data.contains_key(&token) {
          return token;
        }
      }
      _ => panic!(),
    }

    match self._data.write() {
      LockResult::Ok(mut data) => {
          data.insert(token, string);
      }
      _ => panic!(),
    }

    token
  }

  fn get_str<'a>(&'a self, token: StringToken) -> Option<GuardedStr<'a>> {
    match self._data.read() {
      LockResult::Ok(store) => {
        Some(GuardedStr(token, None, Some(store)))
      }
      _ => panic!(),
    }
  }

  #[cfg(test)]
  /// Returns the number of strings stored within.
  pub fn len<'a>(&'a self) -> usize {
    match self._data.read() {
      LockResult::Ok(store) => store.len(),
      _ => panic!(),
    }
  }
}

/// A reference to a string interned within a String Store. Maintains
/// a read lock to the store as long as this object lives.
///
/// This should never be assigned to any object that outlives it's current
/// calling context. Should be dropped as soon as possible.
pub(crate) struct GuardedStr<'a>(StringToken, Option<&'a str>, Option<RwLockReadGuard<'a, InnerStringStore>>);


impl<'a> GuardedStr<'a> {
  pub fn as_str(&'a self) -> &'a str {
    if(self.1.is_none()) {
      self.2.as_ref().unwrap().get(&self.0).unwrap().as_str()
    }else {
      self.1.unwrap()
    }
  }
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub(crate) enum SherpaString {
  SmallString { len: u8, data: [u8; 14] },
  LargeString(StringToken),
}

impl Default for SherpaString {
  fn default() -> Self {
    SherpaString::SmallString { len: 0, data: [0; 14] }
  }
}

impl SherpaString {
  pub fn to_string(&self, store: &StringStore) -> String {
    self.to_str(store).as_str().to_string()
  }

  pub fn to_str<'a>(&'a self, store: &'a StringStore) -> GuardedStr<'a> {
    match self {
      SherpaString::LargeString(token) => store.get_str(*token).unwrap(),
      SherpaString::SmallString { len, data } => {
        let slice = &data.as_slice()[0..*len as usize];
        GuardedStr(StringToken(0), Some(unsafe { std::str::from_utf8_unchecked(slice) }), None)
      }
    }
  }
}

pub(crate) trait CachedString {
  /// Get the SherpaString representation without interning
  /// the string. This can useful when needing to compare a
  /// SherpaString with a standard string type.
  fn proxy(&self) -> SherpaString {
    let len = self.get_bytes().len();

    if len > 14 {
      SherpaString::LargeString(StringToken(hash_id_value_u64(
        self.get_string(),
      )))
    } else {
      let mut data = [0; 14];
      let bytes = self.get_bytes();

      for i in 0..len {
        data[i] = bytes[i];
      }

      SherpaString::SmallString { len: len as u8, data }
    }
  }
  /// Returns a SherpaString after interning the string within
  /// the given store. Only `LargeString` sub-types are interned.
  fn intern(&self, store: &StringStore) -> SherpaString {
    let len = self.get_bytes().len();

    if len > 14 {
      SherpaString::LargeString(store.intern(self.get_string()))
    } else {
      let mut data = [0; 14];
      let bytes = self.get_bytes();

      for i in 0..len {
        data[i] = bytes[i];
      }

      SherpaString::SmallString { len: len as u8, data }
    }
  }

  fn get_string(&self) -> String;

  fn get_bytes(&self) -> &[u8];
}

impl CachedString for String {
  fn get_bytes(&self) -> &[u8] {
    self.as_bytes()
  }

  fn get_string(&self) -> String {
    self.clone()
  }
}

impl CachedString for &String {
  fn get_bytes(&self) -> &[u8] {
    self.as_bytes()
  }

  fn get_string(&self) -> String {
    self.to_string()
  }
}

impl CachedString for &str {
  fn get_bytes(&self) -> &[u8] {
    self.as_bytes()
  }

  fn get_string(&self) -> String {
    self.to_string()
  }
}

impl CachedString for &PathBuf {
  fn get_bytes(&self) -> &[u8] {
    self.to_str().unwrap().as_bytes()
  }

  fn get_string(&self) -> String {
    self.to_str().unwrap().to_owned()
  }
}

impl CachedString for PathBuf {
  fn get_bytes(&self) -> &[u8] {
    self.to_str().unwrap().as_bytes()
  }

  fn get_string(&self) -> String {
    self.to_str().unwrap().to_owned()
  }
}

#[test]
fn interning_small_string() {
  let store = StringStore::default();

  let small_string = "test";

  let tok = small_string.intern(&store);

  assert_eq!(tok.to_string(&store), "test");
  assert_eq!(tok.to_string(&store).as_str(), "test");
}

#[test]
fn interning_large_string() {
  let store = StringStore::default();

  let large_string = "
  Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let tok = large_string.intern(&store);

  assert_eq!(tok.to_string(&store), large_string);
  assert_eq!(tok.to_str(&store).as_str(), large_string);
}

#[test]
fn interning_same_large_string() {
  let store = StringStore::default();

  let large_str = "
  Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let string = String::from(large_str);

  let tokA = large_str.intern(&store);
  let tokB = string.intern(&store);

  assert_eq!(tokA.to_string(&store), large_str);
  assert_eq!(tokB.to_str(&store).as_str(), large_str);
  assert_eq!(tokA, tokB);
}

#[test]
fn interning_different_large_string() {
  let store = StringStore::default();

  let large_strA = "1: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let large_strB = "2: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let tokA = large_strA.intern(&store);
  let tokB = large_strB.intern(&store);

  assert_eq!(tokA.to_string(&store), large_strA);
  assert_eq!(tokB.to_string(&store), large_strB);
  assert_ne!(tokA.to_string(&store), large_strB);
  assert_ne!(tokB.to_string(&store), large_strA);
  assert_ne!(tokA, tokB);
  assert_eq!(store.len(), 2);
}

#[test]
fn interning_strings_on_different_threads() {
  let store = StringStore::default();

  std::thread::scope(|scope| {
    for u in 0..4 {
      let store = store.clone();
      scope.spawn(move || {
        let large_strA = "1: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
        Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
        confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
        dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
        timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
        relicto orbe quid, adit nec possederat simque conprendere defecerat avus
        Cyparisse multarum!";
        
          let large_strB = "2: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
        Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
        confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
        dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
        timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
        relicto orbe quid, adit nec possederat simque conprendere defecerat avus
        Cyparisse multarum!";

        for i in 0..400 {
          (large_strA.to_string() + &format!(" {i} {u}")).intern(&store);
          (large_strB.to_string() + &format!(" {i}")).intern(&store);
        }
        1
      });
    }
  });

  assert_eq!(store.len(), 2000);
}