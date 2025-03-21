use super::Map;
use crate::utils::create_u64_hash;
use std::{
  fmt::Debug,
  path::{Path, PathBuf},
  sync::{Arc, LockResult, RwLock, RwLockReadGuard},
};

type InnerStringStore = Map<IString, String>;

#[derive(Default, Clone)]
pub struct IStringStore {
  _data: Arc<RwLock<InnerStringStore>>,
}

impl Debug for IStringStore {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str("IStringStore")
  }
}

impl IStringStore {
  fn intern(&self, string: String, token: IString) -> IString {
    match self._data.read() {
      LockResult::Ok(data) => {
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

  fn get_str<'a>(&'a self, token: IString) -> Option<GuardedStr<'a>> {
    match self._data.read() {
      LockResult::Ok(store) => Some(GuardedStr(token, None, Some(store))),
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

/// A reference to a string interned within an [IStringStore]. A read lock on
/// the store is maintained as long as this object is in scope.
///
/// This should never be assigned to any object that outlives its current
/// function context. Should be dropped as soon as possible.
pub struct GuardedStr<'a>(IString, Option<IString>, Option<RwLockReadGuard<'a, InnerStringStore>>);

impl<'a> GuardedStr<'a> {
  pub fn as_str(&'a self) -> &'a str {
    if let Some(istring) = self.1.as_ref() {
      unsafe { istring.small_to_str() }
    } else if cfg!(debug_assertions) {
      self.2.as_ref().unwrap().get(&self.0).unwrap().as_str()
    } else {
      unsafe { self.2.as_ref().unwrap_unchecked().get(&self.0).unwrap_unchecked().as_str() }
    }
  }
}

/// An **I**nterned **String** for fast string operations. Combines a small
/// string type with an interned string type for larger string using
/// [StringStore].
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IString(u64);

impl Default for IString {
  fn default() -> Self {
    Self(0)
  }
}

impl std::fmt::Debug for IString {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    unsafe {
      if self.is_large() {
        let mut s = f.debug_tuple("IString::Large");
        // This is an interned string.
        s.field(&self.0);
        s.finish()
      } else {
        let mut s = f.debug_tuple("IString::Small");
        s.field(&self.small_to_str());
        s.finish()
      }
    }
  }
}

impl IString {
  pub fn is_empty(&self) -> bool {
    self.0 == 0
  }

  pub fn from_u64(val: u64) -> Self {
    Self(val)
  }

  pub fn as_u64(&self) -> u64 {
    self.0
  }

  pub fn is_small(&self) -> bool {
    unsafe {
      let val_bytes = self as *const IString as *const [u8; 8];
      (*val_bytes)[7] & 0x80 == 0
    }
  }

  pub fn is_large(&self) -> bool {
    !self.is_small()
  }

  pub fn to_string(&self, store: &IStringStore) -> String {
    self.to_str(store).as_str().to_string()
  }

  pub fn to_path(&self, store: &IStringStore) -> PathBuf {
    PathBuf::from(self.to_str(store).as_str().to_string())
  }

  /// Returns a [GuardedStr] that can be used to access the `&str` the [IString]
  /// token represents.
  pub fn to_str<'a>(self, store: &'a IStringStore) -> GuardedStr<'a> {
    if self.is_large() {
      store.get_str(self).unwrap()
    } else {
      GuardedStr(self, Some(self), None)
    }
  }

  unsafe fn small_to_str<'a>(&'a self) -> &'a str {
    let val_bytes = (&self.0) as *const u64 as *const [u8; 8];
    // This is a small string. The upper 7 bits comprise the length of the
    // string.

    let mut len = 8;

    for i in 0..8 {
      if (*val_bytes)[i] == 0 {
        len = i;
        break;
      }
    }

    let data = std::slice::from_raw_parts(&((*val_bytes)[0]), len);
    let out = std::str::from_utf8_unchecked(data);

    out
  }

  fn from_bytes(string: &[u8]) -> Self {
    if string.is_empty() {
      Self(0)
    } else {
      let byte_len = string.len();
      if byte_len > 8 || (byte_len == 8 && (string[7] & 0x80) > 0) {
        let mut val = create_u64_hash(string);
        unsafe {
          let val_bytes = &mut val as *mut u64 as *mut [u8; 8];
          (*val_bytes)[7] |= 0x80;
        }
        Self(val)
      } else {
        let bytes = string;
        let mut val = 0u64;
        unsafe {
          let val_bytes = &mut val as *mut u64 as *mut [u8; 8];
          for (off, byte) in bytes.iter().enumerate() {
            (*val_bytes)[off] = *byte;
          }
          if byte_len < 8 {
            (*val_bytes)[byte_len] = 0;
          }
        }
        Self(val)
      }
    }
  }
}

pub trait CachedString {
  /// Derive the IString representation without interning
  /// the string. This can be useful when needing to compare an already
  /// interned IString with a standard string type.
  fn to_token(&self) -> IString {
    IString::from_bytes(self.get_bytes())
  }
  /// Returns a IString after interning the string within
  /// the given store. Only `LargeString` sub-types are interned.
  fn intern(&self, store: &IStringStore) -> IString {
    let bytes = self.get_bytes();
    let token = IString::from_bytes(bytes);

    if bytes.len() > 7 {
      store.intern(self.get_string(), token)
    } else {
      token
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

impl CachedString for &[u8] {
  fn get_bytes(&self) -> &[u8] {
    self
  }

  fn get_string(&self) -> String {
    String::from_utf8(self.to_vec()).unwrap()
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

impl CachedString for &Path {
  fn get_bytes(&self) -> &[u8] {
    self.to_str().unwrap().as_bytes()
  }

  fn get_string(&self) -> String {
    self.to_str().unwrap().to_owned()
  }
}

#[test]
fn interning_empty_string() {
  let store = IStringStore::default();

  let empty_string = "";

  let tok = empty_string.intern(&store);
  "".to_string().intern(&store);

  assert_eq!(tok.to_string(&store), "");
  assert_eq!(tok.to_string(&store).as_str(), "");
  assert_eq!(store._data.read().unwrap().is_empty(), true);
}

#[test]
fn interning_small_string() {
  let store = IStringStore::default();

  let small_string = "test";

  let tok = small_string.intern(&store);

  assert_eq!(tok.to_string(&store), "test");
  assert_eq!(tok.to_string(&store).as_str(), "test");
  assert_eq!("B".intern(&store).to_string(&store), "B");
  assert_eq!("B".intern(&store).to_str(&store).as_str(), "B");
  assert!("function".intern(&store).is_small());
  assert_eq!("function".intern(&store).to_str(&store).as_str(), "function");
}

#[test]
fn interning_large_string() {
  let store = IStringStore::default();

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
  let store = IStringStore::default();

  let large_str = "
  Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let string = String::from(large_str);

  let tok_a = large_str.intern(&store);
  let tok_b = string.intern(&store);

  assert_eq!(tok_a.to_string(&store), large_str);
  assert_eq!(tok_b.to_str(&store).as_str(), large_str);
  assert_eq!(tok_a, tok_b);
}

#[test]
fn interning_different_large_string() {
  let store = IStringStore::default();

  let large_str_a = "1: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let large_str_b = "2: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
relicto orbe quid, adit nec possederat simque conprendere defecerat avus
Cyparisse multarum!";

  let tok_a = large_str_a.intern(&store);
  let tok_b = large_str_b.intern(&store);

  assert!(tok_a.is_large());
  assert!(tok_b.is_large());
  assert_eq!(tok_a.to_string(&store), large_str_a);
  assert_eq!(tok_b.to_string(&store), large_str_b);
  assert_ne!(tok_a.to_string(&store), large_str_b);
  assert_ne!(tok_b.to_string(&store), large_str_a);
  assert_ne!(tok_a, tok_b);
  assert_eq!(store.len(), 2);
}

#[test]
fn interning_strings_on_different_threads() {
  let store = IStringStore::default();

  std::thread::scope(|scope| {
    for u in 0..4 {
      let store = store.clone();
      scope.spawn(move || {
        let large_str_a = "1: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
        Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
        confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
        dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
        timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
        relicto orbe quid, adit nec possederat simque conprendere defecerat avus
        Cyparisse multarum!";

        let large_str_b = "2: Lumina eiusdem a sororibus est agant montis tu urbes succedit gavisa dolore
        Perseus incerti, repente pariter. Omnes morsu rediit flores, nisi scelus
        confessis cristati ramis silentum arentis centimanum sacrilegae pone. Silvas
        dieque ire *fuit*, resides videt quodcumque illi circumflua petii laeva ne
        timidas Venilia. Accipiunt saltem apta modo! Annos tu tale concita nostro
        relicto orbe quid, adit nec possederat simque conprendere defecerat avus
        Cyparisse multarum!";

        for i in 0..400 {
          (large_str_a.to_string() + &format!(" {i} {u}")).intern(&store);
          (large_str_b.to_string() + &format!(" {i}")).intern(&store);
        }
        1
      });
    }
  });

  assert_eq!(store.len(), 2000);
}
