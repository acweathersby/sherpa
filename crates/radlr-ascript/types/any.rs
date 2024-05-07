use crate::{AscriptAggregateType, AscriptDatabase, AscriptScalarType, AscriptType, AscriptTypes};
use radlr_core::CachedString;
use radlr_formatter::*;
use std::{
  collections::{BTreeMap, BTreeSet},
  fmt::Debug,
};

#[derive(Debug)]
pub struct AscriptAny {
  pub(crate) types: AscriptTypes,
  pub(crate) name:  String,
  pub(crate) used:  bool,
  pub(crate) root:  bool,
}

impl ValueObj for AscriptAny {
  fn get_keys<'scope>(&'scope self) -> &'static [&'static str] {
    &["name", "types"]
  }

  fn get_val<'scope>(&'scope self, key: &str, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.intern(s_store)),
      "types" => Value::Obj(&self.types),
      "has_token" => Value::Int(self.types.0.iter().any(contains_token_reference) as isize),
      "is_used" => Value::Int(self.used as isize),
      "is_root" => Value::Int(self.root as isize),
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptAnyEnum"
  }
}

fn contains_token_reference(t: &AscriptType) -> bool {
  use AscriptAggregateType::*;
  use AscriptScalarType::*;
  use AscriptType::*;
  match t {
    Scalar(Token) | Scalar(Struct(_, false)) | Scalar(TokenRange) => true,
    Aggregate(Vec { val_type }) => match val_type {
      Token | Struct(_, false) | TokenRange => true,
      _ => false,
    },
    Aggregate(Map { key_type, val_type }) => {
      (match key_type {
        Token | Struct(_, false) | TokenRange => true,
        _ => false,
      }) || (match val_type {
        Token | Struct(_, false) | TokenRange => true,
        _ => false,
      })
    }
    _ => false,
  }
}

#[derive(Debug)]
/// Provides services to remap AnyTypes to enums of Any
pub struct AscriptAnys<'db> {
  db:   &'db AscriptDatabase,
  anys: Vec<AscriptAny>,
  len:  usize,
}

impl<'db> AscriptAnys<'db> {
  pub fn new(db: &'db AscriptDatabase) -> Self {
    let mut used_indices = Vec::from_iter(db.any_type_lu.iter().cloned().map(|index| 0));
    Self {
      db,
      len: used_indices.len(),
      anys: db
        .any_types
        .iter()
        .enumerate()
        .map(|(i, _)| {
          let remapped_index = db.any_type_lu[i];
          let entry = used_indices.get_mut(remapped_index);
          let any = &db.any_types[remapped_index];

          let (used, root) = if let Some(entry) = entry {
            let root = *entry;
            *entry += 1;
            (true, root == 0)
          } else {
            (false, false)
          };

          AscriptAny {
            types: AscriptTypes(any.1.iter().map(|t| AscriptType::Scalar(*t)).collect()),
            name: format!("{}Any", any.0),
            used,
            root,
          }
        })
        .collect(),
    }
  }
}

impl<'a> ValueObj for AscriptAnys<'a> {
  fn get_type<'scope>(&'scope self) -> &str {
    "any_enums"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len
  }

  fn get_index<'scope>(&'scope self, index: usize, _: &radlr_core::IStringStore) -> Value<'scope> {
    //let remapped_index = self.db.any_type_lu[index];
    Value::Obj(&self.anys[index])
  }

  fn get_iter<'scope>(&'scope self, _: &radlr_core::IStringStore) -> Vec<(Value<'scope>, Value<'scope>)> {
    dbg!(&self.anys);
    self.anys.iter().filter(|i| i.used && i.root).enumerate().map(|(i, any)| (Value::Int(i as isize), Value::Obj(any))).collect()
  }
}
