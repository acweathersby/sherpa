use crate::{AscriptAggregateType, AscriptDatabase, AscriptScalarType, AscriptType, AscriptTypes};
use radlr_core::CachedString;
use radlr_formatter::*;
use std::fmt::Debug;

#[derive(Debug)]
pub struct AscriptMulti {
  pub(crate) types:             AscriptTypes,
  pub(crate) requires_template: bool,
  pub(crate) name:              String,
  pub(crate) used:              bool,
  pub(crate) root:              bool,
}

impl ValueObj for AscriptMulti {
  fn get_keys<'scope>(&'scope self) -> &'static [&'static str] {
    &["name", "types"]
  }

  fn get_val<'scope>(&'scope self, key: &str, s_store: &radlr_core::IStringStore) -> Value<'scope> {
    match key {
      "name" => Value::Str(self.name.intern(s_store)),
      "types" => Value::Obj(&self.types),
      "requires_template" => Value::Int(self.requires_template as isize),
      "is_used" => Value::Int(self.used as isize),
      "is_root" => Value::Int(self.root as isize),
      _ => Value::None,
    }
  }

  fn get_type<'scope>(&'scope self) -> &str {
    "AscriptMultiEnum"
  }
}

#[derive(Debug)]
/// Provides services to remap AnyTypes to enums of Any
pub struct AscriptMultis<'db> {
  db:   &'db AscriptDatabase,
  anys: Vec<AscriptMulti>,
  len:  usize,
}

impl<'db> AscriptMultis<'db> {
  pub fn new(db: &'db AscriptDatabase) -> Self {
    let mut used_indices = Vec::from_iter(db.multi_type_lu.iter().cloned().map(|index| 0));
    Self {
      db,
      len: used_indices.len(),
      anys: db
        .multi_types
        .iter()
        .enumerate()
        .map(|(i, _)| {
          let remapped_index = db.multi_type_lu[i];
          let entry = used_indices.get_mut(remapped_index);
          let any = &db.multi_types[remapped_index];

          let (used, root) = if let Some(entry) = entry {
            let root = *entry;
            *entry += 1;
            (true, root == 0)
          } else {
            (false, false)
          };
          let types = AscriptTypes(any.1.iter().map(|t| AscriptType::Scalar(*t)).collect());
          AscriptMulti {
            name: format!("{}Values", any.0),
            requires_template: types.0.iter().any(|t| t.requires_template(db)),
            types,
            used,
            root,
          }
        })
        .collect(),
    }
  }
}

impl<'a> ValueObj for AscriptMultis<'a> {
  fn get_type<'scope>(&'scope self) -> &str {
    "multi_enums"
  }

  fn get_len<'scope>(&'scope self) -> usize {
    self.len
  }

  fn get_index<'scope>(&'scope self, index: usize, _: &radlr_core::IStringStore) -> Value<'scope> {
    //let remapped_index = self.db.multi_type_lu[index];
    Value::Obj(&self.anys[index])
  }

  fn get_iter<'scope>(&'scope self, _: &radlr_core::IStringStore) -> Vec<(Value<'scope>, Value<'scope>)> {
    self.anys.iter().filter(|i| i.used && i.root).enumerate().map(|(i, any)| (Value::Int(i as isize), Value::Obj(any))).collect()
  }
}
