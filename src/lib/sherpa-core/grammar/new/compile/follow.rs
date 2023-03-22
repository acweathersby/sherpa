use super::{super::types::*, types::*};
use crate::{
  ascript::types::AScriptStore,
  compile::ParseState,
  grammar::{
    compile::parser::sherpa::Ascript,
    hash_id_value_u64,
    new::types::{OrderedMap, ParserDatabase, ProductionSubType, Rule},
  },
  parser::hash_group_btreemap,
  tasks::{new_taskman, Executor, Spawner},
  types::SherpaErrorSeverity,
  Journal,
  ReportType,
  SherpaError,
  SherpaResult,
};
use core::panic;
use sherpa_runtime::{types::BlameColor, utf8::lookup_table::CodePointClass};
use std::{collections::VecDeque, ops::Index, path::PathBuf, sync::Arc};

pub(super) type FollowSets<'db> = Array<OrderedSet<ItemRef<'db>>>;

pub fn create_follow_sets<'db>(db: &'db ParserDatabase) -> FollowSets<'db> {
  let mut follow = Array::with_capacity(db.prod_len());

  for i in 0..db.prod_len() {
    follow.push(OrderedSet::new())
  }

  for prod_id in 0..db.prod_len() {
    for mut item in Items::start_items(prod_id.into(), db) {
      loop {
        match item.get_type() {
          ItemType::NonTerminal(prod_id)
          | ItemType::TokenNonTerminal(prod_id, ..) => {
            let index: usize = prod_id.into();
            follow[index].insert(item.increment().unwrap());
          }
          _ => {}
        }

        if let Some(i) = item.increment() {
          item = i;
        } else {
          break;
        }
      }
    }
  }
  follow
}
