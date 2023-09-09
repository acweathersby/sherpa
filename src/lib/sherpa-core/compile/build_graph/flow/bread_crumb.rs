#![allow(unused)]

use super::super::{build::TransitionGroups, graph::*, items::peek_items_are_from_goto_state};
use crate::{
  compile::build_graph::errors::peek_not_allowed_error,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::BTreeSet;

use GraphState::*;

pub(crate) fn handle_bread_crumb_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut TransitionGroups<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  Ok(())
}
