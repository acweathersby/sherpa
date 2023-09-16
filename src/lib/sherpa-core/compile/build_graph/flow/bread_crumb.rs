#![allow(unused)]

use super::super::{build::GroupedFirsts, graph::*};
use crate::{
  compile::build_graph::errors::peek_not_allowed_error,
  types::*,
  utils::{hash_group_btree_iter, hash_group_btreemap},
};
use std::collections::BTreeSet;

use GraphBuildState::*;

pub(crate) fn handle_bread_crumb_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: Lookaheads<'db>,
) -> SherpaResult<()> {
  Ok(())
}
