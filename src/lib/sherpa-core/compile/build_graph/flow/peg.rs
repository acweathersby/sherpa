#![allow(unused)]

use super::super::{build::TransitionGroups, graph::*};
use crate::types::*;
use std::collections::BTreeSet;

use GraphState::*;

pub(crate) fn handle_peg_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut TransitionGroups<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: OrderedSet<FollowPair<'db>>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  Ok(())
}
