#![allow(unused)]

use super::super::{build::GroupedFirsts, graph::*};
use crate::types::*;
use std::collections::BTreeSet;

use GraphBuildState::*;

pub(crate) fn handle_peg_complete_groups(
  gb: &mut GraphBuilder,
  groups: &mut GroupedFirsts,
  prec_sym: PrecedentSymbol,
  follow_pairs: Lookaheads,
) -> SherpaResult<()> {
  Ok(())
}
