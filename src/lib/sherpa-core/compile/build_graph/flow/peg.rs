#![allow(unused)]

use super::super::{build::GroupedFirsts, graph::*};
use crate::types::*;
use std::collections::BTreeSet;

use GraphBuildState::*;

pub(crate) fn handle_peg_complete_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  prec_sym: PrecedentSymbol,
  follow_pairs: Lookaheads<'db>,
) -> SherpaResult<()> {
  Ok(())
}
