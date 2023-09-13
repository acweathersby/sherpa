use super::{
  flow::{
    handle_bread_crumb_complete_groups,
    handle_nonterminal_shift,
    handle_peek_complete_groups,
    handle_peek_incomplete_items,
    handle_peg_complete_groups,
    handle_regular_complete_groups,
    handle_regular_incomplete_items,
  },
  graph::*,
  items::{get_completed_item_artifacts, merge_occluding_token_items},
};
use crate::{types::*, utils::hash_group_btree_iter};

use GraphBuildState::*;

pub(crate) type TransitionGroup<'db> = (u16, Vec<TransitionPair<'db>>);
pub(crate) type GroupedFirsts<'db> = OrderedMap<SymbolId, TransitionGroup<'db>>;

pub(crate) fn handle_kernel_items(gb: &mut GraphBuilder) -> SherpaResult<()> {
  let mut groups = get_firsts(gb)?;

  let max_precedence = handle_completed_items(gb, &mut groups)?;

  let groups = handle_scanner_items(max_precedence, gb, groups)?;

  handle_incomplete_items(gb, groups)?;

  handle_nonterminal_shift(gb)?;

  Ok(())
}

// Iterate over each item's closure and collect the terminal transition symbols
// of each item. The item's are then catagorized by these nonterminal symbols.
// Completed items are catagorized by the default symbol.
fn get_firsts<'db>(gb: &mut GraphBuilder<'db>) -> SherpaResult<GroupedFirsts<'db>> {
  let iter = gb.current_state().kernel_items_ref().iter().flat_map(|i| {
    i.closure_iter().term_items_iter(gb.is_scanner()).enumerate().map(|(t, t_item)| -> TransitionPair {
      if i.is_canonically_equal(&t_item) {
        (*i, *i, gb.get_mode()).into()
      } else {
        (*i, t_item.align(i).to_origin_state(gb.current_state_id()), gb.get_mode())
      }
      .into()
    })
  });

  let groups = hash_group_btree_iter::<Vec<_>, _, _, _, _>(iter, |_, first| first.sym);

  let groups: OrderedMap<SymbolId, (u16, Vec<TransitionPair<'db>>)> =
    groups.into_iter().map(|(s, g)| (s, (g.iter().map(|f| f.prec).max().unwrap_or_default(), g))).collect();

  SherpaResult::Ok(groups)
}

fn handle_scanner_items<'db>(
  max_precedence: u16,
  gb: &GraphBuilder<'db>,
  mut groups: GroupedFirsts<'db>,
) -> SherpaResult<GroupedFirsts<'db>> {
  if gb.is_scanner() {
    if max_precedence > CUSTOM_TOKEN_PRECEDENCE_BASELINE {
      groups = groups
        .into_iter()
        .filter_map(|(s, (p, g))| {
          if s == SymbolId::Default {
            // Completed items are an automatic pass
            Some((s, (p, g)))
          } else {
            let g = g.into_iter().filter(|i| i.prec >= max_precedence).collect::<Vec<_>>();
            if g.is_empty() {
              None
            } else {
              Some((s, (p, g)))
            }
          }
        })
        .collect();
    }

    merge_occluding_token_items(groups.clone(), &mut groups);
  }

  Ok(groups)
}

fn handle_incomplete_items<'nt_set, 'db: 'nt_set>(gb: &mut GraphBuilder<'db>, groups: GroupedFirsts<'db>) -> SherpaResult<()> {
  for (sym, group) in groups {
    let ____is_scan____ = gb.is_scanner();
    let prec_sym: PrecedentSymbol = (sym, group.0).into();

    match gb.current_state_id().state() {
      BreadCrumb(_level) => {
        todo!("Complete breadcrumb parsing");
      }
      PEG => {
        todo!("Complete peg parsing");
      }
      Peek(level) => handle_peek_incomplete_items(gb, prec_sym, group, level),
      _REGULAR_ => handle_regular_incomplete_items(gb, prec_sym, group),
    }?;
  }
  Ok(())
}

fn handle_completed_items<'db>(gb: &mut GraphBuilder<'db>, groups: &mut GroupedFirsts<'db>) -> SherpaResult<u16> {
  let ____is_scan____ = gb.is_scanner();
  let mut max_precedence = 0;

  if let Some(completed) = groups.remove(&SymbolId::Default) {
    max_precedence = max_precedence.max(completed.0);

    let CompletedItemArtifacts { mut follow_pairs, default_only, .. } =
      get_completed_item_artifacts(gb, completed.1.iter().map(|i| &i.kernel))?;

    if ____is_scan____ {
      //gb.add_kernel_items(follow_pairs.iter().to_next().filter(|i|
      // !i.is_complete()).cloned().collect::<Vec<_>>());
      /*       merge_occluding_token_items(
        hash_group_btree_iter::<Vec<_>, _, _, _, _>(follow_pairs.iter(), |_, item| match item.next.get_type() {
          ItemType::Terminal(sym) => sym,
          _ => SymbolId::Undefined,
        })
        .into_iter()
        .map(|(g, s)| (g, (0u16, s)))
        .collect(),
        groups,
      ); */
      /* get_oos_follow_from_completed(gb, &completed.1.iter().to_kernel().to_vec(), &mut |follow| {
        merge_follow_items_into_group(&follow, gb.state_id(), groups)
      })?; */
    } /* else {
        follow_pairs = follow_pairs
          .into_iter()
          .map(|pair| {
            if false {
              (pair.kernel, pair.next.to_oos_index().to_origin(Origin::GoalCompleteOOS), gb.get_mode()).into()
            } else {
              pair
            }
          })
          .collect();
      } */

    let contains_in_scope_items = completed.1.iter().any(|i| !i.next.goal_is_oos());

    let default: Follows = if contains_in_scope_items {
      completed.1.iter().filter(|i| !i.is_out_of_scope()).cloned().collect()
    } else {
      completed.1.iter().map(|i| -> TransitionPair<'db> { (i.kernel, i.kernel, gb.get_mode()).into() }).collect()
    };

    handle_completed_groups(gb, groups, SymbolId::Default, default, &default_only)?;

    if !follow_pairs.is_empty() {
      // Create reduce states for follow items that have not already been covered.
      let mut completed_groups: OrderedMap<SymbolId, Vec<TransitionPair>> =
        hash_group_btree_iter(follow_pairs.into_iter(), |_, fp| match fp.next.get_type() {
          ItemType::Completed(_) => {
            unreachable!("Should be handled outside this path")
          }
          ItemType::TokenNonTerminal(_, sym) if !gb.is_scanner() => sym,
          ItemType::Terminal(sym) => sym,
          _ => SymbolId::Undefined,
        });

      completed_groups.remove(&SymbolId::Undefined);

      for (sym, follow_pairs) in completed_groups {
        handle_completed_groups(gb, groups, sym, follow_pairs, &default_only)?;
      }
    }
  }

  SherpaResult::Ok(max_precedence)
}

pub(crate) fn handle_completed_groups<'db>(
  gb: &mut GraphBuilder<'db>,
  groups: &mut GroupedFirsts<'db>,
  sym: SymbolId,
  follow_pairs: Follows<'db>,
  default_only_items: &ItemSet<'db>,
) -> SherpaResult<()> {
  let ____is_scan____ = gb.is_scanner();
  let prec_sym: PrecedentSymbol = (sym, follow_pairs.iter().max_precedence()).into();

  match gb.current_state_id().state() {
    GraphBuildState::PEG => handle_bread_crumb_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
    GraphBuildState::BreadCrumb(_) => handle_peg_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
    GraphBuildState::Peek(_) => handle_peek_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
    _REGULAR_ => handle_regular_complete_groups(gb, groups, prec_sym, follow_pairs, default_only_items),
  }
}
