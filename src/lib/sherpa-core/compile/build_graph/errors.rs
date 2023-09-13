use super::graph::*;
use crate::types::*;

use ErrorClass::*;

pub(super) fn _create_reduce_reduce_error(_gb: &GraphBuilder, _end_items: ItemSet) -> SherpaError {
  /*   j.report_mut().add_error(SherpaError::SourcesError {
    id:       "reduce-conflict",
    msg:      "Unresolvable parse conflict encountered".into(),
    ps_msg:   {
      let mut string = "Enable the following configs to use an alternative parse strategy".into();

      if !graph.is_scanner() {
      } else {
        let nterm = &goals.first().expect("Should have at least one goal").nonterm_index();
        let name = db.nonterm_guid_name_string(*nterm).as_str().to_string();

        string +=
          ("\n - Turn non-terminal <".to_string() + &name + " > into a PEG by using one of the PEG mode specifiers:").as_str();
        string += "\n    [ FIRST_MATCH | LONGEST_MATCH | SHORTEST_MATCH ]";
        string += format!("\n\n    Example: <> {name} FIRST_MATCH >  ...",).as_str();
      }
      string
    },
    severity: SherpaErrorSeverity::Critical,
    sources:  Default::default(),
  }); */

  SherpaError::StaticText("TODO - create error message for reduce-reduce conflicts")
}

/// Produces errors that result the banning of LR states.
pub(super) fn lr_disabled_error<'db>(gb: &GraphBuilder, lr_items: Items) -> SherpaResult<()> {
  let db = gb.graph().get_db();

  let s_store = db.string_store();

  let nonterms = lr_items;

  if nonterms.len() == 1 {
    let first = nonterms.first().unwrap();
    if first.is_left_recursive(gb.get_mode()) {
      return Err(SherpaError::SourceError {
        loc:        first.rule().tok.clone(),
        path:       first.rule().g_id.path.to_path(s_store),
        id:         (ForbiddenLR, 0, "left-recursion-forbidden").into(),
        msg:        "Could not construct parse graph without LR states, this rule is left recursive".into(),
        inline_msg: "".into(),
        ps_msg:     "Try another configuration of this rule such as: TODO".into(),
        severity:   SherpaErrorSeverity::Critical,
      });
    }
  }

  return Err(SherpaError::SourcesError {
    sources:  nonterms
      .iter()
      .map(|i| i.rule_id)
      .collect::<OrderedSet<_>>()
      .into_iter()
      .map(|r| db.rule(r))
      .map(|r| (r.tok.clone(), r.g_id.path.to_path(s_store), "LR disabled".into()))
      .collect(),
    id:       (ForbiddenLR, 1, "goto-states-forbidden").into(),
    msg:      "Since LR parsing is disabled could not construct goto state to handle the parsing of the nonterminal ["
      .to_string()
      + &db.nonterm_friendly_name_string(gb.graph().get_goal_nonterm_index())
      + "]",
    ps_msg:   "Consider enabling lr parsing through the parser config object".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}

pub(crate) fn conflicting_symbols_error<'db>(
  gb: &GraphBuilder<'db>,
  groups: OrderedMap<(u16, SymbolId), Follows<'db>>,
) -> SherpaError {
  let graph = gb.graph();
  let db = graph.get_db();
  SherpaError::SourcesError {
    id:       (GraphConstruction, 0, "conflicting-symbols").into(),
    msg:      "Found ".to_string() + &groups.len().to_string() + " conflicting tokens. This grammar has an ambiguous scanner",
    ps_msg:   Default::default(),
    severity: SherpaErrorSeverity::Critical,
    sources:  groups
      .iter()
      .map(|(_sym, follows)| {
        follows
          .iter()
          .map(|p| (p.kernel.rule().tok.clone(), p.kernel.rule().g_id.path.to_path(db.string_store()), Default::default()))
      })
      .flatten()
      .collect(),
  }
}

pub(crate) fn peek_not_allowed_error<'db, T>(
  gb: &GraphBuilder<'db>,
  conflicting_groups: &[Vec<TransitionPair<'db>>],
  submessage: &str,
) -> SherpaResult<T> {
  Err(peek_not_allowed_error_internal(gb, conflicting_groups, submessage))
}

fn peek_not_allowed_error_internal<'db>(
  gb: &GraphBuilder<'db>,
  conflicting_groups: &[Vec<TransitionPair<'db>>],
  submessage: &str,
) -> SherpaError {
  let db = gb.db;
  SherpaError::SourcesError {
    id:       (ForbiddenPeek, 0, "disabled-peeking").into(),
    msg:      "The following items cannot be resolved within k=1 lookahead when peeking is disabled".into(),
    ps_msg:   if submessage.is_empty() { "Enable peeking to disambiguate these states.".into() } else { submessage.into() },
    severity: SherpaErrorSeverity::Critical,
    sources:  conflicting_groups
      .iter()
      .flat_map(|i| i.into_iter())
      .map(|p| {
        let i = p.kernel;
        (
          if !i.is_complete() { i.rule().symbols[i.sym_index as usize].loc.clone() } else { i.rule().tok.clone() },
          i.rule().g_id.path.to_path(db.string_store()),
          if i.is_complete() {
            "Reduce to [".to_string()
              + &db.nonterm_friendly_name_string(i.nonterm_index())
              + "]? | "
              + &i.to_canonical()._debug_string_()
          } else {
            "Continue shifting? | ".to_string() + &i.to_canonical()._debug_string_()
          },
        )
      })
      .dedup(),
  }
}
