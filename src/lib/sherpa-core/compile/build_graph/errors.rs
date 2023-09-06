use super::items::get_goal_items;
use crate::types::*;

use ErrorClass::*;

pub(super) fn create_reduce_reduce_error(graph: &GraphHost, end_items: ItemSet) -> SherpaError {
  let _db = graph.get_db();
  let _goals = end_items.iter().flat_map(|i| get_goal_items(&graph, i)).collect::<OrderedSet<_>>();
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
pub(super) fn lr_disabled_error<'db>(graph: &mut GraphHost<'db>, parent: StateId) -> SherpaResult<()> {
  let db = graph.get_db();

  let s_store = db.string_store();

  let nonterms = graph[parent].get_nonterm_items();

  if nonterms.len() == 1 {
    let first = nonterms.first().unwrap();
    if first.is_left_recursive() {
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
      + &db.nonterm_friendly_name_string(graph.get_goal_nonterm_index())
      + "]",
    ps_msg:   "Consider enabling lr parsing through the parser config object".into(),
    severity: SherpaErrorSeverity::Critical,
  });
}

pub fn conflicting_symbols_error<'db>(
  graph: &mut GraphHost<'db>,
  groups: OrderedMap<(u16, SymbolId), ItemSet<'db>>,
) -> SherpaError {
  let db = graph.get_db();
  SherpaError::SourcesError {
    id:       (GraphConstruction, 0, "conflicting-symbols").into(),
    msg:      "Found ".to_string() + &groups.len().to_string() + " conflicting tokens. This grammar has an ambiguous scanner",
    ps_msg:   Default::default(),
    severity: SherpaErrorSeverity::Critical,
    sources:  groups
      .iter()
      .map(|(_sym, items)| {
        items.iter().map(|i| (i.rule().tok.clone(), i.rule().g_id.path.to_path(db.string_store()), Default::default()))
      })
      .flatten()
      .collect(),
  }
}

pub fn peek_not_allowed_error<'db>(graph: &mut GraphHost<'db>, parent: StateId) -> SherpaError {
  let state = &graph[parent];
  let parent = &graph[graph[parent].get_parent()];

  let db = graph.get_db();
  let peek_groups = state.get_resolve_states();

  if let sym @ SymbolId::DBNonTerminal { key } = state.get_symbol().sym() {
    let source_nonterms = parent.get_nonterm_items().iter().filter(|i| i.nonterm_index_at_sym().is_some_and(|k| k == key));
    SherpaError::SourcesError {
      id:       (ForbiddenPeek, 0, "disabled-peeking").into(),
      msg:      "Cannot create lookahead states to resolve items in rules for [".to_string()
        + &db.nonterm_friendly_name_string(key)
        + "] when peeking is disabled",
      ps_msg:   "Enable peeking to disambiguate these states.".into(),
      severity: SherpaErrorSeverity::Critical,
      sources:  peek_groups
        .clone()
        .flatten()
        .filter(|i| !i.is_out_of_scope())
        .map(|i| {
          (
            if !i.is_complete() { i.rule().symbols[i.sym_index as usize].loc.clone() } else { i.rule().tok.clone() },
            i.rule().g_id.path.to_path(db.string_store()),
            if i.is_complete() {
              "Reduce to [".to_string()
                + &db.nonterm_friendly_name_string(i.nonterm_index())
                + "]? | "
                + &i.to_canonical().debug_string()
            } else {
              "Continue shifting? | ".to_string() + &i.to_canonical().debug_string()
            },
          )
        })
        .chain(source_nonterms.filter(|i: &&Item<'_>| !i.is_out_of_scope()).map(|i| {
          (
            i.rule().tok.clone(),
            i.rule().g_id.path.to_path(db.string_store()),
            "Yield the non-terminal [".to_string() + &sym.debug_string(db) + "]?",
          )
        }))
        .dedup(),
    }
  } else {
    todo!("Handle normal LL case")
  }
}
