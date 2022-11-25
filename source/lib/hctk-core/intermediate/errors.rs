use std::collections::BTreeSet;
use std::sync::Arc;

use crate::grammar::get_closure_cached;
use crate::types::*;

/// Warning when a fork state is generated for items that have ambiguous production calls.
#[derive(Debug)]
pub struct WarnTransitionAmbiguousProduction {
  source_production: Production,
  body_refs:         Vec<(Arc<GrammarIds>, Token)>,
}

impl WarnTransitionAmbiguousProduction {
  pub const friendly_name: &'static str = "ErrTransitionAmbiguousProduction";

  /// Checks to see if the conditions of this warning are met. If so
  /// then returns `HCResult::Ok(HCError)`, or `HCResult::None` otherwise.
  pub fn check(t: &TransitionPack, conflicting_goals: &Vec<TGNId>) -> HCResult<HCError> {
    let conflicting_nodes = conflicting_goals.iter().map(|n| t.get_node(*n)).collect::<Vec<_>>();
    let peek_root_node = t.get_peek_origin(conflicting_goals[0]);

    // Look for a common production in each goal. If such production(s) exist,
    // issue warning(s) about production occlusion.
    let mut closures = conflicting_goals
      .iter()
      .map(|i| {
        get_closure_cached(&t.get_node(*i).first_item(), &t.g)
          .iter()
          .map(|i| (i.get_symbol(&t.g), i))
          .collect::<Vec<_>>()
      })
      .collect::<Vec<_>>();

    let smallest = closures
      .iter()
      .fold(&closures[0], |f, i| match i.len() < f.len() {
        true => i,
        false => f,
      })
      .clone();

    // Get a set of symbols that are present in all closures.
    let common_symbols = smallest
      .iter()
      .filter_map(|(sym, i)| match closures.iter().all(|c| c.iter().any(|(s, _)| s == sym)) {
        true => Some(sym),
        false => None,
      })
      .collect::<BTreeSet<_>>();
    // For each closure, remove all items that do not have a symbols that matches one in common_symbols,
    // or that is of a production whose id is in common_symbols
    closures.iter_mut().for_each(|c| {
      c.drain_filter(|(s, i)| {
        (!common_symbols.contains(s) || common_symbols.contains(&i.get_prod_as_sym_id(&t.g)))
      });
    });
    // At this point we should have isolated the items responsible for the ambiguous parse, provided
    // we have set of non-empty closures. We can now display an appropriate message to the
    // user regarding the nature of the ambiguous parse producing bodies.
    if closures.iter().all(|c| !c.is_empty()) {
      HCResult::Ok(HCError::ExtendedError(Arc::new(Self {
        source_production: t.g.get_production(t.root_prod_ids.first().unwrap()).unwrap().clone(),
        body_refs:         closures
          .iter()
          .flat_map(|c| {
            c.iter().map(|(_, i)| {
              let prod = i.get_body_ref(&t.g).unwrap();
              (prod.grammar_ref.clone(), prod.tok.clone())
            })
          })
          .collect(),
      })))
    } else {
      HCResult::None
    }
  }
}

impl ExtendedError for WarnTransitionAmbiguousProduction {
  fn report(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let WarnTransitionAmbiguousProduction { source_production, body_refs } = self;

    // let conflicting_productions

    f.write_fmt(format_args!(
      "In the parse path of {}, these production lead to an ambiguous parse:\n{}",
      source_production.name,
      body_refs
        .iter()
        .map(|(grammar_ref, tok)| {
          let Range { start_line, start_column, .. } = tok.get_range();
          format!(
            "[{}:{}:{}]\n{}",
            grammar_ref.path.to_str().unwrap(),
            start_line,
            start_column,
            tok.blame(1, 1, "conflicts with other productions", BlameColor::Red)
          )
        })
        .collect::<Vec<_>>()
        .join("\n")
    ))
  }

  fn severity(&self) -> crate::types::HCErrorSeverity {
    crate::errors::Warning
  }

  fn friendly_name(&self) -> &str {
    WarnTransitionAmbiguousProduction::friendly_name
  }
}
