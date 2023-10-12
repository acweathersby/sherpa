pub(crate) mod build;
mod errors;
pub(crate) mod flow;
pub mod graph;
pub(crate) mod items;

use self::graph::{GraphBuilder, GraphHost, GraphType};
use crate::types::*;

use super::build_states::StateConstructionError;

pub(crate) fn build<'follow, 'db: 'follow>(
  name: IString,
  graph_type: GraphType,
  kernel_items: ItemSet<'db>,
  db: &'db ParserDatabase,
  config: ParserConfig,
) -> Result<(ParserClassification, GraphHost<'db>), StateConstructionError<()>> {
  let mut gb = GraphBuilder::new(db, name, graph_type, config, kernel_items);

  gb.run();

  #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
  if !gb.is_scanner() {
    crate::test::utils::write_debug_file(
      db,
      "parse_graph.tmp",
      "----------------------------------------------\n".to_string()
        + &gb.get_classification().get_type()
        + "\n"
        + &gb.graph()._debug_string_(),
      true,
    )
    .unwrap();
  } else {
    crate::test::utils::write_debug_file(db, "scanner_graph.tmp", gb.graph()._debug_string_(), true).unwrap();
  }

  let (class, graph, errors, have_non_deterministic_peek) = gb.into_inner();

  if errors.len() > 0 {
    if have_non_deterministic_peek {
      Err(StateConstructionError::NonDeterministicPeek((), errors))
    } else {
      Err(StateConstructionError::OtherErrors((), errors))
    }
  } else {
    Ok((class, graph))
  }
}
