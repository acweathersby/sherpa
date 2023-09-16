mod build;
mod errors;
mod flow;
pub mod graph;
pub(crate) mod items;

use crate::{journal::Journal, types::*};

use self::graph::{GraphBuilder, GraphHost, GraphType};

pub(crate) fn build<'follow, 'db: 'follow>(
  j: &mut Journal,
  name: IString,
  graph_type: GraphType,
  kernel_items: ItemSet<'db>,
  db: &'db ParserDatabase,
  config: ParserConfig,
) -> SherpaResult<(ParserClassification, GraphHost<'db>)> {
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
    )?;
  } else {
    crate::test::utils::write_debug_file(db, "scanner_graph.tmp", gb.graph()._debug_string_(), true)?;
  }

  let (class, graph, errors) = gb.into_inner();

  for error in errors {
    j.report_mut().add_error(error)
  }

  j.report_mut().wrap_ok_or_return_errors((class, graph))
}
