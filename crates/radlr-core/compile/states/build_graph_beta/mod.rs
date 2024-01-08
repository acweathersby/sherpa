pub(crate) mod build;
mod errors;
pub(crate) mod flow;
pub mod graph;
pub(crate) mod items;
use self::graph::{ConcurrentGraphBuilder, GraphNodeBuilder};
use super::build_graph::graph::{GraphType, StateType};
use crate::{compile::states::build_graph_beta::build::handle_kernel_items, types::*, RadlrGrammar};

/// Add a root node to the graph queue. This type of node is also added to the
/// global pending root nodes pool.
pub fn addRootNode(
  name: IString,
  graph_type: GraphType,
  kernel_items: ItemSet,
  builder: &mut ConcurrentGraphBuilder,
) -> RadlrResult<()> {
  GraphNodeBuilder::new().set_kernel_items(kernel_items.to_vec().into_iter()).set_type(StateType::Start).commit(builder)?;
  Ok(())
}

pub fn run(mut graph_builder: ConcurrentGraphBuilder) -> RadlrResult<()> {
  loop {
    if let Some(node) = graph_builder.get_local_work() {
      handle_kernel_items(&mut graph_builder, &node)?;
    } else if let Some(node) = graph_builder.get_global_work() {
      handle_kernel_items(&mut graph_builder, &node)?;
    } else {
      break;
    }
  }
  Ok(())
}

#[test]
fn test_build_graph() -> RadlrResult<()> {
  let config = ParserConfig::default();
  let db = RadlrGrammar::new()
    .add_source_from_string(
      r###"
  IGNORE { c:sp }


  A > "test"
  
  "###,
      "",
      false,
    )
    .unwrap()
    .build_db("default", config)
    .unwrap()
    .into_internal();
  Ok(())
}
