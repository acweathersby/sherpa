pub(crate) mod build;
mod errors;
pub(crate) mod flow;
pub mod graph;
pub(crate) mod items;
use self::graph::{ConcurrentGraphBuilder, StagedNode};
use super::build_graph::graph::{GraphType, Origin, StateId, StateType};
use crate::{compile::states::build_graph_beta::build::handle_kernel_items, types::*, RadlrGrammar};

/// Add a root node to the graph queue. This type of node is also added to the
/// global pending root nodes pool.
pub fn add_root(
  name: IString,
  graph_type: GraphType,
  kernel_items: ItemSet,
  builder: &mut ConcurrentGraphBuilder,
) -> RadlrResult<()> {
  StagedNode::new()
    .kernel_items(kernel_items.to_vec().into_iter())
    .ty(StateType::Start)
    .graph_ty(graph_type)
    .make_root()
    .commit(builder);

  builder.commit(false, StateId::default());
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

  dbg!(graph_builder);
  Ok(())
}

#[test]
fn test_build_graph() -> RadlrResult<()> {
  let config = ParserConfig::default();
  let db = RadlrGrammar::new()
    .add_source_from_string(
      r###"
  IGNORE { c:sp }
  

  <> C > A "dd"

  <> A > "test" "d"
  
  "###,
      "",
      false,
    )
    .unwrap()
    .build_db("", config)
    .unwrap()
    .into_internal();

  let mut builder = ConcurrentGraphBuilder::new(db.clone(), config);

  for result in db.nonterms().iter().enumerate().map(|(index, sym)| {
    let nt_id: DBNonTermKey = (index as u32).into();
    let kernel_items = ItemSet::start_items(nt_id, &db).to_origin(Origin::NonTermGoal(nt_id));
    add_root(IString::default(), if sym.is_term() { GraphType::Scanner } else { GraphType::Parser }, kernel_items, &mut builder)
  }) {
    result?;
  }

  run(builder)?;

  Ok(())
}
