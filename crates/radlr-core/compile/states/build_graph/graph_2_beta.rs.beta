struct GraphNode {
  canonical_hash: u64,
  parent: Arc<GraphNode>,
  kernel: OrderedSet<Item>,
}


GraphNode -> (GraphNode'NonLeaf[], GraphNode'Leaf[])

GraphNode'NonLeaf[] : ContinueProcessing;

GraphNode'Leaf[] : ReversedGraphTreeRoots'sync;

Work<GraphNode'NonLeaf>'sync

ConanicalSet<Graphnode'conanical_hash, Rw<preds>>'sync

Complete -> Work<GraphNode'NoneLeaf is empty && Our GraphNode only has Leafs or Discard States>



std::thread_local! {

  const mut OOS_STATE_CACHE: OrderedMap<GraphNode> = OrderedMap::new();

}




thread_local! {} 

struct GraphNode {
  canonical_hash: u64,
  is_leaf: bool,
  is_root: bool,
  predecessors: Rw<HashSet<Arc<GraphNode>>>,
  kernel: ItemSet
}


struct Leaves {
  leaves: HashMap<NonTermId, Rw<Vec<GraphNode>>>
}