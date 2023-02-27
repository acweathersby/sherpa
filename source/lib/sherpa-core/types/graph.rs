use super::{
  item::{Item, ItemContainer, ItemContainerIter, ItemType},
  GrammarStore,
  ItemSet,
  Items,
  ProductionId,
  RuleId,
  Symbol,
  SymbolID,
};
use crate::grammar::{
  compile::parser::sherpa::Grammar,
  get_production_start_items,
  hash_id_value_u64,
};
use std::{
  collections::{
    binary_heap::Iter,
    hash_map::DefaultHasher,
    BTreeMap,
    BTreeSet,
    HashMap,
    VecDeque,
  },
  fmt::format,
  hash::{Hash, Hasher},
  ops::{Index, IndexMut},
  sync::Arc,
};

/// Indicates the State type that generated
/// the item
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Origin {
  None,
  ProdGoal(ProductionId),
  SymGoal(SymbolID),
  Peek(u16, StateId),
  // Out of scope item that was generated from the
  // completion of a token production.
  ScanCompleteOOS,
  /// Generated when the a goal production is completed.
  /// Goal productions are determined by the
  /// root state (`StateId(0)`) kernel items
  GoalCompleteOOS,
}

impl Default for Origin {
  fn default() -> Self {
    Self::None
  }
}

impl Origin {
  pub fn debug_string(&self, g: &GrammarStore) -> String {
    match self {
      Origin::ProdGoal(prod_id) => {
        let prod = g.get_production(&prod_id).unwrap();
        format!("ProdGoal[ {} {:?} ]", prod.name, prod.bytecode_id)
      }
      Origin::SymGoal(sym_id) => {
        format!("SymGoal[ {} {:?} ]", sym_id.debug_string(g), sym_id.bytecode_id(g))
      }
      _ => format!("{:?}", self),
    }
  }

  pub fn is_none(&self) -> bool {
    matches!(self, Origin::None)
  }

  pub fn is_out_of_scope(&self) -> bool {
    matches!(self, Origin::GoalCompleteOOS | Origin::ScanCompleteOOS)
  }

  pub fn get_symbol(&self) -> SymbolID {
    match self {
      Origin::SymGoal(sym_id) => *sym_id,
      _ => SymbolID::Undefined,
    }
  }
}

// Transtion Type ----------------------------------------------------

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub(crate) enum StateType {
  Undefined,
  Start,
  Shift,
  KernelGoto,
  GotoLoop,
  GotoPass,
  Peek,
  PeekEnd,
  Complete,
  Follow,
  AssignAndFollow(SymbolID),
  Reduce(RuleId),
  AssignToken(SymbolID),
  /// Calls made on items within a state's closure but
  /// are not kernel items.
  InternalCall(ProductionId),
  /// Calls made on kernel items
  KernelCall(ProductionId),
  /// Creates a leaf state that has a single `pop` instruction,
  /// with the intent of removing a goto floor state.
  ProductionCompleteOOS,
  /// Creates a leaf state that has a single `pop` instruction,
  /// with the intent of removing a goto floor state.
  PeekProductionCompleteOOS,
  /// Creates a leaf state that has a single `pass` instruction.
  ScannerCompleteOOS,
  FirstMatch,
  LongestMatch,
  ShortestMatch,
}

impl Default for StateType {
  fn default() -> Self {
    Self::Undefined
  }
}

impl StateType {
  pub fn is_out_of_scope(&self) -> bool {
    use StateType::*;
    matches!(self, ProductionCompleteOOS | ScannerCompleteOOS | PeekProductionCompleteOOS)
  }

  pub fn is_goto(&self) -> bool {
    use StateType::*;
    matches!(self, GotoLoop | GotoPass | KernelGoto)
  }

  fn debug_string(&self, g: &GrammarStore) -> String {
    match self {
      Self::KernelCall(prod_id) => {
        format!("KernelCall({})", g.get_production_plain_name(prod_id))
      }
      Self::InternalCall(prod_id) => {
        format!("InternalCall({})", g.get_production_plain_name(prod_id))
      }
      Self::AssignAndFollow(sym_id) => {
        format!("AssignAndFollow({})", sym_id.debug_string(g))
      }
      Self::AssignToken(sym_id) => {
        format!("AssignToken({})", sym_id.debug_string(g))
      }
      Self::Reduce(rule_id) => {
        format!("Reduce({})", g.get_production_plain_name(&g.rules.get(rule_id).unwrap().prod_id))
      }
      _ => format!("{:?}", self),
    }
  }
}
pub(crate) const OutScopeIndex: u32 = 0xFEEDDEED;

// State -------------------------------------------------------------

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct State {
  id: StateId,
  term_symbol: SymbolID,
  t_type: StateType,
  parent: StateId,
  predecessors: BTreeSet<StateId>,
  kernel_items: BTreeSet<Item>,
  peek_resolve_items: BTreeMap<u16, Items>,
  non_terminals: BTreeSet<Item>,
  reduce_item: Option<Item>,
  leaf_state: bool,
  closure: Option<ItemSet>,
}

impl Hash for State {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.t_type.hash(state);

    self.term_symbol.hash(state);

    for item in &self.kernel_items {
      item.rule_id.hash(state);
      item.off.hash(state);
      item.origin.hash(state);
    }

    for item in &self.peek_resolve_items.values().flatten().collect::<BTreeSet<_>>() {
      item.rule_id.hash(state);
      item.off.hash(state);
    }

    if let Some(item) = self.reduce_item {
      item.rule_id.hash(state);
      item.off.hash(state);
    }
  }
}

impl State {
  pub(crate) fn get_hash(&self) -> u64 {
    let mut hasher = DefaultHasher::new();
    self.hash(&mut hasher);
    hasher.finish()
  }

  pub(crate) fn set_reduce_item(&mut self, item: Item) {
    debug_assert!(item.is_completed());
    self.reduce_item = Some(item);
  }

  pub(crate) fn set_transition_type(&mut self, transition_type: StateType) {
    self.t_type = transition_type;
  }

  /// Set a group of items that a peek item will resolve to.
  pub(crate) fn set_peek_resolve_items(&mut self, peek_origin: u16, items: Items) {
    self.peek_resolve_items.insert(peek_origin, items);
  }

  pub(crate) fn get_resolve_items(&self, peek_origin: u16) -> Items {
    self.peek_resolve_items.get(&peek_origin).unwrap().clone()
  }

  pub(crate) fn get_nonterminal_items(&self) -> Items {
    self.non_terminals.iter().to_vec()
  }

  pub(crate) fn get_kernel_items_vec(&self) -> Items {
    self.kernel_items.iter().to_vec()
  }

  pub(crate) fn get_kernel_items_ref(&self) -> &ItemSet {
    &self.kernel_items
  }

  pub(crate) fn get_kernel_items(&self) -> ItemSet {
    self.kernel_items.clone()
  }

  pub(crate) fn get_predecessors(&self) -> &BTreeSet<StateId> {
    &self.predecessors
  }

  pub(crate) fn get_id(&self) -> StateId {
    self.id
  }

  pub(crate) fn is_leaf_state(&self) -> bool {
    self.leaf_state
  }

  pub(crate) fn set_non_terminals(&mut self, non_terms: &BTreeSet<Item>) {
    self.non_terminals = non_terms.clone();
  }

  pub(crate) fn get_type(&self) -> StateType {
    self.t_type
  }

  pub(crate) fn num_of_kernel_items(&self) -> usize {
    self.kernel_items.len()
  }

  pub(crate) fn get_closure(&self, g: &GrammarStore, is_scanner: bool) -> BTreeSet<Item> {
    if self.id.is_root() {
      self.closure.as_ref().unwrap().clone().inscope_items()
    } else {
      self.closure.as_ref().unwrap().clone()
    }
  }

  pub(crate) fn get_root_closure(&self, g: &GrammarStore, is_scanner: bool) -> BTreeSet<Item> {
    self.closure.as_ref().unwrap().clone()
  }

  fn compute_closure(&mut self, g: &GrammarStore, is_scanner: bool) {
    if self.kernel_items.len() > 0 {
      let origin_state = self.id;
      let mut closure = get_closure(self.kernel_items.iter(), g, origin_state, is_scanner);

      if origin_state.is_root() {
        let mut signatures = closure.iter().map(|i| (i.rule_id, i.off)).collect::<BTreeSet<_>>();
        // Incorporate items that are out of scope within this closure.
        let mut queue = VecDeque::from_iter(self.kernel_items.iter().map(|i| i.get_prod_id(g)));
        let mut seen = BTreeSet::new();
        while let Some(prod_id) = queue.pop_front() {
          if seen.insert(prod_id) {
            if let Some(lr_items) = g.lr_items.get(&prod_id) {
              for item in lr_items {
                if signatures.insert((item.get_rule_id(), item.off)) {
                  if item.increment().unwrap().is_completed() {
                    queue.push_back(item.get_prod_id(g));
                  }

                  closure.insert(item.to_origin_state(origin_state).to_oos_index());
                }
              }
            }
          }
        }
      }

      self.closure = Some(closure);
    }
  }

  /// Returns true if the the call of the production leads to infinite loop due to left recursion.
  pub(crate) fn conflicting_production_call(
    &self,
    prod_id: ProductionId,
    g: &GrammarStore,
    is_scanner: bool,
  ) -> bool {
    if self.id.is_root() {
      let prod_ids = self.kernel_items.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>();

      get_closure(get_production_start_items(&prod_id, g).iter(), g, self.id, is_scanner)
        .into_iter()
        .any(|i| {
          prod_ids.contains(&i.get_production_id_at_sym(g)) || prod_ids.contains(&i.get_prod_id(g))
        })
    } else {
      false
    }
  }

  pub(crate) fn add_kernel_items<T: ItemContainer>(
    &mut self,
    g: &GrammarStore,
    kernel_items: T,
    is_scanner: bool,
  ) {
    let mut kernel_items =
      kernel_items.into_iter().map(|mut i| i.to_origin_state(self.id)).collect();

    self.kernel_items.append(&mut kernel_items);

    self.compute_closure(g, is_scanner);
  }

  pub(crate) fn set_kernel_items(
    &mut self,
    g: &GrammarStore,
    kernel_items: Items,
    is_scanner: bool,
  ) {
    let kernel_items = kernel_items
      .into_iter()
      .map(|mut i| {
        if i.origin_state.is_invalid() {
          i.origin_state = self.id;
        }
        i
      })
      .collect();

    self.kernel_items = kernel_items;

    self.compute_closure(g, is_scanner);
  }

  pub(crate) fn get_parent(&self) -> StateId {
    self.parent
  }

  pub(crate) fn get_reduce_item(&self) -> Option<Item> {
    self.reduce_item
  }

  pub(crate) fn get_symbol(&self) -> SymbolID {
    self.term_symbol
  }

  pub(crate) fn debug_string(&self, g: &GrammarStore) -> String {
    let mut string = String::new();
    string += &format!("\n\nSTATE -- [{:}] --", self.id.0);

    if self.predecessors.len() > 0 {
      string += &format!(
        r##" preds [{}]"##,
        self.predecessors.iter().map(|p| p.0.to_string()).collect::<Vec<_>>().join(" ")
      );
    }

    string += &format!(
      "\n\n Type {:?}; Sym: [{} : {:X}]",
      self.t_type.debug_string(g),
      self.term_symbol.debug_string(g),
      self.term_symbol.bytecode_id(g)
    );

    for (index, items) in &self.peek_resolve_items {
      string += &format!("\n  Peek Resolve: {}", index);
      for item in items {
        string += &format!("\n   - {}", item.debug_string(g));
      }
    }

    if !self.non_terminals.is_empty() {
      string += "\n-- non-terms:";
      for (item) in &self.non_terminals {
        string += &format!("\n   - {}", item.debug_string(g));
      }
    }

    if let Some(item) = &self.reduce_item {
      string += &format!("\n  Reduce:");
      string += &format!("\n   - {}", item.debug_string(g));
    }

    string += "\n";

    for item in &self.kernel_items {
      string += &format!("\n    {}", item.debug_string(g));
    }
    string
  }
}

pub(crate) fn get_closure<'a, T: ItemContainerIter<'a>>(
  items: T,
  g: &GrammarStore,
  origin_state: StateId,
  is_scanner: bool,
) -> BTreeSet<Item> {
  let mut out = BTreeSet::new();
  let mut queue = VecDeque::from_iter(items.cloned());
  while let Some(kernel_item) = queue.pop_front() {
    match kernel_item.get_type(g) {
      ItemType::ExclusiveCompleted(..) | ItemType::Completed(_) | ItemType::Terminal(_) => {
        out.insert(kernel_item);
      }
      ItemType::TokenProduction(prod_id, sym) => {
        if out.insert(kernel_item) && is_scanner {
          for item_in in get_production_start_items(&prod_id, g) {
            queue.push_back(item_in.align(&kernel_item).to_origin_state(origin_state));
          }
        }
      }
      ItemType::NonTerminal(prod_id) => {
        if out.insert(kernel_item) {
          for item_in in get_production_start_items(&prod_id, g) {
            queue.push_back(item_in.align(&kernel_item).to_origin_state(origin_state));
          }
        }
      }
    }
  }
  out
}

// Graph -------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum GraphState {
  Normal,
  Peek,
  LongestMatch,
  ShortestMatch,
  FirstMatch,
  //BreadCrumb,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GraphMode {
  /// Classic Recursive Descent Ascent with unlimited lookahead.
  SherpaClimber,
  // Scanner mode for creating tokens, analagous to regular expressions.
  Scanner,
}

pub(crate) struct Graph {
  state_map: HashMap<u64, StateId>,
  states: Vec<State>,
  leaf_states: BTreeSet<StateId>,
  pending_states: VecDeque<(GraphState, StateId)>,
  mode: GraphMode,
  state_name_prefix: String,
  pub grammar: Arc<GrammarStore>,
}

impl Graph {
  pub(crate) fn new(grammar: Arc<GrammarStore>, mode: GraphMode) -> Self {
    Self {
      mode,
      state_map: Default::default(),
      states: Default::default(),
      leaf_states: Default::default(),
      pending_states: Default::default(),
      state_name_prefix: Default::default(),
      grammar,
    }
  }

  pub fn is_scan(&self) -> bool {
    matches!(self.mode, GraphMode::Scanner)
  }

  pub(crate) fn set_prefix(&mut self, append_string: &str) {
    let mut hasher = DefaultHasher::new();
    self.goal_items().hash(&mut hasher);
    self.state_name_prefix = format!(
      "s{:0>16X}{}_{append_string}",
      hasher.finish(),
      self.is_scan().then_some("_scan").unwrap_or_default()
    );
  }

  pub(crate) fn get_prefix(&self) -> &str {
    &self.state_name_prefix
  }

  pub(crate) fn create_state(
    &mut self,
    symbol: SymbolID,
    t_type: StateType,
    parent: Option<StateId>,
    kernel_items: Items,
  ) -> StateId {
    let id = StateId(self.states.len());
    let is_scan = self.is_scan();
    let g = &(self.grammar.clone());

    let mut state = match parent {
      Some(parent) => State {
        id,
        term_symbol: symbol,
        t_type,
        parent,
        predecessors: BTreeSet::from_iter(vec![parent]),
        ..Default::default()
      },
      None => State { id, term_symbol: symbol, t_type, ..Default::default() },
    };

    state.set_kernel_items(
      g,
      if self.states.is_empty() {
        /// Automatically setup lanes a goal values.
        kernel_items
          .into_iter()
          .enumerate()
          .map(|(i, mut item)| {
            item.goal_index = i as u32;
            //item.origin = Origin::ProdGoal(i as u16);
            item
          })
          .collect()
      } else {
        kernel_items
      },
      is_scan,
    );

    self.states.push(state);

    id
  }

  pub(crate) fn __debug_print__(&self) {
    let string = self.__debug_string__();
    println!(
      "\n\n\n--------------------------------------------------------
      Leaf States -- [ {} ]",
      self.leaf_states.iter().map(|s| s.0.to_string()).collect::<Vec<_>>().join(" ")
    );
    println!("{string}");
  }

  pub(crate) fn __debug_string__(&self) -> String {
    let mut string = String::new();
    let g = &self.grammar;

    for state in &self.states {
      let is_leaf_state = self.leaf_states.contains(&state.get_id());
      if is_leaf_state {
        string += "\nLEAF STATE ----------------------------------";
        string += &state.debug_string(g);
        string += "\n---------------------------------------------";
      } else {
        string += &state.debug_string(g);
      }
    }
    string
  }

  pub(crate) fn add_leaf_state(&mut self, state: StateId) {
    self.leaf_states.insert(state);
    self[state].leaf_state = true;
  }

  pub(crate) fn get_leaf_states(&self) -> Vec<&State> {
    self.leaf_states.iter().map(|s| &self[*s]).collect()
  }

  pub(crate) fn enqueue_pending_state(
    &mut self,
    graph_state: GraphState,
    state: StateId,
  ) -> Option<StateId> {
    let hash_id = hash_id_value_u64(&self[state]);

    if let Some(original_state) = self.state_map.get(&hash_id).cloned() {
      let parent = self[state].parent;
      self[original_state].predecessors.insert(parent);
      if state.0 == self.states.len() - 1 {
        drop(self.states.pop());
      }
      None
    } else {
      self.state_map.insert(hash_id, state);
      self.pending_states.push_back((graph_state, state));
      Some(state)
    }
  }

  pub(crate) fn dequeue_pending_state(&mut self) -> Option<(GraphState, StateId)> {
    self.pending_states.pop_front()
  }

  pub(crate) fn goal_items(&self) -> &ItemSet {
    &self.states[0].kernel_items
  }

  pub(crate) fn item_is_goal(&self, item: Item) -> bool {
    if !item.is_completed() {
      return false;
    }

    self.states[0].kernel_items.iter().any(|i| i.rule_id == item.rule_id)
  }
}

impl Index<usize> for Graph {
  type Output = State;

  fn index(&self, index: usize) -> &Self::Output {
    &self.states[index]
  }
}

impl Index<StateId> for Graph {
  type Output = State;

  fn index(&self, index: StateId) -> &Self::Output {
    &self.states[index.0]
  }
}

impl IndexMut<StateId> for Graph {
  fn index_mut(&mut self, index: StateId) -> &mut Self::Output {
    &mut self.states[index.0]
  }
}

// STATE ID -------------------------------------------------------------

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct StateId(pub usize);

impl Default for StateId {
  fn default() -> Self {
    Self(usize::MAX)
  }
}

impl StateId {
  pub fn is_invalid(&self) -> bool {
    self.0 == usize::MAX
  }

  pub fn is_root(&self) -> bool {
    self.0 == 0
  }

  pub fn to_post_reduce(&self) -> Self {
    Self(self.0 + (usize::MAX >> 2))
  }

  pub fn to_goto(&self) -> Self {
    Self(self.0 + (usize::MAX >> 1))
  }
}

impl PartialEq<usize> for State {
  fn eq(&self, other: &usize) -> bool {
    self.id.0 == *other
  }
}
