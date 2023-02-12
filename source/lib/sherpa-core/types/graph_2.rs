use super::{
  item_2::{Item, ItemContainer, ItemType},
  GrammarStore,
  ProductionId,
  Symbol,
  SymbolID,
};
use crate::grammar::{
  compile::parser::sherpa::Grammar,
  get_production_start_items2,
  hash_id_value_u64,
};
use std::{
  collections::{binary_heap::Iter, BTreeMap, BTreeSet, HashMap, VecDeque},
  hash::Hash,
  ops::{Index, IndexMut},
  sync::Arc,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Origin {
  None,
  ProdGoal(ProductionId),
  SymGoal(SymbolID),
  Peek(u16, StateId),
  OutOfScope,
  GotoOutOfScope,
  GotoOutOfScopePass,
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
        format!("ProdGoal[ {} ]", g.get_production_plain_name(&prod_id))
      }
      Origin::SymGoal(sym_id) => {
        format!("SymGoal[ {} ]", sym_id.debug_string(g))
      }
      _ => format!("{:?}", self),
    }
  }

  pub fn is_out_of_scope(&self) -> bool {
    matches!(self, Origin::OutOfScope | Origin::GotoOutOfScope | Origin::GotoOutOfScopePass)
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
pub(crate) enum TransitionType {
  Undefined,
  Start,
  Shift,
  Goto,
  GotoLoop,
  GotoPass,
  Peek,
  ReduceProduction,
  AssignToken,
  Call,
  /// Creates a leaf state that has a single `pop` instruction
  OutOfScopePop,
  /// Creates a leaf state that has a single `pass` instruction
  OutOfScopePass,
  FirstMatch,
  LongestMatch,
  ShortestMatch,
}

impl Default for TransitionType {
  fn default() -> Self {
    Self::Undefined
  }
}

pub(crate) const OutScopeLane: u32 = 600000;

// State -------------------------------------------------------------

#[derive(Debug, Default, PartialEq, Eq)]
pub(crate) struct State {
  id: StateId,
  term_symbol: SymbolID,
  call_prod: Option<ProductionId>,
  t_type: TransitionType,
  parent: StateId,
  predecessors: BTreeSet<StateId>,
  kernel_items: Vec<Item>,
  peek_resolve_items: BTreeMap<u16, Vec<Item>>,
  non_terminals: BTreeSet<Item>,
  reduce_item: Option<Item>,
  leaf_state: bool,
}

impl Hash for State {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.t_type.hash(state);

    self.term_symbol.hash(state);

    for item in &self.kernel_items.as_set() {
      item.rule.hash(state);
      item.off.hash(state);
    }

    for item in &self.peek_resolve_items.values().flatten().collect::<BTreeSet<_>>() {
      item.rule.hash(state);
      item.off.hash(state);
    }

    if let Some(item) = self.reduce_item {
      item.rule.hash(state);
      item.off.hash(state);
    }

    self.call_prod.hash(state);
  }
}

impl State {
  pub(crate) fn set_reduce_item(&mut self, item: Item) {
    debug_assert!(item.completed());
    self.reduce_item = Some(item);
  }

  pub(crate) fn set_transition_type(&mut self, transition_type: TransitionType) {
    self.t_type = transition_type;
  }

  /// Set a group of items that a peek item will resolve to.
  pub(crate) fn set_peek_resolve_items(&mut self, peek_origin: u16, items: Vec<Item>) {
    self.peek_resolve_items.insert(peek_origin, items);
  }

  pub(crate) fn get_resolve_items(&self, peek_origin: u16) -> Vec<Item> {
    self.peek_resolve_items.get(&peek_origin).unwrap().clone()
  }

  pub(crate) fn get_nonterminal_items(&self) -> Vec<Item> {
    self.non_terminals.clone().as_vec()
  }

  pub(crate) fn get_kernel_items(&self) -> Vec<Item> {
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

  pub(crate) fn set_non_terminals(&mut self, non_terms: BTreeSet<Item>) {
    self.non_terminals = non_terms;
  }

  pub(crate) fn get_transition_type(&self) -> TransitionType {
    self.t_type
  }

  pub(crate) fn num_of_kernel_items(&self) -> usize {
    self.kernel_items.len()
  }

  pub(crate) fn get_closure(&self, g: &GrammarStore, is_scanner: bool) -> BTreeSet<Item> {
    let items = self.kernel_items.clone();
    let origin_state = self.id;

    get_closure(items, g, origin_state, is_scanner)
  }

  pub(crate) fn get_root_closure(&self, g: &GrammarStore, is_scanner: bool) -> BTreeSet<Item> {
    let mut closure = self.get_closure(g, is_scanner);
    if self.id.is_root() {
      let mut signatures = closure.iter().map(|i| (i.rule, i.off)).collect::<BTreeSet<_>>();
      // Incorporate items that are out of scope within this closure.
      let mut queue = VecDeque::from_iter(self.kernel_items.iter().map(|i| i.get_prod_id(g)));
      let mut seen = BTreeSet::new();
      while let Some(prod_id) = queue.pop_front() {
        if seen.insert(prod_id) {
          if let Some(lr_items) = g.lr_items.get(&prod_id) {
            for item in lr_items {
              if signatures.insert((item.get_rule_id(), item.get_offset())) {
                if item.increment().unwrap().completed() {
                  queue.push_back(item.get_prod_id(g));
                }
                let item = Item {
                  goal_index: OutScopeLane,
                  len: item.len() as u8,
                  rule: item.get_rule_id(),
                  off: item.get_offset(),
                  origin: Origin::OutOfScope,
                  origin_state: self.id,
                  ..Default::default()
                };
                closure.insert(item);
              }
            }
          }
        }
      }
    }
    closure
  }

  pub(crate) fn conflicting_production_call(
    &self,
    prod_id: ProductionId,
    g: &GrammarStore,
    is_scanner: bool,
  ) -> bool {
    if self.id.is_root() {
      let prod_ids = self.kernel_items.iter().map(|i| i.get_prod_id(g)).collect::<BTreeSet<_>>();

      get_closure(get_production_start_items2(&prod_id, g), g, self.id, is_scanner).into_iter().any(
        |i| {
          prod_ids.contains(&i.get_production_id_at_sym(g)) || prod_ids.contains(&i.get_prod_id(g))
        },
      )
    } else {
      false
    }
  }

  pub(crate) fn set_kernel_items(&mut self, kernel_items: Vec<Item>) {
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
  }

  pub(crate) fn set_call_symbol(&mut self, prod_id: ProductionId) {
    self.call_prod = Some(prod_id);
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

  pub(crate) fn get_call_production_id(&self) -> Option<ProductionId> {
    self.call_prod
  }

  pub(crate) fn debug_string(&self, g: &GrammarStore) -> String {
    let mut string = String::new();
    string += &format!("\n\nSTATE -- [{:}] -- ", self.id.0);

    if self.predecessors.len() > 0 {
      string += &format!(
        r##" preds [{}]"##,
        self.predecessors.iter().map(|p| p.0.to_string()).collect::<Vec<_>>().join(" ")
      );
    }

    string += &format!("\n\n Type {:?}; Sym: [{}]", self.t_type, self.term_symbol.debug_string(g));

    if let Some(call_prod) = &self.call_prod {
      string += &format!(" Call: {}", g.get_production_plain_name(call_prod));
    }

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

pub(crate) fn get_closure(
  items: Vec<Item>,
  g: &GrammarStore,
  origin_state: StateId,
  is_scanner: bool,
) -> BTreeSet<Item> {
  let mut out = BTreeSet::new();
  let mut queue = VecDeque::from_iter(items);
  while let Some(kernel_item) = queue.pop_front() {
    match kernel_item.get_type(g) {
      ItemType::Completed(_) | ItemType::Terminal(_) => {
        out.insert(kernel_item);
      }
      ItemType::TokenProduction(prod_id, sym) => {
        if out.insert(kernel_item) && is_scanner {
          for item_in in get_production_start_items2(&prod_id, g) {
            queue.push_back(item_in.align(&kernel_item).to_origin_state(origin_state));
          }
        }
      }
      ItemType::NonTerminal(prod_id) => {
        if out.insert(kernel_item) {
          for item_in in get_production_start_items2(&prod_id, g) {
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

pub(crate) struct Graph {
  state_map:      HashMap<u64, StateId>,
  states:         Vec<State>,
  leaf_states:    BTreeSet<StateId>,
  pending_states: VecDeque<(GraphState, StateId)>,
  pub grammar:    Arc<GrammarStore>,
}

impl Graph {
  pub(crate) fn new(grammar: Arc<GrammarStore>) -> Self {
    Self {
      state_map: Default::default(),
      states: Default::default(),
      leaf_states: Default::default(),
      pending_states: Default::default(),
      grammar,
    }
  }

  pub(crate) fn create_state(
    &mut self,
    symbol: SymbolID,
    t_type: TransitionType,
    parent: Option<StateId>,
    kernel_items: Vec<Item>,
  ) -> StateId {
    let id = StateId(self.states.len());

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

    state.set_kernel_items(if self.states.is_empty() {
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
    });

    self.states.push(state);

    id
  }

  pub(crate) fn __debug_print__(&self, g: &GrammarStore) {
    let string = self.__debug_string__(g);
    eprintln!(
      "Leaf States -- [ {} ]",
      self.leaf_states.iter().map(|s| s.0.to_string()).collect::<Vec<_>>().join(" ")
    );
    eprintln!("{string}");
  }

  pub(crate) fn __debug_string__(&self, g: &GrammarStore) -> String {
    let mut string = String::new();
    let g = &self.grammar;

    for state in &self.states {
      string += &state.debug_string(g);
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

  pub(crate) fn item_is_goal(&self, item: Item) -> bool {
    if !item.completed() {
      return false;
    }

    self.states[0].kernel_items.iter().any(|i| i.rule == item.rule)
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
