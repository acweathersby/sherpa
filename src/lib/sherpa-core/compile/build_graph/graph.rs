use crate::{hash_id_value_u64, types::*};
use std::{
  collections::{hash_map::DefaultHasher, BTreeSet, VecDeque},
  hash::Hash,
  ops::{Index, IndexMut},
};

pub const OUT_SCOPE_INDEX: u32 = 0xFEEDDEED;

/// Indicates the State type that generated
/// the item
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Origin {
  None,
  /// The goal non-terminal that this item or it's predecessors will reduce to
  NonTermGoal(DBNonTermKey),
  /// The goal symbol id that this item or its predecessors will recognize
  TerminalGoal(DBTermKey, u16),
  /// The hash and state of the goal items set the peek item will resolve to
  Peek(u64, StateId),
  /// The item
  BreadCrumb(DBRuleKey),
  PEG(DBNonTermKey),
  // Out of scope item that was generated from the
  // completion of a token non-terminal.
  ScanCompleteOOS,
  /// Generated when the a goal non-terminal is completed.
  /// Goal non-terminals are determined by the
  /// root state (`StateId(0)`) kernel items
  GoalCompleteOOS,
}

impl Hash for Origin {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    if let Origin::Peek(resolve_id, _) = self {
      resolve_id.hash(state)
    }
    std::mem::discriminant(self).hash(state)
  }
}

impl Default for Origin {
  fn default() -> Self {
    Self::None
  }
}

impl Origin {
  #[cfg(debug_assertions)]
  pub fn debug_string(&self, db: &ParserDatabase) -> String {
    match self {
      Origin::NonTermGoal(nterm) => {
        format!("NonTermGoal[ {} {:?} ]", db.nonterm_guid_name(*nterm).to_string(db.string_store()), nterm)
      }
      Origin::TerminalGoal(sym_id, prec) => {
        format!("TerminalGoal[ {:?} {prec} ]", sym_id)
      }
      Origin::BreadCrumb(rule_id) => {
        let item = Item::from_rule(*rule_id, db);
        format!("BreadCrumb[ {:?} ]", item.debug_string())
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

  pub fn get_symbol(&self, db: &ParserDatabase) -> SymbolId {
    match self {
      Origin::TerminalGoal(sym_id, ..) => db.sym(*sym_id),
      _ => SymbolId::Undefined,
    }
  }
}

// Transtion Type ----------------------------------------------------

#[derive(Hash, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum StateType {
  Undefined,
  Start,
  Shift,
  /// The completion of this branch will complete one or more kernel items.
  NonTerminalResolve,
  /// The completion of this branch will complete one or more intermediary goto
  /// items.
  NonTerminalShiftLoop,
  NonTerminalComplete,
  Peek,
  /// A peek path has been resolved to a single peek group located in the peek
  /// origin state.
  PeekEndComplete(u64),
  Complete,
  Follow,
  DifferedReduce,
  ShiftPrefix,
  AssignAndFollow(DBTermKey),
  Reduce(DBRuleKey, usize),
  AssignToken(DBTermKey),
  /// Calls made on items within a state's closure but
  /// are not kernel items.
  InternalCall(DBNonTermKey),
  /// Calls made on kernel items
  KernelCall(DBNonTermKey),
  /// Creates a leaf state that has a single `pop` instruction. This represents
  /// the completion of a non-terminal that had a left recursive rule.
  NonTermCompleteOOS,
  /// Creates a leaf state that has a single `pop` instruction,
  /// with the intent of removing a goto floor state.
  _PeekNonTerminalCompleteOOS,
  /// Creates a leaf state that has a single `pass` instruction.
  ScannerCompleteOOS,
  _FirstMatch,
  _LongestMatch,
  _ShortestMatch,
}

impl Default for StateType {
  fn default() -> Self {
    Self::Undefined
  }
}

impl StateType {
  pub fn is_goto(&self) -> bool {
    use StateType::*;
    matches!(self, NonTerminalShiftLoop | NonTerminalComplete | NonTerminalResolve)
  }

  #[cfg(debug_assertions)]
  fn debug_string(&self, db: &ParserDatabase) -> String {
    match self {
      Self::KernelCall(nterm) => {
        format!("KernelCall({})", db.nonterm_friendly_name_string(*nterm))
      }
      Self::InternalCall(nterm) => {
        format!("InternalCall({})", db.nonterm_friendly_name_string(*nterm))
      }
      Self::AssignAndFollow(sym_id) => {
        format!("AssignAndFollow({})", db.sym(*sym_id).debug_string(db))
      }
      Self::AssignToken(sym_id) => {
        format!("AssignToken({})", db.sym(*sym_id).debug_string(db))
      }
      Self::Reduce(nterm, _) => {
        format!("Reduce({nterm:?})",)
      }
      _ => format!("{:?}", self),
    }
  }
}

// State -------------------------------------------------------------

#[derive(Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct State<'db> {
  pub id: StateId,
  pub parent: StateId,
  pub predecessors: OrderedSet<StateId>,
  pub t_type: StateType,
  pub term_symbol: PrecedentSymbol,
  pub kernel_items: ItemSet<'db>,
  pub peek_resolve_items: OrderedMap<u64, ItemSet<'db>>,
  pub reduce_item: Option<Item<'db>>,
  pub nonterm_items: ItemSet<'db>,
  pub leaf_state: bool,
  pub closure: Option<ItemSet<'db>>,
}

impl<'db> Hash for State<'db> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.t_type.hash(state);

    self.term_symbol.hash(state);

    for item in &self.kernel_items {
      item.rule_id.hash(state);
      item.sym_index.hash(state);
      item.origin.hash(state);
      item.goto_distance.hash(state);
    }
  }
}

impl<'db> State<'db> {
  pub fn get_resolve_state(&self, peek_origin_key: u64) -> ItemSet<'db> {
    self.peek_resolve_items.get(&peek_origin_key).unwrap().clone()
  }

  pub fn get_resolve_states(&self) -> impl Iterator<Item = &ItemSet<'db>> + Clone {
    self.peek_resolve_items.values()
  }

  pub fn get_hash(&self) -> u64 {
    use std::hash::Hasher;
    let mut hasher = DefaultHasher::new();
    self.hash(&mut hasher);
    hasher.finish()
  }

  pub fn kernel_items_ref(&self) -> &ItemSet<'db> {
    &self.kernel_items
  }

  pub fn get_closure_ref(&self) -> SherpaResult<&ItemSet<'db>> {
    return o_to_r(self.closure.as_ref(), "Closure Not Created");
  }

  pub fn set_reduce_item(&mut self, item: Item<'db>) {
    debug_assert!(item.is_complete());
    self.reduce_item = Some(item);
  }

  pub fn get_predecessors(&self) -> &BTreeSet<StateId> {
    &self.predecessors
  }

  pub fn get_id(&self) -> StateId {
    self.id
  }

  pub fn get_skipped(&self) -> OrderedSet<SymbolId> {
    self.kernel_items_ref().iter().filter_map(|i| i.get_skipped()).flatten().cloned().collect()
  }

  pub fn set_nonterm_items(&mut self, nonterm_items: &ItemSet<'db>) {
    self.nonterm_items = nonterm_items.clone();
  }

  pub fn get_nonterm_items(&self) -> &ItemSet<'db> {
    &self.nonterm_items
  }

  pub fn get_type(&self) -> StateType {
    self.t_type
  }

  pub fn has_goto_state(&self) -> bool {
    self.nonterm_items.len() > 0
  }

  pub fn get_goto_state(&self) -> Option<Self> {
    if self.nonterm_items.len() > 0 {
      Some(Self {
        term_symbol: self.term_symbol,
        id: self.id.to_goto(),
        t_type: StateType::NonTerminalResolve,
        kernel_items: self.nonterm_items.clone(),
        nonterm_items: self.kernel_items.clone(),
        ..Default::default()
      })
    } else {
      None
    }
  }

  pub(crate) fn get_symbol(&self) -> PrecedentSymbol {
    self.term_symbol
  }

  pub fn get_parent(&self) -> StateId {
    self.parent
  }

  #[cfg(debug_assertions)]
  pub fn debug_string(&self, db: &'db ParserDatabase) -> String {
    let mut string = String::new();
    string += &format!("\n\nSTATE -- [{:}][{:}] --", self.id.0, self.get_hash());

    if self.predecessors.len() > 0 {
      string += &format!(r##" preds [{}]"##, self.predecessors.iter().map(|p| p.0.to_string()).collect::<Vec<_>>().join(" "));
    }

    string += &format!(
      "\n\n Type {:?}; Sym: [{}|{}]",
      self.t_type.debug_string(db),
      self.term_symbol.sym().debug_string(db),
      self.term_symbol.precedence()
    );

    for (index, state) in &self.peek_resolve_items {
      string += &format!("\n  Peek Resolve: {}", index);

      string += &format!("\n   - {}", state.to_debug_string("\n     "));
    }

    if let Some(item) = &self.reduce_item {
      string += &format!("\n  Reduce:");
      string += &format!("\n   - {}", item.debug_string());
    }

    string += "\n-- kernel-items:";
    for item in &self.kernel_items {
      string += &format!("\n   - {}", item.debug_string());
    }

    if !self.nonterm_items.is_empty() {
      if let Some(goto_hash) = self.get_goto_state().and_then(|s| Some(s.get_hash())) {
        string += &format!("\n\nGOTO -- [{:}][{:}] --", self.id.0, goto_hash);
      }
      string += "\n-- non-terms:";
      for item in &self.nonterm_items {
        string += &format!("\n   - {}", item.debug_string());
      }
    }
    string
  }
}

// Graph -------------------------------------------------------------

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphState {
  #[default]
  Normal,
  NormalGoto,
  Peek(u16),
  PEG,
  BreadCrumb(u16),
  Leaf,
  _LongestMatch,
  _ShortestMatch,
  _FirstMatch,
  _BreadCrumb,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphType {
  /// Classic Recursive Descent Ascent with unlimited lookahead.
  Parser,
  // Scanner mode for creating tokens, analogous to regular expressions.
  Scanner,
}

use sherpa_rust_runtime::types::SharedSymbolBuffer;
use GraphState::*;

use super::build::handle_kernel_items;
impl GraphState {
  pub fn peek_level(&self) -> usize {
    match self {
      Peek(level) => *level as usize,
      _ => 0,
    }
  }

  pub fn currently_peeking(&self) -> bool {
    match self {
      Peek(_) => true,
      _ => false,
    }
  }
}

pub struct GraphHost<'db> {
  pub leaf_states: OrderedSet<StateId>,
  pub states:      Array<State<'db>>,
  pub db:          &'db ParserDatabase,
  pub name:        IString,
  pub graph_type:  GraphType,
}

impl<'follow, 'db: 'follow> GraphHost<'db> {
  pub fn new(db: &'db ParserDatabase, name: IString, graph_type: GraphType) -> Self {
    Self {
      states: Default::default(),
      leaf_states: Default::default(),
      db,
      name,
      graph_type,
    }
  }

  pub fn get_db(&self) -> &'db ParserDatabase {
    self.db
  }

  pub fn add_leaf_state(&mut self, state: StateId) {
    self.leaf_states.insert(state);
    self[state].leaf_state = true;
  }

  pub fn get_leaf_states(&self) -> Vec<&State> {
    self.leaf_states.iter().map(|s| &self[*s]).collect()
  }

  pub fn is_scanner(&self) -> bool {
    matches!(self.graph_type, GraphType::Scanner)
  }

  pub fn get_goal_nonterm_index(&self) -> DBNonTermKey {
    self.goal_items().iter().next().unwrap().nonterm_index()
  }

  pub fn item_is_goal(&self, item: &Item<'db>) -> bool {
    if !item.is_complete() {
      return false;
    }

    self.states[0].kernel_items.iter().any(|i| i.rule_id == item.rule_id)
  }

  #[allow(unused)]
  pub fn goal_items(&self) -> &ItemSet {
    &self.states[0].kernel_items
  }

  pub fn get_state_name(&self, state: StateId) -> String {
    if state.is_goto() {
      format!("{}__S{:0>4}_gt", self.name.to_string(self.get_db().string_store()), state.core().0)
    } else if state.is_post_reduce() {
      format!("{}__S{:0>4}_pr", self.name.to_string(self.get_db().string_store()), state.core().0)
    } else {
      format!("{}__S{:0>4}", self.name.to_string(self.get_db().string_store()), state.0)
    }
  }

  #[cfg(debug_assertions)]
  pub fn debug_string(&self) -> String {
    let mut string = String::new();

    for state in &self.states {
      let is_leaf_state = self.leaf_states.contains(&state.get_id());
      if is_leaf_state {
        string += "\nLEAF STATE ----------------------------------";
        string += &state.debug_string(self.db);
        string += "\n---------------------------------------------";
      } else {
        string += &state.debug_string(self.db);
      }
    }

    string
  }

  #[inline(always)]
  #[allow(unused)]
  pub fn _debug_print_(&self) {
    #[cfg(debug_assertions)]
    {
      println!("{}", self.debug_string());
    }
  }

  pub fn _goal_nonterm_index_is_(&self, index: u32) -> bool {
    self.get_goal_nonterm_index().to_val() == index
  }
}

impl<'db> Index<usize> for GraphHost<'db> {
  type Output = State<'db>;

  fn index(&self, index: usize) -> &Self::Output {
    &self.states[index]
  }
}

impl<'db> Index<StateId> for GraphHost<'db> {
  type Output = State<'db>;

  fn index(&self, index: StateId) -> &Self::Output {
    &self.states[index.0 as usize]
  }
}

impl<'db> IndexMut<StateId> for GraphHost<'db> {
  fn index_mut(&mut self, index: StateId) -> &mut Self::Output {
    &mut self.states[index.0 as usize]
  }
}

// STATE ID -------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct StateId(pub u32, pub GraphState);

impl Hash for StateId {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.0.hash(state)
  }
}

impl Default for StateId {
  fn default() -> Self {
    Self(u32::MAX, Normal)
  }
}

impl StateId {
  pub fn state(&self) -> GraphState {
    self.1
  }

  pub fn root() -> Self {
    Self(0, Normal)
  }

  pub fn is_invalid(&self) -> bool {
    self.0 == u32::MAX
  }

  pub fn is_root(&self) -> bool {
    self.0 == 0
  }

  pub fn to_post_reduce(&self) -> Self {
    Self(self.0 + (u32::MAX >> 2), self.1)
  }

  pub fn is_post_reduce(&self) -> bool {
    self.0 >= (u32::MAX >> 2) && !self.is_goto()
  }

  pub fn to_goto(&self) -> Self {
    Self(self.0 + (u32::MAX >> 1), self.1)
  }

  pub fn is_goto(&self) -> bool {
    self.0 >= (u32::MAX >> 1)
  }

  pub fn core(&self) -> StateId {
    if self.is_goto() {
      StateId(self.0 - (u32::MAX >> 1), self.1)
    } else {
      StateId(self.0 - (u32::MAX >> 2), self.1)
    }
  }
}

impl<'db> PartialEq<u32> for State<'db> {
  fn eq(&self, other: &u32) -> bool {
    self.id.0 == *other
  }
}

pub struct StateBuilder<'graph_iter, 'db: 'graph_iter> {
  state_id: StateId,
  builder:  &'graph_iter mut GraphBuilder<'db>,
}

impl<'graph_iter, 'db> StateBuilder<'graph_iter, 'db> {
  pub fn to_state(self) -> StateId {
    self.state_id
  }

  pub fn enque(self) -> Option<StateId> {
    let StateBuilder { state_id, builder } = self;
    builder.enqueue_pending_state(state_id)
  }

  pub fn enque_leaf(self) -> Option<StateId> {
    let StateBuilder { state_id, builder } = self;
    if let Some(_) = builder.enqueue_pending_state(state_id) {
      builder.graph.add_leaf_state(state_id);
      Some(state_id)
    } else {
      None
    }
  }

  pub fn set_parent(&mut self, parent: StateId) {
    let StateBuilder { state_id, builder } = self;
    builder.get_state_mut(*state_id).parent = parent;
    builder.get_state_mut(*state_id).predecessors = BTreeSet::from_iter([parent]);
  }

  pub fn to_leaf(self) {
    let StateBuilder { state_id, builder } = self;
    builder.graph.add_leaf_state(state_id)
  }

  pub fn to_pending(self) {
    let StateBuilder { state_id, builder } = self;
    builder.add_pending(state_id)
  }

  pub fn set_peek_resolve_state<T: ItemContainer<'db> + Hash>(&mut self, items: &T) -> Origin {
    let index = hash_id_value_u64(&items);
    let StateBuilder { state_id, builder } = self;
    builder.get_state_mut(*state_id).peek_resolve_items.insert(index, items.clone().to_set());
    Origin::Peek(index, self.state_id)
  }

  pub fn add_kernel_items<T: ItemContainer<'db> + Hash>(&mut self, items: T) {
    let StateBuilder { state_id, builder } = self;
    add_kernel_items(builder.get_state_mut(*state_id), items);
  }

  pub fn set_reduce_item(&mut self, item: Item<'db>) {
    let StateBuilder { state_id, builder } = self;
    builder.get_state_mut(*state_id).set_reduce_item(item);
  }
}

pub(crate) struct GraphBuilder<'db> {
  graph:       GraphHost<'db>,
  state_id:    StateId,
  pending:     Vec<StateId>,
  state_queue: VecDeque<StateId>,
  errors:      Vec<SherpaError>,
  state_map:   OrderedMap<u64, StateId>,
  pub db:      &'db ParserDatabase,
  pub config:  ParserConfig,
}

impl<'db> GraphBuilder<'db> {
  pub fn new(
    db: &'db ParserDatabase,
    name: IString,
    graph_type: GraphType,
    config: ParserConfig,
    kernel_items: Items<'db>,
  ) -> Self {
    let mut graph = GraphHost::new(db, name, graph_type);

    let mut state = State {
      id: StateId::root(),
      term_symbol: (SymbolId::Default, 0).into(),
      t_type: StateType::Start,
      ..Default::default()
    };

    // Automatically setup lanes a goal values.
    let kernel_items = kernel_items
      .into_iter()
      .enumerate()
      .map(|(i, mut item)| {
        item.goal = i as u32;
        item
      })
      .collect::<Items>();

    set_kernel_items(&mut state, kernel_items.iter());

    graph.states.push(state);

    Self {
      graph,
      state_id: StateId::root(),
      pending: Vec::new(),
      errors: Vec::new(),
      state_queue: VecDeque::from_iter([StateId::root()]),
      state_map: Default::default(),
      db,
      config,
    }
  }

  pub fn into_inner(self) -> (GraphHost<'db>, Vec<SherpaError>) {
    (self.graph, self.errors)
  }

  pub fn run(&mut self) {
    while let Some(parent) = self.state_queue.pop_front() {
      self.state_id = parent;
      self.pending.clear();
      match handle_kernel_items(self) {
        Err(err) => {
          self.errors.push(err);
        }
        _ => {}
      }
    }
  }

  pub fn get_pending_items(&self) -> ItemSet<'db> {
    self
      .pending
      .iter()
      .flat_map(|s| {
        if let GraphState::Peek(_) = s.state() {
          self.get_state(*s).peek_resolve_items.values().flatten().cloned().to_set().into_iter()
        } else {
          self.get_state(*s).kernel_items_ref().clone().to_set().into_iter()
        }
      })
      .collect()
  }

  pub fn enqueue_pending_state(&mut self, state_id: StateId) -> Option<StateId> {
    let state = &self.graph[state_id];
    let hash_id = hash_id_value_u64(state);

    if let Some(original_state) = self.state_map.get(&hash_id).cloned() {
      let parent = state.parent;
      self.graph[original_state].predecessors.insert(parent);
      if state_id.0 == (self.graph.states.len() - 1) as u32 {
        drop(self.graph.states.pop());
      }
      None
    } else {
      self.state_map.insert(hash_id, state_id);
      self.state_queue.push_back(state_id);
      Some(state_id)
    }
  }

  pub fn add_pending(&mut self, state: StateId) {
    self.pending.push(state);
  }

  pub fn state_id(&self) -> StateId {
    self.state_id
  }

  pub fn current_state(&self) -> &State<'db> {
    &self.graph[self.state_id]
  }

  pub fn current_state_mut(&mut self) -> &mut State<'db> {
    &mut self.graph[self.state_id]
  }

  pub fn get_mode(&self) -> GraphType {
    self.graph.graph_type
  }

  pub fn is_scanner(&self) -> bool {
    matches!(self.graph.graph_type, GraphType::Scanner)
  }

  pub fn graph(&self) -> &GraphHost<'db> {
    &self.graph
  }

  pub fn get_state(&self, state_id: StateId) -> &State<'db> {
    &self.graph[state_id]
  }

  pub fn get_state_mut(&mut self, state_id: StateId) -> &mut State<'db> {
    &mut self.graph[state_id]
  }

  pub fn create_state<'a>(
    &'a mut self,
    state: GraphState,
    symbol: PrecedentSymbol,
    t_type: StateType,
    kernel_items: Items<'db>,
  ) -> StateBuilder<'a, 'db> {
    let id = StateId(self.graph.states.len() as u32, state);

    let mut state = State {
      id,
      term_symbol: symbol,
      t_type,
      parent: self.state_id,
      predecessors: BTreeSet::from_iter(vec![self.state_id]),
      ..Default::default()
    };

    set_kernel_items(&mut state, kernel_items.iter());

    self.graph.states.push(state);

    StateBuilder { state_id: id, builder: self }
  }

  /// Add kernel items to the current state.
  pub fn add_kernel_items<T: ItemContainer<'db>>(&mut self, kernel_items: T) {
    add_kernel_items(&mut self.current_state_mut(), kernel_items)
  }

  pub fn process_pending(&mut self, should_increment_gotos: bool) {
    for state in self.pending.drain(..).collect::<Vec<_>>() {
      if should_increment_gotos {
        increment_gotos(self, state);
      }
      self.enqueue_pending_state(state);
    }
  }

  #[cfg(debug_assertions)]
  pub fn _print_state_(&self) {
    println!("{}", self.get_state(self.state_id).debug_string(self.db));
  }

  #[cfg(debug_assertions)]
  pub fn _print_graph_(&self) {
    self.graph._debug_print_()
  }

  #[cfg(debug_assertions)]
  pub fn _is_nonterminal_state_(&self, nterm_id: u32, state_id: u32) -> bool {
    self.graph._goal_nonterm_index_is_(nterm_id) && self.state_id().0 == state_id
  }
}

fn increment_gotos(gb: &mut GraphBuilder, parent_id: StateId) {
  if gb.get_state(parent_id).peek_resolve_items.len() > 0 {
    let resolve_states = gb
      .get_state(parent_id)
      .peek_resolve_items
      .iter()
      .map(|(id, i)| (*id, i.iter().map(|i| i.calculate_goto_distance(gb, parent_id)).collect::<ItemSet>()))
      .collect::<OrderedMap<_, _>>();
    gb.get_state_mut(parent_id).peek_resolve_items = resolve_states;
  } else {
    let incremented: Items =
      gb.get_state(parent_id).kernel_items_ref().iter().map(|i| i.calculate_goto_distance(gb, parent_id)).collect();

    set_kernel_items(gb.get_state_mut(parent_id), incremented.iter())
  }
}

fn set_kernel_items<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db>>(state: &mut State<'db>, kernel_items: T) {
  let mut kernel_items =
    kernel_items.into_iter().map(|i| if i.origin_state.is_invalid() { i.to_origin_state(state.id) } else { *i }).collect();

  state.kernel_items.append(&mut kernel_items);

  calculate_closure(state);
}

/// Add kernel items and set their origin to this state.
fn add_kernel_items<'db, T: ItemContainer<'db>>(state: &mut State<'db>, kernel_items: T) {
  let mut kernel_items = kernel_items.into_iter().map(|i| i.to_origin_state(state.id)).collect();

  state.kernel_items.append(&mut kernel_items);

  calculate_closure(state);
}

fn calculate_closure(state: &mut State) {
  if !state.kernel_items.is_empty() {
    state.closure = Some(state.kernel_items.iter().closure::<ItemSet>(state.id));
  }
}
