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
  Fork(DBRuleKey),
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
    match self {
      Origin::Peek(resolve_id, _) => resolve_id.hash(state),
      Origin::TerminalGoal(resolve_id, prec) => {
        resolve_id.hash(state);
        prec.hash(state);
      }
      Origin::Fork(resolve_id) | Origin::BreadCrumb(resolve_id) => {
        resolve_id.hash(state);
      }
      _ => {}
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
        format!("BreadCrumb[ {:?} ]", item._debug_string_())
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

  pub fn get_symbol_key(&self) -> DBTermKey {
    match self {
      Origin::TerminalGoal(sym_id, ..) => *sym_id,
      _ => DBTermKey::default(),
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
  KernelShift,
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
        format!("AssignAndFollow({}:{})", sym_id.to_val(db), db.token(*sym_id).name.to_str(db.string_store()).as_str())
      }
      Self::AssignToken(sym_id) => {
        format!("AssignToken({}:{})", sym_id.to_val(db), db.token(*sym_id).name.to_str(db.string_store()).as_str())
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
pub struct GraphState<'db> {
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
  canonical_hash: u64,
  pub lookahead_hash: u64,
  pub symbol_set_id: u64,
}

impl<'db> GraphState<'db> {
  pub fn get_resolve_item_set(&self, peek_origin_key: u64) -> ItemSet<'db> {
    self.peek_resolve_items.get(&peek_origin_key).unwrap().clone()
  }

  pub fn get_resolve_items(&self) -> impl Iterator<Item = &ItemSet<'db>> + Clone {
    self.peek_resolve_items.values()
  }

  pub fn get_hash(&self, db: &'db ParserDatabase) -> u64 {
    debug_assert!(self.lookahead_hash != 0, "Hash should not be 0 {}", self._debug_string_(db));
    self.lookahead_hash
  }

  pub fn kernel_items_ref(&self) -> &ItemSet<'db> {
    &self.kernel_items
  }

  pub fn set_reduce_item(&mut self, item: Item<'db>) {
    debug_assert!(item.is_complete(), "Expected item to be completed in order to reduce  it:\n  item: {}", item._debug_string_());
    self.reduce_item = Some(item);
  }

  pub fn get_predecessors(&self) -> &BTreeSet<StateId> {
    &self.predecessors
  }

  pub fn get_id(&self) -> StateId {
    self.id
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

  pub(crate) fn get_symbol(&self) -> PrecedentSymbol {
    self.term_symbol
  }

  pub fn get_parent(&self) -> StateId {
    self.parent
  }

  pub fn get_goto_state(&self) -> Option<GraphState<'db>> {
    if self.nonterm_items.len() > 0 {
      let mut goto_state = GraphState {
        term_symbol: self.term_symbol,
        id: self.id.to_goto(),
        t_type: StateType::NonTerminalResolve,
        kernel_items: self.nonterm_items.clone(),
        nonterm_items: self.kernel_items.clone(),
        ..Default::default()
      };

      goto_state.canonical_hash = GraphBuilder::create_state_hash(&goto_state, 0, DefaultHasher::new());
      goto_state.lookahead_hash = goto_state.canonical_hash;

      Some(goto_state)
    } else {
      None
    }
  }

  #[cfg(debug_assertions)]
  pub fn _debug_string_(&self, db: &'db ParserDatabase) -> String {
    let mut string = String::new();
    string += &format!(
      "STATE -- [{: >8}][c:{: <24} h:{: <24}] [sym:{: <24}] --",
      self.id.0, self.canonical_hash, self.lookahead_hash, self.symbol_set_id
    );

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
      string += &format!("\n   - {}", item._debug_string_());
    }

    string += "\n-- kernel-items:";
    for item in &self.kernel_items {
      string += &format!("\n   - {}", item._debug_string_());
    }

    if !self.nonterm_items.is_empty() {
      if let Some(goto_hash) = self.get_goto_state().and_then(|s| Some(s.canonical_hash)) {
        string += &format!("\n\nGOTO -- [{:}][{:}] --", self.id.0, goto_hash);
      }
      string += "\n-- non-terms:";
      for item in &self.nonterm_items {
        string += &format!("\n   - {}", item._debug_string_());
      }
    }

    string
  }
}

// Graph -------------------------------------------------------------

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphBuildState {
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

use GraphBuildState::*;

use super::{build::handle_kernel_items, items::get_follow_internal};
impl GraphBuildState {
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
  pub states:      Array<GraphState<'db>>,
  pub db:          &'db ParserDatabase,
  pub name:        IString,
  pub graph_type:  GraphType,
  pub symbol_sets: Map<u64, OrderedSet<DBTermKey>>,
}

impl<'follow, 'db: 'follow> GraphHost<'db> {
  pub fn new(db: &'db ParserDatabase, name: IString, graph_type: GraphType) -> Self {
    Self {
      states: Default::default(),
      leaf_states: Default::default(),
      symbol_sets: Default::default(),
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

  pub fn get_leaf_states(&self) -> Vec<&GraphState> {
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
  pub fn _debug_string_(&self) -> String {
    let mut string = String::new();

    for state in &self.states {
      let is_leaf_state = self.leaf_states.contains(&state.get_id());
      if is_leaf_state {
        string += "\n\nLEAF ======================================\n";
        string += &state._debug_string_(self.db);
        if let Some(la) = self.symbol_sets.get(&state.symbol_set_id) {
          string += &format!(
            "\n\n  Symbols: {}",
            la.iter().map(|sym| self.db.sym(*sym).debug_string(self.db)).collect::<Vec<_>>().join(" | ")
          );
        }
        string += "\n===============================================";
      } else {
        string += "\n\n---------------------------------------------";
        string += &("\n".to_string() + &state._debug_string_(self.db));
        if let Some(la) = self.symbol_sets.get(&state.symbol_set_id) {
          string += &format!(
            "\n\n  Symbols: {}",
            la.iter().map(|sym| self.db.sym(*sym).debug_string(self.db)).collect::<Vec<_>>().join(" | ")
          );
        }
        string += "\n\n---------------------------------------------";
      }
    }

    string
  }

  #[inline(always)]
  #[allow(unused)]
  pub fn _debug_print_(&self) {
    #[cfg(debug_assertions)]
    {
      println!("{}", self._debug_string_());
    }
  }

  pub fn _goal_nonterm_index_is_(&self, index: u32) -> bool {
    self.get_goal_nonterm_index().to_val() == index
  }
}

impl<'db> Index<usize> for GraphHost<'db> {
  type Output = GraphState<'db>;

  fn index(&self, index: usize) -> &Self::Output {
    &self.states[index]
  }
}

impl<'db> Index<StateId> for GraphHost<'db> {
  type Output = GraphState<'db>;

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
pub struct StateId(pub u32, pub GraphBuildState);

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
  pub fn state(&self) -> GraphBuildState {
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
    if self.0 < u32::MAX >> 2 {
      Self(self.0 + (u32::MAX >> 2), self.1)
    } else {
      *self
    }
  }

  pub fn is_post_reduce(&self) -> bool {
    self.0 >= (u32::MAX >> 2) && !self.is_goto()
  }

  pub fn to_goto(&self) -> Self {
    if self.0 < u32::MAX >> 1 {
      Self(self.0 + (u32::MAX >> 1), self.1)
    } else {
      *self
    }
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

impl<'db> PartialEq<u32> for GraphState<'db> {
  fn eq(&self, other: &u32) -> bool {
    self.id.0 == *other
  }
}

pub struct StateBuilder<'graph_iter, 'db: 'graph_iter> {
  state_id: StateId,
  builder:  &'graph_iter mut GraphBuilder<'db>,
}

impl<'graph_iter, 'db> StateBuilder<'graph_iter, 'db> {
  pub fn set_parent(&mut self, parent: StateId) {
    let StateBuilder { state_id, builder } = self;
    builder.get_state_mut(*state_id).parent = parent;
    builder.get_state_mut(*state_id).predecessors = BTreeSet::from_iter([parent]);
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

  pub fn to_leaf(self) {
    let StateBuilder { state_id, builder } = self;
    builder.prepare_state(state_id);
    builder.graph.add_leaf_state(state_id)
  }

  pub fn to_pending(self) {
    let StateBuilder { state_id, builder } = self;
    builder.prepare_state(state_id);
    builder.add_pending(state_id)
  }

  pub fn to_enqueued_leaf(self) -> Option<StateId> {
    let StateBuilder { state_id, builder } = self;
    builder.prepare_state(state_id);
    if let Some(_) = builder.enqueue_state(state_id) {
      builder.graph.add_leaf_state(state_id);
      Some(state_id)
    } else {
      None
    }
  }

  pub fn to_enqueued(self) -> Option<StateId> {
    let StateBuilder { state_id, builder } = self;
    builder.prepare_state(state_id);
    builder.enqueue_state(state_id)
  }

  pub fn to_state(self) -> StateId {
    let StateBuilder { state_id, builder } = self;
    builder.prepare_state(state_id);
    state_id
  }
}

pub(crate) struct GraphBuilder<'db> {
  graph:       GraphHost<'db>,
  state_id:    StateId,
  pending:     Vec<StateId>,
  state_queue: VecDeque<StateId>,
  errors:      Vec<SherpaError>,
  state_map:   OrderedMap<u64, StateId>,

  pub db:     &'db ParserDatabase,
  pub config: ParserConfig,
}

impl<'db> GraphBuilder<'db> {
  pub fn new(
    db: &'db ParserDatabase,
    name: IString,
    graph_type: GraphType,
    config: ParserConfig,
    kernel_items: ItemSet<'db>,
  ) -> Self {
    let mut builder = Self {
      graph: GraphHost::new(db, name, graph_type),
      state_id: StateId::root(),
      pending: Vec::new(),
      errors: Vec::new(),
      state_queue: VecDeque::from_iter([StateId::root()]),
      state_map: Default::default(),
      db,
      config,
    };

    builder.create_root_state(kernel_items);

    builder
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

  fn create_state_hash<'a, H: std::hash::Hasher>(state: &GraphState<'a>, lookahead: u64, mut hasher: H) -> u64 {
    let hasher = &mut hasher;

    state.t_type.hash(hasher);

    state.term_symbol.hash(hasher);

    for item in &state.kernel_items {
      item.rule_id.hash(hasher);
      item.sym_index.hash(hasher);
      item.origin.hash(hasher);
      item.goal.hash(hasher);
      item.goto_distance.hash(hasher);
    }

    lookahead.hash(hasher);

    hasher.finish()
  }

  fn get_state_symbols<'a>(state: &GraphState<'a>, graph: &GraphHost<'a>) -> Option<OrderedSet<DBTermKey>> {
    if graph.is_scanner() {
      return None;
    };

    let mode = graph.graph_type;

    let mut symbols = OrderedSet::new();

    for item in &state.kernel_items {
      if let Some(term) = item.term_index_at_sym(mode) {
        symbols.insert(term);
      } else if item.is_nonterm(mode) {
        for item in graph.db.get_closure(item) {
          if let Some(term) = item.term_index_at_sym(graph.graph_type) {
            symbols.insert(term);
          }
        }
      } else {
        if let Ok((follow, _)) = get_follow_internal(graph, *item) {
          for item in follow {
            if let Some(term) = item.term_index_at_sym(mode) {
              symbols.insert(term);
            } else if item.is_nonterm(mode) {
              for item in graph.db.get_closure(&item) {
                if let Some(term) = item.term_index_at_sym(graph.graph_type) {
                  symbols.insert(term);
                }
              }
            }
          }
        }
      }
    }

    Some(symbols)
  }

  pub fn get_pending_items(&self) -> ItemSet<'db> {
    self
      .pending
      .iter()
      .flat_map(|s| {
        if let GraphBuildState::Peek(_) = s.state() {
          self.get_state(*s).peek_resolve_items.values().flatten().cloned().to_set().into_iter()
        } else {
          self.get_state(*s).kernel_items_ref().clone().to_set().into_iter()
        }
      })
      .collect()
  }

  fn create_root_state(&mut self, kernel_items: BTreeSet<Item<'db>>) {
    self
      .create_state(
        Normal,
        (SymbolId::Default, 0).into(),
        StateType::Start,
        kernel_items
          .into_iter()
          .enumerate()
          .map(|(i, mut item)| {
            item.goal = i as u32;
            item
          })
          .collect::<Items>(),
      )
      .to_enqueued();
  }

  fn prepare_state(&mut self, state_id: StateId) {
    let Self { config, graph, .. } = self;

    let lookahead_id = match Self::get_state_symbols(&graph[state_id], &graph) {
      Some(lookahead_set) if !lookahead_set.is_empty() => {
        let lookahead_id = hash_id_value_u64(&lookahead_set);
        graph.symbol_sets.insert(lookahead_id, lookahead_set);
        lookahead_id
      }
      _ => 0,
    };

    let hash = if config.ALLOW_LOOKAHEAD_MERGE {
      Self::create_state_hash(&graph[state_id], 0, DefaultHasher::new())
    } else {
      Self::create_state_hash(&graph[state_id], lookahead_id, DefaultHasher::new())
    };

    graph[state_id].symbol_set_id = lookahead_id;
    graph[state_id].canonical_hash = hash;
    graph[state_id].lookahead_hash = hash_id_value_u64((hash, lookahead_id));
  }

  pub fn enqueue_state(&mut self, state_id: StateId) -> Option<StateId> {
    let Self { config, graph, .. } = self;
    let state = &graph[state_id];
    let hash_id = state.canonical_hash;

    debug_assert!(hash_id != 0, "State has not been hashed!\n{}", state._debug_string_(self.db));

    if let Some(original_state) = self.state_map.get(&hash_id).cloned() {
      let parent: StateId = state.parent;
      graph[original_state].predecessors.insert(parent);

      if config.ALLOW_LOOKAHEAD_MERGE {
        let original_la_id = graph[original_state].symbol_set_id;
        let new_lookahead_id = graph[state_id].symbol_set_id;

        // Create a new lookahead set for this state.
        if let Some(existing_lookahead) = graph.symbol_sets.get(&original_la_id) {
          if let Some(other_lookaheads) = graph.symbol_sets.get(&new_lookahead_id) {
            let new_lookaheads = existing_lookahead.clone();
            new_lookaheads.clone().extend(other_lookaheads);
            let lookahead_id = hash_id_value_u64(&new_lookaheads);
            graph[original_state].symbol_set_id = lookahead_id;
            graph[original_state].lookahead_hash = hash_id_value_u64((hash_id, lookahead_id));
            graph.symbol_sets.insert(lookahead_id, new_lookaheads);
          }
        }
      }

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

  pub fn current_state(&self) -> &GraphState<'db> {
    &self.graph[self.state_id]
  }

  pub fn current_state_mut(&mut self) -> &mut GraphState<'db> {
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

  pub fn get_state(&self, state_id: StateId) -> &GraphState<'db> {
    &self.graph[state_id]
  }

  pub fn get_state_mut(&mut self, state_id: StateId) -> &mut GraphState<'db> {
    &mut self.graph[state_id]
  }

  pub fn create_state<'a>(
    &'a mut self,
    state: GraphBuildState,
    symbol: PrecedentSymbol,
    t_type: StateType,
    kernel_items: Items<'db>,
  ) -> StateBuilder<'a, 'db> {
    let id = StateId(self.graph.states.len() as u32, state);

    let mut state = GraphState {
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

  pub fn process_pending(&mut self, should_increment_gotos: bool) {
    for state in self.pending.drain(..).collect::<Vec<_>>() {
      if should_increment_gotos {
        increment_gotos(self, state);
      }
      self.enqueue_state(state);
    }
  }

  #[cfg(debug_assertions)]
  pub fn _print_state_(&self) {
    println!("{}", self.get_state(self.state_id)._debug_string_(self.db));
  }

  #[cfg(debug_assertions)]
  pub fn _print_graph_(&self) {
    self.graph._debug_print_()
  }

  #[cfg(debug_assertions)]
  pub fn _is_nonterminal_state_(&self, nterm_id: u32, state_id: u32) -> bool {
    if self.is_scanner() {
      self.graph.goal_items().iter().any(|i| i.origin.get_symbol_key() == DBTermKey::from(nterm_id))
        && self.state_id().0 == state_id
    } else {
      self.graph.goal_items().iter().any(|i| i.nonterm_index().to_val() == nterm_id) && self.state_id().0 == state_id
    }
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

fn set_kernel_items<'a, 'db: 'a, T: ItemRefContainerIter<'a, 'db>>(state: &mut GraphState<'db>, kernel_items: T) {
  let mut kernel_items =
    kernel_items.into_iter().map(|i| if i.origin_state.is_invalid() { i.to_origin_state(state.id) } else { *i }).collect();
  state.kernel_items.append(&mut kernel_items);
}

/// Add kernel items and set their origin to this state.
fn add_kernel_items<'db, T: ItemContainer<'db>>(state: &mut GraphState<'db>, kernel_items: T) {
  let mut kernel_items = kernel_items.into_iter().map(|i| i.to_origin_state(state.id)).collect();
  state.kernel_items.append(&mut kernel_items);
}

/// Iterates over valid state nodes within the graph. Valid nodes are those that
/// reachable from leaf states.
pub struct GraphIterator<'a, 'db: 'a> {
  graph:      &'a GraphHost<'db>,
  queue:      Queue<StateId>,
  links:      OrderedMap<StateId, OrderedSet<&'a GraphState<'db>>>,
  empty_hash: OrderedSet<&'a GraphState<'db>>,
}

impl<'a, 'db: 'a> GraphIterator<'a, 'db> {
  pub fn new(graph: &'a GraphHost<'db>) -> Self {
    let leaf_states = graph.get_leaf_states();
    let mut queue = Queue::from_iter(leaf_states.iter().map(|i| i.get_id()));
    let mut links: OrderedMap<StateId, OrderedSet<&GraphState>> = OrderedMap::new();
    let mut seen = OrderedSet::new();
    let empty_hash = OrderedSet::new();

    while let Some(state) = queue.pop_front() {
      if !seen.insert(state) {
        continue;
      }
      let predecessors = graph[state].get_predecessors().clone();
      for predecessor in &predecessors {
        links.entry(*predecessor).or_insert(OrderedSet::new()).insert(&graph[state]);
        queue.push_back(*predecessor);
      }
    }

    let mut set = OrderedSet::from_iter(leaf_states.iter().map(|i| i.get_id()));

    set.extend(links.keys().rev().map(|id| *id));

    queue.extend(set.into_iter());

    Self { graph, queue, links, empty_hash }
  }

  pub fn next<'b>(&'b mut self) -> Option<(&'b GraphHost<'db>, &'b GraphState<'db>, &'b OrderedSet<&'a GraphState<'db>>)> {
    while let Some(state) = self.queue.pop_front() {
      return Some((self.graph, &self.graph[state], self.links.get(&state).unwrap_or(&self.empty_hash)));
    }

    None
  }
}
