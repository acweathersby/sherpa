use std::{
  collections::{hash_map::DefaultHasher, BTreeSet, HashSet, VecDeque},
  hash::Hash,
  ops::{Index, IndexMut},
};

use crate::utils::create_u64_hash;

use super::*;

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
  /// The goal is a goto kernel
  Goto(StateId),
  // Out of scope item that was generated from the
  // completion of a token non-terminal.
  ScanCompleteOOS,
  /// Generated when the a goal non-terminal is completed.
  /// Goal nonterminals are determined by the
  /// root state (`StateId(0)`) kernel items
  GoalCompleteOOS,
}

impl Hash for Origin {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
  /// Shifts that occur on kernel items.
  KernelShift,
  /// The completion of this branch will complete one or more kernel items.
  NonTerminalResolve,
  /// The completion of this branch will complete one or more intermediary goto
  /// items.
  NonTerminalShiftLoop,
  NonTerminalComplete,
  Peek,
  PeekEndComplete,
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
  id: StateId,
  term_symbol: PrecedentSymbol,
  t_type: StateType,
  parent: StateId,
  predecessors: OrderedSet<StateId>,
  kernel_items: ItemSet<'db>,
  peek_resolve_items: OrderedMap<u64, ItemSet<'db>>,
  non_terminals: OrderedSet<Item<'db>>,
  reduce_item: Option<Item<'db>>,
  leaf_state: bool,
  closure: Option<OrderedSet<Item<'db>>>,
  root_closure: Option<OrderedSet<Item<'db>>>,
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
  /// Set a group of items that a peek item will resolve to.
  pub fn set_peek_resolve_state(&mut self, peek_origin_key: u64, state: ItemSet<'db>) {
    self.peek_resolve_items.insert(peek_origin_key, state);
  }

  pub fn get_resolve_state(&self, peek_origin_key: u64) -> ItemSet<'db> {
    self.peek_resolve_items.get(&peek_origin_key).unwrap().clone()
  }

  pub fn peek_resolve_state_len(&self) -> usize {
    self.peek_resolve_items.len()
  }

  pub fn get_hash(&self) -> u64 {
    use std::hash::Hasher;
    let mut hasher = DefaultHasher::new();
    self.hash(&mut hasher);
    hasher.finish()
  }

  pub fn calculate_closure(&mut self, db: &'db ParserDatabase) {
    if !self.kernel_items.is_empty() {
      self.closure = None;
      let state_id = self.id;
      let closure = self.kernel_items.iter().closure::<ItemSet>(state_id);

      if self.id.is_root() {
        let nterms = self.kernel_items.iter().map(|i| i.nonterm_index()).collect::<OrderedSet<_>>();
        let sigs = closure.iter().map(|i| (i.rule_id, i.sym_index)).collect::<HashSet<_>>();
        // Get all follow items

        let mut oos_closure = closure.clone();

        for nterm in &nterms {
          for item in db.nonterm_follow_items(*nterm) {
            let i = item;
            if !sigs.contains(&(i.rule_id, i.sym_index)) {
              oos_closure.insert(i.to_origin_state(state_id).to_oos_index());
            }
          }
        }

        self.root_closure = Some(oos_closure);
      }

      self.closure = Some(closure);
    }
  }

  pub fn kernel_items_ref(&self) -> &ItemSet<'db> {
    &self.kernel_items
  }

  pub fn get_closure_ref(&self) -> SherpaResult<&ItemSet<'db>> {
    return o_to_r(self.closure.as_ref(), "Closure Not Created");
  }

  pub fn get_root_closure_ref(&self) -> SherpaResult<&ItemSet<'db>> {
    if self.id.is_root() {
      return o_to_r(self.root_closure.as_ref(), "Root Closure Not Created");
    } else {
      return o_to_r(self.closure.as_ref(), "Closure Not Created");
    }
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

  pub fn set_non_terminals(&mut self, non_terms: &BTreeSet<Item<'db>>) {
    self.non_terminals = non_terms.clone();
  }

  pub fn get_type(&self) -> StateType {
    self.t_type
  }

  pub fn has_goto_state(&self) -> bool {
    self.non_terminals.len() > 0
  }

  pub fn get_goto_state(&self) -> Option<Self> {
    if self.non_terminals.len() > 0 {
      Some(Self {
        term_symbol: self.term_symbol,
        id: self.id.to_goto(),
        t_type: StateType::NonTerminalResolve,
        kernel_items: self.non_terminals.clone(),
        non_terminals: self.kernel_items.clone(),
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

  /// Add kernel items without adjusting their origin
  pub fn set_kernel_items<T: ItemContainer<'db>>(&mut self, kernel_items: T, db: &'db ParserDatabase) {
    let mut kernel_items =
      kernel_items.into_iter().map(|i| if i.origin_state.is_invalid() { i.to_origin_state(self.id) } else { i }).collect();

    self.kernel_items.append(&mut kernel_items);

    self.calculate_closure(db);
  }

  /// Add kernel items and set their origin to this state.
  pub fn add_kernel_items<T: ItemContainer<'db>>(&mut self, kernel_items: T, db: &'db ParserDatabase) {
    let mut kernel_items = kernel_items.into_iter().map(|i| i.to_origin_state(self.id)).collect();

    self.kernel_items.append(&mut kernel_items);

    self.calculate_closure(db);
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

      string += &format!("\n   - {:?}", state);
    }

    if let Some(item) = &self.reduce_item {
      string += &format!("\n  Reduce:");
      string += &format!("\n   - {}", item.debug_string());
    }

    string += "\n-- kernel-items:";
    for item in &self.kernel_items {
      string += &format!("\n   - {}", item.debug_string());
    }

    if !self.non_terminals.is_empty() {
      if let Some(goto_hash) = self.get_goto_state().and_then(|s| Some(s.get_hash())) {
        string += &format!("\n\nGOTO -- [{:}][{:}] --", self.id.0, goto_hash);
      }
      string += "\n-- non-terms:";
      for item in &self.non_terminals {
        string += &format!("\n   - {}", item.debug_string());
      }
    }
    string
  }
}

// Graph -------------------------------------------------------------

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphState {
  Normal,
  NormalGoto,
  Peek(usize),
  _LongestMatch,
  _ShortestMatch,
  _FirstMatch,
  _BreadCrumb,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphMode {
  /// Classic Recursive Descent Ascent with unlimited lookahead.
  Parser,
  // Scanner mode for creating tokens, analogous to regular expressions.
  Scanner,
}

use GraphState::*;
impl GraphState {
  pub fn peek_level(&self) -> usize {
    match self {
      Peek(level) => *level,
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
  state_map: OrderedMap<u64, StateId>,
  states: Array<State<'db>>,
  leaf_states: OrderedSet<StateId>,
  pending_states: VecDeque<(GraphState, StateId)>,
  mode: GraphMode,
  db: &'db ParserDatabase,
  name: IString,
  config: ParserConfig,
}

impl<'follow, 'db: 'follow> GraphHost<'db> {
  pub fn new(db: &'db ParserDatabase, mode: GraphMode, name: IString, config: ParserConfig) -> Self {
    Self {
      mode,
      state_map: Default::default(),
      states: Default::default(),
      leaf_states: Default::default(),
      pending_states: Default::default(),
      db,
      name,
      config,
    }
  }

  pub fn create_state(
    &mut self,
    symbol: PrecedentSymbol,
    t_type: StateType,
    parent: Option<StateId>,
    kernel_items: Items<'db>,
  ) -> StateId {
    let id = StateId(self.states.len() as u32);

    debug_assert!(!matches!(symbol.sym(), SymbolId::NonTerminalToken { .. }));

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
      if self.states.is_empty() {
        // Automatically setup lanes a goal values.
        kernel_items
          .into_iter()
          .enumerate()
          .map(|(i, mut item)| {
            item.goal = i as u32;
            item
          })
          .collect()
      } else {
        kernel_items
      },
      self.db,
    );

    self.states.push(state);

    id
  }

  pub fn config(&self) -> &ParserConfig {
    &self.config
  }

  pub fn get_db(&self) -> &'db ParserDatabase {
    self.db
  }

  pub fn is_scanner(&self) -> bool {
    matches!(self.mode, GraphMode::Scanner)
  }

  pub fn add_leaf_state(&mut self, state: StateId) {
    self.leaf_states.insert(state);
    self[state].leaf_state = true;
  }

  pub fn get_leaf_states(&self) -> Vec<&State> {
    self.leaf_states.iter().map(|s| &self[*s]).collect()
  }

  pub fn enqueue_pending_state(&mut self, graph_state: GraphState, state: StateId) -> Option<StateId> {
    let hash_id = create_u64_hash(&self[state]);

    if let Some(original_state) = self.state_map.get(&hash_id).cloned() {
      let parent = self[state].parent;
      self[original_state].predecessors.insert(parent);
      if state.0 == (self.states.len() - 1) as u32 {
        drop(self.states.pop());
      }
      None
    } else {
      self.state_map.insert(hash_id, state);
      self.pending_states.push_back((graph_state, state));
      Some(state)
    }
  }

  pub fn goal_nonterm_index_is(&self, index: u32) -> bool {
    self.goal_items().iter().next().unwrap().nonterm_index().to_val() == index
  }

  pub fn dequeue_pending_state(&mut self) -> Option<(GraphState, StateId)> {
    self.pending_states.pop_front()
  }

  pub fn item_is_goal(&self, item: &Item<'db>) -> bool {
    if !item.is_complete() {
      return false;
    }

    self.states[0].kernel_items.iter().any(|i| i.rule_id == item.rule_id)
  }

  pub fn get_mode(&self) -> GraphMode {
    self.mode
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

  #[allow(unused)]
  #[inline(always)]
  pub fn debug_print(&self) {
    #[cfg(debug_assertions)]
    {
      println!("{}", self.debug_string());
    }
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

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct StateId(pub u32);

impl Default for StateId {
  fn default() -> Self {
    Self(u32::MAX)
  }
}

impl StateId {
  pub fn is_invalid(&self) -> bool {
    self.0 == u32::MAX
  }

  pub fn is_root(&self) -> bool {
    self.0 == 0
  }

  pub fn to_post_reduce(&self) -> Self {
    Self(self.0 + (u32::MAX >> 2))
  }

  pub fn is_post_reduce(&self) -> bool {
    self.0 >= (u32::MAX >> 2) && !self.is_goto()
  }

  pub fn to_goto(&self) -> Self {
    Self(self.0 + (u32::MAX >> 1))
  }

  pub fn is_goto(&self) -> bool {
    self.0 >= (u32::MAX >> 1)
  }

  pub fn core(&self) -> StateId {
    if self.is_goto() {
      StateId(self.0 - (u32::MAX >> 1))
    } else {
      StateId(self.0 - (u32::MAX >> 2))
    }
  }
}

impl<'db> PartialEq<u32> for State<'db> {
  fn eq(&self, other: &u32) -> bool {
    self.id.0 == *other
  }
}
