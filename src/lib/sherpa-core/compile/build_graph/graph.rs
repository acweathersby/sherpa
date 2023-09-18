use crate::{hash_id_value_u64, types::*};
use std::{
  collections::{hash_map::DefaultHasher, BTreeSet, VecDeque},
  fmt::Debug,
  hash::Hash,
  ops::{Index, IndexMut},
};

/// Indicates the State type that generated
/// the item
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[allow(non_camel_case_types)]
pub enum Origin {
  None,
  /// The goal non-terminal that this item or it's predecessors will reduce to
  NonTermGoal(DBNonTermKey),
  /// The goal symbol id that this item or its predecessors will recognize
  TerminalGoal(DBTermKey, u16),
  /// The hash and state of the goal items set the peek item will resolve to
  Peek(u32),
  /// The item
  BreadCrumb(DBRuleKey),
  Fork(DBRuleKey),
  PEG(DBNonTermKey),
  Closure(StateId),
  Goto(StateId),
  __OOS_CLOSURE__,
  __OOS_ROOT__,
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
      Origin::Peek(resolve_id) => resolve_id.hash(state),
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
        let item = Item::from((*rule_id, db));
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
  /// Contains kernel items for an out of scope state tha is used to
  /// disambiguate completed items. This states are internal only and do not
  /// appear in finalized graphs.
  _OosClosure_,
  /// The completion of this branch will complete one or more intermediary goto
  /// items.
  NonTerminalShiftLoop,
  NonTerminalComplete,
  Peek(u32),
  /// A peek path has been resolved to a single peek group located in the peek
  /// origin state.
  PeekEndComplete(u32),
  CompleteToken,
  Follow,
  AssignAndFollow(DBTermKey),
  Reduce(DBRuleKey, usize),
  AssignToken(DBTermKey),
  /// Calls made on items within a state's closure but
  /// are not kernel items.
  InternalCall(DBNonTermKey),
  /// Calls made on kernel items
  KernelCall(DBNonTermKey),
  /// A shift that pushes pushes a non-terminal shift onto the stack before
  /// jumping to a terminal shift shifts into a terminal state
  ShiftFrom(StateId),
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
  pub fn currently_peeking(&self) -> bool {
    match self {
      StateType::Peek(_) => true,
      _ => false,
    }
  }

  pub fn peek_level(&self) -> u32 {
    match self {
      StateType::Peek(level) => *level,
      _ => 0,
    }
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
      Self::ShiftFrom(state_id) => {
        format!("ShiftOver({state_id:?})",)
      }
      _ => format!("{:?}", self),
    }
  }
}

// State -------------------------------------------------------------

#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct GraphState {
  id: StateId,
  build_state: GraphBuildState,
  parent: StateId,
  t_type: StateType,
  canonical_hash: u64,
  lookahead_hash: u64,
  symbol_set_id: u64,
  leaf_state: bool,
  used: bool,
}

impl<'graph, 'db: 'graph> GraphState {
  pub fn as_ref(&self, graph: &'graph GraphHost<'db>) -> GraphStateRef<'graph, 'db> {
    GraphStateRef { id: self.id, graph }
  }

  pub fn as_mut_ref(&self, graph: &'graph mut GraphHost<'db>) -> GraphStateMutRef<'graph, 'db> {
    GraphStateMutRef { id: self.id, graph }
  }
}

pub trait GraphStateReference<'graph, 'db: 'graph> {
  fn graph(&self) -> &'graph GraphHost<'db>;
  fn _id_(&self) -> StateId;

  fn get_hash(&'graph self) -> u64 {
    self.internal().lookahead_hash
  }

  fn get_conanical_hash(&'graph self) -> u64 {
    self.internal().canonical_hash
  }

  fn get_id(&self) -> StateId {
    self._id_()
  }

  fn get_type(&'graph self) -> StateType {
    self.internal().t_type
  }

  fn get_parent(&'graph self) -> StateId {
    self.internal().parent
  }

  fn build_state(&'graph self) -> GraphBuildState {
    self.internal().build_state
  }

  fn get_goto_state(&self) -> Option<GotoGraphStateRef<'graph, 'db>> {
    if let Some(nonterm_items) = self.graph().nonterm_items.get(&self._id_()) {
      Some(GotoGraphStateRef {
        id: self._id_(),
        goto_id: self._id_().to_goto(),
        graph: self.graph(),
        nonterm_items,
        canonical_hash: 0,
      })
    } else {
      None
    }
  }

  fn internal(&self) -> GraphState {
    self.graph()[self._id_()]
  }

  fn get_resolve_item_set<'a>(&'a self, peek_origin_key: u32) -> &'graph PeekGroup<'db> {
    self.graph().peek_resolve_items.get(&peek_origin_key).as_ref().unwrap()
  }

  fn get_peek_resolve_items<'a: 'graph>(&'a self) -> Option<impl Iterator<Item = (u32, &'graph PeekGroup<'db>)>> {
    if let Some(resolve_ids) = self.graph().peek_resolve_ids.get(&self._id_()) {
      Some(resolve_ids.iter().map(|i| (*i, self.get_resolve_item_set(*i))))
    } else {
      None
    }
  }

  fn get_kernel_items<'a: 'graph>(&'a self) -> &'graph ItemSet<'db> {
    self.graph().kernel_items.get(&self._id_()).as_ref().unwrap()
  }

  fn get_nonterm_items<'a: 'graph>(&'a self) -> Option<&'graph ItemSet<'db>> {
    self.graph().nonterm_items.get(&self._id_())
  }

  fn get_predecessors<'a: 'graph>(&'a self) -> Option<&'graph BTreeSet<StateId>> {
    let id = self._id_();
    self.graph().state_predecessors.get(&id)
  }

  fn get_symbol<'a: 'graph>(&'a self) -> PrecedentSymbol {
    self.graph().term_symbol.get(&self._id_()).cloned().unwrap_or((SymbolId::Undefined, 0).into())
  }

  fn get_reduce_item<'a: 'graph>(&'a self) -> Option<&'graph Item<'db>> {
    self.graph().reduce_item.get(&self._id_())
  }

  fn get_symbols<'a: 'graph>(&'a self) -> Option<&'graph OrderedSet<DBTermKey>> {
    let sym_id = self.internal().symbol_set_id;
    self.graph().symbol_sets.get(&sym_id)
  }

  #[cfg(debug_assertions)]
  fn _debug_print_(&'graph self) {
    println!("{}", self._debug_string_());
  }

  #[cfg(debug_assertions)]
  fn _debug_string_(&'graph self) -> String {
    let db = self.graph().db;
    let state = self.internal();
    let mut string = String::new();
    string += &format!(
      "STATE -- [{: >8}-{:?}][c:{: <24} h:p_{:}] [sym:{: <24}] --",
      state.id.index(),
      state.id.subtype(),
      state.canonical_hash,
      state.lookahead_hash,
      state.symbol_set_id
    );

    if let Some(predecessors) = self.get_predecessors() {
      if predecessors.len() > 0 {
        string += &format!(r##" preds [{}]"##, predecessors.iter().map(|p| p.index().to_string()).collect::<Vec<_>>().join(" "));
      }
    }

    string += &format!(
      "\n\n Type {:?}; Sym: [{}|{}]",
      state.t_type.debug_string(db),
      self.get_symbol().sym().debug_string(db),
      self.get_symbol().precedence()
    );

    if let Some(peek_items_sets) = self.get_peek_resolve_items() {
      for (index, PeekGroup { items, .. }) in peek_items_sets {
        string += &format!("\n  Peek Resolve: {}", index);

        string += &format!("\n   - {}", items.to_debug_string("\n     "));
      }
    }

    if let Some(item) = &self.get_reduce_item() {
      string += &format!("\n  Reduce:");
      string += &format!("\n   - {}", item._debug_string_());
    }

    string += "\n-- kernel-items:";
    for item in self.get_kernel_items() {
      string += &format!("\n   - {}", item._debug_string_());
    }

    if let Some(non_term_items) = self.get_nonterm_items() {
      if let Some(goto_hash) = self.get_goto_state().and_then(|s| Some(s.get_hash())) {
        string += &format!("\n\nGOTO -- [{:}][g_{:}] --", self._id_().index(), goto_hash);
      }
      string += "\n-- non-terms:";
      for item in non_term_items {
        string += &format!("\n   - {}", item._debug_string_());
      }
    }

    string
  }
}

#[derive(Clone, Copy)]
pub struct GraphStateRef<'graph, 'db: 'graph> {
  pub id:    StateId,
  pub graph: &'graph GraphHost<'db>,
}
impl<'graph, 'db: 'graph> Ord for GraphStateRef<'graph, 'db> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.id.cmp(&other.id)
  }
}
impl<'graph, 'db: 'graph> PartialOrd for GraphStateRef<'graph, 'db> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.id.partial_cmp(&other.id)
  }
}

impl<'graph, 'db: 'graph> Eq for GraphStateRef<'graph, 'db> {}
impl<'graph, 'db: 'graph> PartialEq for GraphStateRef<'graph, 'db> {
  fn eq(&self, other: &Self) -> bool {
    self.id.eq(&other.id)
  }
}

impl<'graph, 'db: 'graph> GraphStateReference<'graph, 'db> for GraphStateRef<'graph, 'db> {
  fn graph(&self) -> &'graph GraphHost<'db> {
    self.graph
  }

  fn _id_(&self) -> StateId {
    self.id
  }
}

#[derive(Clone, Copy)]
pub struct GotoGraphStateRef<'graph, 'db: 'graph> {
  pub id: StateId,
  pub goto_id: StateId,
  pub graph: &'graph GraphHost<'db>,
  pub nonterm_items: &'graph ItemSet<'db>,
  pub canonical_hash: u64,
}

impl<'graph, 'db: 'graph> GraphStateReference<'graph, 'db> for GotoGraphStateRef<'graph, 'db> {
  fn graph(&self) -> &'graph GraphHost<'db> {
    self.graph
  }

  fn _id_(&self) -> StateId {
    self.id
  }

  fn get_id(&self) -> StateId {
    self.goto_id
  }
}

pub struct GraphStateMutRef<'graph, 'db: 'graph> {
  pub id:    StateId,
  pub graph: &'graph mut GraphHost<'db>,
}

impl<'graph, 'db: 'graph> GraphStateMutRef<'graph, 'db> {
  pub fn set_nonterm_items(&mut self, nonterm_items: &ItemSet<'db>) {
    self.graph.nonterm_items.insert(self.id, nonterm_items.clone());
  }

  pub(crate) fn set_kernel_items<T: Iterator<Item = Item<'db>>>(&mut self, kernel_items: T) {
    self.graph.kernel_items.insert(
      self.id,
      kernel_items
        .map(|i| {
          if i.origin_state.is_invalid() {
            debug_assert!(!self.id.is_invalid());
            i.to_origin_state(self.id)
          } else {
            i
          }
        })
        .collect(),
    );
  }

  pub(crate) fn add_kernel_items<T: ItemContainerIter<'db>>(&mut self, kernel_items: T) {
    self.graph.kernel_items.entry(self.id).or_default().extend(kernel_items.map(|i| {
      if i.origin_state.is_invalid() {
        i.to_origin_state(self.id)
      } else {
        i
      }
    }));
  }
}

// Graph -------------------------------------------------------------

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphBuildState {
  #[default]
  Normal,
  NormalGoto,
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

use super::{build::handle_kernel_items, items::get_follow};
impl GraphBuildState {}

#[derive(Hash)]
pub struct PeekGroup<'db> {
  pub items:  ItemSet<'db>,
  pub is_oos: bool,
}

pub struct GraphHost<'db> {
  pub leaf_states: OrderedSet<StateId>,
  pub states: Array<GraphState>,
  pub peek_resolve_items: OrderedMap<u32, PeekGroup<'db>>,
  pub peek_resolve_ids: OrderedMap<StateId, Vec<u32>>,
  pub state_predecessors: OrderedMap<StateId, OrderedSet<StateId>>,
  pub term_symbol: OrderedMap<StateId, PrecedentSymbol>,
  pub reduce_item: OrderedMap<StateId, Item<'db>>,
  pub kernel_items: OrderedMap<StateId, ItemSet<'db>>,
  pub nonterm_items: OrderedMap<StateId, ItemSet<'db>>,
  pub db: &'db ParserDatabase,
  pub name: IString,
  pub graph_type: GraphType,
  pub symbol_sets: Map<u64, OrderedSet<DBTermKey>>,
}

impl<'follow, 'db: 'follow> GraphHost<'db> {
  pub fn new(db: &'db ParserDatabase, name: IString, graph_type: GraphType) -> Self {
    Self {
      states: Array::with_capacity(1024),
      leaf_states: Default::default(),
      peek_resolve_ids: Default::default(),
      symbol_sets: Default::default(),
      kernel_items: Default::default(),
      nonterm_items: Default::default(),
      peek_resolve_items: Default::default(),
      reduce_item: Default::default(),
      term_symbol: Default::default(),
      state_predecessors: Default::default(),
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

    self.kernel_items.get(&StateId::root()).is_some_and(|i| i.iter().any(|i| i.rule_id() == item.rule_id()))
  }

  #[allow(unused)]
  pub fn goal_items(&self) -> &ItemSet {
    self.kernel_items.get(&StateId::root()).as_ref().unwrap()
  }

  pub fn get_state_name(&self, state: StateId) -> String {
    if state.is_goto() {
      format!("{}__S{:0>4}_gt", self.name.to_string(self.get_db().string_store()), state.index())
    } else if state.is_post_reduce() {
      format!("{}__S{:0>4}_pr", self.name.to_string(self.get_db().string_store()), state.index())
    } else {
      format!("{}__S{:0>4}", self.name.to_string(self.get_db().string_store()), state.index())
    }
  }

  #[cfg(debug_assertions)]
  pub fn _debug_string_(&self) -> String {
    let mut string = String::new();

    for state in &self.states {
      let state_ref = state.as_ref(self);
      let is_leaf_state = self.leaf_states.contains(&state.id);
      if !state.used && !is_leaf_state {
        continue;
      }
      if is_leaf_state {
        string += "\n\nLEAF ======================================\n";
        string += &state_ref._debug_string_();
        if let Some(la) = self.symbol_sets.get(&state.symbol_set_id) {
          string += &format!(
            "\n\n  Symbols: {}",
            la.iter().map(|sym| self.db.sym(*sym).debug_string(self.db)).collect::<Vec<_>>().join(" | ")
          );
        }
        string += "\n===============================================";
      } else {
        string += "\n\n---------------------------------------------";
        string += &("\n".to_string() + &state_ref._debug_string_());
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
  type Output = GraphState;

  fn index(&self, index: usize) -> &Self::Output {
    &self.states[index]
  }
}

// STATE ID -------------------------------------------------------------
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphIdSubType {
  Root    = 0,
  Regular,
  Goto,
  PostReduce,
  ExtendedClosure,
  ExtendSled,
  Invalid = 0xF,
}

impl<'db> Index<StateId> for GraphHost<'db> {
  type Output = GraphState;

  fn index(&self, index: StateId) -> &Self::Output {
    &self.states[index.index()]
  }
}

impl<'db> IndexMut<StateId> for GraphHost<'db> {
  fn index_mut(&mut self, index: StateId) -> &mut Self::Output {
    &mut self.states[index.index()]
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct StateId(pub u32);

#[cfg(debug_assertions)]
impl Debug for StateId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut t = f.debug_tuple("StateId");
    t.field(&self.index());
    t.field(&self.subtype());
    t.finish()
  }
}

impl Hash for StateId {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.index().hash(state)
  }
}

impl Default for StateId {
  fn default() -> Self {
    Self(u32::MAX)
  }
}

impl StateId {
  pub fn new(index: usize, sub_type: GraphIdSubType) -> Self {
    Self((index as u32) | ((sub_type as u32) << 28))
  }

  pub fn index(&self) -> usize {
    (self.0 & 0x0F_FF_FF_FF) as usize
  }

  pub fn subtype(&self) -> GraphIdSubType {
    match (self.0 >> 28) as u8 {
      0 => GraphIdSubType::Root,
      1 => GraphIdSubType::Regular,
      2 => GraphIdSubType::Goto,
      3 => GraphIdSubType::PostReduce,
      4 => GraphIdSubType::ExtendedClosure,
      5 => GraphIdSubType::ExtendSled,
      _ => GraphIdSubType::Invalid,
    }
  }

  pub fn is_extended(&self) -> bool {
    match self.subtype() {
      GraphIdSubType::ExtendSled | GraphIdSubType::ExtendedClosure => true,
      _ => false,
    }
  }

  pub fn is_extended_entry_base(&self) -> bool {
    self.subtype() == GraphIdSubType::ExtendSled
  }

  pub fn is_extend_closure(&self) -> bool {
    self.subtype() == GraphIdSubType::ExtendedClosure
  }

  pub fn extended_entry_base() -> Self {
    Self::new(0, GraphIdSubType::ExtendSled)
  }

  pub fn root() -> Self {
    Self::new(0, GraphIdSubType::Root)
  }

  pub fn is_invalid(&self) -> bool {
    self.subtype() == GraphIdSubType::Invalid
  }

  pub fn is_root(&self) -> bool {
    self.subtype() == GraphIdSubType::Root && self.index() == 0
  }

  pub fn is_post_reduce(&self) -> bool {
    self.subtype() == GraphIdSubType::PostReduce
  }

  pub fn is_goto(&self) -> bool {
    self.subtype() == GraphIdSubType::Goto
  }

  pub fn to_post_reduce(&self) -> Self {
    Self::new(self.index(), GraphIdSubType::PostReduce)
  }

  pub fn to_goto(&self) -> Self {
    Self::new(self.index(), GraphIdSubType::Goto)
  }
}

impl PartialEq<u32> for GraphState {
  fn eq(&self, other: &u32) -> bool {
    self.id.0 == *other
  }
}

pub(crate) enum EnqueResult {
  Enqueued(StateId),
  Merged(StateId),
}

pub struct StateBuilder<'graph_builder, 'db: 'graph_builder> {
  state_id: StateId,
  builder:  &'graph_builder mut GraphBuilder<'db>,
  resolved: bool,
}

impl<'graph_iter, 'db: 'graph_iter> StateBuilder<'graph_iter, 'db> {
  pub fn state_ref<'a>(&'a self) -> GraphStateRef<'a, 'db> {
    self.builder.get_state(self.state_id)
  }

  pub fn set_parent(&mut self, parent: StateId) {
    let StateBuilder { state_id, builder, .. } = self;
    let old_parent = builder.graph[*state_id].parent;
    builder.graph[*state_id].parent = parent;
    let predecessors = builder.graph.state_predecessors.entry(*state_id).or_insert(Default::default());

    predecessors.remove(&old_parent);
    predecessors.insert(parent);
  }

  pub fn set_peek_resolve_state<T: Iterator<Item = Item<'db>>>(&mut self, items: T, is_oos: bool) -> Origin {
    let peek_group = PeekGroup { items: items.collect(), is_oos };

    let index = hash_id_value_u64(&peek_group) as u32;

    let StateBuilder { state_id, builder, .. } = self;

    builder.graph.peek_resolve_ids.entry(*state_id).or_insert(Default::default()).push(index);
    builder.graph.peek_resolve_items.entry(index).or_insert(peek_group);

    Origin::Peek(index)
  }

  pub fn add_kernel_items<T: ItemContainerIter<'db>>(&mut self, items: T) {
    let StateBuilder { builder, .. } = self;
    builder.get_state_mut(self.state_id).add_kernel_items(items);
  }

  pub fn set_kernel_items<T: ItemContainerIter<'db>>(&mut self, items: T) {
    let StateBuilder { builder, .. } = self;
    builder.get_state_mut(self.state_id).set_kernel_items(items);
  }

  pub fn set_reduce_item(&mut self, item: Item<'db>) {
    let StateBuilder { state_id, builder, .. } = self;
    builder.graph.reduce_item.insert(*state_id, item);
  }

  pub fn to_leaf(self) -> StateId {
    let StateBuilder { state_id, builder, resolved } = self;

    if !resolved {
      builder.prepare_state(state_id);
      builder.graph.add_leaf_state(state_id);
    }

    state_id
  }

  pub fn to_pending(self) -> Option<StateId> {
    let StateBuilder { state_id, builder, resolved } = self;

    if !resolved {
      builder.add_pending(state_id);
      Some(state_id)
    } else {
      None
    }
  }

  pub fn to_enqueued_leaf(self) -> Option<StateId> {
    let StateBuilder { state_id, builder, resolved } = self;
    if !resolved {
      builder.prepare_state(state_id);
      if let EnqueResult::Enqueued(_) = builder.enqueue_state(state_id) {
        builder.graph.add_leaf_state(state_id);
        Some(state_id)
      } else {
        None
      }
    } else {
      None
    }
  }

  pub fn to_enqueued(self) -> Option<StateId> {
    let StateBuilder { state_id, builder, resolved } = self;
    if !resolved {
      builder.prepare_state(state_id);
      match builder.enqueue_state(state_id) {
        EnqueResult::Enqueued(s) | EnqueResult::Merged(s) => Some(s),
      }
    } else {
      None
    }
  }

  pub fn to_state(self) -> StateId {
    let StateBuilder { state_id, builder, resolved } = self;
    if !resolved {
      builder.prepare_state(state_id);
    }
    state_id
  }
}

pub type DefaultIter<'db> = std::vec::IntoIter<Item<'db>>;
pub(crate) struct GraphBuilder<'db> {
  graph: GraphHost<'db>,
  state_id: StateId,
  pending: Vec<StateId>,
  state_queue: VecDeque<StateId>,
  errors: Vec<SherpaError>,
  state_map: OrderedMap<u64, StateId>,
  oos_roots: OrderedMap<DBNonTermKey, StateId>,
  oos_closure_states: OrderedMap<Item<'db>, StateId>,
  classification: ParserClassification,
  pub db: &'db ParserDatabase,
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
      state_id: StateId::default(),
      pending: Vec::new(),
      errors: Vec::new(),
      oos_roots: Default::default(),
      oos_closure_states: Default::default(),
      state_queue: VecDeque::default(),
      state_map: Default::default(),
      classification: Default::default(),
      db,
      config,
    };

    builder.create_root_state(kernel_items);

    builder
  }

  pub fn get_oos_root_state(&mut self, nterm: DBNonTermKey) -> StateId {
    if let Some(state_id) = self.oos_roots.get(&nterm) {
      *state_id
    } else {
      let id = StateId::new(self.graph.states.len(), GraphIdSubType::ExtendedClosure);

      let closure = self
        .db
        .nonterm_follow_items(nterm)
        //.filter(|i| i.nonterm_index_at_sym(self.get_mode()) == Some(nterm))
        .map(|i| i.to_origin(Origin::__OOS_CLOSURE__).to_origin_state(id))
        .filter_map(|i| i.increment());

      let state = GraphState {
        id,
        canonical_hash: hash_id_value_u64(nterm),
        t_type: StateType::_OosClosure_,
        build_state: GraphBuildState::Normal,
        ..Default::default()
      };

      self.graph.kernel_items.insert(id, closure.collect());
      self.graph.term_symbol.insert(id, Default::default());
      self.graph.states.push(state);
      self.oos_roots.insert(nterm, id);
      self.get_oos_root_state(nterm)
    }
  }

  pub fn get_oos_closure_state(&mut self, item: Item<'db>) -> StateId {
    debug_assert!(matches!(item.origin, Origin::__OOS_CLOSURE__ | Origin::__OOS_ROOT__), "{}", item._debug_string_());

    if let Some(state_id) = self.oos_closure_states.get(&item) {
      *state_id
    } else {
      let id = StateId::new(self.graph.states.len(), GraphIdSubType::ExtendedClosure);

      let closure = item.closure_iter_align(item.to_origin_state(id).to_origin(Origin::__OOS_CLOSURE__));

      let state = GraphState {
        id,
        t_type: StateType::_OosClosure_,
        parent: item.origin_state,
        canonical_hash: hash_id_value_u64(item),
        build_state: GraphBuildState::Normal,
        ..Default::default()
      };

      self.graph.kernel_items.insert(id, closure.collect());
      self.graph.term_symbol.insert(id, Default::default());
      self.graph.states.push(state);
      self.oos_closure_states.insert(item, id);
      self.get_oos_closure_state(item)
    }
  }

  pub fn into_inner(self) -> (ParserClassification, GraphHost<'db>, Vec<SherpaError>) {
    (self.classification, self.graph, self.errors)
  }

  pub fn run(&mut self) {
    while let Some(parent) = self.state_queue.pop_front() {
      self.pending.clear();
      self.state_id = parent;
      match handle_kernel_items(self) {
        Err(err) => {
          self.errors.push(err);
        }
        _ => {}
      }
      for state_id in self.pending.drain(..).collect::<Vec<_>>() {
        self.prepare_state(state_id);
        self.enqueue_state(state_id);
      }
    }
  }

  fn create_state_hash<'a, H: std::hash::Hasher>(state: GraphStateRef, lookahead: u64, mut hasher: H) -> u64 {
    let hasher = &mut hasher;

    let core_state = state.internal();

    core_state.t_type.hash(hasher);

    state.get_symbol().hash(hasher);

    for item in state.get_kernel_items() {
      item.index().hash(hasher);
      item.origin.hash(hasher);
      item.from.hash(hasher);
      item.from_goto_origin.hash(hasher);
      item.goto_distance.hash(hasher);
    }

    lookahead.hash(hasher);

    hasher.finish()
  }

  #[allow(unused)]
  pub fn get_classification(&mut self) -> ParserClassification {
    self.classification
  }

  pub fn set_classification(&mut self, classification: ParserClassification) {
    self.classification |= classification;
  }

  fn get_state_symbols<'a>(&mut self, state: StateId) -> Option<OrderedSet<DBTermKey>> {
    if self.graph.is_scanner() || state.is_invalid() {
      return None;
    };

    let mode = self.graph.graph_type;

    let mut symbols = OrderedSet::new();

    for item in self.get_state(state).get_kernel_items().clone() {
      if let Some(term) = item.term_index_at_sym(mode) {
        symbols.insert(term);
      } else if item.is_nonterm(mode) {
        for item in self.db.get_closure(&item) {
          if let Some(term) = item.term_index_at_sym(self.graph.graph_type) {
            symbols.insert(term);
          }
        }
      } else {
        let (follow, _) = get_follow(self, item);
        for item in follow {
          if let Some(term) = item.term_index_at_sym(mode) {
            symbols.insert(term);
          } else if item.is_nonterm(mode) {
            for item in self.db.get_closure(&item) {
              if let Some(term) = item.term_index_at_sym(self.graph.graph_type) {
                symbols.insert(term);
              }
            }
          }
        }
      }
    }

    Some(symbols)
  }

  pub fn iter_pending_states_mut(&mut self, closure: &dyn Fn(StateBuilder)) {
    for state in self.pending.clone() {
      closure(StateBuilder { builder: self, resolved: true, state_id: state })
    }
  }

  pub fn get_pending_items(&self) -> ItemSet<'db> {
    let items = self
      .pending
      .iter()
      .filter_map(|s| {
        if let StateType::Peek(_) = self.graph[*s].t_type {
          self.graph[*s]
            .as_ref(&self.graph)
            .get_peek_resolve_items()
            .map(|i| i.flat_map(|(_, PeekGroup { items, .. })| items).cloned().collect::<ItemSet<'db>>())
        } else {
          Some(self.get_state(*s).get_kernel_items().iter().to_set())
        }
      })
      .flatten()
      .collect();
    items
  }

  fn create_root_state(&mut self, kernel_items: BTreeSet<Item<'db>>) {
    self.create_state(Normal, (SymbolId::Default, 0).into(), StateType::Start, Some(kernel_items.into_iter())).to_enqueued();
  }

  fn prepare_state(&mut self, state_id: StateId) {
    let lookahead_id = match self.get_state_symbols(state_id) {
      Some(lookahead_set) if !lookahead_set.is_empty() => {
        let lookahead_id = hash_id_value_u64(&lookahead_set);
        self.graph.symbol_sets.insert(lookahead_id, lookahead_set);
        lookahead_id
      }
      _ => 0,
    };
    let hash = if self.config.ALLOW_LOOKAHEAD_MERGE {
      Self::create_state_hash(self.get_state(state_id), 0, DefaultHasher::new())
    } else {
      Self::create_state_hash(self.get_state(state_id), lookahead_id, DefaultHasher::new())
    };

    self.graph[state_id].symbol_set_id = lookahead_id;
    self.graph[state_id].canonical_hash = hash;
    self.graph[state_id].lookahead_hash = hash_id_value_u64((hash, lookahead_id));
  }

  pub fn enqueue_state(&mut self, state_id: StateId) -> EnqueResult {
    let core_state = self.get_state(state_id).internal();
    let Self { config, graph, .. } = self;
    let hash_id = core_state.canonical_hash;

    //debug_assert!(hash_id != 0, "State has not been hashed!\n{}",
    // state._debug_string_());
    let prev = self.state_map.get(&hash_id).cloned();
    match (state_id.is_root(), prev) {
      (false, Some(original_state)) => {
        let parent: StateId = core_state.parent;

        graph.state_predecessors.entry(original_state).or_default().insert(parent);

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

        EnqueResult::Merged(original_state)
      }
      _ => {
        self.graph[state_id].used = true;
        self.state_map.insert(hash_id, state_id);
        self.state_queue.push_back(state_id);
        EnqueResult::Enqueued(state_id)
      }
    }
  }

  pub fn add_pending(&mut self, state: StateId) {
    self.pending.push(state);
  }

  pub fn current_state_id(&self) -> StateId {
    self.state_id
  }

  pub fn current_state<'a>(&'a self) -> GraphStateRef<'a, 'db> {
    GraphStateRef { graph: &self.graph, id: self.state_id }
  }

  pub fn current_state_mut<'a>(&'a mut self) -> GraphStateMutRef<'a, 'db> {
    self.get_state_mut(self.state_id)
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

  pub fn get_state<'a>(&'a self, state_id: StateId) -> GraphStateRef<'a, 'db> {
    self.graph[state_id].as_ref(&self.graph)
  }

  pub fn get_state_mut<'a>(&'a mut self, state_id: StateId) -> GraphStateMutRef<'a, 'db> {
    GraphStateMutRef { graph: &mut self.graph, id: state_id }
  }

  pub fn create_state<'a, T: Iterator<Item = Item<'db>>>(
    &'a mut self,
    state: GraphBuildState,
    symbol: PrecedentSymbol,
    t_type: StateType,
    kernel_items: Option<T>,
  ) -> StateBuilder<'a, 'db> {
    let index = self.graph.states.len();
    let id = StateId::new(index, if index > 0 { GraphIdSubType::Regular } else { GraphIdSubType::Root });

    let state = GraphState {
      id,
      t_type,
      parent: self.state_id,
      build_state: state,
      ..Default::default()
    };

    self.graph.term_symbol.insert(id, symbol);

    if !self.state_id.is_invalid() {
      self.graph.state_predecessors.insert(id, BTreeSet::from_iter(vec![self.state_id]));
    }

    if let Some(kernel_items) = kernel_items {
      state.as_mut_ref(&mut self.graph).set_kernel_items(kernel_items);
    } else {
      state.as_mut_ref(&mut self.graph).set_kernel_items([].into_iter());
    }

    self.graph.states.push(state);

    StateBuilder { state_id: id, builder: self, resolved: false }
  }

  #[cfg(debug_assertions)]
  pub fn _print_state_(&self) {
    println!("{}", self.get_state(self.state_id)._debug_string_());
  }

  #[cfg(debug_assertions)]
  pub fn _print_graph_(&self) {
    self.graph._debug_print_()
  }

  #[cfg(debug_assertions)]
  pub fn _is_terminal_state_(&self, nterm_id: u32, state_id: u32) -> bool {
    self.graph.goal_items().iter().any(|i| i.origin.get_symbol_key() == DBTermKey::from(nterm_id))
      && self.current_state_id().0 == state_id
      || self.graph.goal_items().iter().any(|i| i.nonterm_index().to_val() == nterm_id) && self.current_state_id().0 == state_id
  }

  #[cfg(debug_assertions)]
  pub fn _is_nonterminal_state_(&self, nterm_id: u32, state_id: u32) -> bool {
    self.graph.goal_items().iter().any(|i| i.nonterm_index().to_val() == nterm_id) && self.current_state_id().0 == state_id
  }
}

/// Iterates over valid state nodes within the graph. Valid nodes are those that
/// reachable from leaf states.
pub struct GraphIterator<'graph, 'db: 'graph> {
  pub graph:   &'graph GraphHost<'db>,
  queue:       Queue<StateId>,
  #[allow(unused)]
  used_states: OrderedSet<StateId>,
  links:       OrderedMap<StateId, OrderedSet<GraphStateRef<'graph, 'db>>>,
  empty_hash:  OrderedSet<GraphStateRef<'graph, 'db>>,
}

impl<'a, 'db: 'a> GraphIterator<'a, 'db> {
  pub fn new(graph: &'a GraphHost<'db>) -> Self {
    let (used_states, links) = graph
      .get_leaf_states()
      .iter()
      .filter_map(|leaf| {
        let mut seen = OrderedSet::new();
        let mut queue = Queue::from_iter([leaf.id]);
        let mut have_root = false;
        let mut links = OrderedMap::new();
        while let Some(state) = queue.pop_front() {
          have_root |= state.is_root();
          if !seen.insert(state) {
            continue;
          }

          if let Some(predecessors) = graph[state].as_ref(graph).get_predecessors().clone() {
            for predecessor in predecessors {
              links.entry(*predecessor).or_insert(OrderedSet::new()).insert(graph[state].as_ref(graph));
              queue.push_back(*predecessor);
            }
          }
        }

        have_root.then_some((seen, links))
      })
      .fold((OrderedSet::default(), OrderedMap::default()), |(mut a1, mut a2), (mut b1, b2)| {
        a1.append(&mut b1);
        b2.into_iter().for_each(|(key, mut val)| a2.entry(key).or_insert(OrderedSet::new()).append(&mut val));
        (a1, a2)
      });

    Self {
      queue: Queue::from_iter(used_states.iter().cloned()),
      empty_hash: OrderedSet::new(),
      used_states,
      links,
      graph,
    }
  }

  pub fn next<'b>(&'b mut self) -> Option<(GraphStateRef, &'b OrderedSet<GraphStateRef>)> {
    while let Some(state) = self.queue.pop_front() {
      return Some((self.graph[state].as_ref(self.graph), self.links.get(&state).unwrap_or(&self.empty_hash)));
    }

    None
  }
}
