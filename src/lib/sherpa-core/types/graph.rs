use std::{
  collections::{hash_map::DefaultHasher, BTreeSet, HashSet, VecDeque},
  hash::{Hash, Hasher},
  ops::{Index, IndexMut},
};

use crate::utils::create_u64_hash;

use super::*;

/// Indicates the State type that generated
/// the item
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Origin {
  None,
  /// The goal production that this item or it's predecessors will reduce to
  ProdGoal(DBProdKey),
  /// The goal symbol id that this item or its predecessors will recognize
  TokenGoal(DBTokenKey),
  /// The goal origin item set that this item will resolve to
  Peek(u64, StateId),
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
  #[cfg(debug_assertions)]
  pub fn debug_string(&self, db: &ParserDatabase) -> String {
    match self {
      Origin::ProdGoal(prod_id) => {
        format!(
          "ProdGoal[ {} {:?} ]",
          db.prod_name(*prod_id).to_string(db.string_store()),
          prod_id
        )
      }
      Origin::TokenGoal(sym_id) => {
        format!("TokenGoal[ {:?} ]", sym_id)
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
      Origin::TokenGoal(sym_id) => db.sym(*sym_id),
      _ => SymbolId::Undefined,
    }
  }
}

// Transtion Type ----------------------------------------------------

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum StateType {
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
  AssignAndFollow(DBTokenKey),
  Reduce(DBRuleKey),
  AssignToken(DBTokenKey),
  /// Calls made on items within a state's closure but
  /// are not kernel items.
  InternalCall(DBProdKey),
  /// Calls made on kernel items
  KernelCall(DBProdKey),
  /// Creates a leaf state that has a single `pop` instruction,
  /// with the intent of removing a goto floor state.
  ProductionCompleteOOS,
  /// Creates a leaf state that has a single `pop` instruction,
  /// with the intent of removing a goto floor state.
  _PeekProductionCompleteOOS,
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
    matches!(self, GotoLoop | GotoPass | KernelGoto)
  }

  #[cfg(debug_assertions)]
  fn debug_string(&self, db: &ParserDatabase) -> String {
    match self {
      Self::KernelCall(prod_id) => {
        format!("KernelCall({prod_id:?})")
      }
      Self::InternalCall(prod_id) => {
        format!("InternalCall({prod_id:?})",)
      }
      Self::AssignAndFollow(sym_id) => {
        format!("AssignAndFollow({})", db.sym(*sym_id).debug_string(db))
      }
      Self::AssignToken(sym_id) => {
        format!("AssignToken({})", db.sym(*sym_id).debug_string(db))
      }
      Self::Reduce(prod_id) => {
        format!("Reduce({prod_id:?})",)
      }
      _ => format!("{:?}", self),
    }
  }
}
pub const OUT_SCOPE_INDEX: u32 = 0xFEEDDEED;

// State -------------------------------------------------------------

#[derive(Default, PartialEq, Eq)]
pub struct State<'db> {
  id: StateId,
  term_symbol: SymbolId,
  t_type: StateType,
  parent: StateId,
  predecessors: OrderedSet<StateId>,
  kernel_items: ItemSet<'db>,
  peek_resolve_items: OrderedMap<u64, Items<'db>>,
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

      match item.origin {
        Origin::Peek(hash_id, _) => hash_id.hash(state),
        other => other.hash(state),
      }
    }

    for item in &self.non_terminals {
      item.rule_id.hash(state);
      item.sym_index.hash(state);
      item.origin.hash(state);

      match item.origin {
        Origin::Peek(hash_id, _) => hash_id.hash(state),
        other => other.hash(state),
      }
    }

    for item in
      &self.peek_resolve_items.values().flatten().collect::<OrderedSet<_>>()
    {
      item.rule_id.hash(state);
      item.sym_index.hash(state);
    }

    if let Some(item) = self.reduce_item {
      item.rule_id.hash(state);
      item.sym_index.hash(state);
    }
  }
}

impl<'db> State<'db> {
  /// Set a group of items that a peek item will resolve to.
  pub fn set_peek_resolve_items(
    &mut self,
    peek_origin_key: u64,
    items: Items<'db>,
  ) {
    self.peek_resolve_items.insert(peek_origin_key, items);
  }

  pub fn get_hash(&self) -> u64 {
    let mut hasher = DefaultHasher::new();
    self.hash(&mut hasher);
    hasher.finish()
  }

  pub fn get_resolve_items(&self, peek_origin_key: u64) -> Items<'db> {
    self.peek_resolve_items.get(&peek_origin_key).unwrap().clone()
  }

  pub fn calculate_closure(
    &mut self,
    is_scanner: bool,
    db: &'db ParserDatabase,
  ) {
    self.closure = None;
    let state_id = self.id;
    let closure = self.kernel_items.create_closure(is_scanner, state_id);

    if self.id.is_root() {
      let prods = self
        .kernel_items
        .iter()
        .map(|i| i.prod_index())
        .collect::<OrderedSet<_>>();
      let sigs = closure
        .iter()
        .map(|i| (i.rule_id, i.sym_index))
        .collect::<HashSet<_>>();
      // Get all follow items

      let mut oos_closure = closure.clone();

      for prod in &prods {
        for item in db.follow_items(*prod) {
          let i = item;
          if !sigs.contains(&(i.rule_id, i.sym_index)) {
            oos_closure.insert(i.to_origin_state(state_id).to_oos_index());
          }
        }
      }

      self.root_closure = Some(oos_closure);
    }

    if !self.kernel_items.is_empty() {
      self.closure = Some(closure);
    }
  }

  pub fn kernel_items_ref(&self) -> &ItemSet<'db> {
    &self.kernel_items
  }

  pub fn get_closure_ref(&self) -> Option<&ItemSet<'db>> {
    return self.closure.as_ref();
  }

  pub fn get_root_closure_ref(&self) -> Option<&ItemSet<'db>> {
    if self.id.is_root() {
      return self.root_closure.as_ref();
    } else {
      return self.closure.as_ref();
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

  pub fn set_non_terminals(&mut self, non_terms: &BTreeSet<Item<'db>>) {
    self.non_terminals = non_terms.clone();
  }

  pub fn get_type(&self) -> StateType {
    self.t_type
  }

  pub fn get_goto_state(&self) -> Option<Self> {
    if self.non_terminals.len() > 0 {
      Some(Self {
        term_symbol: self.term_symbol,
        id: self.id.to_goto(),
        t_type: StateType::KernelGoto,
        kernel_items: self.non_terminals.clone(),
        non_terminals: self.kernel_items.clone(),
        ..Default::default()
      })
    } else {
      None
    }
  }

  pub(crate) fn get_symbol(&self) -> SymbolId {
    self.term_symbol
  }

  pub fn get_parent(&self) -> StateId {
    self.parent
  }

  pub fn add_kernel_items<T: ItemContainer<'db>>(
    &mut self,
    kernel_items: T,
    is_scanner: bool,
    db: &'db ParserDatabase,
  ) {
    let mut kernel_items =
      kernel_items.into_iter().map(|i| i.to_origin_state(self.id)).collect();

    self.kernel_items.append(&mut kernel_items);

    self.calculate_closure(is_scanner, db);
  }

  /// Returns true if the the call of the production leads to infinite loop due
  /// to left recursion.
  pub fn conflicting_production_call(
    &self,
    prod_id: DBProdKey,
    is_scanner: bool,
    db: &ParserDatabase,
  ) -> bool {
    if self.id.is_root() {
      let prod_ids = self
        .kernel_items
        .iter()
        .map(|i| i.prod_index())
        .collect::<OrderedSet<_>>();

      Items::start_items(prod_id, db)
        .create_closure(is_scanner, self.id)
        .into_iter()
        .any(|i| {
          prod_ids.contains(&i.prod_index_at_sym().unwrap_or_default())
            || prod_ids.contains(&i.prod_index())
        })
    } else {
      false
    }
  }

  #[cfg(debug_assertions)]
  pub fn debug_string(&self, db: &'db ParserDatabase) -> String {
    let mut string = String::new();
    string += &format!("\n\nSTATE -- [{:}] --", self.id.0);

    if self.predecessors.len() > 0 {
      string += &format!(
        r##" preds [{}]"##,
        self
          .predecessors
          .iter()
          .map(|p| p.0.to_string())
          .collect::<Vec<_>>()
          .join(" ")
      );
    }

    string += &format!(
      "\n\n Type {:?}; Sym: [{}]",
      self.t_type.debug_string(db),
      self.term_symbol.debug_string(db),
    );

    for (index, items) in &self.peek_resolve_items {
      string += &format!("\n  Peek Resolve: {}", index);
      for item in items {
        string += &format!("\n   - {}", item.debug_string());
      }
    }

    if !self.non_terminals.is_empty() {
      string += "\n-- non-terms:";
      for item in &self.non_terminals {
        string += &format!("\n   - {}", item.debug_string());
      }
    }

    if let Some(item) = &self.reduce_item {
      string += &format!("\n  Reduce:");
      string += &format!("\n   - {}", item.debug_string());
    }

    string += "\n";

    for item in &self.kernel_items {
      string += &format!("\n    {}", item.debug_string());
    }
    string
  }
}

// Graph -------------------------------------------------------------

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphState {
  Normal,
  Peek,
  LongestMatch,
  ShortestMatch,
  FirstMatch,
  //BreadCrumb,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum GraphMode {
  /// Classic Recursive Descent Ascent with unlimited lookahead.
  Parser,
  // Scanner mode for creating tokens, analogous to regular expressions.
  Scanner,
}

pub struct Graph<'follow, 'db: 'follow> {
  state_map: Map<u64, StateId>,
  states: Array<State<'db>>,
  leaf_states: OrderedSet<StateId>,
  pending_states: VecDeque<(GraphState, StateId)>,
  mode: GraphMode,
  db: &'db ParserDatabase,
  follow: &'follow FollowSets<'db>,
}

impl<'follow, 'db: 'follow> Graph<'follow, 'db> {
  pub fn new(
    db: &'db ParserDatabase,
    mode: GraphMode,
    follow: &'follow FollowSets<'db>,
  ) -> Self {
    Self {
      mode,
      state_map: Default::default(),
      states: Default::default(),
      leaf_states: Default::default(),
      pending_states: Default::default(),
      db,
      follow,
    }
  }

  pub fn create_state(
    &mut self,
    symbol: SymbolId,
    t_type: StateType,
    parent: Option<StateId>,
    kernel_items: Items<'db>,
  ) -> StateId {
    let id = StateId(self.states.len() as u32);
    let is_scan = self.is_scan();

    if (matches!(symbol, SymbolId::NonTerminalToken { .. })) {
      panic!("WTF!");
    }

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

    state.add_kernel_items(
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
      is_scan,
      self.db,
    );

    self.states.push(state);

    id
  }

  pub fn get_db(&self) -> &'db ParserDatabase {
    self.db
  }

  pub fn is_scan(&self) -> bool {
    matches!(self.mode, GraphMode::Scanner)
  }

  pub fn add_leaf_state(&mut self, state: StateId) {
    self.leaf_states.insert(state);
    self[state].leaf_state = true;
  }

  pub fn get_leaf_states(&self) -> Vec<&State> {
    self.leaf_states.iter().map(|s| &self[*s]).collect()
  }

  pub fn enqueue_pending_state(
    &mut self,
    graph_state: GraphState,
    state: StateId,
  ) -> Option<StateId> {
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

  pub fn dequeue_pending_state(&mut self) -> Option<(GraphState, StateId)> {
    self.pending_states.pop_front()
  }

  pub fn item_is_goal(&self, item: &Item<'db>) -> bool {
    if !item.is_complete() {
      return false;
    }

    self.states[0].kernel_items.iter().any(|i| i.rule_id == item.rule_id)
  }

  pub fn goal_items(&self) -> &ItemSet {
    &self.states[0].kernel_items
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
}

impl<'follow, 'db: 'follow> Index<usize> for Graph<'follow, 'db> {
  type Output = State<'db>;

  fn index(&self, index: usize) -> &Self::Output {
    &self.states[index]
  }
}

impl<'follow, 'db: 'follow> Index<StateId> for Graph<'follow, 'db> {
  type Output = State<'db>;

  fn index(&self, index: StateId) -> &Self::Output {
    &self.states[index.0 as usize]
  }
}

impl<'follow, 'db: 'follow> IndexMut<StateId> for Graph<'follow, 'db> {
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

  pub fn to_goto(&self) -> Self {
    Self(self.0 + (u32::MAX >> 1))
  }
}

impl<'db> PartialEq<u32> for State<'db> {
  fn eq(&self, other: &u32) -> bool {
    self.id.0 == *other
  }
}
