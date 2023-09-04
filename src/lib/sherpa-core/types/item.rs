use super::{
  super::types::*,
  graph::{Origin, StateId, OUT_SCOPE_INDEX},
};
use std::{collections::VecDeque, hash::Hash};

pub enum ItemType {
  Terminal(SymbolId),
  NonTerminal(DBNonTermKey),
  TokenNonTerminal(DBNonTermKey, SymbolId),
  Completed(DBNonTermKey),
}

pub type StaticItem = (DBRuleKey, u16);

#[derive(Clone, Copy)]
pub struct Item<'db> {
  db: &'db ParserDatabase,
  /// The NonTerminal non-terminal or symbol that the item directly or
  /// indirectly resolves to
  pub origin: Origin,
  /// The Graph goal
  pub goal: u32,
  /// The graph state the item originated from
  pub origin_state: StateId,
  /// The index location of the item's Rule
  pub rule_id: DBRuleKey,
  /// The number of symbols that comprise the items's Rule
  pub len: u16,
  /// The index of the active symbol. If `len == sym_index` then
  /// the item is considered complete.
  pub sym_index: u16,
  ////
  pub goto_distance: u8,
}

#[cfg(debug_assertions)]
impl<'db> std::fmt::Debug for Item<'db> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("ItemRef");
    s.field("val", &self.debug_string());
    s.field("origin", &self.origin);
    s.field("goal", &self.goal);
    s.field("origin_state", &self.origin_state);
    s.finish()
  }
}

impl<'db> Hash for Item<'db> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    (
      self.rule_id,
      self.origin,
      self.origin_state,
      self.goal,
      self.len,
      self.sym_index,
      self.token_precedence(),
      self.symbol_precedence(),
    )
      .hash(state)
  }
}

impl<'a> PartialEq for Item<'a> {
  fn eq(&self, other: &Self) -> bool {
    let a = (self.rule_id, self.origin, self.goal, self.len, self.sym_index, self.origin_state);
    let b = (other.rule_id, other.origin, other.goal, other.len, other.sym_index, other.origin_state);
    a == b
  }
}

impl<'a> Eq for Item<'a> {}

impl<'a> PartialOrd for Item<'a> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a = (self.rule_id, self.origin, self.goal, self.len, self.sym_index, self.origin_state);
    let b = (other.rule_id, other.origin, other.goal, other.len, other.sym_index, other.origin_state);
    Some(a.cmp(&b))
  }
}

impl<'a> Ord for Item<'a> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.partial_cmp(other).unwrap()
  }
}

impl<'db> Item<'db> {
  /// Creates a new [ItemRef] with the same rule info as the original, but
  /// with the meta info of `other`.
  pub fn align(&self, other: Item<'db>) -> Self {
    Self {
      rule_id: self.rule_id,
      len: self.len,
      sym_index: self.sym_index,
      ..other
    }
  }

  pub fn get_db(&self) -> &ParserDatabase {
    self.db
  }

  pub fn to_origin(&self, origin: Origin) -> Self {
    Self { origin, ..self.clone() }
  }

  pub fn to_oos_index(&self) -> Self {
    Self { goal: OUT_SCOPE_INDEX, ..self.clone() }
  }

  pub fn to_origin_state(&self, origin_state: StateId) -> Self {
    Self { origin_state, ..self.clone() }
  }

  pub fn from_rule(rule_id: DBRuleKey, db: &'db ParserDatabase) -> Self {
    let rule = db.rule(rule_id);
    Self {
      db,
      rule_id,
      len: rule.symbols.len() as u16,
      origin_state: StateId::default(),
      sym_index: 0,
      origin: Default::default(),
      goal: 0,
      goto_distance: 0,
    }
  }

  pub fn from_static((rule_id, sym_index): StaticItem, db: &'db ParserDatabase) -> Self {
    let rule = db.rule(rule_id);
    Self {
      db,
      rule_id,
      len: rule.symbols.len() as u16,
      origin_state: StateId::default(),
      sym_index,
      origin: Default::default(),
      goal: 0,
      goto_distance: 0,
    }
  }

  /// Returns the reduce type of the item.
  pub fn resolve_type(&self) -> ReductionType {
    self.db.get_reduce_type(self.rule_id)
  }

  pub fn to_absolute(&self) -> Self {
    Self { goal: Default::default(), origin: Default::default(), ..self.clone() }
  }

  /// Returns the canonical item, that is an item that does not have any other
  /// attributes aside from a rule id and a symbol offset
  pub fn to_canonical(&self) -> Self {
    Self {
      goal: Default::default(),
      origin: Default::default(),
      origin_state: StateId::default(),
      ..self.clone()
    }
  }

  /// Calculates the number of GOTO states that would be pushed to the stack
  /// following the state this item originated from.
  pub fn calculate_goto_distance(&self, parent: StateId, graph: &GraphHost<'db>) -> Self {
    let mut state = parent;
    let mut goto_distance = 0;

    while state != self.origin_state {
      debug_assert!(!state.is_root());
      if graph[state].has_goto_state() {
        goto_distance += 1;
      }
      state = graph[state].get_parent();
    }

    Self { goto_distance, ..self.clone() }
  }

  pub fn increment(&self) -> Option<Self> {
    if !self.is_complete() {
      Some(Self { len: self.len, sym_index: self.sym_index + 1, ..self.clone() })
    } else {
      None
    }
  }

  /// Increments the Item if it is not in the completed position,
  /// otherwise returns the Item as is.
  pub fn try_increment(&self) -> Self {
    if !self.is_complete() {
      self.increment().unwrap()
    } else {
      self.clone()
    }
  }

  pub fn is_start(&self) -> bool {
    self.sym_index == 0
  }

  pub fn decrement(&self) -> Option<Self> {
    if !self.is_start() {
      Some(Self { len: self.len, sym_index: self.sym_index - 1, ..self.clone() })
    } else {
      None
    }
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.goal == OUT_SCOPE_INDEX || self.origin.is_out_of_scope()
  }

  pub fn to_start(&self) -> Self {
    Self { sym_index: 0, ..self.clone() }
  }

  pub fn to_null(&self) -> Self {
    Self { len: 0, ..self.clone() }
  }

  pub fn to_complete(&self) -> Self {
    Self { sym_index: self.len, ..self.clone() }
  }

  /// Returns `true` if the item is in the initial position
  pub fn is_at_initial(&self) -> bool {
    self.sym_index == 0
  }

  pub fn get_skipped(&self) -> Option<&'db Vec<SymbolId>> {
    if self.is_complete() {
      None
    } else {
      let rule = self.rule();
      Some(&rule.skipped)
    }
  }

  pub fn rule(&self) -> &'db Rule {
    self.db.rule(self.rule_id)
  }

  pub fn nonterm_name(&self) -> IString {
    self.db.nonterm_friendly_name(self.nonterm_index())
  }

  /// The non-terminal the rule reduces to
  pub fn nonterm_index(&self) -> DBNonTermKey {
    self.db.rule_nonterm(self.rule_id)
  }

  /// Return `true` if the item is from a left recursive rule.
  pub fn is_left_recursive(&self) -> bool {
    let p = self.nonterm_index();
    p == self.to_start().nontermlike_index_at_sym().unwrap_or_default()
  }

  pub fn is_null(&self) -> bool {
    self.len == 0
  }

  pub fn is_complete(&self) -> bool {
    self.len == self.sym_index
  }

  pub fn is_penultimate(&self) -> bool {
    (self.len - 1) == self.sym_index
  }

  pub fn is_nonterm(&self) -> bool {
    if self.is_complete() {
      false
    } else {
      match self.rule().symbols[self.sym_index as usize].id {
        SymbolId::NonTerminal { .. } | SymbolId::DBNonTerminal { .. } => true,
        _ => false,
      }
    }
  }

  /// Returns the item's active symbol.
  pub fn sym(&self) -> SymbolId {
    if self.is_complete() {
      SymbolId::EndOfFile
    } else {
      self.rule().symbols[self.sym_index as usize].id
    }
  }

  /// Returns the [IndexedProdId] of the active symbol if the symbol
  /// is a NonTerm or NonTermToken, or return `None`
  pub fn nontermlike_index_at_sym(&self) -> Option<DBNonTermKey> {
    match self.sym() {
      SymbolId::DBNonTerminal { key: index } | SymbolId::DBNonTerminalToken { nonterm_key: index, .. } => Some(index),
      _ => None,
    }
  }

  /// Returns the [IndexedProdId] of the active symbol if the symbol
  /// is a NonTerm, or return `None`
  pub fn nonterm_index_at_sym(&self) -> Option<DBNonTermKey> {
    match self.sym() {
      SymbolId::DBNonTerminal { key: index } => Some(index),
      _ => None,
    }
  }

  /// Returns the type of item based on the active symbol.
  pub fn get_type(&self) -> ItemType {
    use ItemType::*;
    if self.is_complete() {
      Completed(self.nonterm_index())
    } else {
      match self.sym() {
        SymbolId::DBNonTerminal { key: index } => NonTerminal(index),
        SymbolId::DBNonTerminalToken { nonterm_key: index, .. } => TokenNonTerminal(index, self.sym()),
        sym => Terminal(sym),
      }
    }
  }

  /// Get the precedence appropriate for the graph mode
  pub fn precedence(&self, mode: GraphMode) -> u16 {
    match mode {
      GraphMode::Parser => self.symbol_precedence(),
      GraphMode::Scanner => self.token_precedence(),
    }
  }

  pub fn origin_precedence(&self) -> u16 {
    match self.origin {
      Origin::TerminalGoal(_, prec) => prec,
      _ => 0,
    }
  }

  pub fn token_precedence(&self) -> u16 {
    if self.is_complete() {
      self.rule().symbols[(self.sym_index - 1) as usize].token_precedence
    } else {
      self.rule().symbols[self.sym_index as usize].token_precedence
    }
  }

  pub fn symbol_precedence(&self) -> u16 {
    if self.is_complete() {
      self.rule().symbols[(self.sym_index - 1) as usize].symbol_precedence as u16
    } else {
      self.rule().symbols[self.sym_index as usize].symbol_precedence as u16
    }
  }

  pub fn is_term(&self) -> bool {
    if self.is_complete() {
      false
    } else {
      !self.is_nonterm()
    }
  }

  #[cfg(debug_assertions)]
  pub fn debug_string(&self) -> String {
    if self.is_null() {
      format!("null")
    } else {
      let s_store = self.db.string_store();

      let mut string = self
        .origin
        .is_none()
        .then_some(String::new())
        .unwrap_or_else(|| format!("<[{}-{:?}]  [{:X}] ", self.origin.debug_string(self.db), self.origin_state, self.goal));

      string += &self.nonterm_name().to_string(s_store);

      string += " >";

      let mut item = Some(self.to_start());

      while let Some(i) = item.clone() {
        if i.is_complete() {
          break;
        };

        if i.sym_index == self.sym_index {
          string += " •";
        }

        string += " ";

        string += &i.sym().debug_string(self.db);

        string += &match (i.symbol_precedence(), i.token_precedence()) {
          (0, 0) => String::default(),
          (sym, 0) => "{".to_string() + &sym.to_string() + "}",
          (0, tok) => "{:".to_string() + &tok.to_string() + "}",
          (sym, tok) => "{".to_string() + &sym.to_string() + ":" + &tok.to_string() + "}",
        };

        item = i.increment();
      }

      if self.is_complete() {
        string += " •";
      }

      string += &(" [".to_string() + &self.goto_distance.to_string() + "]");

      string.replace("\n", "\\n")
    }
  }
}

pub type ItemSet<'db> = OrderedSet<Item<'db>>;
pub type Items<'db> = Array<Item<'db>>;

impl<'db> ItemContainer<'db> for ItemSet<'db> {}
impl<'db> ItemContainer<'db> for Items<'db> {}

impl<'a, 'db: 'a, T: Clone + Iterator<Item = &'a Item<'db>>> ItemRefContainerIter<'a, 'db> for T {}
impl<'a, 'db: 'a, T: Clone + Iterator<Item = Item<'db>>> ItemContainerIter<'a, 'db> for T {}

macro_rules! common_iter_functions {
  () => {
    fn get_max_precedence(self, mode: GraphMode) -> u16 {
      match mode {
        GraphMode::Parser => self.get_max_symbol_precedence(),
        GraphMode::Scanner => self.get_max_token_precedence(),
      }
    }

    fn get_max_origin_precedence(self) -> u16 {
      self.map(|i| i.origin_precedence()).max().unwrap_or_default()
    }

    fn get_max_token_precedence(self) -> u16 {
      self.map(|i| i.token_precedence()).max().unwrap_or_default()
    }

    fn get_max_symbol_precedence(self) -> u16 {
      self.map(|i| i.symbol_precedence()).max().unwrap_or_default()
    }

    fn intersects(&mut self, set: &ItemSet) -> bool {
      self.any(|i| set.contains(&i))
    }

    #[cfg(debug_assertions)]
    fn debug_print(self, _comment: &str) {
      println!("------>{} \n {}", _comment, self.to_debug_string("\n\n"));
    }

    #[cfg(debug_assertions)]
    fn to_debug_string(self, sep: &str) -> String {
      self.map(|i| i.debug_string()).collect::<Vec<_>>().join(sep)
    }

    /// Returns the [DBNonTermKey] of the symbol in non-terminal items. Items that
    /// do not have a nonterm as the active symbol are skipped.
    fn nonterm_ids_at_index(self) -> OrderedSet<DBNonTermKey> {
      self.filter_map(|i| i.nonterm_index_at_sym()).collect()
    }

    /// Returns a set of all non-terminal ids the items reduce to.
    fn rule_nonterm_ids(&mut self) -> OrderedSet<DBNonTermKey> {
      self.map(|i| i.nonterm_index()).collect()
    }

    fn peek_is_resolved(&mut self) -> bool {
      self.all_items_are_from_same_peek_origin()
    }

    fn follow_items_are_the_same(&mut self) -> bool {
      self.map(|i| i.to_absolute()).collect::<ItemSet>().len() == 1
    }

    fn contains_out_of_scope(&mut self) -> bool {
      self.any(|i| i.is_out_of_scope())
    }

    fn all_are_out_of_scope(&mut self) -> bool {
      self.all(|i| i.origin.is_out_of_scope() || i.is_out_of_scope())
    }

    fn all_items_are_from_same_peek_origin(&mut self) -> bool {
      let origin_set = self.map(|i| i.origin).collect::<OrderedSet<_>>();
      match (origin_set.len(), origin_set.first()) {
        (1, Some(Origin::Peek(..))) => true,
        _ => false,
      }
    }

    fn nonterm_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| i.is_nonterm().then(|| i.clone())).collect()
    }

    fn term_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| i.is_term().then(|| i.clone())).collect()
    }

    fn null_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| i.is_null().then(|| i.clone())).collect()
    }

    fn incomplete_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| (!i.is_complete()).then(|| i.clone())).collect()
    }

    fn completed_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| (i.is_complete()).then(|| i.clone())).collect()
    }

    fn inscope_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| (!i.is_out_of_scope()).then(|| i.clone())).collect()
    }

    fn outscope_items<T: ItemContainer<'db>>(self) -> T {
      self.filter_map(|i| (!i.is_out_of_scope()).then(|| i.clone())).collect()
    }

    fn to_absolute<T: ItemContainer<'db>>(self) -> T {
      self.map(|i| i.to_absolute()).collect()
    }

    fn to_canonical<T: ItemContainer<'db>>(self) -> T {
      self.map(|i| i.to_canonical()).collect()
    }

    fn to_origin<T: ItemContainer<'db>>(self, origin: Origin) -> T {
      self.map(|i| i.to_origin(origin)).collect()
    }

    #[inline(always)]
    fn try_increment(self) -> Items<'db> {
      self.map(|i| i.try_increment()).collect()
    }

    #[inline(always)]
    fn try_decrement(self) -> Items<'db> {
      self.map(|i| if i.sym_index > 0 { i.decrement().unwrap() } else { i.clone() }).collect()
    }

    fn terminals(self) -> OrderedSet<SymbolId> {
      self.filter_map(|i| (!i.is_nonterm()).then_some(i.sym())).collect()
    }

    fn closure<T: ItemContainer<'db>>(self, state_id: StateId) -> T {
      let mut closure = ItemSet::from_iter(self.clone().map(|i| i.clone()));
      closure.extend(self.filter(|i| !i.is_complete()).flat_map(|i| {
        let a = i.to_owned().to_canonical();
        let b = i.to_owned();
        i.db
          .get_closure(i.rule_id, i.sym_index as usize)
          .filter(move |i| i.to_canonical() != a)
          .map(move |a| a.align(b).to_origin_state(state_id))
      }));
      closure.into_iter().collect()
    }
  };
}

pub trait ItemContainerIter<'a, 'db: 'a>: Iterator<Item = Item<'db>> + Sized + Clone {
  fn to_set(self) -> ItemSet<'db> {
    self.collect()
  }

  fn to_vec(self) -> Items<'db> {
    self.collect()
  }

  common_iter_functions!();
}

pub trait ItemRefContainerIter<'a, 'db: 'a>: Iterator<Item = &'a Item<'db>> + Sized + Clone {
  fn to_set(self) -> ItemSet<'db> {
    self.cloned().collect()
  }

  fn to_vec(self) -> Items<'db> {
    self.cloned().collect()
  }

  common_iter_functions!();
}

impl<'db> From<Item<'db>> for Items<'db> {
  fn from(value: Item<'db>) -> Self {
    let db = value.db;
    if let Some(nterm) = value.nontermlike_index_at_sym() {
      if let Ok(rules) = db.nonterm_rules(nterm) {
        rules.iter().map(|r| Item::from_rule(*r, db)).collect()
      } else {
        Default::default()
      }
    } else {
      Default::default()
    }
  }
}

pub trait ItemContainer<'db>: Clone + IntoIterator<Item = Item<'db>> + FromIterator<Item<'db>> {
  /// Given a [CompileDatabase] and [DBProdId] returns the initial
  /// items of the non-terminal.
  fn start_items(nterm: DBNonTermKey, db: &'db ParserDatabase) -> Self {
    let Ok(rules) = db.nonterm_rules(nterm) else { panic!("Could not get rules") };
    rules.iter().map(|r| Item::from_rule(*r, db)).collect()
  }

  fn nonterm_items(self) -> Self {
    self.into_iter().filter(|i| i.is_nonterm()).collect()
  }

  fn term_items(self) -> Self {
    self.into_iter().filter(|i| i.is_term()).collect()
  }

  fn null_items(self) -> Self {
    self.into_iter().filter(|i| i.is_null()).collect()
  }

  fn incomplete_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_complete()).collect()
  }

  fn completed_items(self) -> Self {
    self.into_iter().filter(|i| i.is_complete()).collect()
  }

  fn inscope_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_out_of_scope()).collect()
  }

  fn outscope_items(self) -> Self {
    self.into_iter().filter(|i| i.is_out_of_scope()).collect()
  }

  fn uncompleted_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_complete()).collect()
  }

  fn to_origin(self, origin: Origin) -> Self {
    self.into_iter().map(|i| i.to_origin(origin)).collect()
  }

  fn to_origin_state(self, origin: StateId) -> Self {
    self.into_iter().map(|i| i.to_origin_state(origin)).collect()
  }

  #[inline(always)]
  fn try_increment(&self) -> Items<'db> {
    self.clone().to_vec().into_iter().map(|i| i.try_increment()).collect()
  }

  #[inline(always)]
  fn try_decrement(&self) -> Items<'db> {
    self.clone().to_vec().into_iter().map(|i| if i.sym_index > 0 { i.decrement().unwrap() } else { i }).collect()
  }

  #[cfg(debug_assertions)]
  fn debug_print(&self, _comment: &str) {
    debug_items(_comment, self.clone());
  }

  #[cfg(debug_assertions)]
  fn to_debug_string(&self, sep: &str) -> String {
    self.clone().to_vec().iter().map(|i| i.debug_string()).collect::<Vec<_>>().join(sep)
  }

  fn to_set(self) -> ItemSet<'db> {
    self.into_iter().collect()
  }

  fn to_vec(self) -> Items<'db> {
    self.into_iter().collect()
  }

  /// Creates a closure set over the given items.
  fn _create_closure(&self, is_scanner: bool, state_id: StateId) -> ItemSet<'db> {
    let mut closure = ItemSet::new();
    let mut queue = VecDeque::from_iter(self.clone());

    while let Some(kernel_item) = queue.pop_front() {
      if closure.insert(kernel_item) {
        match kernel_item.get_type() {
          ItemType::TokenNonTerminal(..) => {
            if is_scanner {
              for item in Items::from(kernel_item) {
                queue.push_back(item.align(kernel_item).to_origin_state(state_id))
              }
            }
          }
          ItemType::NonTerminal(..) => {
            for item in Items::from(kernel_item) {
              queue.push_back(item.align(kernel_item).to_origin_state(state_id))
            }
          }
          _ => {}
        }
      }
    }
    closure
  }
}

#[allow(unused)]
#[cfg(debug_assertions)]
fn debug_items<'db, T: IntoIterator<Item = Item<'db>>>(comment: &str, items: T) {
  println!("\n {} --> ", comment);

  for item in items {
    println!("    {}", item.debug_string());
  }
}

/// A tuple like type comprised of a completed item an one
/// other item that transitions on the nonterminal the completed item
/// reduces to.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(debug_assertions, derive(std::fmt::Debug))]
pub struct FollowPair<'db> {
  pub completed: Item<'db>,
  pub follow:    Item<'db>,
}

impl<'db> From<(Item<'db>, Item<'db>)> for FollowPair<'db> {
  fn from((completed, follow): (Item<'db>, Item<'db>)) -> Self {
    Self { completed, follow }
  }
}

pub struct CompletedItemArtifacts<'db> {
  pub follow_pairs: OrderedSet<FollowPair<'db>>,
  pub follow_items: ItemSet<'db>,
  pub default_only: ItemSet<'db>,
}

impl<'a, 'db: 'a> FollowPairContainerIter<'a, 'db> for std::collections::btree_set::Iter<'a, FollowPair<'db>> {}
impl<'a, 'db: 'a> FollowPairContainerIter<'a, 'db> for std::slice::Iter<'a, FollowPair<'db>> {}
pub trait FollowPairContainerIter<'a, 'db: 'a>: Iterator<Item = &'a FollowPair<'db>> + Sized {
  fn to_completed_set(&mut self) -> ItemSet<'db> {
    self.map(|i| i.completed).collect()
  }

  fn to_completed_vec(&mut self) -> Items<'db> {
    self.map(|i| i.completed).collect()
  }

  fn to_follow_set(&mut self) -> ItemSet<'db> {
    self.map(|i| i.follow).collect()
  }

  fn to_follow_vec(&mut self) -> Items<'db> {
    self.map(|i| i.follow).collect()
  }
}
