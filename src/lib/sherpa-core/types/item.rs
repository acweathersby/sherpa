use super::super::types::*;
use crate::compile::build_graph::graph::*;
use std::hash::Hash;

const OUT_SCOPE_LANE: u32 = 0x80_00_00_00;

pub enum ItemType {
  Terminal(SymbolId),
  NonTerminal(DBNonTermKey),
  TokenNonTerminal(DBNonTermKey, SymbolId),
  Completed(DBNonTermKey),
}

pub type StaticItem = (DBRuleKey, u16);

/// Stores an item in a compact form.
#[derive(Clone, Copy)]
pub struct ItemIndex(u32);

impl ItemIndex {
  const RULE_SHIFT: u32 = 10;

  /// Returns a tuple comprised of the item's rule and symbol offset
  pub fn get_parts(&self) -> StaticItem {
    (((self.0 >> 10) as usize).into(), (self.0 & 0x3FF) as u16)
  }
}

impl<'db> From<(u32, u32)> for ItemIndex {
  #[inline(always)]
  fn from((rule_id, sym_id): (u32, u32)) -> Self {
    debug_assert!(rule_id < (1 << (32 - ItemIndex::RULE_SHIFT)), "Rule index is too high to store in this form");
    debug_assert!(sym_id < (1 << ItemIndex::RULE_SHIFT), "Symbol index is too high to store in this form");
    Self((rule_id << ItemIndex::RULE_SHIFT) | ((sym_id & 0b1111_1111_1111) as u32))
  }
}

impl<'db> From<Item<'db>> for ItemIndex {
  #[inline(always)]
  fn from(i: Item<'db>) -> Self {
    Self::from((i.rule_id.into(), i.sym_index as u32))
  }
}

impl From<u32> for ItemIndex {
  fn from(value: u32) -> Self {
    Self(value)
  }
}

impl Into<u32> for ItemIndex {
  fn into(self) -> u32 {
    self.0
  }
}

#[cfg(debug_assertions)]
impl std::fmt::Debug for ItemIndex {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let (r, off) = self.get_parts();
    f.write_str(&("r".to_string() + &r.to_string() + "•" + &off.to_string()))
  }
}

#[derive(Clone, Copy, Default, Hash, Eq, PartialOrd, Ord, PartialEq)]
pub struct ItemLane {
  /// The previous lane the item was in. This usually a kernel
  /// item from which the closure of this item was derived.
  prev: u32,
  /// The current lane the item is in
  curr: u32,
}

impl ItemLane {
  pub fn is_from_lane(&self, other: Self) -> bool {
    /* self.curr == other.curr || */
    // This item follows the other item
    self.prev == other.curr || 
    // This item comes from the same closure as the other item
    self.prev == other.prev
  }

  pub fn new(lane: ItemIndex) -> Self {
    Self { prev: lane.into(), curr: lane.into() }
  }

  pub fn oos() -> Self {
    Self { prev: OUT_SCOPE_LANE, curr: OUT_SCOPE_LANE }
  }

  pub fn is_oos(&self) -> bool {
    (self.prev & OUT_SCOPE_LANE) == OUT_SCOPE_LANE
  }

  pub fn is_oos_closure(&self) -> bool {
    self.is_oos() && self.curr != OUT_SCOPE_LANE
  }

  pub fn has_split(&self) -> bool {
    self.curr != self.prev
  }

  pub fn get_curr(&self) -> u32 {
    self.curr & !OUT_SCOPE_LANE
  }

  pub fn get_prev(&self) -> u32 {
    self.prev & !OUT_SCOPE_LANE
  }

  pub fn to_prev(&self) -> Self {
    Self { prev: self.prev, curr: self.prev }
  }

  pub fn to_curr(&self) -> Self {
    Self { prev: self.curr, curr: self.curr }
  }

  pub fn split(&self, new_lane: ItemIndex) -> Self {
    if self.is_oos() {
      //Self { prev: self.curr | OUT_SCOPE_LANE, curr: new_lane | OUT_SCOPE_LANE }
      *self
    } else {
      Self { prev: self.curr, curr: new_lane.into() }
    }
  }
}

#[cfg(debug_assertions)]
impl std::fmt::Debug for ItemLane {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut t = f.debug_tuple("");

    if self.is_oos() {
      if self.has_split() {
        t.field(&self.prev);
        t.field(&self.curr);
      } else {
        t.field(&self.curr);
      }
    } else {
      let curr: ItemIndex = self.curr.into();
      let prev: ItemIndex = self.prev.into();
      if self.has_split() {
        t.field(&prev);
        t.field(&curr);
      } else {
        t.field(&curr);
      }
    }
    t.finish()
  }
}

#[derive(Clone, Copy)]
pub struct Item<'db> {
  /// The non-terminal or token that the item directly or
  /// indirectly resolves to
  pub origin: Origin,
  db: &'db ParserDatabase,
  /// The graph state the item originated from
  pub origin_state: StateId,
  /// The index location of the item's Rule
  pub rule_id: DBRuleKey,
  /// The graph goal lane
  pub lane: ItemLane,
  /// The number of symbols that comprise the items's Rule
  pub len: u16,
  /// The index of the active symbol. If `len == sym_index` then
  /// the item is considered complete.
  pub sym_index: u16,
  ////
  pub goto_distance: u8,
  pub from_goto_origin: bool,
}

#[cfg(debug_assertions)]
impl<'db> std::fmt::Debug for Item<'db> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("ItemRef");
    s.field("val", &self._debug_string_());
    s.field("origin", &self.origin);
    s.field("lane", &self.lane);
    s.field("origin_state", &self.origin_state);
    s.finish()
  }
}

impl<'db> From<(ItemIndex, &'db ParserDatabase)> for Item<'db> {
  fn from(value: (ItemIndex, &'db ParserDatabase)) -> Self {
    let (rule_id, sym_index) = value.0.get_parts();
    let mut item = Self::from_rule(rule_id.into(), value.1);
    item.sym_index = sym_index as u16;
    item
  }
}

impl<'db> Hash for Item<'db> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    (
      self.rule_id,
      self.origin,
      self.origin_state,
      self.lane,
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
    let a = (self.rule_id, self.origin, self.lane, self.len, self.sym_index, self.origin_state);
    let b = (other.rule_id, other.origin, other.lane, other.len, other.sym_index, other.origin_state);
    a == b
  }
}

impl<'a> Eq for Item<'a> {}

impl<'a> PartialOrd for Item<'a> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a = (self.origin, self.rule_id, self.lane, self.len, self.sym_index, self.origin_state);
    let b = (other.origin, other.rule_id, other.lane, other.len, other.sym_index, other.origin_state);
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
  /// with the meta info of `other`. Resets goto metadata
  pub fn align<'a>(&self, other: &'a Item<'db>) -> Self {
    Self {
      rule_id: self.rule_id,
      len: self.len,
      sym_index: self.sym_index,
      goto_distance: 0,
      from_goto_origin: false,
      ..other.clone()
    }
  }

  pub fn index(&self) -> ItemIndex {
    (*self).into()
  }

  pub fn get_db(&self) -> &ParserDatabase {
    self.db
  }

  pub fn to_origin(&self, origin: Origin) -> Self {
    Self { origin, ..self.clone() }
  }

  pub fn to_lane(&self, lane: ItemLane) -> Self {
    Self { lane, ..self.clone() }
  }

  pub fn to_oos_lane(&self) -> Self {
    Self { lane: ItemLane::oos(), ..self.clone() }
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
      lane: Default::default(),
      goto_distance: 0,
      from_goto_origin: false,
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
      lane: Default::default(),
      goto_distance: 0,
      from_goto_origin: false,
    }
  }

  /// Returns the reduce type of the item.
  pub fn reduction_type(&self) -> ReductionType {
    self.db.get_reduce_type(self.rule_id)
  }

  pub fn to_absolute(&self) -> Self {
    Self { lane: Default::default(), origin: Default::default(), ..self.clone() }
  }

  pub fn is_canonically_equal(&self, other: &Self) -> bool {
    self.sym_index == other.sym_index && self.rule_id == other.rule_id
  }

  pub fn is_canonical(&self) -> bool {
    *self == self.to_canonical()
  }

  /// Returns the canonical item, that is an item that does not have any other
  /// attributes aside from its rule id and symbol offset
  pub fn to_canonical(&self) -> Self {
    Self {
      lane: Default::default(),
      origin: Default::default(),
      origin_state: StateId::default(),
      goto_distance: 0,
      ..self.clone()
    }
  }

  /// Calculates the number of GOTO states that would be pushed to the stack
  /// following the state this item originated from.
  pub(crate) fn increment_goto(&self) -> Self {
    return Self { goto_distance: self.goto_distance + 1, ..self.clone() };
  }

  pub fn increment(&self) -> Option<Self> {
    if !self.is_complete() {
      Some(Self { len: self.len, sym_index: self.sym_index + 1, ..self.clone() })
    } else {
      None
    }
  }

  pub fn increment_with_lane_remap(&self) -> Option<Self> {
    if !self.is_complete() {
      let mut s = Self { len: self.len, sym_index: self.sym_index + 1, ..self.clone() };
      s.lane = ItemLane { curr: s.index().into(), prev: self.lane.prev };
      Some(s)
    } else {
      None
    }
  }

  /// Increments the Item if it is not in the completed position,
  /// otherwise returns the Item as is.
  pub fn try_increment(&self) -> Self {
    if !self.is_complete() {
      let new_item = self.increment().unwrap();
      new_item.to_lane(ItemLane { prev: self.lane.prev, curr: new_item.index().into() })
    } else {
      self.clone()
    }
  }

  pub fn is_start(&self) -> bool {
    self.sym_index == 0
  }

  pub fn is_recursive(&self) -> bool {
    match self.db.nonterm_recursion_type(self.nonterm_index()) {
      RecursionType::None => false,
      _ => true,
    }
  }

  pub fn decrement(&self) -> Option<Self> {
    if !self.is_start() {
      Some(Self { len: self.len, sym_index: self.sym_index - 1, ..self.clone() })
    } else {
      None
    }
  }

  pub fn split_lane(&self, new_lane: ItemIndex) -> Self {
    self.to_lane(self.lane.split(new_lane))
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.lane == ItemLane::oos() || self.origin.is_out_of_scope()
  }

  pub fn to_goto_origin(&self) -> Self {
    Self { from_goto_origin: true, ..self.clone() }
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
  pub fn is_left_recursive(&self, mode: GraphType) -> bool {
    let p = self.nonterm_index();
    p == self.to_start().nonterm_index_at_sym(mode).unwrap_or_default()
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

  pub fn is_nonterm(&self, mode: GraphType) -> bool {
    if self.is_complete() {
      false
    } else {
      match self.rule().symbols[self.sym_index as usize].id {
        SymbolId::DBNonTerminalToken { .. } if mode == GraphType::Scanner => true,
        SymbolId::DBNonTerminal { .. } => true,
        _ => false,
      }
    }
  }

  /// Returns the [IndexedProdId] of the active symbol if the symbol
  /// is a NonTerm, or return `None`
  pub fn nonterm_index_at_sym(&self, mode: GraphType) -> Option<DBNonTermKey> {
    if self.is_complete() {
      return None;
    }
    match self.rule().symbols[self.sym_index as usize].id {
      SymbolId::DBNonTerminalToken { nonterm_key: index, .. } if mode == GraphType::Scanner => Some(index),
      SymbolId::DBNonTerminal { key: index } => Some(index),
      _ => None,
    }
  }

  pub fn term_index_at_sym(&self, mode: GraphType) -> Option<DBTermKey> {
    if self.is_complete() {
      return None;
    }
    match self.rule().symbols[self.sym_index as usize].id {
      SymbolId::DBNonTerminalToken { sym_key, .. } if mode == GraphType::Parser => sym_key,
      SymbolId::DBToken { key: index } => Some(index),
      _ => None,
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
  pub fn precedence(&self, mode: GraphType) -> u16 {
    match mode {
      GraphType::Parser => self.symbol_precedence(),
      GraphType::Scanner => self.token_precedence(),
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

  pub fn is_term(&self, mode: GraphType) -> bool {
    if self.is_complete() {
      false
    } else {
      !self.is_nonterm(mode)
    }
  }

  /// Returns an iterator over this item's closure.
  /// > note: The closure will be over canonical items, except fo the kernel
  /// > item `self`
  pub fn closure_iter<'a>(&self) -> impl ItemContainerIter<'db> {
    [*self].into_iter().chain(self.db.get_closure(self))
  }

  /// Same as `Item::closure_iter`, except takes an extran `Item` as an
  /// argument, from which the meta attributes will be assigned to the closure
  /// items (note: the kernel item `self` is left untouched.)
  pub fn closure_iter_align<'a>(&self, other: Self) -> impl ItemContainerIter<'db> {
    [*self].into_iter().chain(self.db.get_closure(self).map(move |i| i.align(&other)))
  }

  pub fn closure_iter_align_with_lane_split<'a>(&self, other: Self) -> impl ItemContainerIter<'db> {
    let index = self.lane.to_curr();
    [*self].into_iter().chain(self.db.get_closure(self).map(move |i| i.align(&other).to_lane(index).split_lane(i.index())))
  }

  #[cfg(debug_assertions)]
  pub fn _debug_print_(&self, label: &str) {
    if label.len() > 0 {
      println!("\n{label}: {}", self._debug_string_());
    } else {
      println!("\n{}", self._debug_string_());
    }
  }

  pub fn _debug_string_(&self) -> String {
    if self.is_null() {
      "null".to_string()
    } else {
      let s_store = self.db.string_store();

      #[cfg(debug_assertions)]
      let mut string = self
        .origin
        .is_none()
        .then_some(String::new())
        .unwrap_or_else(|| format!("<[{}-{:?}]  {:?} ", self.origin.debug_string(self.db), self.origin_state, self.lane));
      #[cfg(not(debug_assertions))]
      let mut string = String::new();

      string += &format!("{:?} ", self.index());

      if !self.is_canonical() {
        if self.from_goto_origin {
          string += &(" @".to_string() + &("[".to_string() + &self.goto_distance.to_string() + "] "));
        } else {
          string += &("[".to_string() + &self.goto_distance.to_string() + "] ");
        }
      }

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

        if !self.is_canonical() {
          string += &match (i.symbol_precedence(), i.token_precedence()) {
            (0, 0) => String::default(),
            (sym, 0) => "{".to_string() + &sym.to_string() + "}",
            (0, tok) => "{:".to_string() + &tok.to_string() + "}",
            (sym, tok) => "{".to_string() + &sym.to_string() + ":" + &tok.to_string() + "}",
          };
        }

        item = i.increment();
      }

      if self.is_complete() {
        string += " •";
      }

      string.replace("\n", "\\n")
    }
  }
}

/// Represents either a FIRST or a FOLLOW depending on whether the root item
/// is incomplete or not.
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TransitionPair<'db> {
  pub kernel: Item<'db>,
  pub next:   Item<'db>,
  pub sym:    SymbolId,
  pub prec:   u16,
}

pub type First<'db> = TransitionPair<'db>;
pub type Firsts<'db> = Array<First<'db>>;
pub type Lookahead<'db> = TransitionPair<'db>;
pub type Lookaheads<'db> = Array<Lookahead<'db>>;

impl<'db> TransitionPair<'db> {
  pub fn is_kernel_terminal(&self) -> bool {
    !self.is_complete() && self.kernel.is_canonically_equal(&self.next)
  }

  pub fn is_complete(&self) -> bool {
    self.kernel.is_complete() && self.kernel.is_canonically_equal(&self.next)
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.kernel.is_out_of_scope()
  }

  #[cfg(debug_assertions)]
  pub fn _debug_string_(&self) -> String {
    format!(
      "\nsym {}{{{}}} oos:{}\n  base: {}\n  next: {}",
      self.sym.debug_string(self.kernel.db),
      self.prec,
      self.is_out_of_scope(),
      self.kernel._debug_string_(),
      self.next._debug_string_()
    )
  }
}

impl<'db> From<(Item<'db>, Item<'db>, GraphType)> for TransitionPair<'db> {
  fn from((root, next, mode): (Item<'db>, Item<'db>, GraphType)) -> Self {
    Self {
      kernel: root,
      next,
      sym: if next.is_complete() { SymbolId::Default } else { next.sym() },
      prec: next.precedence(mode),
    }
  }
}

impl<'db> From<(&Item<'db>, &Item<'db>, GraphType)> for TransitionPair<'db> {
  fn from((root, next, mode): (&Item<'db>, &Item<'db>, GraphType)) -> Self {
    (*root, *next, mode).into()
  }
}

pub trait TransitionPairIter<'db>: Iterator<Item = TransitionPair<'db>> + Sized + Clone {
  fn to_next(self) -> impl ItemContainerIter<'db> {
    self.map(|i| i.next)
  }

  fn to_root(self) -> impl ItemContainerIter<'db> {
    self.map(|i| i.kernel)
  }
}

impl<'db, T: Iterator<Item = TransitionPair<'db>> + Sized + Clone> TransitionPairIter<'db> for T {}

pub trait TransitionPairRefIter<'a, 'db: 'a>: Iterator<Item = &'a TransitionPair<'db>> + Sized + Clone {
  fn to_next(self) -> impl ItemRefContainerIter<'a, 'db> {
    self.map(|i| &i.next)
  }

  fn to_kernel(self) -> impl ItemRefContainerIter<'a, 'db> {
    self.map(|i| &i.kernel)
  }

  fn max_precedence(self) -> u16 {
    self.map(|i| i.prec).max().unwrap_or_default()
  }

  fn in_scope(self) -> impl TransitionPairRefIter<'a, 'db> {
    self.filter(|i| !i.is_out_of_scope())
  }

  fn out_scope(self) -> impl TransitionPairRefIter<'a, 'db> {
    self.filter(|i| i.is_out_of_scope())
  }

  fn kernel_nonterm_sym(self, mode: GraphType) -> OrderedSet<Option<DBNonTermKey>> {
    self.map(|p| p.kernel.nonterm_index_at_sym(mode)).collect()
  }

  #[cfg(debug_assertions)]
  fn _debug_print_(self, message: &str) {
    println!("=====> {}\n{}\n=====<\n", message, self._debug_string_())
  }

  #[cfg(debug_assertions)]
  fn _debug_string_(self) -> String {
    format!("{}", self.map(|i| i._debug_string_()).collect::<Vec<_>>().join("\n"))
  }
}

impl<'a, 'db: 'a, T: Iterator<Item = &'a TransitionPair<'db>> + Sized + Clone> TransitionPairRefIter<'a, 'db> for T {}

pub type ItemSet<'db> = OrderedSet<Item<'db>>;
pub type Items<'db> = Array<Item<'db>>;

impl<'db> ItemContainer<'db> for ItemSet<'db> {}
impl<'db> ItemContainer<'db> for Items<'db> {}

impl<'a, 'db: 'a, T: Clone + Iterator<Item = &'a Item<'db>>> ItemRefContainerIter<'a, 'db> for T {}
impl<'db, T: Clone + Iterator<Item = Item<'db>>> ItemContainerIter<'db> for T {}

macro_rules! common_iter_functions {
  () => {
    fn get_max_precedence(self, mode: GraphType) -> u16 {
      match mode {
        GraphType::Parser => self.get_max_symbol_precedence(),
        GraphType::Scanner => self.get_max_token_precedence(),
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
    fn _debug_print_(self, _comment: &str) {
      println!("------>{} \n {}", _comment, self.to_debug_string("\n\n"));
    }

    #[cfg(debug_assertions)]
    fn to_debug_string(self, sep: &str) -> String {
      self.map(|i| i._debug_string_()).collect::<Vec<_>>().join(sep)
    }

    /// Returns the [DBNonTermKey] of the symbol in non-terminal items. Items that
    /// do not have a nonterm as the active symbol are skipped.
    fn nonterm_ids_at_index(self, mode: GraphType) -> OrderedSet<DBNonTermKey> {
      self.filter_map(move |i| i.nonterm_index_at_sym(mode)).collect()
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

    fn items_are_the_same_rule(self) -> bool {
      let mut base_rule: Option<_> = None;
      for next in self {
        if *(base_rule.get_or_insert(next.rule_id)) != next.rule_id {
          return false;
        }
      }
      return true;
    }

    fn nonterm_items<T: ItemContainer<'db>>(self, mode: GraphType) -> T {
      self.filter_map(|i| i.is_nonterm(mode).then(|| i.clone())).collect()
    }

    fn term_items<T: ItemContainer<'db>>(self, mode: GraphType) -> T {
      self.filter_map(|i| i.is_term(mode).then(|| i.clone())).collect()
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

    fn terminals(self, mode: GraphType) -> OrderedSet<SymbolId> {
      self.filter_map(|i| (!i.is_nonterm(mode)).then_some(i.sym())).collect()
    }

    fn _closure<T: ItemContainer<'db>>(self, state_id: StateId) -> T {
      let mut closure = ItemSet::from_iter(self.clone().map(|i| i.clone()));
      closure.extend(self.filter(|i| !i.is_complete()).flat_map(|i| {
        let a = i.to_owned().to_canonical();
        let b = i.to_owned();
        i.db.get_closure(&i).filter(move |i| i.to_canonical() != a).map(move |a| a.align(&b).to_origin_state(state_id))
      }));
      closure.into_iter().collect()
    }
  };
}

pub trait ItemContainerIter<'db>: Iterator<Item = Item<'db>> + Sized + Clone {
  fn to_set(self) -> ItemSet<'db> {
    self.collect()
  }

  fn to_vec(self) -> Items<'db> {
    self.collect()
  }

  fn to_origin_state_iter(self, state_id: StateId) -> impl ItemContainerIter<'db> {
    self.map(move |i| i.to_origin_state(state_id))
  }

  fn align_iter(self, a: &'db Item<'db>) -> impl ItemContainerIter<'db> {
    self.map(|i| i.align(a))
  }

  fn term_items_iter(self, is_scanner: bool) -> impl ItemContainerIter<'db> {
    self.filter(move |i| match i.get_type() {
      ItemType::Completed(_) | ItemType::Terminal(_) => true,
      ItemType::TokenNonTerminal(..) if !is_scanner => true,
      _ => false,
    })
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

pub trait ItemContainer<'db>: Clone + IntoIterator<Item = Item<'db>> + FromIterator<Item<'db>> {
  /// Given a [CompileDatabase] and [DBProdId] returns the initial
  /// items of the non-terminal.
  fn start_items(nterm: DBNonTermKey, db: &'db ParserDatabase) -> Self {
    let Ok(rules) = db.nonterm_rules(nterm) else { panic!("Could not get rules") };
    rules.iter().map(|r| Item::from_rule(*r, db)).collect()
  }

  fn nonterm_items(self, mode: GraphType) -> Self {
    self.into_iter().filter(|i| i.is_nonterm(mode)).collect()
  }

  fn term_items(self, mode: GraphType) -> Self {
    self.into_iter().filter(|i| i.is_term(mode)).collect()
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
  fn _debug_print_(&self, _comment: &str) {
    debug_items(_comment, self.clone());
  }

  #[cfg(debug_assertions)]
  fn to_debug_string(&self, sep: &str) -> String {
    self.clone().to_vec().iter().map(|i| i._debug_string_()).collect::<Vec<_>>().join(sep)
  }

  fn to_set(self) -> ItemSet<'db> {
    self.into_iter().collect()
  }

  fn to_vec(self) -> Items<'db> {
    self.into_iter().collect()
  }
}

#[allow(unused)]
#[cfg(debug_assertions)]
fn debug_items<'db, T: IntoIterator<Item = Item<'db>>>(comment: &str, items: T) {
  println!("\n {} --> ", comment);

  for item in items {
    println!("    {}", item._debug_string_());
  }
}

pub struct CompletedItemArtifacts<'db> {
  pub lookahead_pairs: OrderedSet<TransitionPair<'db>>,
  /// Items that completed a nonterminal that did not lead to a transition
  /// in the root closure.
  pub default_only:    ItemSet<'db>,
}
