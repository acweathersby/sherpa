use radlr_core_common::{GuardedStr, StackVec};

use super::{
  super::types::*,
  parser_db::{ParserDatabase, RecursionType, ReductionType},
  rule::Rule,
  symbol::PrecedentDBTerm,
};
use std::{collections::BTreeSet, hash::Hash};

pub enum ItemType {
  Terminal(SymbolId),
  NonTerminal(DBNonTermKey),
  TokenNonTerminal(DBNonTermKey, SymbolId),
  Completed(DBNonTermKey),
}

/// Indicates the State type that generated
/// the item
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[allow(non_camel_case_types)]
pub enum Origin {
  None,
  /// The goal non-terminal that this item or it's predecessors will reduce to
  NonTermGoal(DBNonTermKey),
  /// The goal symbol id that this item or its predecessors will recognize
  TerminalGoal(DBTermKey, u16),
  /// The hash and state of the goal items set the peek item will resolve to
  Peek(u32),
  Fork(DBRuleKey),
  Closure,
  Goto,
  __OOS_CLOSURE__,
  __OOS_ROOT__,
  __OOS_SCANNER_ROOT__(PrecedentDBTerm),
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
      Origin::Fork(resolve_id) => {
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
  pub fn _debug_string_(&self) -> String {
    match self {
      Origin::NonTermGoal(nterm) => {
        format!("NonTermGoal[ {:?} ]", nterm)
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
    matches!(self, Origin::GoalCompleteOOS | Origin::__OOS_CLOSURE__ | Origin::__OOS_ROOT__ | Origin::__OOS_SCANNER_ROOT__(..))
  }

  pub fn is_scanner_oos(&self) -> bool {
    matches!(self, Origin::__OOS_SCANNER_ROOT__(..))
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

  pub fn is_oos(&self) -> bool {
    match self {
      Self::__OOS_ROOT__ | Self::__OOS_CLOSURE__ | Self::__OOS_SCANNER_ROOT__(..) => true,
      _ => false,
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ItemHeritage {
  index: ItemIndex,
  from:  ItemIndex,
}

impl From<Item> for ItemHeritage {
  fn from(value: Item) -> Self {
    Self { from: value.from, index: value.index }
  }
}

impl From<&Item> for ItemHeritage {
  fn from(value: &Item) -> Self {
    Self { from: value.from, index: value.index }
  }
}

/// Stores an item in a compact form.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemIndex(u32);

impl ItemIndex {
  const RULE_SHIFT: u32 = 12;

  /// Returns a tuple comprised of the item's rule and symbol offset
  pub fn get_parts(&self) -> (DBRuleKey, u16) {
    (((self.0 >> ItemIndex::RULE_SHIFT) as usize).into(), (self.0 & 0x3FF) as u16)
  }

  pub fn oos() -> Self {
    Self(u32::MAX)
  }

  pub fn is_oos(self) -> bool {
    self.0 == u32::MAX
  }

  pub fn _debug_string_(&self) -> String {
    if self.is_oos() {
      "OOS".to_string()
    } else {
      let (r, off) = self.get_parts();
      "r".to_string() + &r.to_string() + "•" + &off.to_string()
    }
  }
}

impl From<(u32, u32)> for ItemIndex {
  #[inline(always)]
  fn from((rule_id, sym_id): (u32, u32)) -> Self {
    debug_assert!(rule_id < (1 << (32 - ItemIndex::RULE_SHIFT)), "Rule index is too high to store in this form");
    debug_assert!(sym_id < (1 << ItemIndex::RULE_SHIFT), "Symbol index is too high to store in this form");
    Self((rule_id << ItemIndex::RULE_SHIFT) | ((sym_id & 0b1111_1111_1111) as u32))
  }
}

impl From<(DBRuleKey, u16)> for ItemIndex {
  #[inline(always)]
  fn from((rule_id, sym_id): (DBRuleKey, u16)) -> Self {
    let val: (u32, u32) = (rule_id.into(), sym_id as u32);
    val.into()
  }
}

impl From<Item> for ItemIndex {
  #[inline(always)]
  fn from(i: Item) -> Self {
    i.index
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

impl std::fmt::Debug for ItemIndex {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self._debug_string_())
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

#[derive(Clone, Copy)]
#[repr(align(32))]
pub struct Item {
  /// The form of the state the item's initial position originates from.
  pub origin:           Origin,
  /// The graph state the item originated from
  pub origin_state:     StateId,
  pub index:            ItemIndex,
  pub from:             ItemIndex,
  pub len:              u16,
  pub goto_distance:    u8,
  pub from_goto_origin: bool,
}

impl Hash for Item {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    (self.index, self.from, self.origin, self.origin_state).hash(state)
  }
}

impl PartialEq for Item {
  fn eq(&self, other: &Self) -> bool {
    let a = (self.index, self.origin, self.from, self.origin_state);
    let b = (other.index, other.origin, other.from, other.origin_state);
    a == b
  }
}

impl Eq for Item {}

impl PartialOrd for Item {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a = (self.origin, self.index, self.from, self.origin_state);
    let b = (other.origin, other.index, other.from, other.origin_state);
    Some(a.cmp(&b))
  }
}

impl Ord for Item {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.partial_cmp(other).unwrap()
  }
}

impl From<(ItemIndex, &[Rule])> for Item {
  /// Creates an item from an [ItemIndex] and its corresponding [ParserDatabase]
  fn from((index, rules): (ItemIndex, &[Rule])) -> Self {
    let mut item = Self {
      origin: Default::default(),
      origin_state: StateId::default(),
      index,
      from: index,
      len: 0,
      goto_distance: 0,
      from_goto_origin: false,
    };
    item.len = item.rule(rules).symbols.len() as u16;
    item
  }
}

impl From<(DBRuleKey, &[Rule])> for Item {
  /// Creates an initial item from a [DBRuleKey] and its [ParserDatabase]
  fn from((rule_id, rules): (DBRuleKey, &[Rule])) -> Self {
    let index: ItemIndex = (rule_id, 0).into();
    (index, rules).into()
  }
}

impl std::fmt::Debug for Item {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut s = f.debug_struct("ItemRef");
    s.field("val", &self._debug_string_());
    s.finish()
  }
}

#[allow(unused)]
impl Item {
  /// If true, `self` is an item that was derived from `other`, or
  /// the `from` ancestor of `other`.
  pub fn is_successor_of(&self, other: &Self) -> bool {
    // This item follows the other item
    self.from == other.index ||
    // This item comes from the same closure as the other item
    self.from == other.from
  }

  /// True if this item is the successor of another item, that is
  /// this item originate from the closure of another item.
  pub fn is_successor(&self) -> bool {
    self.index != self.from
  }

  #[inline]
  pub fn is_initial(&self) -> bool {
    self.sym_index() == 0
  }

  #[inline]
  /// Returns true if the item proceeds the final position: `A = X ... • S `
  pub fn is_penultimate(&self) -> bool {
    self.sym_index() == (self.len - 1)
  }

  #[inline]

  /// Returns true if the item is in the final position: `A = ... S • `
  pub fn is_complete(&self) -> bool {
    self.sym_index() >= self.sym_len()
  }

  #[inline]
  /// Returns true if the item is not in the final position: `A = • ... S `
  pub fn is_incomplete(&self) -> bool {
    !self.is_complete()
  }

  #[inline]
  pub fn is_null(&self) -> bool {
    self.len == 0
  }

  /// Return `true` if the item is from a left recursive rule.
  pub fn rule_is_left_recursive(&self, mode: GraphType, rules: &[Rule], nonterms: &[NonTermId]) -> bool {
    let p = rules[self.index.get_parts().0 .0 as usize].nonterm_id;
    p == self.to_initial().nonterm_index_at_sym(mode, rules).unwrap_or_default()
  }

  #[inline]
  /// Returns the [ItemIndex] of this item.
  pub fn index(&self) -> ItemIndex {
    self.index
  }

  #[inline]
  /// Returns the reduce type of the item.
  pub fn reduction_type(&self, db: &ParserDatabase) -> ReductionType {
    db.get_reduce_type(self.rule_id())
  }

  #[inline]
  /// Returns the [Rule] this item was derived from.
  pub fn db_rule<'db>(&self, rules: &'db [Rule]) -> &'db Rule {
    &rules[self.index.get_parts().0 .0 as usize]
  }

  #[inline]
  pub fn rule<'db>(&self, rules: &'db [Rule]) -> &'db Rule {
    &self.db_rule(rules)
  }

  #[inline]
  /// Returns the [DBRuleKey] this item was derived from.
  pub fn rule_id(&self) -> DBRuleKey {
    self.index.get_parts().0
  }

  #[inline]
  pub fn sym_index(&self) -> u16 {
    self.index.get_parts().1
  }

  #[inline]
  pub fn is_oos(&self) -> bool {
    self.origin.is_oos()
  }

  /// Returns true if the rule of this item has some form of direct
  /// or indirect recursion.
  #[inline]
  pub fn rule_is_recursive(&self, db: &ParserDatabase) -> bool {
    match db.nonterm_recursion_type(self.nonterm_index(db.rules())) {
      RecursionType::None => false,
      _ => true,
    }
  }

  /// Returns the type of item based on the active symbol.
  pub fn get_type(&self, rules: &[Rule]) -> ItemType {
    use ItemType::*;
    if self.is_complete() {
      Completed(self.nonterm_index(rules))
    } else {
      match self.sym_id(rules) {
        SymbolId::DBNonTerminal { key: index } => NonTerminal(index),
        sym_id @ SymbolId::DBNonTerminalToken { nonterm_key: index, .. } => TokenNonTerminal(index, sym_id),
        sym => Terminal(sym),
      }
    }
  }

  #[inline]
  /// Returns a list of terminal symbols that are skipped at this item's
  /// position.
  pub fn get_skipped<'db>(&self, rules: &[Rule]) -> Option<&'db Vec<SymbolId>> {
    if self.is_complete() {
      None
    } else {
      let rule = self.rule(rules);
      todo!("Non Term rule");
      //Some(&rule.skipped)
    }
  }

  #[inline]
  /// Returns the number of symbols within the item. This is the same as the
  /// number of symbols in the item's rule.
  pub fn sym_len(&self) -> u16 {
    self.len
  }

  #[inline]
  /// Returns the symbols at the current position of the item, or None if the
  /// item is in the final position
  pub fn sym_id(&self, rules: &[Rule]) -> SymbolId {
    if self.is_complete() {
      SymbolId::Default
    } else {
      self.rule(rules).symbols[self.sym_index() as usize].id
    }
  }

  #[inline]
  pub fn sym<'db>(&self, rules: &'db [Rule]) -> Option<&'db SymbolRef> {
    if self.is_complete() {
      None
    } else {
      Some(&self.rule(rules).symbols[self.sym_index() as usize])
    }
  }

  /// Get the precedence appropriate for the graph mode
  pub fn precedence(&self, mode: GraphType, rules: &[Rule]) -> u16 {
    match mode {
      GraphType::Parser => self.symbol_precedence(rules),
      GraphType::Scanner => self.token_precedence(rules),
    }
  }

  /// The precedence of this items origin if this item has a
  /// [Origin::TerminalGoal] origin type.
  pub fn origin_precedence(&self) -> u16 {
    match self.origin {
      Origin::TerminalGoal(_, prec) => prec,
      _ => 0,
    }
  }

  pub fn token_precedence(&self, rules: &[Rule]) -> u16 {
    if self.is_null() {
      0
    } else {
      match self.sym(rules) {
        Some(sym) => sym.tok_prec,
        None => unsafe { self.decrement().unwrap_unchecked().token_precedence(rules) },
      }
    }
  }

  pub fn symbol_precedence(&self, rules: &[Rule]) -> u16 {
    if self.is_null() {
      0
    } else {
      match self.sym(rules) {
        Some(sym) => sym.sym_prec,
        None => unsafe { self.decrement().unwrap_unchecked().token_precedence(rules) },
      }
    }
  }

  /// Returns `true` if the symbol at the items position is a non-terminal,
  /// otherwise returns `false`
  pub fn is_nonterm(&self, mode: GraphType, rules: &[Rule]) -> bool {
    match self.sym_id(rules) {
      SymbolId::DBNonTerminalToken { .. } if mode == GraphType::Scanner => true,
      SymbolId::DBNonTerminal { .. } => true,
      _ => false,
    }
  }

  /// Returns `true` if the symbol at the items position is a terminal,
  /// otherwise returns `false`
  pub fn is_term(&self, mode: GraphType, rules: &[Rule]) -> bool {
    if self.is_complete() {
      false
    } else {
      !self.is_nonterm(mode, rules)
    }
  }

  pub fn nonterm_index_at_sym_parser(&self, rules: &[Rule]) -> Option<DBNonTermKey> {
    self.nonterm_index_at_sym(GraphType::Parser, rules)
  }

  /// Returns the [IndexedProdId] of the active symbol if the symbol
  /// is a NonTerm, or return `None`
  pub fn nonterm_index_at_sym(&self, mode: GraphType, rules: &[Rule]) -> Option<DBNonTermKey> {
    match self.sym_id(rules) {
      SymbolId::DBNonTerminalToken { nonterm_key: index, .. } if mode == GraphType::Scanner => Some(index),
      SymbolId::DBNonTerminal { key: index } => Some(index),
      _ => None,
    }
  }

  pub fn term_index_at_sym(&self, mode: GraphType, rules: &[Rule]) -> Option<DBTermKey> {
    match self.sym_id(rules) {
      SymbolId::DBNonTerminalToken { sym_key, .. } if mode == GraphType::Parser => sym_key,
      SymbolId::DBToken { key: index } => Some(index),
      _ => None,
    }
  }

  pub fn precedent_db_key_at_sym(&self, mode: GraphType, rules: &[Rule]) -> Option<PrecedentDBTerm> {
    match self.sym_id(rules) {
      SymbolId::DBNonTerminalToken { sym_key, .. } if mode == GraphType::Parser => {
        sym_key.map(|sym_key| (sym_key, self.token_precedence(rules), false).into())
      }
      SymbolId::DBToken { key: index } => Some((index, self.token_precedence(rules), false).into()),
      _ => None,
    }
  }

  #[inline]
  /// The non-terminal the rule reduces to
  pub fn nonterm_index(&self, rules: &[Rule]) -> DBNonTermKey {
    self.db_rule(rules).nonterm_id
  }

  // --------------- ITEM TRANSFORMATION METHODS
  // -------------------------------------------------

  /// Increments the item if it is not in the final position, otherwise returns
  /// None
  pub fn increment(&self) -> Option<Self> {
    if !self.is_complete() {
      let (rule_id, position) = self.index.get_parts();

      let index: ItemIndex = (rule_id, position + 1).into();

      let new_item = Self { index, ..self.clone() };

      Some(new_item)
    } else {
      None
    }
  }

  /// Decrements the item if it is not in the initial position, otherwise
  /// returns None
  pub fn decrement(&self) -> Option<Self> {
    if self.sym_index() != 0 {
      let (rule_id, position) = self.index.get_parts();
      let index: ItemIndex = (rule_id, position - 1).into();
      Some(Self { index, ..self.clone() })
    } else {
      None
    }
  }

  #[inline]
  /// Increments a clone of `self` if `self` is not in the final position,
  /// otherwise returns a clone of `self`, unmodified.
  pub fn try_increment(&self) -> Self {
    match self.increment() {
      Some(incremented) => incremented,
      _ => *self,
    }
  }

  #[inline]
  pub fn as_goto_origin(&self) -> Self {
    Self { from_goto_origin: true, goto_distance: 1, ..self.clone() }
  }

  #[inline]
  pub fn to_origin(&self, origin: Origin) -> Self {
    Self { origin, ..self.clone() }
  }

  #[inline]
  pub fn as_from(&self, other: Self) -> Self {
    Self { from: other.index, ..self.clone() }
  }

  #[inline]
  pub fn as_from_index(&self, from: ItemIndex) -> Self {
    Self { from, ..self.clone() }
  }

  #[inline]
  pub fn to_extend_from(&self) -> Self {
    Self { from: ItemIndex::oos(), ..self.clone() }
  }

  #[inline]
  pub fn to_origin_state(&self, origin_state: StateId) -> Self {
    Self { origin_state, ..self.clone() }
  }

  #[inline]
  pub fn to_initial(&self) -> Self {
    let (rule_id, position) = self.index.get_parts();
    let index: ItemIndex = (rule_id, 0).into();
    Self { index, ..self.clone() }
  }

  #[inline]
  pub fn to_complete(&self) -> Self {
    let (rule_id, position) = self.index.get_parts();
    let index: ItemIndex = (rule_id, self.sym_len()).into();
    Self { index, ..self.clone() }
  }

  #[inline]
  pub fn to_penultimate(&self) -> Self {
    let (rule_id, position) = self.index.get_parts();
    let index: ItemIndex = (rule_id, (self.sym_len() as i32 - 1).max(0) as u16).into();
    Self { index, ..self.clone() }
  }

  #[inline]
  /// Creates a new [ItemRef] with the same rule info as the original, but
  /// with the meta info of `other`. Resets goto metadata
  pub fn align(&self, other: &Item) -> Self {
    Self { index: self.index, len: self.len, ..other.clone() }
  }

  #[inline]
  pub fn is_canonical(&self) -> bool {
    *self == self.to_canonical()
  }

  #[inline]
  pub fn to_canonical(&self) -> Self {
    Self {
      origin: Default::default(),
      origin_state: StateId::default(),
      goto_distance: 0,
      from_goto_origin: false,
      ..self.clone()
    }
  }

  /// Increment the goto counter, which tracks the number of GOTO states that
  /// proceed the state this item originates from.
  pub(crate) fn increment_goto(&self) -> Self {
    return Self { goto_distance: self.goto_distance + 1, ..self.clone() };
  }

  /// Returns an iterator over this item's closure.
  /// > note: The closure will be over canonical items, except fo the kernel
  /// > item `self`
  pub fn closure_iter<'db>(&self, db: &'db ParserDatabase) -> impl ItemContainerIter + 'db {
    [*self].into_iter().chain(db.get_closure(self))
  }

  /// Same as `Item::closure_iter`, except takes an extra `Item` as an
  /// argument, from which the meta attributes will be assigned to the closure
  /// items (note: the kernel item `self` is left untouched.)
  pub fn closure_iter_align<'db>(&self, other: Self, db: &'db ParserDatabase) -> impl ItemContainerIter + 'db {
    [*self].into_iter().chain(db.get_closure(self).map(move |i| i.align(&other)))
  }

  pub fn closure_iter_align_with_lane_split<'db>(&self, other: Self, db: &'db ParserDatabase) -> impl ItemContainerIter + 'db {
    let from = self.index;
    [*self].into_iter().chain(db.get_closure(self).map(move |i| i.align(&other).as_from_index(from)))
  }

  // --------------- DEBUGGING ORIENTED METHODS
  // --------------------------------------------------

  #[inline]
  pub fn nonterm_name(&self, db: &ParserDatabase) -> IString {
    db.nonterm_friendly_name(self.nonterm_index(db.rules()))
  }

  #[inline]
  pub fn nonterm_name_str<'db>(&self, db: &'db ParserDatabase) -> GuardedStr<'db> {
    let name = db.nonterm_friendly_name(self.nonterm_index(db.rules()));
    name.to_str()
  }

  #[cfg(debug_assertions)]
  pub fn _debug_print_(&self, db: &ParserDatabase) {
    eprintln!("{}", self._debug_string_w_db_(db))
  }

  pub fn _debug_string_(&self) -> String {
    if self.is_null() {
      "null".to_string()
    } else {
      #[cfg(debug_assertions)]
      let mut string = self
        .origin
        .is_none()
        .then_some(String::new())
        .unwrap_or_else(|| format!("<[{}-{:?}]  ", self.origin._debug_string_(), self.origin_state));
      #[cfg(not(debug_assertions))]
      let mut string = String::new();

      string += &("( ".to_string() + &self.index._debug_string_() + " ");

      if self.index != self.from {
        string += &("from ".to_string() + &self.from._debug_string_() + " ) ");
      } else {
        string += ") ";
      }

      if !self.is_canonical() {
        if self.from_goto_origin {
          string += &(" @".to_string() + &("[".to_string() + &self.goto_distance.to_string() + "] "));
        } else {
          string += &("[".to_string() + &self.goto_distance.to_string() + "] ");
        }
      }

      string.replace("\n", "\\n")
    }
  }

  pub fn _debug_string_w_db_(&self, db: &ParserDatabase) -> String {
    if self.is_null() {
      "null".to_string()
    } else {
      let mut string = self._debug_string_();

      string += &self.nonterm_name(db).to_string();

      string += " >";

      let mut item = Some(self.to_initial());

      while let Some(i) = item.clone() {
        if i.is_complete() {
          break;
        };

        if i.sym_index() == self.sym_index() {
          string += " •";
        }

        string += " ";

        string += &i.sym_id(db.rules()).debug_string(db);

        if !self.is_canonical() {
          string += &match (i.symbol_precedence(db.rules()), i.token_precedence(db.rules())) {
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TransitionPair {
  pub kernel:       Item,
  pub next:         Item,
  pub sym:          SymbolId,
  pub allow_assign: bool,
  pub prec:         u16,
}

pub type Lookahead = TransitionPair;
pub type Lookaheads = Vec<Lookahead>;

impl TransitionPair {
  pub fn is_kernel_terminal(&self) -> bool {
    !self.is_eoi_complete() && self.kernel.index == self.next.index
  }

  /// True if the kernel item is the same as the next item, and both are
  /// complete. Indicates a situation where the item has reached some goal
  /// state and the FOLLOW set is empty.
  pub fn is_eoi_complete(&self) -> bool {
    self.kernel.is_complete() && self.kernel.index == self.next.index
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.kernel.is_oos()
  }

  #[cfg(debug_assertions)]
  pub fn _debug_string_(&self, db: &ParserDatabase) -> String {
    format!(
      "\nsym {}{{{}}} oos:{}\n  base: {}\n  next: {}",
      self.sym.debug_string(db),
      self.prec,
      self.is_out_of_scope(),
      self.kernel._debug_string_w_db_(db),
      self.next._debug_string_w_db_(db)
    )
  }
}

impl From<(Item, Item, GraphType, &[Rule])> for TransitionPair {
  fn from((root, next, mode, rules): (Item, Item, GraphType, &[Rule])) -> Self {
    Self {
      kernel: root,
      next,
      allow_assign: true,
      sym: if next.is_complete() { SymbolId::Default } else { next.sym_id(rules) },
      prec: next.precedence(mode, rules),
    }
  }
}

impl From<(&Item, &Item, GraphType, &[Rule])> for TransitionPair {
  fn from((root, next, mode, rules): (&Item, &Item, GraphType, &[Rule])) -> Self {
    (*root, *next, mode, rules).into()
  }
}

pub trait TransitionPairIter: Iterator<Item = TransitionPair> + Sized + Clone {
  fn to_next(self) -> StackVec<32, Item> {
    self.map(|i| i.next).into()
  }

  fn to_root(self) -> StackVec<32, Item> {
    self.map(|i| i.kernel).into()
  }
}

impl<'db, T: Iterator<Item = TransitionPair> + Sized + Clone> TransitionPairIter for T {}

pub trait TransitionPairRefIter<'a>: Iterator<Item = &'a TransitionPair> + Sized + Clone {
  fn to_next(self) -> StackVec<32, &'a Item> {
    self.map(|i| &i.next).into()
  }

  fn to_kernel(self) -> StackVec<32, &'a Item> {
    self.map(|i| &i.kernel).into()
  }

  fn max_precedence(self) -> u16 {
    self.map(|i| i.prec).max().unwrap_or_default()
  }

  fn in_scope(self) -> StackVec<32, &'a TransitionPair> {
    self.filter(|i| !i.is_out_of_scope()).into()
  }

  fn out_scope(self) -> StackVec<32, &'a TransitionPair> {
    self.filter(|i| i.is_out_of_scope()).into()
  }

  fn kernel_nonterm_sym(self, mode: GraphType, rules: &[Rule]) -> BTreeSet<Option<DBNonTermKey>> {
    self.map(|p| p.kernel.nonterm_index_at_sym(mode, rules)).collect()
  }

  #[cfg(debug_assertions)]
  fn _debug_print_(self, db: &ParserDatabase, message: &str) {
    eprintln!("=====> {}\n{}\n=====<\n", message, self._debug_string_(db))
  }

  #[cfg(debug_assertions)]
  fn _debug_string_(self, db: &ParserDatabase) -> String {
    format!("{}", self.map(|i| i._debug_string_(db)).collect::<Vec<_>>().join("\n"))
  }
}

impl<'a: 'a, T: Iterator<Item = &'a TransitionPair> + Sized + Clone> TransitionPairRefIter<'a> for T {}

pub type ItemSet = BTreeSet<Item>;
pub type Items = Vec<Item>;

impl ItemContainer for ItemSet {}
impl ItemContainer for Items {}

impl<'a: 'a, T: Clone + Iterator<Item = &'a Item>> ItemRefContainerIter<'a> for T {}
impl<'db, T: Clone + Iterator<Item = Item>> ItemContainerIter for T {}

macro_rules! common_iter_functions {
  () => {
    fn get_max_precedence(self, mode: GraphType, rules: &[Rule]) -> u16 {
      match mode {
        GraphType::Parser => self.get_max_symbol_precedence(rules),
        GraphType::Scanner => self.get_max_token_precedence(rules),
      }
    }

    fn get_max_origin_precedence(self) -> u16 {
      self.map(|i| i.origin_precedence()).max().unwrap_or_default()
    }

    fn get_max_token_precedence(self, rules: &[Rule]) -> u16 {
      self.map(|i| i.token_precedence(rules)).max().unwrap_or_default()
    }

    fn get_max_symbol_precedence(self, rules: &[Rule]) -> u16 {
      self.map(|i| i.symbol_precedence(rules)).max().unwrap_or_default()
    }

    fn intersects(&mut self, set: &ItemSet) -> bool {
      self.any(|i| set.contains(&i))
    }

    #[cfg(debug_assertions)]
    fn _debug_print_(self, _comment: &str) {
      eprintln!("------>{} \n {}", _comment, self.to_debug_string("\n\n"));
    }

    fn to_debug_string(self, sep: &str) -> String {
      self.map(|i| i._debug_string_()).collect::<Vec<_>>().join(sep)
    }

    /// Returns the [DBNonTermKey] of the symbol in non-terminal items. Items that
    /// do not have a nonterm as the active symbol are skipped.
    fn nonterm_ids_at_index(self, mode: GraphType, rules: &[Rule]) -> BTreeSet<DBNonTermKey> {
      self.filter_map(move |i| i.nonterm_index_at_sym(mode, rules)).collect()
    }

    /// Returns a set of all non-terminal ids the items reduce to.
    fn rule_nonterm_ids(&mut self, rules: &[Rule]) -> BTreeSet<DBNonTermKey> {
      self.map(|i| i.nonterm_index(rules)).collect()
    }

    fn peek_is_resolved(&mut self) -> bool {
      self.all_items_are_from_same_peek_origin()
    }

    fn follow_items_are_the_same(&mut self) -> bool {
      self.map(|i| i.index()).collect::<BTreeSet<_>>().len() == 1
    }

    fn contains_out_of_scope(&mut self) -> bool {
      self.any(|i| i.is_oos())
    }

    fn all_are_out_of_scope(&mut self) -> bool {
      self.all(|i| i.is_oos())
    }

    fn all_items_are_from_same_peek_origin(&mut self) -> bool {
      let origin_set = self.map(|i| i.origin).collect::<BTreeSet<_>>();
      match (origin_set.len(), origin_set.first()) {
        (1, Some(Origin::Peek(..))) => true,
        _ => false,
      }
    }

    fn items_are_the_same_rule(self) -> bool {
      let mut base_rule: Option<_> = None;
      for next in self {
        if *(base_rule.get_or_insert(next.rule_id())) != next.rule_id() {
          return false;
        }
      }
      return true;
    }

    fn nonterm_items<T: ItemContainer>(self, mode: GraphType, rules: &[Rule]) -> T {
      self.filter_map(|i| i.is_nonterm(mode, rules).then(|| i.clone())).collect()
    }

    fn term_items<T: ItemContainer>(self, mode: GraphType, rules: &[Rule]) -> T {
      self.filter_map(|i| i.is_term(mode, rules).then(|| i.clone())).collect()
    }

    fn null_items<T: ItemContainer>(self) -> T {
      self.filter_map(|i| i.is_null().then(|| i.clone())).collect()
    }

    fn incomplete_items<T: ItemContainer>(self) -> T {
      self.filter_map(|i| (!i.is_complete()).then(|| i.clone())).collect()
    }

    fn completed_items<T: ItemContainer>(self) -> T {
      self.filter_map(|i| (i.is_complete()).then(|| i.clone())).collect()
    }

    fn inscope_items<T: ItemContainer>(self) -> T {
      self.filter_map(|i| (!i.is_oos()).then(|| i.clone())).collect()
    }

    fn outscope_items<T: ItemContainer>(self) -> T {
      self.filter_map(|i| (!i.is_oos()).then(|| i.clone())).collect()
    }

    fn to_canonical<T: ItemContainer>(self) -> T {
      self.map(|i| i.to_canonical()).collect()
    }

    fn indices(self) -> BTreeSet<ItemIndex> {
      self.map(|i| i.index).collect()
    }

    fn heritage(self) -> BTreeSet<ItemHeritage> {
      self.map(|i| i.into()).collect()
    }

    fn to_origin<T: ItemContainer>(self, origin: Origin) -> T {
      self.map(|i| i.to_origin(origin)).collect()
    }

    #[inline(always)]
    fn try_increment(self) -> Items {
      self.map(|i| i.try_increment()).collect()
    }

    #[inline(always)]
    fn try_decrement(self) -> Items {
      self.map(|i| if i.sym_index() > 0 { i.decrement().unwrap() } else { i.clone() }).collect()
    }

    fn terminals(self, mode: GraphType, rules: &[Rule]) -> BTreeSet<SymbolId> {
      self.filter_map(|i| (!i.is_nonterm(mode, rules)).then_some(i.sym_id(rules))).collect()
    }
  };
}

pub trait ItemContainerIter: Iterator<Item = Item> + Sized + Clone {
  fn to_set(self) -> ItemSet {
    self.collect()
  }

  fn to_vec(self) -> Items {
    self.collect()
  }

  fn to_origin_state_iter(self, state_id: StateId) -> StackVec<32, Item> {
    self.map(move |i| i.to_origin_state(state_id)).into()
  }

  fn align_iter(self, a: &Item) -> StackVec<32, Item> {
    self.map(|i| i.align(a)).into()
  }

  fn term_items_iter(self, is_scanner: bool, rules: &[Rule]) -> StackVec<32, Item> {
    self
      .filter(move |i| match i.get_type(rules) {
        ItemType::Completed(_) | ItemType::Terminal(_) => true,
        ItemType::TokenNonTerminal(..) if !is_scanner => true,
        _ => false,
      })
      .into()
  }

  common_iter_functions!();
}

pub trait ItemRefContainerIter<'a: 'a>: Iterator<Item = &'a Item> + Sized + Clone {
  fn to_set(self) -> ItemSet {
    self.cloned().collect()
  }

  fn to_vec(self) -> Items {
    self.cloned().collect()
  }

  common_iter_functions!();
}

pub trait ItemContainer: Clone + IntoIterator<Item = Item> + FromIterator<Item> {
  /// Given a [CompileDatabase] and [DBProdId] returns the initial
  /// items of the non-terminal.
  fn start_items(nterm: DBNonTermKey, db: &ParserDatabase) -> Self {
    let Some(rules) = db.nonterm_rules(nterm) else { panic!("Could not get rules") };
    rules.iter().map(|r| Item::from((*r, db.as_ref()))).collect()
  }

  fn nonterm_items(self, mode: GraphType, rules: &[Rule]) -> Self {
    self.into_iter().filter(|i| i.is_nonterm(mode, rules)).collect()
  }

  fn term_items(self, mode: GraphType, rules: &[Rule]) -> Self {
    self.into_iter().filter(|i| i.is_term(mode, rules)).collect()
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
    self.into_iter().filter(|i| !i.is_oos()).collect()
  }

  fn outscope_items(self) -> Self {
    self.into_iter().filter(|i| i.is_oos()).collect()
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
  fn try_increment(&self) -> Items {
    self.clone().to_vec().into_iter().map(|i| i.try_increment()).collect()
  }

  #[inline(always)]
  fn try_decrement(&self) -> Items {
    self.clone().to_vec().into_iter().map(|i| if i.sym_index() > 0 { i.decrement().unwrap() } else { i }).collect()
  }

  fn _debug_print_(&self, db: &ParserDatabase, _comment: &str) {
    debug_items(_comment, self.clone(), db);
  }

  fn to_debug_string(&self, db: &ParserDatabase, sep: &str) -> String {
    self.clone().to_vec().iter().map(|i| i._debug_string_w_db_(db)).collect::<Vec<_>>().join(sep)
  }

  fn to_set(self) -> ItemSet {
    self.into_iter().collect()
  }

  fn to_vec(self) -> Items {
    self.into_iter().collect()
  }
}

#[allow(unused)]
fn debug_items<'db, T: IntoIterator<Item = Item>>(comment: &str, items: T, db: &ParserDatabase) {
  eprintln!("\n {} --> ", comment);

  for item in items {
    eprintln!("    {}", item._debug_string_w_db_(db));
  }
}

pub struct CompletedItemArtifacts {
  pub lookahead_pairs: BTreeSet<TransitionPair>,
  /// Items that completed a nonterminal that did not lead to a transition
  /// in the root closure.
  pub default_only:    ItemSet,
}
