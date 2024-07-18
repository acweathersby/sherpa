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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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

#[derive(Clone, Copy, Default)]
#[repr(align(8))]
pub struct Item {
  pub index:            ItemIndex,
  pub from:             ItemIndex,
  pub len:              u16,
  pub goto_distance:    u16,
  pub from_goto_origin: bool,
}

impl Hash for Item {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    (self.index, self.from).hash(state)
  }
}

impl PartialEq for Item {
  fn eq(&self, other: &Self) -> bool {
    let a = (self.index, self.from);
    let b = (other.index, other.from);
    a == b
  }
}

impl Eq for Item {}

impl PartialOrd for Item {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a = (self.index, self.from);
    let b = (other.index, other.from);
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
    let mut item = Self { index, from: index, len: 0, goto_distance: 0, from_goto_origin: false };
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

  /// Increment the goto counter, which tracks the number of GOTO states that
  /// proceed the state this item originates from.
  pub(crate) fn increment_goto(&self) -> Self {
    return Self { goto_distance: self.goto_distance + 1, ..self.clone() };
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
      /*       #[cfg(debug_assertions)]
      let mut string = self
        .origin
        .is_none()
        .then_some(String::new())
        .unwrap_or_else(|| format!("<[{}-{:?}]  ", self.origin._debug_string_(), self.origin_state)); */

      let mut string = String::new();

      string += &("( ".to_string() + &self.index._debug_string_() + " ");

      if self.index != self.from {
        string += &("from ".to_string() + &self.from._debug_string_() + " ) ");
      } else {
        string += ") ";
      }

      /*       if !self.is_canonical() {
        if self.from_goto_origin {
          string += &(" @".to_string() + &("[".to_string() + &self.goto_distance.to_string() + "] "));
        } else {
          string += &("[".to_string() + &self.goto_distance.to_string() + "] ");
        }
      } */

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

        /*       if !self.is_canonical() {
          string += &match (i.symbol_precedence(db.rules()), i.token_precedence(db.rules())) {
            (0, 0) => String::default(),
            (sym, 0) => "{".to_string() + &sym.to_string() + "}",
            (0, tok) => "{:".to_string() + &tok.to_string() + "}",
            (sym, tok) => "{".to_string() + &sym.to_string() + ":" + &tok.to_string() + "}",
          };
        } */

        item = i.increment();
      }

      if self.is_complete() {
        string += " •";
      }

      string.replace("\n", "\\n")
    }
  }
}
