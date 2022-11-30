use crate::{
  debug::debug_items,
  grammar::{get_closure_cached, get_closure_cached_with_state, get_production_start_items},
};

use super::*;
use std::{
  collections::{BTreeSet, VecDeque},
  fmt::Display,
};

use super::HCResult;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ItemState {
  curr_lane: u32,
  prev_lane: u32,

  /// An index to the original object that produced this item, be it a production, a symbol,
  /// or undefined
  origin: OriginData,
}

impl ItemState {
  pub const OUT_OF_SCOPE: ItemState = ItemState::new(0x8000, OriginData::OutOfScope);

  pub const fn default() -> Self {
    ItemState { curr_lane: 0, prev_lane: 0, origin: OriginData::Undefined }
  }

  /// Create a new [Item]
  pub const fn new(lane: u32, origin: OriginData) -> Self {
    ItemState { prev_lane: lane, curr_lane: lane, origin }
  }

  /// Get the current lane of the state
  pub fn get_lane(&self) -> u32 {
    self.curr_lane
  }

  /// Get the current and previous lane of the state
  pub fn get_lanes(&self) -> (u32, u32) {
    (self.curr_lane, self.prev_lane)
  }

  /// Get the group the item belongs to
  pub fn get_origin(&self) -> OriginData {
    self.origin
  }

  /// Get the SymbolId in the origin if the origin is
  /// of type `OriginData::Symbol`. Otherwise returns `SymbolId::Undefined`
  pub fn get_origin_sym(&self) -> SymbolID {
    match self.get_origin() {
      OriginData::Symbol(sym) => sym,
      _ => SymbolID::Undefined,
    }
  }

  pub fn to_new_lane(&self, new_lane: u32) -> Self {
    Self { curr_lane: new_lane, prev_lane: self.curr_lane, origin: self.origin }
  }

  pub fn in_same_lane(&self, other: &ItemState) -> bool {
    match (self.curr_lane, self.prev_lane, other.curr_lane) {
      (a, b, c) if a == c || b == c => true,
      (_, 0, _) | (0, ..) => true,
      _ => false,
    }
  }

  pub fn is_out_of_scope(&self) -> bool {
    matches!(self.origin, OriginData::OutOfScope)
  }

  /// Create a new Item with the given group
  pub fn to_lane(&self, lane: u32) -> Self {
    ItemState { curr_lane: lane, ..self.clone() }
  }

  /// Create a new Item with the given group
  pub fn to_lanes(&self, curr_lane: u32, prev_lane: u32) -> Self {
    ItemState { curr_lane, prev_lane, ..self.clone() }
  }

  /// Create a new Item with the given group
  pub fn to_origin(&self, origin: OriginData) -> Self {
    ItemState { origin, ..self.clone() }
  }

  pub fn to_out_of_scope(&self) -> Self {
    ItemState { origin: OriginData::OutOfScope, ..self.clone() }
  }

  pub fn debug_string(&self, g: &GrammarStore) -> String {
    if self.curr_lane != self.prev_lane {
      format!("[{}]->[{}] | {}", self.curr_lane, self.prev_lane, self.origin.debug_string(g))
    } else {
      format!("[{}] | {}", self.curr_lane, self.origin.debug_string(g))
    }
  }
}

impl Display for ItemState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if self.curr_lane != self.prev_lane {
      f.write_fmt(format_args!("[{}]->[{}] | {:?}", self.curr_lane, self.prev_lane, self.origin))
    } else {
      f.write_fmt(format_args!("[{}] | {:?}", self.curr_lane, self.origin))
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum OriginData {
  Undefined,
  OutOfScope,
  Symbol(SymbolID),
  RuleId(RuleId),
}

impl OriginData {
  pub fn debug_string(&self, g: &GrammarStore) -> String {
    use OriginData::*;
    match self {
      Symbol(sym_id) => sym_id.to_string(g),
      RuleId(rule_id) => {
        let rule = g.get_rule(rule_id).unwrap();
        let prod = g.get_production(&rule.prod_id).unwrap();
        format!("{}[{}]", prod.name, rule.bytecode_id)
      }
      OutOfScope => "Out Of Scope".to_string(),
      Undefined => "undefined".to_string(),
    }
  }
}

/// Represents a specific point in a parse sequence
/// defined by a rule and a positional offset that
/// indicates the next expected terminal or non-terminal.
#[repr(C, align(64))]
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]

pub struct Item {
  rule:  RuleId,
  state: ItemState,
  len:   u8,
  off:   u8,
}

impl Item {
  /// Creates a view of the item useful for debugging
  pub fn debug_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("{} null", self.state)
    } else {
      let rule = g.rules.get(&self.rule).unwrap();

      let mut string = String::new();

      string += &format!("<{}> ", self.state.debug_string(g));

      string += &g.productions.get(&rule.prod_id).unwrap().name;

      string += " >";

      for (index, RuleSymbol { sym_id, .. }) in rule.syms.iter().enumerate() {
        if index == self.off as usize {
          string += " •";
        }

        string += " ";

        string += &sym_id.to_string(g)
      }

      if self.completed() {
        string += " •";
      }
      string
    }
  }

  /// Two items belong to the same lane if one of the following
  /// conditions is met:
  /// 1. Both Items have states that are in the same lane.
  /// 2. Either Item is in the root [0] lane.
  pub fn in_same_lane(&self, other: &Item) -> bool {
    self.state.in_same_lane(&other.state)
  }

  /// Creates a view of the item usefully for error reporting.
  /// > Note: The item's position signifier `•` is not rendered.
  pub fn blame_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("{} null", self.state)
    } else {
      let rule = g.rules.get(&self.rule).unwrap();

      let mut string = String::new();

      string += &g.productions.get(&rule.prod_id).unwrap().name;

      string += " =>";

      for RuleSymbol { sym_id, .. } in rule.syms.iter() {
        string += " ";

        string += &sym_id.to_string(g)
      }

      string
    }
  }

  pub fn id_string(&self) -> String {
    format!("<{}>:{}-{}-{}", self.state, self.get_rule_id(), self.off, self.len)
  }

  //#[inline(always)]
  pub fn is_null(&self) -> bool {
    self.rule.is_null() && self.len == 0 && self.off == 0
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.state.is_out_of_scope()
  }

  #[inline(always)]
  pub fn null(state: ItemState) -> Self {
    Item { len: 0, rule: RuleId::default(), off: 0, state }
  }

  #[inline(always)]
  pub fn to_null(&self) -> Self {
    Item { len: 0, rule: RuleId::default(), off: 0, state: self.state }
  }

  /// Create an Item from a rule_id and a grammar store. Returns
  /// None if the rule_id does not match a stored rule in the
  /// grammar. Sets the items origin to OriginData::RuleID

  pub fn from_rule(rule_id: &RuleId, g: &GrammarStore) -> Option<Self> {
    g.rules.get(rule_id).map(|rule| Item::from(rule).to_origin(OriginData::RuleId(rule.id)))
  }

  pub fn completed(&self) -> bool {
    self.off >= self.len
  }

  pub fn at_start(&self) -> bool {
    self.off == 0
  }

  pub fn to_state(&self, state: ItemState) -> Item {
    Item { len: self.len, off: self.off, rule: self.rule, state }
  }

  pub fn to_origin(&self, origin: OriginData) -> Self {
    Item {
      rule:  self.rule,
      len:   self.len,
      off:   self.off,
      state: self.state.to_origin(origin),
    }
  }

  pub fn to_last_sym(self) -> Self {
    Item {
      rule:  self.rule,
      len:   self.len,
      off:   if self.len > 0 { self.len - 1 } else { 0 },
      state: self.state,
    }
  }

  pub fn to_start(&self) -> Item {
    Item { rule: self.rule, len: self.len, off: 0, state: self.state }
  }

  pub fn to_end(&self) -> Item {
    Item { rule: self.rule, len: self.len, off: self.len, state: self.state }
  }

  pub fn to_origin_only_state(&self) -> Item {
    Item {
      rule:  self.rule,
      len:   self.len,
      off:   self.off,
      state: self.state.to_lanes(0, 0),
    }
  }

  pub fn to_empty_state(&self) -> Item {
    Item {
      rule:  self.rule,
      len:   self.len,
      off:   self.off,
      state: ItemState::default(),
    }
  }

  pub fn increment(&self) -> Option<Item> {
    if !self.completed() {
      Some(Item {
        len:   self.len,
        off:   self.off + 1,
        rule:  self.rule,
        state: self.state,
      })
    } else {
      None
    }
  }

  /// Increments the Item if it is not in the completed position,
  /// otherwise returns the Item as is.
  pub fn try_increment(&self) -> Item {
    if !self.completed() {
      self.increment().unwrap()
    } else {
      self.clone()
    }
  }

  pub fn decrement(&self) -> Option<Item> {
    if !self.is_start() {
      Some(Item {
        len:   self.len,
        off:   self.off - 1,
        rule:  self.rule,
        state: self.state,
      })
    } else {
      None
    }
  }

  pub fn is_start(&self) -> bool {
    self.off == 0
  }

  pub fn get_rule_id(&self) -> RuleId {
    self.rule
  }

  pub fn get_rule<'a>(&self, g: &'a GrammarStore) -> HCResult<&'a Rule> {
    g.get_rule(&self.get_rule_id())
  }

  #[inline(always)]
  pub fn get_rule_ref<'a>(&self, g: &'a GrammarStore) -> HCResult<&'a RuleSymbol> {
    match (self.completed(), self.get_rule(&g)) {
      (false, HCResult::Ok(rule)) => HCResult::Ok(&rule.syms[self.off as usize]),
      _ => HCResult::None,
    }
  }

  #[inline(always)]
  pub fn get_offset(&self) -> u32 {
    self.off as u32
  }

  #[inline(always)]
  pub fn get_state(&self) -> ItemState {
    self.state
  }

  #[inline(always)]
  pub fn to_local_state(&self) -> ItemState {
    self.state.to_lanes(self.state.curr_lane, self.state.curr_lane)
  }

  #[inline(always)]
  pub fn get_origin(&self) -> OriginData {
    self.state.get_origin()
  }

  /// Get the SymbolId in the origin if the origin is
  /// of type `OriginData::Symbol`. Otherwise returns `SymbolId::Undefined`
  #[inline(always)]
  pub fn get_origin_sym(&self) -> SymbolID {
    match self.get_origin() {
      OriginData::Symbol(sym) => sym,
      _ => SymbolID::Undefined,
    }
  }

  #[inline(always)]
  pub fn len(&self) -> u32 {
    self.len as u32
  }

  /// Retrieve the symbol at the items position. This will be
  /// `SymbolId::Completed` if the item is at the completed position.
  pub fn get_symbol(&self, g: &GrammarStore) -> SymbolID {
    if self.completed() {
      SymbolID::EndOfInput
    } else {
      match g.rules.get(&self.rule) {
        Some(rule) => rule.syms[self.off as usize].sym_id,
        _ => SymbolID::Undefined,
      }
    }
  }

  /// If the symbol at this item's position is a non-term, then the ProductionId
  /// for that symbol is returned. Otherwise an invalid ProductionId is returned.
  pub fn get_production_id_at_sym(&self, g: &GrammarStore) -> ProductionId {
    match self.get_symbol(g) {
      SymbolID::Production(prod, _) => prod,
      _ => Default::default(),
    }
  }

  pub fn is_term(&self, g: &GrammarStore) -> bool {
    if self.completed() {
      false
    } else {
      !matches!(self.get_symbol(g), SymbolID::Production(_, _))
    }
  }

  pub fn is_nonterm(&self, g: &GrammarStore) -> bool {
    if self.completed() {
      false
    } else {
      !self.is_term(g)
    }
  }

  pub fn get_prod_id(&self, g: &GrammarStore) -> ProductionId {
    g.rules.get(&self.get_rule_id()).unwrap().prod_id
  }

  pub fn get_prod_as_sym_id(&self, g: &GrammarStore) -> SymbolID {
    g.get_production(&g.rules.get(&self.get_rule_id()).unwrap().prod_id).unwrap().sym_id
  }

  pub fn to_hash(&self) -> u64 {
    ((self.rule.0 & 0xFFFF_FFF0_F000_F000) ^ ((self.off as u64) << 32))
      | (self.state.curr_lane as u64)
  }

  pub fn print_blame(&self, g: &GrammarStore) {
    let rule = g.rules.get(&self.rule).unwrap();

    if self.completed() {
    } else {
      eprintln!("{}", rule.syms[self.off as usize].tok.blame(1, 1, "", None));
    }
  }
}

impl From<&Rule> for Item {
  fn from(rule: &Rule) -> Self {
    Item {
      rule:  rule.id,
      len:   rule.len as u8,
      off:   0,
      state: ItemState::default(),
    }
  }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub struct LinkedItem {
  pub item:         Item,
  pub closure_node: MaybeNodeId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LinkedItemWithGoto {
  pub item:         Item,
  pub closure_node: MaybeNodeId,
  pub goto_items:   Items,
}

pub struct FollowItemGroups {
  pub uncompleted_items: Vec<LinkedItem>,
  pub intermediate_completed_items: Vec<LinkedItem>,
  pub final_completed_items: Vec<LinkedItem>,
}

impl FollowItemGroups {
  pub fn get_all_items(&self) -> Items {
    self.uncompleted_items.iter().chain(self.final_completed_items.iter()).map(|t| t.item).collect()
  }

  pub fn get_uncompleted_items(&self) -> Items {
    self.uncompleted_items.iter().map(|t| t.item).collect()
  }

  pub fn get_completed_items(&self) -> Items {
    self.final_completed_items.iter().map(|t| t.item).collect()
  }
}

impl Into<LinkedItemWithGoto> for LinkedItem {
  fn into(self) -> LinkedItemWithGoto {
    LinkedItemWithGoto {
      item:         self.item,
      closure_node: self.closure_node,
      goto_items:   Default::default(),
    }
  }
}

pub type Items = Vec<Item>;
pub type ItemSet = BTreeSet<Item>;

pub trait ItemContainer: Clone + IntoIterator<Item = Item> {
  fn term_item_vec(&self, g: &GrammarStore) -> Items;
  fn non_term_item_vec(&self, g: &GrammarStore) -> Items;
  fn completed_item_vec(&self) -> Items;

  fn to_origin_only_state(self) -> Self;
  fn to_zero_state(self) -> Self;
  fn to_state(self, state: ItemState) -> Self;

  #[inline(always)]
  fn term_item_set(&self, g: &GrammarStore) -> ItemSet {
    self.term_item_vec(g).into_iter().collect()
  }

  #[inline(always)]
  fn non_term_item_set(&self, g: &GrammarStore) -> ItemSet {
    self.non_term_item_vec(g).into_iter().collect()
  }

  #[inline(always)]
  fn end_item_set(&self) -> ItemSet {
    self.completed_item_vec().into_iter().collect()
  }

  #[inline(always)]
  fn try_increment(&self) -> Items {
    self.clone().to_vec().into_iter().map(|i| i.try_increment()).collect()
  }

  #[inline(always)]
  fn to_end(&self) -> Items {
    self.clone().to_vec().into_iter().map(|i| i.to_end()).collect()
  }

  #[inline(always)]
  fn from_linked<T: IntoIterator<Item = LinkedItem>>(linked: T) -> Items {
    linked.into_iter().map(|i| i.item).collect()
  }

  #[inline(always)]
  fn from_linked_goto(linked: &Vec<LinkedItemWithGoto>) -> Items {
    linked.iter().map(|i| i.item).collect()
  }

  #[inline(always)]
  fn print_items(&self, g: &GrammarStore, comment: &str) {
    debug_items(comment, self.clone(), g);
  }

  fn to_debug_string(&self, g: &GrammarStore, sep: &str) -> String {
    self.clone().to_vec().iter().map(|i| i.debug_string(g)).collect::<Vec<_>>().join(sep)
  }

  #[inline(always)]
  fn closure(&self, g: &GrammarStore) -> ItemSet {
    let vec = self.clone().to_vec();

    let mut output = vec![];

    for item in vec {
      output.append(&mut get_closure_cached(&item, g).clone())
    }

    output.to_set()
  }

  #[inline(always)]
  fn closure_with_state(&self, g: &GrammarStore) -> ItemSet {
    let vec = self.clone().to_vec();

    let mut output = vec![];

    for item in vec {
      if item.is_null() {
        output.push(item)
      } else {
        output.append(&mut get_closure_cached_with_state(&item, g))
      }
    }

    output.to_set()
  }
  /// Does not enter the closure of Productions that are present
  /// in prod_id. If it detects items the have any member of `prod_ids`
  /// as the initial symbol, it discards those items.
  #[inline(always)]
  fn rd_closure_with_state(
    &self,
    g: &GrammarStore,
    prod_ids: &BTreeSet<ProductionId>,
  ) -> (ItemSet, ItemSet) {
    let mut seen = BTreeSet::<Item>::new();
    let mut left_recursive_items = BTreeSet::<Item>::new();
    let items = self.clone().to_vec();

    let mut queue = VecDeque::<Item>::from_iter(items.iter().cloned());

    while let Some(item) = queue.pop_front() {
      if prod_ids.contains(&item.get_production_id_at_sym(g)) {
        left_recursive_items.insert(item);
        continue;
      }
      if seen.insert(item) {
        if let SymbolID::Production(prod_id, _) = &item.get_symbol(g) {
          for i in get_production_start_items(prod_id, g) {
            queue.push_back(i.to_state(item.get_state()));
          }
        }
      }
    }

    (seen, left_recursive_items)
  }

  fn to_set(self) -> ItemSet;
  fn to_vec(self) -> Items;
}

impl ItemContainer for ItemSet {
  #[inline(always)]
  fn to_origin_only_state(self) -> Self {
    self.into_iter().map(|i| i.to_origin_only_state()).collect()
  }

  #[inline(always)]
  fn to_zero_state(self) -> Self {
    self.into_iter().map(|i| i.to_empty_state()).collect()
  }

  #[inline(always)]
  fn to_state(self, state: ItemState) -> Self {
    self.into_iter().map(|i| i.to_state(state)).collect()
  }

  #[inline(always)]
  fn non_term_item_vec(&self, g: &GrammarStore) -> Items {
    self.iter().filter(|i| i.is_nonterm(g)).cloned().collect()
  }

  #[inline(always)]
  fn term_item_vec(&self, g: &GrammarStore) -> Items {
    self.iter().filter(|i| i.is_term(g)).cloned().collect()
  }

  #[inline(always)]
  fn completed_item_vec(&self) -> Items {
    self.iter().filter(|i| i.completed()).cloned().collect()
  }

  #[inline(always)]
  fn to_set(self) -> ItemSet {
    self
  }

  #[inline(always)]
  fn to_vec(self) -> Items {
    self.into_iter().collect()
  }
}

impl ItemContainer for Items {
  #[inline(always)]
  fn to_origin_only_state(self) -> Self {
    self.into_iter().map(|i| i.to_origin_only_state()).collect()
  }

  #[inline(always)]
  fn to_zero_state(self) -> Self {
    self.into_iter().map(|i| i.to_empty_state()).collect()
  }

  #[inline(always)]
  fn to_state(self, state: ItemState) -> Self {
    self.into_iter().map(|i| i.to_state(state)).collect()
  }

  #[inline(always)]
  fn non_term_item_vec(&self, g: &GrammarStore) -> Items {
    self.iter().filter(|i| i.is_nonterm(g)).cloned().collect()
  }

  #[inline(always)]
  fn term_item_vec(&self, g: &GrammarStore) -> Items {
    self.iter().filter(|i| i.is_term(g)).cloned().collect()
  }

  #[inline(always)]
  fn completed_item_vec(&self) -> Items {
    self.iter().filter(|i| i.completed()).cloned().collect()
  }

  #[inline(always)]
  fn to_set(self) -> ItemSet {
    self.into_iter().collect()
  }

  #[inline(always)]
  fn to_vec(self) -> Items {
    self
  }
}
