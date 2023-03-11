use super::{
  graph::{Origin, OutScopeIndex, State, StateId},
  *,
};
use crate::grammar::{get_closure_cached, get_production_start_items};
use std::{
  collections::{
    btree_set::{self},
    BTreeSet,
    VecDeque,
  },
  fmt::{format, Display},
  io::Chain,
  iter::Filter,
  slice,
  vec,
};

/// Represents a specific point in a parse sequence
/// defined by a rule and a positional offset that
/// indicates the next expected terminal or non-terminal.
#[repr(C, align(64))]
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub(crate) struct Item {
  pub(crate) rule_id:      RuleId,
  pub(crate) len:          u8,
  pub(crate) off:          u8,
  pub(crate) is_exclusive: u8,
  pub(crate) goal_index:   u32,
  pub(crate) origin:       Origin,
  pub(crate) origin_state: StateId,
}

pub(crate) enum ItemType {
  Terminal(SymbolID),
  NonTerminal(ProductionId),
  TokenProduction(ProductionId, SymbolID),
  Completed(ProductionId),
  ExclusiveCompleted(ProductionId, u8),
}

impl Default for Item {
  fn default() -> Self {
    Self {
      len:          0,
      off:          0,
      goal_index:   0,
      is_exclusive: 0,
      origin:       Default::default(),
      origin_state: Default::default(),
      rule_id:      RuleId(0),
    }
  }
}

impl Item {
  #[inline(always)]
  pub fn new_null() -> Self {
    Item { len: 0, rule_id: RuleId::default(), off: 0, ..Default::default() }
  }

  /// Creates a view of the item useful for debugging
  pub fn debug_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("null")
    } else {
      let rule = g.rules.get(&self.rule_id).unwrap();

      let mut string = self.origin.is_none().then_some(String::new()).unwrap_or_else(|| {
        format!(
          "<[{}-{:?}]  [{:X}] ",
          self.origin.debug_string(g),
          self.origin_state,
          self.goal_index
        )
      });

      string += &g.productions.get(&rule.prod_id).unwrap().name;

      string += " >";

      for (index, RuleSymbol { sym_id, .. }) in rule.syms.iter().enumerate() {
        if index == self.off as usize {
          string += " •";
        }

        string += " ";

        string += &sym_id.debug_string(g)
      }

      if self.is_completed() {
        string += " •";
      }

      if self.is_exclusive > 0 {
        string += &format!(" ⦻[{}]", self.is_exclusive);
      }

      string.replace("\n", "\\n")
    }
  }

  /// Creates a view of the rule
  pub fn rule_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("null")
    } else {
      let rule = g.rules.get(&self.rule_id).unwrap();

      let mut string = String::new();

      string += &g.productions.get(&rule.prod_id).unwrap().name;

      string += " =>";

      for (_, RuleSymbol { sym_id, .. }) in rule.syms.iter().enumerate() {
        string += " ";

        string += &sym_id.debug_string(g)
      }
      string
    }
  }

  #[inline(always)]
  pub fn is_null(&self) -> bool {
    self.rule_id.is_null() && self.len == 0 && self.off == 0
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.goal_index == OutScopeIndex || self.origin.is_out_of_scope()
  }

  #[inline(always)]
  pub fn to_null(&self) -> Self {
    Item { ..Default::default() }
  }

  ///
  #[inline(always)]
  pub fn to_absolute(&self) -> Self {
    Item {
      goal_index: Default::default(),
      origin: Default::default(),
      origin_state: Default::default(),
      ..self.clone()
    }
  }

  #[inline(always)]
  pub fn is_completed(&self) -> bool {
    self.off >= self.len
  }

  #[inline(always)]
  pub fn at_start(&self) -> bool {
    self.off == 0
  }

  pub fn is_left_recursive(&self, g: &GrammarStore) -> bool {
    let symA = self.get_prod_id(g);
    let symB = self.get_production_id_at_sym(g);

    symA == symB && self.is_start()
  }

  #[inline(always)]
  pub fn to_start(&self) -> Item {
    Item { off: 0, ..self.clone() }
  }

  #[inline(always)]
  pub fn to_complete(&self) -> Item {
    Item { off: self.len, ..self.clone() }
  }

  pub fn increment(&self) -> Option<Item> {
    if !self.is_completed() {
      Some(Item {
        len: self.len,
        off: self.off + 1,
        rule_id: self.rule_id,
        ..self.clone()
      })
    } else {
      None
    }
  }

  /// Increments the Item if it is not in the completed position,
  /// otherwise returns the Item as is.
  pub fn try_increment(&self) -> Item {
    if !self.is_completed() {
      self.increment().unwrap()
    } else {
      self.clone()
    }
  }

  pub fn decrement(&self) -> Option<Item> {
    if !self.is_start() {
      Some(Item {
        len: self.len,
        off: self.off - 1,
        rule_id: self.rule_id,
        ..Default::default()
      })
    } else {
      None
    }
  }

  pub fn get_precedence(&self, g: &GrammarStore) -> u32 {
    if self.is_completed() {
      self.is_exclusive as u32
    } else {
      self.get_rule(g).unwrap().syms[self.off as usize].precedence
    }
  }

  /// Creates a view of the item usefully for error reporting.
  /// > Note: The item's position signifier `•` is not rendered.
  pub fn blame_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("{:?} null", self.origin)
    } else {
      let rule = g.rules.get(&self.rule_id).unwrap();

      let mut string = String::new();

      string += &g.productions.get(&rule.prod_id).unwrap().name;

      string += " =>";

      for RuleSymbol { sym_id, .. } in rule.syms.iter() {
        string += " ";

        string += &sym_id.debug_string(g)
      }

      string
    }
  }

  pub fn is_start(&self) -> bool {
    self.off == 0
  }

  pub fn get_rule_id(&self) -> RuleId {
    self.rule_id
  }

  pub fn get_rule<'a>(&self, g: &'a GrammarStore) -> SherpaResult<&'a Rule> {
    g.get_rule(&self.get_rule_id())
  }

  #[inline(always)]
  pub fn get_rule_ref<'a>(&self, g: &'a GrammarStore) -> SherpaResult<&'a RuleSymbol> {
    match (self.is_completed(), self.get_rule(&g)) {
      (false, SherpaResult::Ok(rule)) => SherpaResult::Ok(&rule.syms[self.off as usize]),
      _ => SherpaResult::None,
    }
  }

  #[inline(always)]
  pub fn len(&self) -> u32 {
    self.len as u32
  }

  /// Retrieve the symbol at the items position. This will be
  /// `SymbolId::Completed` if the item is at the completed position.
  pub fn get_symbol(&self, g: &GrammarStore) -> SymbolID {
    if self.is_null() {
      SymbolID::Undefined
    } else if self.is_completed() {
      SymbolID::EndOfFile
    } else {
      match g.rules.get(&self.rule_id) {
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

  pub fn to_oos_index(&self) -> Self {
    Self { goal_index: OutScopeIndex, ..self.clone() }
  }

  pub fn to_goal_index(&self, goal_index: u32) -> Self {
    Self { goal_index, ..self.clone() }
  }

  pub fn to_origin_state(&self, origin_state: StateId) -> Self {
    Self { origin_state, ..self.clone() }
  }

  pub fn to_origin(&self, origin: Origin) -> Self {
    Self { origin, ..self.clone() }
  }

  pub fn is_term(&self, g: &GrammarStore) -> bool {
    if self.is_completed() {
      false
    } else {
      !matches!(self.get_symbol(g), SymbolID::Production(_, _))
    }
  }

  pub fn is_nonterm(&self, g: &GrammarStore) -> bool {
    if self.is_completed() {
      false
    } else {
      !self.is_term(g)
    }
  }

  pub fn get_prod_id(&self, g: &GrammarStore) -> ProductionId {
    g.rules.get(&self.get_rule_id()).unwrap().prod_id
  }

  pub fn _get_prod_as_sym_id(&self, g: &GrammarStore) -> SymbolID {
    g.get_production(&g.rules.get(&self.get_rule_id()).unwrap().prod_id).unwrap().sym_id
  }

  pub fn get_type(&self, g: &GrammarStore) -> ItemType {
    if self.is_completed() {
      match self.is_exclusive {
        0 => ItemType::Completed(self.get_prod_id(g)),
        val => ItemType::ExclusiveCompleted(self.get_prod_id(g), val),
      }
    } else {
      let sym = self.get_symbol(g);
      match sym {
        SymbolID::Production(prod_id, _) => ItemType::NonTerminal(prod_id),
        SymbolID::TokenProduction(.., prod_id) => ItemType::TokenProduction(prod_id, sym),
        sym => ItemType::Terminal(sym),
      }
    }
  }

  pub fn align(&self, other: &Self) -> Self {
    Self {
      goal_index: other.goal_index,
      origin: other.origin,
      origin_state: other.origin_state,
      ..self.clone()
    }
  }
}

impl From<&Rule> for Item {
  fn from(rule: &Rule) -> Self {
    Item {
      rule_id: rule.id,
      len: rule.len() as u8,
      is_exclusive: rule.is_exclusive as u8,
      ..Default::default()
    }
  }
}

pub(crate) type Items = Vec<Item>;
pub(crate) type ItemSet = BTreeSet<Item>;

impl<'a> ItemContainerIter<'a> for btree_set::Iter<'a, Item> {}
impl<'a> ItemContainerIter<'a> for slice::Iter<'a, Item> {}
pub(crate) trait ItemContainerIter<'a>: Iterator<Item = &'a Item> + Sized {
  fn contains_out_of_scope(&mut self) -> bool {
    self.any(|i| i.is_out_of_scope())
  }

  fn all_are_out_of_scope(&mut self) -> bool {
    self.all(|i| i.origin.is_out_of_scope())
  }

  fn to_set(&mut self) -> ItemSet {
    self.cloned().collect()
  }

  fn to_vec(&mut self) -> Items {
    self.cloned().collect()
  }

  fn all_items_are_from_same_peek_origin(&mut self) -> bool {
    let origin_set = self.map(|i| i.origin).collect::<BTreeSet<_>>();
    match (origin_set.len(), origin_set.first()) {
      (1, Some(Origin::Peek(..))) => true,
      _ => false,
    }
  }

  fn peek_is_resolved(&mut self) -> bool {
    self.all_items_are_from_same_peek_origin()
  }

  fn follow_items_are_the_same(&mut self) -> bool {
    self.map(|i| i.to_absolute()).collect::<BTreeSet<_>>().len() == 1
  }

  fn to_production_id_set(&mut self, g: &GrammarStore) -> BTreeSet<ProductionId> {
    self.map(|i| i.get_prod_id(g)).collect()
  }

  /// Returns the Production of the non-terminal symbol in each item. For items
  /// whose symbol is a terminal or are complete, the Defualt production id is used.
  fn to_prod_sym_id_set(&mut self, g: &GrammarStore) -> BTreeSet<ProductionId> {
    self.map(|i| i.get_production_id_at_sym(g)).collect()
  }

  fn intersects(&mut self, set: &ItemSet) -> bool {
    self.any(|i| set.contains(i))
  }
}

impl ItemContainer for ItemSet {}
impl ItemContainer for Items {}
pub(crate) trait ItemContainer:
  Clone + IntoIterator<Item = Item> + FromIterator<Item>
{
  fn non_term_items(self, g: &GrammarStore) -> Self {
    self.into_iter().filter(|i| i.is_nonterm(g)).collect()
  }

  fn term_items(self, g: &GrammarStore) -> Self {
    self.into_iter().filter(|i| i.is_term(g)).collect()
  }

  fn null_items(self) -> Self {
    self.into_iter().filter(|i| i.is_null()).collect()
  }

  fn incomplete_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_completed()).collect()
  }

  fn to_production_ids(&self, g: &GrammarStore) -> BTreeSet<ProductionId> {
    self.clone().into_iter().map(|i| i.get_prod_id(g)).collect()
  }

  fn completed_items(self) -> Self {
    self.into_iter().filter(|i| i.is_completed()).collect()
  }

  fn inscope_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_out_of_scope()).collect()
  }

  fn outscope_items(self) -> Self {
    self.into_iter().filter(|i| i.is_out_of_scope()).collect()
  }

  fn uncompleted_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_completed()).collect()
  }

  fn to_absolute(self) -> Self {
    self.into_iter().map(|i| i.to_absolute()).collect()
  }

  #[inline(always)]
  fn try_increment(&self) -> Items {
    self.clone().to_vec().into_iter().map(|i| i.try_increment()).collect()
  }

  #[inline(always)]
  fn try_decrement(&self) -> Items {
    self
      .clone()
      .to_vec()
      .into_iter()
      .map(|i| if i.off > 0 { i.decrement().unwrap() } else { i })
      .collect()
  }

  #[inline(always)]
  fn __print_items__(&self, g: &GrammarStore, comment: &str) {
    debug_items(comment, self.clone(), g);
  }

  fn to_debug_string(&self, g: &GrammarStore, sep: &str) -> String {
    self.clone().to_vec().iter().map(|i| i.debug_string(g)).collect::<Vec<_>>().join(sep)
  }

  fn to_set(self) -> ItemSet {
    self.into_iter().collect()
  }
  fn to_vec(self) -> Items {
    self.into_iter().collect()
  }
}

fn debug_items<T: IntoIterator<Item = Item>>(comment: &str, items: T, g: &GrammarStore) {
  println!("{} --> ", comment);

  for item in items {
    println!("    {}", item.debug_string(g));
  }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct FollowPair {
  pub completed: Item,
  pub follow:    Item,
}

impl From<(Item, Item)> for FollowPair {
  fn from((completed, follow): (Item, Item)) -> Self {
    Self { completed, follow }
  }
}

impl<'a> FollowPairContainerIter<'a> for btree_set::Iter<'a, FollowPair> {}
impl<'a> FollowPairContainerIter<'a> for slice::Iter<'a, FollowPair> {}
pub(crate) trait FollowPairContainerIter<'a>:
  Iterator<Item = &'a FollowPair> + Sized
{
  fn to_completed_set(&mut self) -> BTreeSet<Item> {
    self.map(|i| i.completed).collect()
  }

  fn to_completed_vec(&mut self) -> Vec<Item> {
    self.map(|i| i.completed).collect()
  }

  fn to_follow_set(&mut self) -> BTreeSet<Item> {
    self.map(|i| i.follow).collect()
  }

  fn to_follow_vec(&mut self) -> Vec<Item> {
    self.map(|i| i.follow).collect()
  }
}

pub(crate) struct CompletedItemArtifacts {
  pub follow_pairs: BTreeSet<FollowPair>,
  pub oos_pairs:    BTreeSet<FollowPair>,
  pub follow_items: ItemSet,
  pub default_only: ItemSet,
}
