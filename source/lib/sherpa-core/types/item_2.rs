use super::{
  graph_2::{Origin, State, StateId},
  *,
};
use crate::grammar::{
  get_closure_cached,
  get_closure_cached_with_state,
  get_production_start_items,
};
use std::{
  collections::{BTreeSet, VecDeque},
  fmt::{format, Display},
  vec,
};

/// Represents a specific point in a parse sequence
/// defined by a rule and a positional offset that
/// indicates the next expected terminal or non-terminal.
#[repr(C, align(64))]
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub(crate) struct Item {
  pub(crate) rule:         RuleId,
  pub(crate) len:          u8,
  pub(crate) off:          u8,
  pub(crate) goal_index:   u32,
  pub(crate) origin:       Origin,
  pub(crate) origin_state: StateId,
}

pub(crate) enum ItemType {
  Terminal(SymbolID),
  NonTerminal(ProductionId),
  TokenProduction(ProductionId, SymbolID),
  Completed(ProductionId),
}

impl Default for Item {
  fn default() -> Self {
    Self {
      len:          0,
      off:          0,
      goal_index:   0,
      origin:       Default::default(),
      origin_state: Default::default(),
      rule:         RuleId(0),
    }
  }
}

impl Item {
  /// Creates a view of the item useful for debugging
  pub fn debug_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("null")
    } else {
      let rule = g.rules.get(&self.rule).unwrap();

      let mut string = format!(
        "<[{}-{:?}]  [{}] ",
        self.origin.debug_string(g),
        self.origin_state,
        self.goal_index
      );

      string += &g.productions.get(&rule.prod_id).unwrap().name;

      string += " >";

      for (index, RuleSymbol { sym_id, .. }) in rule.syms.iter().enumerate() {
        if index == self.off as usize {
          string += " •";
        }

        string += " ";

        string += &sym_id.debug_string(g)
      }

      if self.completed() {
        string += " •";
      }
      string
    }
  }

  /// Creates a view of the rule
  pub fn rule_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("null")
    } else {
      let rule = g.rules.get(&self.rule).unwrap();

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

  //#[inline(always)]
  pub fn is_null(&self) -> bool {
    self.rule.is_null() && self.len == 0 && self.off == 0
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.origin.is_out_of_scope()
  }

  #[inline(always)]
  pub fn to_null(&self) -> Self {
    Item { ..Default::default() }
  }

  #[inline(always)]
  pub fn to_cardinal(&self) -> Self {
    Item {
      goal_index: Default::default(),
      origin: Default::default(),
      origin_state: Default::default(),
      ..self.clone()
    }
  }

  #[inline(always)]
  pub fn null() -> Self {
    Item { len: 0, rule: RuleId::default(), off: 0, ..Default::default() }
  }

  #[inline(always)]
  pub fn completed(&self) -> bool {
    self.off >= self.len
  }

  #[inline(always)]
  pub fn at_start(&self) -> bool {
    self.off == 0
  }

  #[inline(always)]
  pub fn to_start(&self) -> Item {
    Item { off: 0, ..self.clone() }
  }

  #[inline(always)]
  pub fn to_completed(&self) -> Item {
    Item { off: self.len, ..self.clone() }
  }

  pub fn increment(&self) -> Option<Item> {
    if !self.completed() {
      Some(Item { len: self.len, off: self.off + 1, rule: self.rule, ..self.clone() })
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
        len: self.len,
        off: self.off - 1,
        rule: self.rule,
        ..Default::default()
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

  pub fn get_rule<'a>(&self, g: &'a GrammarStore) -> SherpaResult<&'a Rule> {
    g.get_rule(&self.get_rule_id())
  }

  #[inline(always)]
  pub fn get_rule_ref<'a>(&self, g: &'a GrammarStore) -> SherpaResult<&'a RuleSymbol> {
    match (self.completed(), self.get_rule(&g)) {
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
    } else if self.completed() {
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

  pub fn to_origin_state(&self, origin_state: StateId) -> Self {
    Self { origin_state, ..self.clone() }
  }

  pub fn to_origin(&self, origin: Origin) -> Self {
    Self { origin, ..self.clone() }
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

  pub fn _get_prod_as_sym_id(&self, g: &GrammarStore) -> SymbolID {
    g.get_production(&g.rules.get(&self.get_rule_id()).unwrap().prod_id).unwrap().sym_id
  }

  pub fn get_type(&self, g: &GrammarStore) -> ItemType {
    if self.completed() {
      ItemType::Completed(self.get_prod_id(g))
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
    Item { rule: rule.id, len: rule.len as u8, ..Default::default() }
  }
}

pub(crate) type Items = Vec<Item>;
pub(crate) type ItemSet = BTreeSet<Item>;

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
    self.into_iter().filter(|i| !i.completed()).collect()
  }

  fn completed_items(self) -> Self {
    self.into_iter().filter(|i| i.completed()).collect()
  }

  fn inscope_items(self) -> Self {
    self.into_iter().filter(|i| !i.is_out_of_scope()).collect()
  }

  fn outscope_items(self) -> Self {
    self.into_iter().filter(|i| i.is_out_of_scope()).collect()
  }

  fn uncompleted_items(self) -> Self {
    self.into_iter().filter(|i| !i.completed()).collect()
  }

  fn to_cardinal(self) -> Self {
    self.into_iter().map(|i| i.to_cardinal()).collect()
  }

  fn contains_out_of_scope(&self) -> bool {
    self.as_vec().iter().any(|i| i.is_out_of_scope())
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

  fn as_vec(&self) -> Items {
    self.clone().to_vec()
  }

  fn as_set(&self) -> ItemSet {
    self.clone().to_set()
  }

  fn to_set(self) -> ItemSet {
    self.into_iter().collect()
  }
  fn to_vec(self) -> Items {
    self.into_iter().collect()
  }
}

impl ItemContainer for ItemSet {}

impl ItemContainer for Items {}

fn debug_items<T: IntoIterator<Item = Item>>(comment: &str, items: T, g: &GrammarStore) {
  eprintln!("{} --> ", comment);

  for item in items {
    eprintln!("    {}", item.debug_string(g));
  }
}
