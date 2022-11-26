use super::*;
use std::fmt::Display;

use super::HCResult;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ItemState(
  u32,
  /// An index to the original object that produced this item, be it a production, a symbol,
  /// or undefined
  OriginData,
);

impl ItemState {
  const GOTO_END_GOAL: u32 = 0x4000;
  const GOTO_END_GOAL_MASK: u32 = (!Self::GOTO_END_GOAL) & 0xFFFF;
  pub const GOTO_END_GOAL_STATE: ItemState =
    ItemState::new(Self::GOTO_END_GOAL, 0, OriginData::UNDEFINED);
  const GOTO_ROOT_END_GOAL: u32 = 0x8000;
  const GOTO_ROOT_END_GOAL_MASK: u32 = (!Self::GOTO_ROOT_END_GOAL) & 0xFFFF;
  pub const GOTO_ROOT_END_GOAL_STATE: ItemState =
    ItemState::new(Self::GOTO_ROOT_END_GOAL, 0, OriginData::UNDEFINED);

  pub const fn default() -> Self {
    ItemState(0, OriginData::UNDEFINED)
  }

  /// Create a new [Item]
  pub const fn new(group: u32, depth: u32, origin: OriginData) -> Self {
    ItemState((group << 16) | (depth & 0xFFFF), origin)
  }

  /// Increase the item's depth by 1
  pub fn increment_depth(&self) -> Self {
    ItemState::new(self.get_group(), self.get_depth() + 1, self.1)
  }

  /// Get the item's depth
  pub fn get_depth(&self) -> u32 {
    self.0 & 0xFFFF
  }

  /// Get the group the item belongs to
  pub fn get_group(&self) -> u32 {
    (self.0 >> 16) & 0xFFFF
  }

  /// Get an index to the closure this item originates from
  pub fn get_closure_index(&self) -> usize {
    (self.get_group() & (Self::GOTO_END_GOAL_MASK | Self::GOTO_ROOT_END_GOAL_MASK)) as usize
  }

  /// Create a new Item with the given depth
  pub fn to_depth(&self, depth: u32) -> Self {
    ItemState::new(self.get_group(), depth, self.1)
  }

  /// Create a new Item with the given group
  pub fn to_group(&self, group: u32) -> Self {
    ItemState::new(group, self.get_depth(), self.1)
  }

  /// Create a new Item with the given group
  pub fn to_origin(&self, origin: OriginData) -> Self {
    ItemState::new(self.get_group(), self.get_depth(), origin)
  }

  /// Indicate's the item originate from a production other
  /// than the one currently being evaluated.
  pub fn is_goto_end_origin(&self) -> bool {
    (self.get_group() & Self::GOTO_END_GOAL) > 0
  }

  /// Indicate's the item originate from a production other
  /// than the one currently being evaluated.
  pub fn is_goto_root_end_origin(&self) -> bool {
    (self.get_group() & Self::GOTO_ROOT_END_GOAL) > 0
  }
}

impl Display for ItemState {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_fmt(format_args!("<g:{},d:{}>", self.get_group(), self.get_depth()))
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum OriginData {
  Symbol(SymbolID),
  Production(ProductionId),
  UNDEFINED,
}

/// Represents a specific point in a parse sequence
/// defined by a body and a positional offset that
/// indicates the next expected terminal or non-terminal.
#[repr(C, align(64))]
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]

pub struct Item {
  body:  BodyId,
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
      let body = g.bodies.get(&self.body).unwrap();

      let mut string = String::new();

      string += &format!("{} ", self.state);

      string += &g.productions.get(&body.prod_id).unwrap().name;

      string += " =>";

      for (index, BodySymbol { sym_id, .. }) in body.syms.iter().enumerate() {
        if index == self.off as usize {
          string += " •";
        }

        string += " ";

        string += &sym_id.to_string(g)
      }

      if self.at_end() {
        string += " •";
      }
      string
    }
  }

  /// Two items belong to the same lane if one of the following
  /// conditions is met:
  /// 1. Both Items have states that are in the same group.
  /// 2. Either Item is in the 0 group.
  pub fn in_same_lane(&self, other: &Item) -> bool {
    match (self.state.get_group(), other.state.get_group()) {
      (a, b) if a == b => true,
      (_, 0) | (0, _) => true,
      _ => false,
    }
  }

  /// Creates a view of the item usefully for error reporting.
  /// > Note: The item's position signifier `•` is not rendered.
  pub fn blame_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("{} null", self.state)
    } else {
      let body = g.bodies.get(&self.body).unwrap();

      let mut string = String::new();

      string += &g.productions.get(&body.prod_id).unwrap().name;

      string += " =>";

      for (index, BodySymbol { sym_id, .. }) in body.syms.iter().enumerate() {
        string += " ";

        string += &sym_id.to_string(g)
      }

      string
    }
  }

  pub fn id_string(&self) -> String {
    format!("<{}>:{}-{}-{}", self.state, self.get_body_id(), self.off, self.len)
  }

  //#[inline(always)]
  pub fn is_null(&self) -> bool {
    self.body.is_null() && self.len == 0 && self.off == 0
  }

  pub fn is_out_of_scope(&self) -> bool {
    self.is_goto_end_origin() || self.is_goto_root_end_origin()
  }

  pub fn is_goto_end_origin(&self) -> bool {
    self.state.is_goto_end_origin()
  }

  pub fn is_goto_root_end_origin(&self) -> bool {
    self.state.is_goto_root_end_origin()
  }

  #[inline(always)]
  pub fn null(state: ItemState) -> Self {
    Item { len: 0, body: BodyId::default(), off: 0, state }
  }

  #[inline(always)]
  pub fn to_null(&self) -> Self {
    Item { len: 0, body: BodyId::default(), off: 0, state: self.state }
  }

  /// Create an Item from a body_id and a grammar store. Returns
  /// None if the body_id does not match a stored body in the
  /// grammar.

  pub fn from_body(body_id: &BodyId, g: &GrammarStore) -> Option<Self> {
    g.bodies.get(body_id).map(|body| Item::from(body))
  }

  pub fn at_end(&self) -> bool {
    self.off == self.len
  }

  pub fn at_start(&self) -> bool {
    self.off == 0
  }

  pub fn to_state(&self, state: ItemState) -> Item {
    Item { len: self.len, off: self.off, body: self.body, state }
  }

  pub fn to_origin(&self, origin: OriginData) -> Self {
    Item {
      body:  self.body,
      len:   self.len,
      off:   self.off,
      state: self.state.to_origin(origin),
    }
  }

  pub fn to_last_sym(self) -> Self {
    Item {
      body:  self.body,
      len:   self.len,
      off:   if self.len > 0 { self.len - 1 } else { 0 },
      state: self.state,
    }
  }

  pub fn to_start(&self) -> Item {
    Item { body: self.body, len: self.len, off: 0, state: self.state }
  }

  pub fn to_end(&self) -> Item {
    Item { body: self.body, len: self.len, off: self.len, state: self.state }
  }

  pub fn to_origin_only_state(&self) -> Item {
    Item {
      body:  self.body,
      len:   self.len,
      off:   self.off,
      state: self.state.to_group(0).to_depth(0),
    }
  }

  pub fn to_zero_state(&self) -> Item {
    Item {
      body:  self.body,
      len:   self.len,
      off:   self.off,
      state: ItemState::default(),
    }
  }

  pub fn increment(&self) -> Option<Item> {
    if !self.at_end() {
      Some(Item {
        len:   self.len,
        off:   self.off + 1,
        body:  self.body,
        state: self.state.increment_depth(),
      })
    } else {
      None
    }
  }

  /// Increments the Item if it is not at the end position,
  /// otherwise returns the Item as is.
  pub fn try_increment(&self) -> Item {
    if !self.at_end() {
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
        body:  self.body,
        state: self.state,
      })
    } else {
      None
    }
  }

  pub fn is_start(&self) -> bool {
    self.off == 0
  }

  pub fn get_body_id(&self) -> BodyId {
    self.body
  }

  pub fn get_body<'a>(&self, g: &'a GrammarStore) -> HCResult<&'a Body> {
    g.get_body(&self.get_body_id())
  }

  pub fn get_body_ref<'a>(&self, g: &'a GrammarStore) -> HCResult<&'a BodySymbol> {
    match (self.at_end(), self.get_body(&g)) {
      (false, HCResult::Ok(body)) => HCResult::Ok(&body.syms[self.off as usize]),
      _ => HCResult::None,
    }
  }

  pub fn get_offset(&self) -> u32 {
    self.off as u32
  }

  pub fn get_state(&self) -> ItemState {
    self.state
  }

  pub fn get_origin(&self) -> OriginData {
    self.state.1
  }

  pub fn get_length(&self) -> u32 {
    self.len as u32
  }

  pub fn get_symbol(&self, g: &GrammarStore) -> SymbolID {
    if self.at_end() {
      SymbolID::EndOfFile
    } else {
      match g.bodies.get(&self.body) {
        Some(body) => body.syms[self.off as usize].sym_id,
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

  pub fn is_end(&self) -> bool {
    self.len <= self.off
  }

  pub fn is_term(&self, g: &GrammarStore) -> bool {
    if self.is_end() {
      false
    } else {
      !matches!(self.get_symbol(g), SymbolID::Production(production, _))
    }
  }

  pub fn is_nonterm(&self, g: &GrammarStore) -> bool {
    if self.is_end() {
      false
    } else {
      !self.is_term(g)
    }
  }

  pub fn get_prod_id(&self, g: &GrammarStore) -> ProductionId {
    g.bodies.get(&self.get_body_id()).unwrap().prod_id
  }

  pub fn get_prod_as_sym_id(&self, g: &GrammarStore) -> SymbolID {
    g.get_production(&g.bodies.get(&self.get_body_id()).unwrap().prod_id).unwrap().sym_id
  }

  pub fn to_hash(&self) -> u64 {
    ((self.body.0 & 0xFFFF_FFF0_F000_F000) ^ ((self.off as u64) << 32)) | (self.state.0 as u64)
  }

  pub fn print_blame(&self, g: &GrammarStore) {
    let body = g.bodies.get(&self.body).unwrap();

    if self.at_end() {
    } else {
      eprintln!("{}", body.syms[self.off as usize].tok.blame(1, 1, "", None));
    }
  }
}

impl From<&Body> for Item {
  fn from(body: &Body) -> Self {
    Item {
      body:  body.id,
      len:   body.len as u8,
      off:   0,
      state: ItemState::default(),
    }
  }
}
