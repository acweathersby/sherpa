use crate::types::BodyId;
use crate::types::BodySymbolRef;
use crate::types::GrammarStore;
use crate::types::ProductionId;
use crate::types::SymbolID;
use std::fmt::Display;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ItemState(u32);

impl ItemState {
  const GOTO_END_GOAL: u32 = 0x4000;
  const GOTO_END_GOAL_MASK: u32 = (!Self::GOTO_END_GOAL) & 0xFFFF;
  pub const GOTO_END_GOAL_STATE: ItemState = ItemState::new(Self::GOTO_END_GOAL, 0);
  const GOTO_ROOT_END_GOAL: u32 = 0x8000;
  const GOTO_ROOT_END_GOAL_MASK: u32 = (!Self::GOTO_ROOT_END_GOAL) & 0xFFFF;
  pub const GOTO_ROOT_END_GOAL_STATE: ItemState = ItemState::new(Self::GOTO_ROOT_END_GOAL, 0);

  pub const fn default() -> Self {
    ItemState(0)
  }

  /// Create a new [Item]
  pub const fn new(group: u32, depth: u32) -> Self {
    ItemState((group << 16) | (depth & 0xFFFF))
  }

  /// Increase the item's depth by 1
  pub fn increment_depth(&self) -> Self {
    ItemState::new(self.get_group(), self.get_depth() + 1)
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
    ItemState::new(self.get_group(), depth)
  }

  /// Create a new Item with the given group
  pub fn to_group(&self, group: u32) -> Self {
    ItemState::new(group, self.get_depth())
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
    f.write_fmt(format_args!("<{},{}>", self.get_group(), self.get_depth()))
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
  body:   BodyId,
  state:  ItemState,
  len:    u8,
  off:    u8,
  // An index to the original object
  // that produced this item, be it a production,
  origin: OriginData,
}

impl Item {
  pub fn debug_string(&self, g: &GrammarStore) -> String {
    if self.is_null() {
      format!("{} null", self.state)
    } else {
      let body = g.bodies.get(&self.body).unwrap();

      let mut string = String::new();

      string += &format!("{} ", self.state);

      string += &g.productions.get(&body.prod).unwrap().original_name;

      string += " =>";

      for (index, BodySymbolRef { sym_id, .. }) in body.syms.iter().enumerate() {
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
    Item {
      len: 0,
      body: BodyId::default(),
      off: 0,
      state,
      origin: OriginData::UNDEFINED,
    }
  }

  #[inline(always)]
  pub fn to_null(&self) -> Self {
    Item {
      len:    0,
      body:   BodyId::default(),
      off:    0,
      state:  self.state,
      origin: self.origin,
    }
  }

  /// Create an Item from a body_id and a grammar store. Returns
  /// None if the body_id does not match a stored body in the
  /// grammar.

  pub fn from_body(body_id: &BodyId, g: &GrammarStore) -> Option<Self> {
    g.bodies.get(body_id).map(|body| Item {
      body:   *body_id,
      len:    body.len as u8,
      off:    0,
      state:  ItemState::default(),
      origin: OriginData::UNDEFINED,
    })
  }

  pub fn at_end(&self) -> bool {
    self.off == self.len
  }

  pub fn at_start(&self) -> bool {
    self.off == 0
  }

  pub fn to_state(&self, state: ItemState) -> Item {
    Item {
      len: self.len,
      off: self.off,
      body: self.body,
      state,
      origin: self.origin,
    }
  }

  pub fn to_origin(&self, origin: OriginData) -> Self {
    Item {
      body: self.body,
      len: self.len,
      off: self.off,
      state: self.state,
      origin,
    }
  }

  pub fn to_last_sym(self) -> Self {
    Item {
      body:   self.body,
      len:    self.len,
      off:    if self.len > 0 { self.len - 1 } else { 0 },
      state:  self.state,
      origin: self.origin,
    }
  }

  pub fn to_start(&self) -> Item {
    Item {
      body:   self.body,
      len:    self.len,
      off:    0,
      state:  self.state,
      origin: self.origin,
    }
  }

  pub fn to_end(&self) -> Item {
    Item {
      body:   self.body,
      len:    self.len,
      off:    self.len,
      state:  self.state,
      origin: self.origin,
    }
  }

  pub fn to_zero_state(&self) -> Item {
    Item {
      body:   self.body,
      len:    self.len,
      off:    self.off,
      state:  ItemState::default(),
      origin: OriginData::UNDEFINED,
    }
  }

  pub fn increment(&self) -> Option<Item> {
    if !self.at_end() {
      Some(Item {
        len:    self.len,
        off:    self.off + 1,
        body:   self.body,
        state:  self.state.increment_depth(),
        origin: self.origin,
      })
    } else {
      None
    }
  }

  pub fn decrement(&self) -> Option<Item> {
    if !self.is_start() {
      Some(Item {
        len:    self.len,
        off:    self.off - 1,
        body:   self.body,
        state:  self.state,
        origin: self.origin,
      })
    } else {
      None
    }
  }

  pub fn is_start(&self) -> bool {
    self.off == 0
  }

  pub fn get_body(&self) -> BodyId {
    self.body
  }

  pub fn get_offset(&self) -> u32 {
    self.off as u32
  }

  pub fn get_state(&self) -> ItemState {
    self.state
  }

  pub fn get_origin(&self) -> OriginData {
    self.origin
  }

  pub fn get_length(&self) -> u32 {
    self.len as u32
  }

  pub fn get_hash(&self) -> u64 {
    let body_id = self.body.0;

    (body_id & 0xFFFF_FFFF_FFFF_FF00) | (self.off as u64)
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
    g.bodies.get(&self.get_body()).unwrap().prod
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
