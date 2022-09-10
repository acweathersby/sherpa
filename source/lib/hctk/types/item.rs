use std::fmt::Display;

use super::BodyId;
use super::BodySymbolRef;
use super::GrammarStore;
use super::ProductionId;
use super::SymbolID;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ItemState(u32);

impl ItemState {
  const OUT_OF_SCOPE_FLAG: u32 = 0x7000;
  const OUT_OF_SCOPE_MASK: u32 = (!Self::OUT_OF_SCOPE_FLAG) & 0xFFFF;
  pub const OUT_OF_SCOPE_STATE: ItemState = ItemState::new(Self::OUT_OF_SCOPE_FLAG, 0);

  pub const fn default() -> Self {
    ItemState(0)
  }

  pub const fn new(group: u32, depth: u32) -> Self {
    ItemState((group << 16) | (depth & 0xFFFF))
  }

  pub fn increment_depth(&self) -> Self {
    ItemState::new(self.get_group(), self.get_depth() + 1)
  }

  pub fn get_depth(&self) -> u32 {
    self.0 & 0xFFFF
  }

  pub fn get_group(&self) -> u32 {
    (self.0 >> 16) & 0xFFFF
  }

  pub fn get_closure_index(&self) -> usize {
    (self.get_group() & Self::OUT_OF_SCOPE_MASK) as usize
  }

  pub fn to_depth(&self, depth: u32) -> Self {
    ItemState::new(self.get_group(), depth)
  }

  pub fn to_group(&self, group: u32) -> Self {
    ItemState::new(group, self.get_depth())
  }

  pub fn is_out_of_scope(&self) -> bool {
    (self.get_group() & Self::OUT_OF_SCOPE_FLAG) > 0
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

      // string += &format!("\n  {:?} ", self.origin);

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
    self.state.is_out_of_scope()
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
      off:    self.len - 1,
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

  pub fn get_production_id_at_sym(&self, g: &GrammarStore) -> ProductionId {
    match self.get_symbol(g) {
      SymbolID::Production(prod, _) => prod,
      _ => ProductionId(0),
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
      eprintln!("{}", body.syms[self.off as usize].tok.blame(1, 1, ""));
    }
  }
}
