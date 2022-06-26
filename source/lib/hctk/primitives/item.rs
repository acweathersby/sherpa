use std::fmt::Display;

use super::BodyId;
use super::BodySymbolRef;
use super::GrammarStore;
use super::ProductionId;
use super::SymbolID;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ItemState(u32);

impl ItemState
{
    const OUT_OF_SCOPE_FLAG: u32 = 0x7000;
    const OUT_OF_SCOPE_MASK: u32 = (!Self::OUT_OF_SCOPE_FLAG) & 0xFFFF;
    pub const OUT_OF_SCOPE_STATE: ItemState =
        ItemState::new(0, Self::OUT_OF_SCOPE_FLAG);

    pub const fn default() -> Self
    {
        ItemState(0)
    }

    pub const fn new(group: u32, depth: u32) -> Self
    {
        ItemState((group << 16) | (depth & 0xFFFF))
    }

    pub fn increment_depth(&self) -> Self
    {
        ItemState::new(self.get_group(), self.get_depth() + 1)
    }

    pub fn get_depth(&self) -> u32
    {
        self.0 & 0xFFFF
    }

    pub fn get_group(&self) -> u32
    {
        (self.0 >> 16) & 0xFFFF
    }

    pub fn get_closure_index(&self) -> usize
    {
        (self.get_group() & Self::OUT_OF_SCOPE_MASK) as usize
    }

    pub fn to_depth(&self, depth: u32) -> Self
    {
        ItemState::new(self.get_group(), depth)
    }

    pub fn to_group(&self, group: u32) -> Self
    {
        ItemState::new(group, self.get_depth())
    }

    pub fn is_out_of_scope(&self) -> bool
    {
        self.get_group() & Self::OUT_OF_SCOPE_FLAG > 0
    }
}

impl Display for ItemState
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        f.write_fmt(format_args!("<{},{}>", self.get_group(), self.get_depth()))
    }
}

/// Represents a specific point in a parse sequence
/// defined by a body and a positional offset that
/// indicates the next expected terminal or non-terminal.
#[repr(C, align(64))]
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash, PartialOrd, Ord)]

pub struct Item
{
    body:   BodyId,
    state:  ItemState,
    length: u8,
    offset: u8,
}

impl Item
{
    pub fn debug_string(&self, grammar: &GrammarStore) -> String
    {
        if self.is_null() {
            format!("{} null", self.state)
        } else {
            let body = grammar.bodies_table.get(&self.body).unwrap();

            let mut string = String::new();

            string += &format!("{} ", self.state);

            string +=
                &grammar.production_table.get(&body.production).unwrap().name;

            string += " =>";

            for (index, BodySymbolRef { sym_id, .. }) in
                body.symbols.iter().enumerate()
            {
                if index == self.offset as usize {
                    string += " •";
                }

                string += " ";

                string += &sym_id.to_string(grammar)
            }

            if self.at_end() {
                string += " •";
            }
            string
        }
    }

    //#[inline(always)]
    pub fn is_null(&self) -> bool
    {
        self.body.is_null() && self.length == 0 && self.offset == 0
    }

    pub fn is_out_of_scope(&self) -> bool
    {
        self.state.is_out_of_scope()
    }

    #[inline(always)]
    pub fn null(state: ItemState) -> Self
    {
        Item {
            length: 0,
            body: BodyId::default(),
            offset: 0,
            state,
        }
    }

    #[inline(always)]
    pub fn to_null(&self) -> Self
    {
        Item {
            length: 0,
            body:   BodyId::default(),
            offset: 0,
            state:  self.state,
        }
    }

    /// Create an Item from a body_id and a grammar store. Returns
    /// None if the body_id does not match a stored body in the
    /// grammar.

    pub fn from_body(body_id: &BodyId, grammar: &GrammarStore) -> Option<Self>
    {
        match grammar.bodies_table.get(&body_id) {
            Some(body) => Some(Item {
                body:   *body_id,
                length: body.length as u8,
                offset: 0,
                state:  ItemState::default(),
            }),
            _ => None,
        }
    }

    pub fn at_end(&self) -> bool
    {
        return self.offset == self.length;
    }

    pub fn to_state(&self, state: ItemState) -> Item
    {
        Item {
            length: self.length,
            offset: self.offset,
            body: self.body,
            state,
        }
    }

    pub fn to_last_sym(self) -> Self
    {
        Item {
            body:   self.body,
            length: self.length,
            offset: self.length - 1,
            state:  self.state,
        }
    }

    pub fn to_start(&self) -> Item
    {
        Item {
            body:   self.body,
            length: self.length,
            offset: 0,
            state:  self.state,
        }
    }

    pub fn to_end(&self) -> Item
    {
        Item {
            body:   self.body,
            length: self.length,
            offset: self.length,
            state:  self.state,
        }
    }

    pub fn to_zero_state(&self) -> Item
    {
        Item {
            body:   self.body,
            length: self.length,
            offset: self.offset,
            state:  ItemState::default(),
        }
    }

    pub fn increment(&self) -> Option<Item>
    {
        if !self.at_end() {
            Some(Item {
                length: self.length,
                offset: self.offset + 1,
                body:   self.body,
                state:  self.state.increment_depth(),
            })
        } else {
            None
        }
    }

    pub fn decrement(&self) -> Option<Item>
    {
        if !self.is_start() {
            Some(Item {
                length: self.length,
                offset: self.offset - 1,
                body:   self.body,
                state:  self.state,
            })
        } else {
            None
        }
    }

    pub fn is_start(&self) -> bool
    {
        self.offset == 0
    }

    pub fn get_body(&self) -> BodyId
    {
        self.body
    }

    pub fn get_offset(&self) -> u32
    {
        self.offset as u32
    }

    pub fn get_state(&self) -> ItemState
    {
        self.state
    }

    pub fn get_length(&self) -> u32
    {
        self.length as u32
    }

    pub fn get_hash(&self) -> u64
    {
        let body_id = self.body.0;

        (body_id & 0xFFFFFFFF_FFFFFF00) | (self.offset as u64)
    }

    pub fn get_symbol(&self, grammar: &GrammarStore) -> SymbolID
    {
        if self.at_end() {
            SymbolID::EndOfFile
        } else {
            match grammar.bodies_table.get(&self.body) {
                Some(body) => body.symbols[self.offset as usize].sym_id,
                _ => SymbolID::Undefined,
            }
        }
    }

    pub fn get_production_id_at_sym(
        &self,
        grammar: &GrammarStore,
    ) -> ProductionId
    {
        match self.get_symbol(grammar) {
            SymbolID::Production(production, _) => production,
            _ => ProductionId(0),
        }
    }

    pub fn is_end(&self) -> bool
    {
        self.length <= self.offset
    }

    pub fn is_term(&self, grammar: &GrammarStore) -> bool
    {
        if self.is_end() {
            false
        } else {
            match self.get_symbol(grammar) {
                SymbolID::Production(production, _) => false,
                _ => true,
            }
        }
    }

    pub fn is_nonterm(&self, grammar: &GrammarStore) -> bool
    {
        if self.is_end() {
            false
        } else {
            !self.is_term(grammar)
        }
    }

    pub fn get_production_id(&self, grammar: &GrammarStore) -> ProductionId
    {
        grammar
            .bodies_table
            .get(&self.get_body())
            .unwrap()
            .production
    }

    pub fn to_hash(&self) -> u64
    {
        ((self.body.0 & 0xFFFFFFF0_F000F000) ^ ((self.offset as u64) << 32))
            | (self.state.0 as u64)
    }
}
