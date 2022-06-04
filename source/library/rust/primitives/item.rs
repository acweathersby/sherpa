use std::collections::HashMap;

///
/// Represents a specific point in a parse sequence
/// defined by a body and a positional offset that
/// indicates the next expected terminal or non-terminal.
///
#[repr(C, align(64))]
struct Item {
    length: u8,
    offset: u8,
    body: u16,
    state: u32,
}

impl Item {
    fn new(body: u16, length: u8, offset: u8, state: u32) -> Item {
        Item {
            body,
            length,
            offset,
            state,
        }
    }

    fn from_body(body: u16, length: u8) -> Item {
        Item {
            body,
            length,
            offset: 0,
            state: 0,
        }
    }

    fn at_end(&self) -> bool {
        return self.offset == self.length;
    }

    fn to_state(&self, state: u32) -> Item {
        Item {
            length: self.length + 1,
            offset: self.offset,
            body: self.body,
            state,
        }
    }

    fn increment(&self) -> Option<Item> {
        if self.offset < self.length {
            Some(Item {
                length: self.length + 1,
                offset: self.offset,
                body: self.body,
                state: self.state,
            })
        } else {
            None
        }
    }

    fn get_body(&self) -> u32 {
        self.body as u32
    }

    fn get_offset(&self) -> u32 {
        self.offset as u32
    }

    fn get_state(&self) -> u32 {
        self.state as u32
    }

    fn get_length(&self) -> u32 {
        self.length as u32
    }
}

type ItemLookupTable = HashMap<String, Item>;
