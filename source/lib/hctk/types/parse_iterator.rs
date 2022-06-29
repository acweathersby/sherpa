use crate::runtime::parser::get_next_action;
use crate::types::ParseAction;

use crate::types::ParseState;

use crate::types::SymbolReader;

pub trait IteratorParser<T: SymbolReader>
{
    fn get_parts(&mut self) -> (&mut T, &mut ParseState, &[u32], &mut bool);
}

pub struct ReferenceParseIterator<'a, T: SymbolReader>
{
    state:    ParseState,
    reader:   T,
    bytecode: &'a [u32],
    active:   bool,
}

impl<'a, T: SymbolReader> Iterator for ReferenceParseIterator<'a, T>
{
    type Item = ParseAction;

    fn next(&mut self) -> Option<Self::Item>
    {
        let (reader, state, bytecode, active) = self.get_parts();

        if *active {
            let action = get_next_action(reader, state, bytecode);
            match action {
                ParseAction::Error { .. } | ParseAction::Accept { .. } => {
                    *active = false;
                    Some(action)
                }
                action => Some(action),
            }
        } else {
            None
        }
    }
}

impl<'a, T: SymbolReader> ReferenceParseIterator<'a, T>
{
    pub fn new(reader: T, data: &'a [u32], entry_point: u32) -> Self
    {
        let mut state = ParseState::new();

        state.init_normal_state(entry_point);

        Self {
            state,
            reader,
            bytecode: data,
            active: true,
        }
    }
}

impl<'a, T: SymbolReader> IteratorParser<T> for ReferenceParseIterator<'a, T>
{
    fn get_parts(&mut self) -> (&mut T, &mut ParseState, &[u32], &mut bool)
    {
        let ReferenceParseIterator {
            reader,
            state,
            bytecode: data,
            active,
            ..
        } = self;

        (reader, state, data, active)
    }
}
