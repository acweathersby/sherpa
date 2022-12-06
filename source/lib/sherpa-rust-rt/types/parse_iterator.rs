use super::*;
use crate::functions::get_next_action;

pub trait IteratorParser<T: BaseCharacterReader + MutCharacterReader> {
  fn get_parts(&mut self) -> (&mut T, &mut ParseContext<T>, &[u32], &mut bool);
}

pub struct ReferenceParseIterator<'a, T: BaseCharacterReader + MutCharacterReader> {
  ctx:      ParseContext<T>,
  reader:   T,
  bytecode: &'a [u32],
  active:   bool,
}

impl<'a, T: BaseCharacterReader + MutCharacterReader + MutCharacterReader> Iterator
  for ReferenceParseIterator<'a, T>
{
  type Item = ParseAction;

  fn next(&mut self) -> Option<Self::Item> {
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

impl<'a, T: BaseCharacterReader + MutCharacterReader> ReferenceParseIterator<'a, T> {
  pub fn new(reader: T, data: &'a [u32], entry_point: u32) -> Self {
    let mut state = ParseContext::bytecode_context();

    state.init_normal_state(entry_point);

    Self { ctx: state, reader, bytecode: data, active: true }
  }
}

impl<'a, T: BaseCharacterReader + MutCharacterReader> IteratorParser<T>
  for ReferenceParseIterator<'a, T>
{
  fn get_parts(&mut self) -> (&mut T, &mut ParseContext<T>, &[u32], &mut bool) {
    let ReferenceParseIterator { reader, ctx: state, bytecode: data, active, .. } = self;

    (reader, state, data, active)
  }
}
