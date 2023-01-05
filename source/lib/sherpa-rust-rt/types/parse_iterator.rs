use super::*;
use crate::functions::get_next_action;

pub trait IteratorParser<R: LLVMByteReader + ByteReader + MutByteReader, UserCTX> {
  fn get_parts(
    &mut self,
  ) -> (&mut R, &mut LLVMParseContext<R, UserCTX>, &mut Vec<u32>, &[u32], &mut bool);
}

pub struct ReferenceParseIterator<'a, R: LLVMByteReader + ByteReader + MutByteReader, UserCTX> {
  ctx:      LLVMParseContext<R, UserCTX>,
  stack:    Vec<u32>,
  reader:   R,
  bytecode: &'a [u32],
  active:   bool,
}

impl<'a, R: LLVMByteReader + ByteReader + MutByteReader, UserCTX> Iterator
  for ReferenceParseIterator<'a, R, UserCTX>
{
  type Item = ParseAction;

  fn next(&mut self) -> Option<Self::Item> {
    let (reader, ctx, stack, bytecode, active) = self.get_parts();

    if *active {
      let action = get_next_action(reader, ctx, stack, bytecode);
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

impl<'a, R: LLVMByteReader + ByteReader + MutByteReader, UserCTX>
  ReferenceParseIterator<'a, R, UserCTX>
{
  pub fn new(reader: R, data: &'a [u32], entry_point: u32) -> Self {
    let mut state = LLVMParseContext::new();

    let stack = vec![0, entry_point | NORMAL_STATE_FLAG];

    Self { ctx: state, reader, bytecode: data, active: true, stack }
  }
}
impl<'a, R: LLVMByteReader + ByteReader + MutByteReader, UserCTX> IteratorParser<R, UserCTX>
  for ReferenceParseIterator<'a, R, UserCTX>
{
  fn get_parts(
    &mut self,
  ) -> (&mut R, &mut LLVMParseContext<R, UserCTX>, &mut Vec<u32>, &[u32], &mut bool) {
    let ReferenceParseIterator { reader, ctx, bytecode: data, stack, active, .. } = self;

    (reader, ctx, stack, data, active)
  }
}
