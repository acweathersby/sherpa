use crate::parsers::error_recovery::create_token;

use super::*;

use std::{
  ops::Index,
  rc::{self, Rc},
  vec::Drain,
};
pub type ReducerNew<Input, Node> = fn(&Input, &AstStackSliceNew<AstSlotNew<Node>>);

/// Used within an LLVM parser to provide access to This intermediate AST
/// data stored on the stack within a dynamically resizable array.
#[repr(C)]
pub struct AstStackSliceNew<T: AstObjectNew> {
  stack_data:         *mut T,
  stack_size:         u32,
  stack_grows_upward: bool,
}

impl<T: AstObjectNew> AstStackSliceNew<T> {
  #[inline(always)]
  fn get_pointer(&self, position: usize) -> *mut T {
    #[cfg(debug_assertions)]
    if position >= (self.stack_size as usize) {
      panic!("Could not get AST node at slot ${} from stack with a length of {}", position, self.stack_size);
    }
    let slot_size: usize = std::mem::size_of::<T>();

    if self.stack_grows_upward {
      // We are using the stack space for these slots,
      // which we ASSUME grows downward, hence the "higher" slots
      // are accessed through lower addresses.
      (self.stack_data as usize - (position * slot_size)) as *mut T
    } else {
      (self.stack_data as usize + (position * slot_size)) as *mut T
    }
  }

  pub fn from_slice(slice: &mut [T], stack_grows_upward: bool) -> Self {
    Self {
      stack_data:         &mut slice[0],
      stack_size:         slice.len() as u32,
      stack_grows_upward: stack_grows_upward,
    }
  }

  /// Assigns the given data to a garbage slot, ignoring any existing value
  /// the slot may contain. This is only used when shifting token data into
  /// an "empty" slot through the Shift action.
  pub unsafe fn assign_to_garbage(&self, position: usize, val: T) {
    std::mem::forget(std::mem::replace(&mut (*self.get_pointer(position)), val));
  }

  pub fn assign(&self, position: usize, val: T) {
    unsafe {
      *self.get_pointer(position) = val;
    }
  }

  /// Removes the value at the given position from the slot and returns it.
  pub fn take(&self, position: usize) -> T {
    unsafe { std::mem::take(&mut (*self.get_pointer(position))) }
  }

  pub fn clone(&self, position: usize) -> T {
    unsafe { (*self.get_pointer(position)).clone() }
  }

  pub fn len(&self) -> usize {
    self.stack_size as usize
  }

  pub fn destroy(self) {
    self.to_vec();
  }

  pub fn to_vec(&self) -> Vec<T> {
    let mut output = vec![];
    for i in 0..self.stack_size {
      output.push(self.take(i as usize));
    }
    output
  }
}

#[cfg(not(debug_assertions))]
pub trait AstObjectNew: Clone + Default + Sized {}
#[cfg(not(debug_assertions))]
impl<T: Clone + Default + Sized> AstObjectNew for T {}
#[cfg(debug_assertions)]
pub trait AstObjectNew: std::fmt::Debug + Clone + Default + Sized {}
#[cfg(debug_assertions)]
impl<T: std::fmt::Debug + Clone + Default + Sized> AstObjectNew for T {}

#[derive(Clone, Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[repr(C)]
pub struct AstSlotNew<Ast: AstObjectNew>(pub Ast, pub TokenRange, pub TokenRange);

pub trait ASTConstructor<T: ParserInput>: ParserIterator<T> + ParserInitializer {
  fn stack_grows_upward(&self) -> bool {
    false
  }

  fn parse_ast<Node: AstObjectNew>(
    &mut self,
    input: &mut T,
    ctx: &mut ParserContext,
    reducers: &[ReducerNew<T, Node>],
  ) -> Result<AstSlotNew<Node>, ParseError> {
    let mut ast_stack: Vec<AstSlotNew<Node>> = vec![];

    while let Some(action) = self.next(input, ctx) {
      match action {
        ParseAction::Accept { .. } => {
          return Ok(ast_stack.pop().unwrap());
        }
        ParseAction::Reduce { rule_id, symbol_count, .. } => {
          let reduce_fn = reducers[rule_id as usize];
          let len = ast_stack.len();
          let count = symbol_count as usize;
          reduce_fn(input, &AstStackSliceNew::from_slice(&mut ast_stack[(len - count)..len], self.stack_grows_upward()));
          ast_stack.resize(len - (count - 1), AstSlotNew::<Node>::default());
        }
        ParseAction::Skip { .. } => {}
        ParseAction::Shift {
          byte_offset: token_byte_offset,
          byte_length: token_byte_length,
          token_line_offset,
          token_line_count,
          ..
        } => {
          let tok = TokenRange {
            len:      token_byte_length,
            off:      token_byte_offset,
            line_num: token_line_count,
            line_off: token_line_offset,
          };
          ast_stack.push(AstSlotNew(Node::default(), tok, Default::default()));
        }
        ParseAction::Error { .. } => {
          let mut last_input = ctx.current_tok();

          let mut start = last_input.off as usize;
          while start < input.len() && input.byte(start) == 32 {
            start += 1;
          }
          let mut end = start;
          while end < input.len() && input.byte(end) != 32 && input.byte(end) != 10 {
            end += 1;
          }

          return Err(ParseError::InputError {
            inline_message: "Unrecognized Token".into(),
            last_nonterminal: 0,
            loc: TokenRange {
              line_num: last_input.line_num,
              line_off: last_input.line_off,
              len:      (end - start) as u32,
              off:      start as u32,
            }
            .to_token_from_ref(input.get_owned_ref()),
            message: "Unrecognized Token".into(),
          });
        }
        _ => {
          return Err(ParseError::InputError {
            inline_message: Default::default(),
            last_nonterminal: 0,
            loc: Default::default(),
            message: "Unrecognized Token".into(),
          });
        }
      }
    }
    return Err(ParseError::Unexpected);
  }
}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> ASTConstructor<I> for T {}

impl<T: AstObjectNew> Index<usize> for AstStackSliceNew<T> {
  type Output = T;

  #[inline(always)]
  fn index(&self, index: usize) -> &Self::Output {
    #[cfg(debug_assertions)]
    if index > self.len() {
      panic!("Index {} out of bounds in an AstStackSlice of len {}", index, self.len());
    }

    unsafe { &*self.get_pointer(index) }
  }
}

#[test]
fn test_slots_from_slice() {
  let mut d = vec![1, 2, 3, 4, 5, 6, 7];
  let len = d.len();
  let slots = AstStackSliceNew::<_>::from_slice(&mut d[len - 3..len], true);

  assert_eq!(slots.take(0), 5);
  assert_eq!(slots.take(1), 6);
  assert_eq!(slots.take(2), 7);

  slots.assign(0, 55);

  drop(slots);

  d.resize(len - 2, Default::default());

  assert_eq!(d.last().cloned(), Some(55));
}

pub enum ASTBaseNode<ASTNode> {
  Token(Rc<TokenNode>),
  Tokens(Vec<Rc<TokenNode>>),
  Node(Box<ASTNode>),
  String(String),
  F64(f64),
  F32(f32),
  I64(i64),
  I32(i32),
  I16(i16),
  I8(i8),
  U64(u64),
  U32(u32),
  U16(u16),
  U8(u8),
  BOOL(bool),
  F32Vec(Vec<f32>),
  F64Vec(Vec<f64>),
  I64Vec(Vec<i64>),
  I32Vec(Vec<i32>),
  I16Vec(Vec<i16>),
  I8Vec(Vec<i8>),
  U64Vec(Vec<u64>),
  U32Vec(Vec<u32>),
  U16Vec(Vec<u16>),
  U8Vec(Vec<u8>),
}

type AstReducer<ASTNode> = fn(symbols: &mut dyn Iterator<Item = ASTBaseNode<ASTNode>>) -> ASTBaseNode<ASTNode>;

/// An object capable of instantiating a parser
pub trait ASTProducer<I: ParserInput, ASTNode>: ParserProducer<I> {
  fn get_reduce_functions(&self) -> &[&AstReducer<ASTNode>];

  fn parse_ast(&self, input: &mut I, entry: EntryPoint) -> Result<ASTBaseNode<ASTNode>, ParseError> {
    let reduce_functions = self.get_reduce_functions();
    let mut nodes: Vec<ASTBaseNode<ASTNode>> = Vec::with_capacity(128);
    let mut parser = self.get_parser()?;
    let mut ctx = parser.init(entry)?;

    while let Some(action) = parser.next(input, &mut ctx) {
      match action {
        ParseAction::Accept { .. } => {}
        ParseAction::Shift {
          byte_length: token_byte_length,
          byte_offset: token_byte_offset,
          token_id,
          ..
        } => {
          nodes.push(ASTBaseNode::Token(TokenNode::new(
            token_id,
            token_byte_length,
            token_byte_offset,
            input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize),
          )));
        }
        ParseAction::Reduce { rule_id: _rule_id, symbol_count, .. } => {
          let node = reduce_functions[_rule_id as usize](&mut nodes.drain((nodes.len() - symbol_count as usize)..));
          nodes.push(node);
        }
        ParseAction::Error { last_nonterminal, .. } => {
          let last_input = TokenRange {
            len:      ctx.tok_byte_len as u32,
            off:      ctx.sym_ptr as u32,
            line_num: 0,
            line_off: 0,
          };
          let token: Token = last_input.to_token_from_ref(input.get_owned_ref());
          return Err(ParseError::InputError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_nonterminal,
          });
        }
        _ => {}
      }
    }

    Ok(nodes.into_iter().next().expect("Should be len 1"))
  }
}
