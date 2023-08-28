use crate::{
  types::{
    ast::{AstObject, AstSlot, AstStackSlice},
    ByteReader,
    Goto,
    MutByteReader,
    ParseAction,
    ParseActionType,
    ParseContext,
    ParseResult,
    TokenRange,
    UTF8StringReader,
  },
  utf8::get_token_class_from_codepoint,
};
use std::{
  alloc::{alloc, dealloc, Layout},
  fmt::Debug,
};

#[no_mangle]
pub extern "C" fn sherpa_free_stack(ptr: *mut Goto, byte_size: usize) {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the
  // bytes size of the stack.
  let layout = Layout::from_size_align(byte_size, 16).unwrap();

  unsafe { dealloc(ptr as *mut u8, layout) }
}

#[no_mangle]
pub extern "C" fn sherpa_get_token_class_from_codepoint(codepoint: u32) -> u32 {
  get_token_class_from_codepoint(codepoint)
}

#[no_mangle]
pub extern "C" fn sherpa_allocate_stack(byte_size: usize) -> *mut Goto {
  // Each goto slot is 16bytes, so we shift left num_of_slots by 4 to get the
  // bytes size of the stack.

  let layout = Layout::from_size_align(byte_size, 16).unwrap();

  unsafe {
    let ptr = alloc(layout) as *mut Goto;

    ptr
  }
}

#[repr(C)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct InputInfo(pub *const u8, pub u32, pub bool);

pub trait LLVMByteReader {
  /// Get a pointer to a sequence of bytes that can be read from the input given
  /// the cursor position. The second tuple values should be the length bytes
  /// that  can be read from the block.
  extern "C" fn get_byte_block_at_cursor_old<T: ByteReader>(self_: &mut T, start_offset: u32, _end_offset: u32) -> InputInfo {
    let cursor = start_offset;
    let size = ((self_.len() as i64) - (cursor as i64)).max(0) as u32;

    if size > 0 {
      let ptr = ((self_.get_bytes().as_ptr() as usize) + cursor as usize) as *const u8;
      InputInfo(ptr, self_.len() as u32, false)
    } else {
      InputInfo(0 as *const u8, self_.len() as u32, false)
    }
  }

  extern "C" fn get_byte_block_at_cursor<T: ByteReader>(
    self_: *mut T,
    beg_ptr: &mut *const u8,
    anchor_ptr: &mut *const u8,
    base_ptr: &mut *const u8,
    head_ptr: &mut *const u8,
    scan_ptr: &mut *const u8,
    end_ptr_and_needed: &mut *const u8,
  ) -> bool {
    let anchor_offset = (*anchor_ptr as usize) - (*beg_ptr as usize);
    let head_delta = (*head_ptr as usize) - (*anchor_ptr as usize);
    let scan_delta = (*scan_ptr as usize) - (*anchor_ptr as usize);
    let base_delta = (*base_ptr as usize) - (*anchor_ptr as usize);
    let needed = *end_ptr_and_needed as usize;

    let _self_ = unsafe { self_.as_mut().unwrap() };

    let size = (((_self_.len() as i64) + 1) - ((anchor_offset + scan_delta + needed as usize) as i64)).max(0) as u32;

    if size > 0 {
      let beg = _self_.get_bytes().as_ptr();
      let anchor = beg as usize + anchor_offset;
      let head = (anchor + head_delta) as *const u8;
      let scan = (anchor + scan_delta) as *const u8;
      let base = (anchor + base_delta) as *const u8;
      let end = (beg as usize + _self_.len()) as *const u8;
      (*beg_ptr) = beg;
      (*anchor_ptr) = anchor as *const u8;
      (*base_ptr) = base;
      (*head_ptr) = head;
      (*scan_ptr) = scan;
      (*end_ptr_and_needed) = end;
    } else {
      let beg = _self_.get_bytes().as_ptr();
      let anchor = beg as usize + anchor_offset;
      let head = (anchor + head_delta) as *const u8;
      let base = (anchor + base_delta) as *const u8;
      let end = (beg as usize + _self_.len()) as *const u8;
      (*beg_ptr) = beg;
      (*anchor_ptr) = anchor as *const u8;
      (*base_ptr) = base;
      (*head_ptr) = head;
      (*scan_ptr) = end;
      (*end_ptr_and_needed) = end;
    }

    true
  }
}

impl<T: ByteReader + LLVMByteReader, M> Debug for ParseContext<T, M> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut dbgstr = f.debug_struct("ParseContext");
    dbgstr.field("anchor_ptr", &self.anchor_ptr);
    dbgstr.field("scan_ptr", &self.scan_ptr);
    dbgstr.field("tok_len", &self.tok_len);
    dbgstr.field("head_ptr", &self.head_ptr);
    dbgstr.field("base_ptr", &self.base_ptr);
    dbgstr.field("end_ptr", &self.end_ptr);
    dbgstr.field("nterm", &self.nterm);
    dbgstr.field("begin_ptr", &self.begin_ptr);
    dbgstr.field("end_line_num", &self.end_line_num);
    dbgstr.field("start_line_num", &self.start_line_num);
    dbgstr.field("chkp_line_num", &self.chkp_line_num);
    dbgstr.field("chkp_line_off", &self.chkp_line_off);
    dbgstr.field("end_line_off", &self.end_line_off);
    dbgstr.field("start_line_off", &self.start_line_off);
    dbgstr.field("state", &self.state);
    dbgstr.field("tok_id", &self.tok_id);
    dbgstr.field("sym_len", &self.sym_len);
    dbgstr.field("rule_id", &self.rule_id);
    dbgstr.field("goto_size", &self.goto_size);
    dbgstr.field("goto_free", &self.goto_free);
    dbgstr.field("line_incr", &self.line_incr);
    dbgstr.field("state", &self.state);
    dbgstr.field("in_peek_mode", &self.in_peek_mode);
    dbgstr.field("is_active", &self.is_active);
    dbgstr.field("goto_stack_ptr", &self.goto_stack_ptr);
    dbgstr.field("goto_size", &self.goto_size);
    dbgstr.field("goto_used", &self.goto_free);
    dbgstr.field("get_input_info", &"FN Pointer".to_string());
    dbgstr.field("reader", &((self.reader) as usize).to_string());
    dbgstr.field("meta_ctx", &self.meta_ctx);
    dbgstr.field("custom_lex", &"FN Pointer".to_string());
    dbgstr.finish()
  }
}
impl<'a> LLVMByteReader for UTF8StringReader<'a> {}

impl<T: ByteReader + LLVMByteReader, M> ParseContext<T, M> {
  pub fn new_llvm() -> Self {
    Self {
      get_input_info: T::get_byte_block_at_cursor,
      custom_lex: Self::default_custom_lex,
      ..Default::default()
    }
  }
}

impl AstObject for u32 {}

impl<V: AstObject> AstObject for (V, TokenRange, TokenRange) {}

pub unsafe fn llvm_map_result_action<'a, T: LLVMByteReader + ByteReader + MutByteReader, M, Node: AstObject>(
  _ctx: &ParseContext<T, M>,
  action: ParseActionType,
  slots: &mut AstStackSlice<AstSlot<Node>>,
) -> ParseResult<Node> {
  match action {
    ParseActionType::Accept => ParseResult::Complete(slots.take(0)),
    ParseActionType::Error => {
      let vec = slots.to_vec();
      let last_input = vec.last().cloned().unwrap_or_default().1;
      ParseResult::Error(last_input, vec)
    }
    ParseActionType::NeedMoreInput => ParseResult::NeedMoreInput(slots.to_vec()),
    _ => unreachable!("This function should only be called when the parse action is  [Error, Accept, or EndOfInput]"),
  }
}

pub unsafe fn llvm_map_shift_action<'a, R: LLVMByteReader + ByteReader + MutByteReader, ExtCTX, ASTNode: AstObject>(
  ctx: &ParseContext<R, ExtCTX>,
  slots: &mut AstStackSlice<AstSlot<ASTNode>>,
) {
  let ParseAction::Shift {
    token_byte_offset,
    token_byte_length,
    token_line_offset,
    token_line_count,
    ..
  } = ctx.get_shift_data()
  else {
    unreachable!()
  };

  let tok = TokenRange {
    len:      token_byte_length,
    off:      token_byte_offset,
    line_num: token_line_count,
    line_off: token_line_offset,
  };

  slots.assign_to_garbage(0, AstSlot(ASTNode::default(), tok, Default::default()));
}
