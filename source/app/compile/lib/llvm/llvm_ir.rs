//! This following is a list of registers that a reserved for s&pecific purposes:&
//! ### While in recognizer mode states:
//! - r15 &- stores the state metadata
//! - rbx - stores the address of the [parser context](hctk::types::ASMParserContext)
//! - rbp - stores the address of the [reader](hctk::types::CharacterReader)
//! ### While in scanner mode states:
//! Same as above, with the additional registers:
//! - rdx - stores packed character data. see [hctk::types::SymbolReader::get_type_info]
//! - r12 - stores token offset data: byte offset in high 32, and codepoint offset in lower 32
//! - r13 - stores token length data: byte length in high 32, and codepoint length in lower 32
//! - r14 - stores accepted token offset data

use core::num;
use hctk::bytecode::BytecodeOutput;
use hctk::debug::grammar;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::VecDeque;
use std::io::Result;
use std::io::Write;

use hctk::grammar::get_exported_productions;
use hctk::grammar::ExportedProduction;
use hctk::types::*;

use crate::builder::table::BranchData;
use crate::builder::table::BranchTableData;
use crate::builder::table::TableType;
use crate::options::BuildOptions;
use crate::writer::code_writer::CodeWriter;
use crate::writer::x86_64_writer::X8664Writer;

pub fn _undefined<W: Write>(
  _grammar: &GrammarStore,
  _bytecode: &[u32],
  _writer: &mut CodeWriter<W>,
) -> Result<()>
{
  Ok(())
}

const parse_context_size: usize = std::mem::size_of::<ParseContext<UTF8StringReader>>();

const stack_ref_size: usize = std::mem::size_of::<Vec<u8>>();

const token_size: usize = std::mem::size_of::<ParseToken>();

pub fn write_preamble<W: Write>(
  writer: &mut CodeWriter<W>,
  output: &BytecodeOutput,
) -> Result<()>
{
  writer.wrt(
    "
%s.Token = type {
  i64, ; Offset   [byte, codepoint] 
  i64, ; Length   [byte, codepoint] 
  i64, ; Type     [garbage, codepoint] 
  i64  ; Line     [#, last_offset] 
}

%getInputBlock = type i64 (  i64 *, i8 **, i64, i64 ) *


%s.Context = type {
  [3 x %s.Token],   ; 0 
  %s.Action *,      ; 1
  %s.Goto *,        ; 2 - Stack Base
  %s.Goto *,        ; 3 - Stack Top
  i64,              ; 4 - Stack Size
  i64,              ; 5 - Active State
  i64,              ; 6
  i8 *,             ; 7 - Input Block
  i64,              ; 8 - Input Block Length
  i64,              ; 9 - Input Block Offset
  i64 *,            ; 10 - Reader 
  %getInputBlock,   ; 11 - Input block request
  i1,               ; 12 - InPeekState
  %s.Goto           ; 13 - Default Goto Space
}

%fn.GT = type i32 ( %s.Context* )

%fn.Goto = type i32 ( %s.Context* ) *

%s.Goto = type {
  i64, %fn.Goto
}

%s.Action = type { i32, i32 }

%s.Action.Undefined = type { 
  i32 ; action_type=0
}

%s.Action.CompleteState = type { 
  i32 ; action_type=1 
}

%s.Action.FailState = type { 
  i32 ; action_type=2 
}

%s.Action.Scanner = type { 
  i32 ; action_type=3
}

%s.Action.Fork = type { 
  i32 ; action_type=4
}

%s.Action.Shift = type { 
  i32,      ; action_type=5
  %s.Token, ; skip symbols
  %s.Token  ; shift token
}

%s.Action.Reduce = type { 
  i32, ; action_type=6
  i32, ; padding
  i32, ; production_id 
  i32, ; body_id
  i32  ; symbol_count
}

%s.Action.Accept = type { 
  i32, ; action_type=7
  i32, ; padding
  i32 ; production_id 
}

; Common functions

define void @construct_context ( %s.Context* %ctx ) {

  %default_goto = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 13

  %base_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 2
  store  %s.Goto * %default_goto, %s.Goto ** %base_ptr

  %top_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 3
  
  store volatile %s.Goto * %default_goto, %s.Goto ** %top_ptr

  ret void
}

define void @emitAccept ( %s.Context* %ctx ) {
  
  %action_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 1
  %action = load %s.Action * , %s.Action ** %action_ptr
  %sa_ptr = bitcast %s.Action * %action to %s.Action.Accept *

  %sa1 = load %s.Action.Accept, %s.Action.Accept* %sa_ptr

  ; Set skip and shift tokens

  %sa2 = insertvalue %s.Action.Accept %sa1, i32 7, 0 ; Sets the type to Acceot
  %sa3 = insertvalue %s.Action.Accept %sa2, i32 1, 1 ; Set the skip characters token=

  store volatile %s.Action.Accept %sa3, %s.Action.Accept* %sa_ptr
  
  ret void
}

define i32 @emitReduce ( %s.Context* %ctx, i32 %production_id, i32 %body_id, i32 %symbol_count ) {
  
  %action_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 1
  %action = load %s.Action * , %s.Action ** %action_ptr
  %sa_ptr = bitcast %s.Action * %action to %s.Action.Reduce *

  %sa1 = load %s.Action.Reduce, %s.Action.Reduce* %sa_ptr

  ; Set skip and shift tokens

  %sa2 = insertvalue %s.Action.Reduce %sa1, i32 6, 0 ; Sets the type to Reduce
  %sa3 = insertvalue %s.Action.Reduce %sa2, i32 %production_id, 2 ; Set the skip characters token
  %sa4 = insertvalue %s.Action.Reduce %sa3, i32 %body_id, 3 ; Set the shift token
  %sa5 = insertvalue %s.Action.Reduce %sa4, i32 %symbol_count, 4 ; Set the shift token

  store volatile %s.Action.Reduce %sa5, %s.Action.Reduce* %sa_ptr
  
  ret i32 1
}

define i32 @emitShift ( %s.Context* %ctx ) {
  
  %action_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 1
  %action = load volatile %s.Action * , %s.Action ** %action_ptr
  %sa_ptr = bitcast %s.Action * %action to %s.Action.Shift *

  %tk_anch_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 0, i64 0
  %tk_anch = load %s.Token, %s.Token * %tk_anch_ptr

  %tk_asrt_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 0, i64 1
  %tk_asrt = load %s.Token, %s.Token * %tk_asrt_ptr

  %sa1 = load volatile %s.Action.Shift, %s.Action.Shift* %sa_ptr

  ; The length of the skip token is equal to the tokens offset minus the 
  ; assert token's offset

  %tk_asrt_off = extractvalue %s.Token %tk_asrt, 0 
  %tk_anch_off = extractvalue %s.Token %tk_anch, 0 
  %tk_anch_len = sub i64 %tk_asrt_off, %tk_anch_off
  %tk_anch2 = insertvalue %s.Token %tk_anch, i64 %tk_anch_len, 1

  ; Set skip and shift tokens

  %sa2 = insertvalue %s.Action.Shift %sa1, i32 5, 0 ; Sets the type to Shift
  %sa3 = insertvalue %s.Action.Shift %sa2, %s.Token %tk_anch2, 1 ; Set the skip characters token
  %sa4 = insertvalue %s.Action.Shift %sa3, %s.Token %tk_asrt, 2 ; Set the shift token

  store volatile %s.Action.Shift %sa4, %s.Action.Shift* %sa_ptr

  ; Set new offset of assert and increment tokens

  %tk_asrt_len = extractvalue %s.Token %tk_asrt, 1
  %tk_asrt_off2 = add i64 %tk_asrt_off, %tk_asrt_len
  %tk_asrt2 = insertvalue %s.Token %tk_asrt, i64 %tk_asrt_off2, 0 
  %tk_asrt3 = insertvalue %s.Token %tk_asrt2, i64 0, 2 ; Set token type to 0

  store volatile %s.Token %tk_asrt3, %s.Token * %tk_anch_ptr
  store volatile %s.Token %tk_asrt3, %s.Token * %tk_asrt_ptr
  
  ret i32 1
}

define fastcc i64 @getAdjustedInputPtrInteger( %s.Context* %ctx_ptr, %s.Token * %tok, i64 %requested_size ) {

  %block_offset1 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 9
  %block_offset = load volatile i64, i64 * %block_offset1

  %block_size1 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 8
  %block_size = load volatile i64, i64 * %block_size1

  ; get the difference between the current offset position and the end of the current 
  ; block
  %token_offset = call i64 @readTokenByteOffset( %s.Token * %tok )
  %needed_size1 = add i64 %token_offset, %requested_size
  %needed_size = sub i64 %needed_size1, %block_offset

  %c1 = icmp ugt i64 %block_size, %needed_size
  br i1 %c1, label %HaveSpace, label %AttemptExtend

AttemptExtend:


  %getInputBlock_fn1 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 11
  %getInputBlock_fn = load %getInputBlock, %getInputBlock * %getInputBlock_fn1

  %input_reader1 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 10
  %input_reader = load volatile i64 *, i64 ** %input_reader1

  %input_store1 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 7

  %new_block_size = call i64 %getInputBlock_fn ( i64 * %input_reader, i8 ** %input_store1, i64 %token_offset,  i64 %requested_size )

  %c2 = icmp eq i64 0, %new_block_size
  br i1 %c2, label %ExtensionFailed, label %ExtensionSuccess

ExtensionFailed:

  ret i64 0

ExtensionSuccess: 

  store i64 %token_offset, i64 * %block_offset1
  store i64 %new_block_size, i64 * %block_size1
  
  br label %HaveSpace

HaveSpace:

  %block_offset3 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 9
  %block_offset2 = load i64, i64 * %block_offset3
  
  %input_ptr1 = getelementptr inbounds %s.Context, %s.Context* %ctx_ptr, i64 0, i32 7
  %input_ptr = load i8*, i8** %input_ptr1

  %extension = sub i64 %token_offset, %block_offset2
  
  %input_int = ptrtoint i8 * %input_ptr to i64
  %input_adjusted = add i64 %input_int, %extension

  ret i64 %input_adjusted
}

define i8 @readi8( %s.Context* %ctx, %s.Token * %tok, i64 %input_adjusted ) alwaysinline {

  %input_adjusted_ptr = inttoptr i64 %input_adjusted to i8 *

  %read_value = load volatile i8, i8 * %input_adjusted_ptr

  ret i8 %read_value
}

define i32 @readi32( %s.Context* %ctx, %s.Token * %tok, i64 %input_adjusted ) alwaysinline {

  %input_adjusted_ptr = inttoptr i64 %input_adjusted to i32 *

  %read_value = load volatile i32, i32 * %input_adjusted_ptr

  ret i32 %read_value
}

define i64 @readi64( %s.Context* %ctx, %s.Token * %tok, i64 %input_adjusted ) alwaysinline {

  %input_adjusted_ptr = inttoptr i64 %input_adjusted to i64 *

  %read_value = load i64, i64 * %input_adjusted_ptr

  ret i64 %read_value
}


define i1 * @getIsPeekPtr( %s.Context* %ctx ) alwaysinline {

  %peek_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 12

  ret i1 * %peek_ptr
}

define %s.Token * @getAnchorTokPtr( %s.Context* %ctx ) alwaysinline {
    
  %tok_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 0, i32 0

    ret %s.Token * %tok_ptr
}

define %s.Token * @getAssertTokPtr( %s.Context* %ctx ) alwaysinline {
  
  %tok_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 0, i32 1
  
  ret %s.Token * %tok_ptr
}

define %s.Token * @getPeekTokPtr( %s.Context* %ctx ) alwaysinline {
  
  %tok_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 0, i32 2
  
  ret %s.Token * %tok_ptr
}

define %s.Action * @getActionPtr( %s.Context* %ctx ) alwaysinline {
  
  %action_store = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 1

  %action_ptr = load volatile %s.Action *, %s.Action ** %action_store
  
  ret %s.Action * %action_ptr
}

define i64 * @getParseState( %s.Context* %ctx ) alwaysinline {
  
  %prod_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 5
  
  ret i64 * %prod_ptr
}

define i64 @readTokenLength( %s.Token* %tok_ptr ) alwaysinline {

  %length_ptr = getelementptr inbounds %s.Token, %s.Token* %tok_ptr, i64 0, i32 1

  %length = load volatile i64, i64 * %length_ptr

  ret i64 %length
}

define void @writeTokenLength( %s.Token* %tok_ptr, i64 %length ) alwaysinline {

  %length_ptr = getelementptr inbounds %s.Token, %s.Token* %tok_ptr, i64 0, i32 1

  store volatile i64 %length, i64 * %length_ptr

  ret void
}

define i64 @readTokenOffset( %s.Token* %tok_ptr ) alwaysinline {

  %offset_ptr = getelementptr inbounds %s.Token, %s.Token* %tok_ptr, i64 0, i32 0

  %offset =  load volatile i64, i64 * %offset_ptr

  ret i64 %offset
}

define i64 @readTokenByteOffset( %s.Token* %tok_ptr ) alwaysinline {

  %offset_ptr = getelementptr inbounds %s.Token, %s.Token* %tok_ptr, i64 0, i32 0

  %offset = load i64, i64 * %offset_ptr

  %offset_adjusted = lshr i64 %offset, 32

  ret i64 %offset_adjusted
}

define void @writeTokenOffset( %s.Token* %tok_ptr, i64 %offset ) alwaysinline {
  
  %offset_ptr = getelementptr inbounds %s.Token, %s.Token* %tok_ptr, i64 0, i32 0

  store volatile i64 %offset, i64 * %offset_ptr

  ret void
}

define void @writeTokenType( %s.Token* %tok_ptr, i64 %type ) alwaysinline {
  
  %type_ptr = getelementptr inbounds %s.Token, %s.Token* %tok_ptr, i64 0, i32 2

  store volatile i64 %type, i64 * %type_ptr

  ret void
}



define void @setFailState ( %s.Context* %ctx ) {

  ; TODO

  ret void
}

define void @setPassState ( %s.Context* %ctx ) {

  ; TODO

  ret void
}

define void @ensureStackHasCapacity( %s.Context* %ctx, i64 %needed_capacity ) alwaysinline {

  ; TODO

  ret void
}

")?.wrtln(&format!("
define void @next ( %s.Context* %ctx, %s.Action* %action ) 
alwaysinline
{{
  
  ; store action pointer into context 
  %action_store = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 1
  store %s.Action* %action, %s.Action** %action_store
  
  br label %Dispatch

Dispatch:

  %gt = call %s.Goto @pop_state( %s.Context* %ctx )
  
  %gt_state = extractvalue %s.Goto %gt, 0

  %cond1 = icmp ne i64 %gt_state, 0
  br i1 %cond1, label %NonEmptyState, label %Quit

NonEmptyState:
  %ctx_state_ptr = call i64 * @getParseState( %s.Context* %ctx )
  %parse_state = load i64, i64 * %ctx_state_ptr

  %1 = and i64 %parse_state, {0}
  %2 = and i64 %gt_state, {0}

  %cond2 = icmp eq i64 %1, %2
  br i1 %cond2, label %ModeAppropriateState, label %Dispatch

ModeAppropriateState:

  %gt_fn = extractvalue %s.Goto %gt, 1
  
  %should_emit = call fastcc inreg i32 %gt_fn( %s.Context* %ctx )

  %cond3 = icmp eq i32 %should_emit, 1
  br i1 %cond3, label %Emit, label %Dispatch

Emit:
  ret void

Quit:
  call void @emitAccept( %s.Context* %ctx )
  ret void
}}
", STATE_MODE_MASK))?.wrtln("
define %s.Goto @pop_state( %s.Context* %ctx ) alwaysinline {
  
  ; Get top of stack, decrement, and get the value at the decremented stack
  
  %gt_top_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 3
  
  %gt_top_ptr1 = load %s.Goto *, %s.Goto ** %gt_top_ptr
  
  %gt_top_ptr_less = getelementptr inbounds %s.Goto, %s.Goto * %gt_top_ptr1, i64 -1
  
  store %s.Goto * %gt_top_ptr_less, %s.Goto ** %gt_top_ptr

  %gt = load volatile %s.Goto, %s.Goto * %gt_top_ptr_less

  ret %s.Goto %gt
}


define void @push_state( %s.Context* %ctx, i64 %state, %fn.Goto %gt_fn_ptr ) alwaysinline {

  %gt_val1 = insertvalue %s.Goto undef, i64 %state, 0
  %gt_val2 = insertvalue %s.Goto %gt_val1, %fn.Goto %gt_fn_ptr, 1

  %gt_top_ptr = getelementptr inbounds %s.Context, %s.Context* %ctx, i64 0, i32 3
  
  %gt_top_ptr1 = load %s.Goto *, %s.Goto ** %gt_top_ptr

  store volatile %s.Goto %gt_val2, %s.Goto * %gt_top_ptr1
  
  %gt_top_ptr_more = getelementptr inbounds %s.Goto, %s.Goto * %gt_top_ptr1, i64 1
  
  store volatile %s.Goto * %gt_top_ptr_more, %s.Goto ** %gt_top_ptr

  ret void
}

define void @destroy_context (  ) {
  ret void
}",
  )?;

  Ok(())
}

fn write_state_init<'a, W: Write>(
  writer: &'a mut CodeWriter<W>,
  output: &BytecodeOutput,
) -> Result<&'a mut CodeWriter<W>>
{
  writer
    .wrtln("define void @prime_context( %s.Context* %ctx, i32 %initial_state ) alwaysinline cold { ")?
    .indent();

  // start points
  let sp = get_exported_productions(output.grammar)
    .iter()
    .enumerate()
    .map(|(i, p)| {
      let address = *(output.state_name_to_offset.get(p.guid_name).unwrap());
      let name = create_offset_label(address as usize);
      (i, address, name)
    })
    .collect::<Vec<_>>();

  writer.wrtln(&format!(
    "call void @push_state ( %s.Context* %ctx,  i64 0, %fn.Goto @emitShift ) ",
  ))?;

  writer.wrtln(&format!(
    "switch i32 %initial_state, label %init_{} [ {} ]",
    sp[0].2,
    sp.iter()
      .map(|p| { format!("i32 {}, label %init_{}", p.0, sp[0].2) })
      .collect::<Vec<_>>()
      .join("\n")
  ))?;

  for (_, (_, _, label)) in sp.iter().map(|p| (p.1, p)).collect::<BTreeMap<_, _>>() {
    writer
      .dedent()
      .newline()?
      .wrtln(&format!("init_{}:", label))?
      .indent()
      .wrtln(&format!(
        "call void @push_state ( %s.Context* %ctx,  i64 {}, %fn.Goto @fn.{} ) ",
        NORMAL_STATE_MASK, label
      ))?;
  }

  writer
    .newline()?
    .wrtln("ret void")?
    .dedent()
    .wrtln("}\n\n")?;

  Ok(writer)
}

fn write_emit_shift<W: Write>(writer: &mut CodeWriter<W>) -> Result<&mut CodeWriter<W>>
{
  Ok(writer)
}

fn write_emit_reduce<W: Write>(writer: &mut CodeWriter<W>) -> Result<&mut CodeWriter<W>>
{
  Ok(writer)
}

/// Set our parse view the cursor position defined in `rsi` so that we
/// can select read enough bytes to satisfy the view length requirements
/// of the current state
fn update_block_data<W: Write>(writer: &mut CodeWriter<W>) -> Result<&mut CodeWriter<W>>
{
  Ok(writer)
}

pub fn write_state<W: Write>(
  build_options: &BuildOptions,
  output: &BytecodeOutput,
  writer: &mut CodeWriter<W>,
  mut address: usize,
  predefined_name: Option<&String>,
  mut is_scanner: bool,
  referenced: &mut Vec<u32>,
) -> Result<(usize, String)>
{
  let BytecodeOutput {
    bytecode,
    offset_to_state_name,
    ..
  } = output;

  // let mut name = String::new();

  if address >= bytecode.len() {
    return Ok((bytecode.len(), "".to_string()));
  }

  if let Some(ir_state_name) = offset_to_state_name.get(&(address as u32)) {
    if let Some(state) = output.ir_states.get(ir_state_name) {
      match state.get_type() {
        IRStateType::ProductionStart
        | IRStateType::ScannerStart
        | IRStateType::ProductionGoto
        | IRStateType::ScannerGoto => {
          // TODO right checker for handling stack expansion.
          let needed_size = state.get_stack_depth() * 2 * 8;
          if state.get_stack_depth() > 0 {
            writer.wrtln(&format!(
              "call void @ensureStackHasCapacity( %s.Context* %ctx, i64 {} )",
              needed_size
            ))?;
          }
        }
        _ => {}
      }

      is_scanner = state.is_scanner();
    }
  }

  while address < bytecode.len() {
    match bytecode[address] & INSTRUCTION_HEADER_MASK {
      INSTRUCTION::I00_PASS => {
        writer.wrtln("ret i32 0")?;
        break;
      }

      INSTRUCTION::I01_CONSUME => {
        write_emit_reentrance(bytecode, address + 1, writer, referenced)?;

        writer
          .wrtln(";  I01_CONSUME")?
          .wrtln(&format!(
            "%val{:X} = call i32 @emitShift( %s.Context* %ctx )",
            address
          ))?
          .wrtln(&format!("ret i32 %val{:X}", address))?;

        break;
      }

      INSTRUCTION::I02_GOTO => {
        let goto_offset = bytecode[address] & GOTO_STATE_ADDRESS_MASK;
        let name = create_offset_label(goto_offset as usize);

        referenced.push(goto_offset);

        if bytecode[address + 1] & INSTRUCTION_HEADER_MASK == INSTRUCTION::I00_PASS {
          // Simply perform a jump to the applicable code
          // skipping the pass instruction entirely
          writer
            .wrtln(&format!("%val = call i32 @fn.{} ( %s.Context* %ctx ) ", name))?
            .wrtln("ret i32 %val")?;
          address += 2;
          break;
        } else {
          writer.wrtln(&format!(
            "call void @push_state( %s.Context* %ctx, i64 {} , %fn.Goto @fn.{} )",
            NORMAL_STATE_MASK, &name
          ))?;
        }
        address += 1;
      }

      INSTRUCTION::I03_SET_PROD => {
        writer.wrtln(";  I03_SET_PROD")?;
        address += 1;
      }

      INSTRUCTION::I04_REDUCE => {
        let instruction = bytecode[address];
        let symbol_count = instruction >> 16 & 0x0FFF;
        let body_id = instruction & 0xFFFF;

        write_emit_reentrance(bytecode, address + 1, writer, referenced)?;

        writer
          .wrtln(";  I04_REDUCE")?
          .wrtln(&format!(
            "%val{:X} = call i32 @emitReduce ( %s.Context* %ctx, i32 {}, i32 {}, i32 {}  )",
            address, 0,  body_id, symbol_count
          ))?
          .wrtln(&format!("ret i32 %val{:X}", address))?;

        break;
      }

      INSTRUCTION::I05_TOKEN => {
        writer.wrtln(";  I05_TOKEN")?;
        address += 1;
      }

      INSTRUCTION::I06_FORK_TO => {
        writer.wrtln(";  I06_FORK_TO")?;
        break;
      }

      INSTRUCTION::I07_SCAN => {
        writer.wrtln(";  I08_NOOP")?;
        address += 1;
      }

      INSTRUCTION::I08_NOOP => {
        writer.wrtln(";  I08_NOOP")?;
        address += 1;
      }

      INSTRUCTION::I09_VECTOR_BRANCH | INSTRUCTION::I10_HASH_BRANCH => {
        if let Some(data) = BranchTableData::from_bytecode(address, output) {
          let table_name = create_offset_label(address + 800000);

          let TableHeaderData {
            input_type,
            lexer_type,
            scanner_address,
            ..
          } = data.data;
          let branches = &data.branches;

          match input_type {
            INPUT_TYPE::T02_TOKEN => {
              if (lexer_type == LEXER_TYPE::ASSERT) {
                writer
                  .wrtln(
                    "%tok_ptr = call %s.Token * @getAssertTokPtr( %s.Context* %ctx ) alwaysinline",
                  )?
                  .wrtln("%is_peek_ptr = call i1 * @getIsPeekPtr( %s.Context* %ctx ) alwaysinline")?
                  .wrtln("store volatile i1 0, i1 * %is_peek_ptr")?;
              } else {
                writer
                    .wrtln(
                      "%tok_ptr = call %s.Token * @getPeekTokPtr( %s.Context* %ctx )",
                    )?
                    .wrtln("%is_peek_ptr = call i1 * @getIsPeekPtr( %s.Context* %ctx )")?
                    .wrtln("%is_peek = load i1, i1 * %is_peek_ptr")?
                    .wrtln("%cond1 = icmp eq i1 %is_peek, i1 1")?
                    .wrtln(&format!(
                      "br i1 %cond1, label %{0}_AlreadyPeeking, label %{0}_NotPeeking",
                      table_name
                    ))?
                    .dedent()
                    .wrtln(&format!("%{0}_AlreadyPeeking", table_name))?
                    .wrtln(
                      "%prev_tok_ptr = call %s.Token * @getPeekTokPtr( %s.Context* %ctx )",
                    )?
                    .indent()
                    .wrtln(&format!("br label %{}_Dispatch", table_name))?
                    .dedent()
                    .wrtln(&format!("%{0}_NotPeeking", table_name))?
                    .wrtln(
                      "%prev_tok_ptr = call %s.Token * @getAssertTokPtr( %s.Context* %ctx )",
                    )?
                    .dedent()
                    .wrtln(&format!("%{}_Dispatch", table_name))?
                    .indent()
                    .wrtln(
                      "%prev_length = call i64 @readTokenLength( %s.Token * %prev_tok_ptr ) ",
                    )?
                    .wrtln(
                      "%curr_offset = call i64 @readTokenOffset( %s.Token * %tok_ptr ) ",
                    )?
                    .wrtln("%new_token_offset = add i64 %prev_length, i64 %curr_offset")?
                    .wrtln(
                      "call void @writeTokenOffset( %s.Token * %tok_ptr, i64 %new_token_offset ) ",
                    )?
                    .wrtln(
                    "call void @writeTokenLength( %s.Token * %tok_ptr, 0 ) ",
                    )?
                    .wrtln("store i1 1, i1 * %peek_ptr")?;
              };
              if data.has_trivial_comparisons() {
                fn string_to_byte_num_and_mask(
                  string: &str,
                  sym: &Symbol,
                ) -> (usize, usize)
                {
                  string.as_bytes().iter().enumerate().fold(
                    (0, 0),
                    |(val, mask), (i, v)| {
                      let shift_amount = 8 * i;
                      (
                        val | ((*v as usize) << shift_amount),
                        mask | (0xFF << shift_amount),
                      )
                    },
                  )
                }

                writer
                  .wrtln(&format!("br label %{}", table_name))?
                  .dedent()
                  .newline()?
                  .wrtln(&format!("{}:", table_name))?
                  .indent();

                let branches = data
                  .branches
                  .iter()
                  .map(|(address, branch)| {
                    let sym = data.get_branch_symbol(branch).unwrap();
                    let string = match sym.guid {
                      id if id.isDefinedSymbol() => {
                        vec![output
                          .grammar
                          .symbols_string_table
                          .get(&id)
                          .unwrap()
                          .as_str()]
                      }
                      SymbolID::GenericSpace => {
                        vec![" "]
                      }
                      _ => vec![""],
                    };
                    (address, branch, string)
                  })
                  .collect::<Vec<_>>();

                let max_length = *branches
                  .iter()
                  .flat_map(|s| s.2.iter().map(|s| s.len()))
                  .collect::<BTreeSet<_>>()
                  .last()
                  .unwrap();

                writer.wrtln(
                    &format!("%adj_input = call i64 @getAdjustedInputPtrInteger( %s.Context* %ctx, %s.Token * %tok_ptr, i64 {} )", max_length)
                  )?;

                for (address, branch, strings) in &branches {
                  let sym = data.get_branch_symbol(branch).unwrap();

                  let next_branch_label = format!("next_{}_{}", table_name, address);
                  let this_branch_label = format!("curr_{}_{}", table_name, address);

                  let branch_name = if branch.is_skipped {
                    create_table_skip_label(&table_name)
                  } else {
                    create_table_branch_label(&table_name, address)
                  };

                  for string in strings {
                    match sym.byte_length {
                      len if len == 1 => {
                        writer
                          .wrtln(&format!(
                            " %input_adjusted_ptr = inttoptr i64 %adj_input to i8 *
                          %val{} = load volatile i8, i8 * %input_adjusted_ptr",
                            address
                          ))?
                          //.wrtln(&format!("%val{} = call i8 @readi8( %s.Context* %ctx, %s.Token * %tok_ptr, i64 %adj_input ) ", address))?
                          .wrtln(&format!(
                            "%cond{0:X} = icmp eq i8 %val{0}, {1}",
                            address,
                            string_to_byte_num_and_mask(string, sym).0
                          ))?;
                      }
                      len if len <= 8 => {
                        let (byte_string, mask) =
                          string_to_byte_num_and_mask(string, sym);
                        writer
                          .wrtln(&format!(
                            " %input_adjusted_ptr{0} = inttoptr i64 %adj_input to i64 *
                              %val{0} = load volatile i64, i64 * %input_adjusted_ptr{0}",
                            address
                          ))?
                          //  .wrtln(&format!("%val{} = call i64 @readi64( %s.Context* %ctx, %s.Token * %tok_ptr, i64 %adj_input ) ", address))?
                          .wrtln(&format!(
                            "%masked_val{0} = and i64 %val{0}, {1} ",
                            address, mask
                          ))?
                          .wrtln(&format!(
                            "%cond{0:X} = icmp eq i64 %masked_val{0}, {1}",
                            address, byte_string
                          ))?;
                      }
                      len if len <= 4 => {}
                      len if len <= 16 => {}
                      len if len <= 32 => {}
                      _ => {}
                    }
                    writer
                      .wrtln(&format!(
                        "br i1 %cond{:X}, label %{}, label %{}",
                        address, this_branch_label, next_branch_label,
                      ))?
                      .dedent()
                      .newline()?
                      .wrtln(&format!("{}:", this_branch_label))?
                      .indent()
                      .wrtln(&format!(
                        "call void @writeTokenLength( %s.Token * %tok_ptr, i64 {} ) alwaysinline",
                        ((sym.byte_length as usize)
                          | ((sym.code_point_length as usize) << 32))
                      ))?
                      .wrtln(&format!(
                        "call void @writeTokenType( %s.Token * %tok_ptr, i64 {} ) alwaysinline",
                        sym.bytecode_id
                      ))?
                      // 9*
                      .wrtln(&format!("br label %{}", branch_name))?
                      .dedent()
                      .newline()?
                      .wrtln(&format!("{}:", next_branch_label))?
                      .indent();
                  }
                }
                writer.wrtln(&format!("br label %{}_default", table_name))?;
              } else {
                // referenced.push(scanner_address);
                //
                // let scan_state = output
                // .offset_to_state_name
                // .get(&(scanner_address as u32))
                // .unwrap();
                //
                // if (lexer_type == LEXER_TYPE::ASSERT) {
                // writer
                // .comment_line(
                // "Bypass the token scanner if token is already typed.",
                // )?
                // .code("mov eax, [r14 + tok_type]")?
                // .code("cmp eax, 0")?
                // .code("jne .cached")?;
                // }
                //
                // writer
                // .code(&format!(
                // "lea r13, [rel {}]",
                // create_named_state_label(scan_state)
                // ))?
                // .code("lea r12, [rel .cached]")?
                // .code("jmp scan_handler")?
                // .label("cached", true)?;
                //
                // write_default_table_jumps(&data, writer, &table_name)?;
              }
            }
            INPUT_TYPE::T01_PRODUCTION => {
              // writer
              // .comment_line("get production value")?
              // .code("mov rax, r15")?
              // .code("and rax, PRODUCTION_META_MASK")?;
              //
              // write_default_table_jumps(&data, writer, &table_name)?;
            }
            _ => {
              // match input_type {
              // INPUT_TYPE::T05_BYTE => {
              // writer
              // .code("mov r13, 0x0000000100000001")?
              // .comment_line("Load the byte data in the low 8 bits")?
              // .code("mov eax, edx")?
              // .code("and eax, 0xFF")?;
              // }
              // INPUT_TYPE::T03_CLASS => {
              // writer
              // .code("mov r13, 0x0000010000000000")?
              // .code("mov r13w, dx")?
              // .code("shr r13, 8")?
              // .comment_line("Load the class data in the high 16 bits")?
              // .code("mov eax, edx")?
              // .code("shr rax, 16")?;
              // }
              // INPUT_TYPE::T04_CODEPOINT => {
              // writer
              // .code("mov r13, 0x0000010000000000")?
              // .code("mov r13w, dx")?
              // .code("shr r13, 8")?
              // .comment_line("Load the codepoint data in the high 32 bits")?
              // .code("mov rax, rdx")?
              // .code("shr rax, 32")?;
              // }
              // _ => {}
              // };
              //
              // write_default_table_jumps(&data, writer, &table_name)?;
            }
          }

          // Write branches, ending with the default branch.

          let mut skip_written = false;

          for (address, is_skip) in branches
            .values()
            .map(|p| (p.address, p.is_skipped))
            .collect::<BTreeSet<_>>()
            .iter()
          {
            if *is_skip {
              if skip_written {
                continue;
              }
              skip_written = true;
              writer
                .dedent()
                .newline()?
                .wrtln(&format!("skip_{}:", table_name))?
                .indent()
                .wrtln(
                  "%curr_length = call i64 @readTokenLength( %s.Token * %tok_ptr ) ",
                )?
                .wrtln(
                  "%curr_offset = call i64 @readTokenOffset( %s.Token * %tok_ptr ) ",
                )?
                .wrtln("%nto = add i64 %curr_length, %curr_offset")?
                .wrtln("call void @writeTokenOffset( %s.Token * %tok_ptr, i64 %nto ) ")?
                .wrtln("call void @writeTokenLength( %s.Token * %tok_ptr, i64 0 ) ")?
                .wrtln(&format!("br label %{}", table_name))?;
            } else {
              writer
                .dedent()
                .newline()?
                .wrtln(&format!("{}:", &create_table_branch_label(&table_name, address)))?
                .indent();

              write_state(
                build_options,
                output,
                writer,
                *address,
                Some(&create_table_branch_label(&table_name, address)),
                is_scanner,
                referenced,
              )?;
            }
          }
          writer
            .dedent()
            .newline()?
            .wrtln(&format!("{}:", &format!("{}_default", table_name)))?
            .indent();

          address = write_state(
            build_options,
            output,
            writer,
            (bytecode[address + 3] as usize) + address,
            Some(&format!("{}_default", table_name)),
            is_scanner,
            referenced,
          )?
          .0;
          break;
        } else {
          return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Invalid branch data",
          ));
        }
      }

      INSTRUCTION::I11_SET_FAIL_STATE => {
        writer.wrtln(";  I11_SET_FAIL_STATE")?;
        address += 1;
      }

      INSTRUCTION::I12_REPEAT => {
        writer.wrtln(";  I12_REPEAT")?;
        address += 1;
      }

      INSTRUCTION::I13_NOOP => {
        writer.wrtln(";  I13_NOOP")?;
        address += 1;
      }

      INSTRUCTION::I14_ASSERT_CONSUME => {
        writer.wrtln(";  I14_ASSERT_CONSUME")?;
        address += 1;
      }
      INSTRUCTION::I15_FAIL => {
        writer.wrtln("call void @setFailState ( %s.Context* %ctx ) ")?;
        writer.wrtln("ret i32 0")?;
        break;
      }
      _ => {
        address += 1;
        writer.wrtln(";  NOOP")?;
      }
    }
  }

  Ok((address, "".to_string()))
}

fn write_emit_reentrance<'a, W: Write>(
  bytecode: &Vec<u32>,
  address: usize,
  writer: &mut CodeWriter<W>,
  referenced: &mut Vec<u32>,
) -> Result<()>
{
  let next_address = match (bytecode[address] & INSTRUCTION_HEADER_MASK) {
    INSTRUCTION::I00_PASS => 0,
    INSTRUCTION::I02_GOTO => {
      if bytecode[address + 1] & INSTRUCTION_HEADER_MASK == INSTRUCTION::I00_PASS {
        (bytecode[address] & GOTO_STATE_ADDRESS_MASK) as usize
      } else {
        address
      }
    }
    _ => address,
  };
  if next_address != 0 {
    let name = create_offset_label(next_address);
    writer.wrtln(&format!(
      "call void @push_state( %s.Context* %ctx, i64 {} , %fn.Goto @fn.{} )",
      NORMAL_STATE_MASK, &name
    ))?;
    referenced.push(next_address as u32);
  }
  Ok(())
}

fn create_table_skip_label(table_name: &String) -> String
{
  format!("skip_{}", table_name)
}

fn create_table_branch_label(table_name: &String, address: &usize) -> String
{
  format!("t_{}_{}", table_name, address)
}

fn write_default_table_jumps<'a, W: Write>(
  data: &BranchTableData,
  writer: &'a mut CodeWriter<W>,
  table_name: &String,
) -> Result<&'a mut CodeWriter<W>>
{
  Ok(writer)
}

fn write_extend_stack_checker<W: Write>(
  writer: &mut CodeWriter<W>,
  needed_size: u32,
) -> Result<&mut CodeWriter<W>>
{
  Ok(writer)
}

fn create_named_state_label(name: &String) -> String
{
  format!("state_{}", name)
}

fn create_offset_label(offset: usize) -> String
{
  format!("off_{:X}", offset)
}

pub fn compile_from_bytecode<W: Write>(
  build_options: &BuildOptions,
  output: &BytecodeOutput,
  writer: &mut CodeWriter<W>,
) -> Result<()>
{
  write_preamble(writer, output)?;

  let mut offset = FIRST_STATE_ADDRESS as usize;

  let mut addresses = output
    .ir_states
    .values()
    .filter(|s| !s.is_scanner())
    .map(|s| output.state_name_to_offset.get(&s.get_name()).unwrap())
    .cloned()
    .collect::<VecDeque<_>>();

  let mut seen = BTreeSet::new();

  while let Some(address) = addresses.pop_front() {
    if (seen.insert(address)) {
      eprintln!("{:X}", address);
      let mut referenced_addresses = Vec::new();

      let name = create_offset_label(address as usize);

      writer
        .wrtln(&format!(
          "\ndefine internal i32 @fn.{} ( %s.Context* %ctx ) noinline optnone {{",
          name
        ))?
        .indent();

      write_state(
        build_options,
        output,
        writer,
        address as usize,
        None,
        false,
        &mut referenced_addresses,
      )?;

      writer.dedent().wrtln("}")?;

      for address in referenced_addresses {
        addresses.push_front(address);
      }
    }
  }

  write_state_init(writer, output)?;

  Ok(())
}
