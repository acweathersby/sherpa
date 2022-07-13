//! This following is a list of registers that a reserved for s&pecific purposes:&
//! ### While in recognizer mode states:
//! - r15 - stores the state metadata
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
use crate::writer::x86_64_writer::X8664Writer;

pub fn _undefined<W: Write, T: X8664Writer<W>>(
  _grammar: &GrammarStore,
  _bytecode: &[u32],
  _writer: &mut T,
) -> Result<()>
{
  Ok(())
}

const parse_context_size: usize = std::mem::size_of::<ParseContext<UTF8StringReader>>();

const stack_ref_size: usize = std::mem::size_of::<Vec<u8>>();

const token_size: usize = std::mem::size_of::<ParseToken>();

pub fn write_preamble<W: Write, T: X8664Writer<W>>(
  grammar: &GrammarStore,
  writer: &mut T,
) -> Result<()>
{
  writer.write_internal(
    "
%struct.Token = type {
  i32, i32,
  i32, i32,
  i32, i32,
  i32, i32
}

%struct.Context = type {
  [3 x %struct.Token]
}

%struct.Action = type { i32, i32 }

define void @construct_context ( %struct.Context* %ctx ) {
  ret void
}

define void @next ( %struct.Context* %ctx, %struct.Action* %action ) 
alwaysinline
{
  
  %type = getelementptr inbounds %struct.Action, %struct.Action* %action, i64 0, i32 0

  store i32 7, i32* %type

  ret void
}

define void @destroy_context ( %struct.Context* %ctx ) {
  ret void
}


define void @prime_context ( %struct.Context* %ctx ) {
  ret void
}
"
    .as_bytes(),
  )?;

  Ok(())
}

fn write_emit_shift<W: Write, T: X8664Writer<W>>(writer: &mut T) -> Result<&mut T>
{
  Ok(writer)
}

fn write_emit_reduce<W: Write, T: X8664Writer<W>>(writer: &mut T) -> Result<&mut T>
{
  Ok(writer)
}

/// Set our parse view the cursor position defined in `rsi` so that we
/// can select read enough bytes to satisfy the view length requirements
/// of the current state
fn update_block_data<W: Write, T: X8664Writer<W>>(writer: &mut T) -> Result<&mut T>
{
  Ok(writer)
}

fn restore_context_external<W: Write, T: X8664Writer<W>>(writer: &mut T)
  -> Result<&mut T>
{
  Ok(writer)
}

fn save_context_external<W: Write, T: X8664Writer<W>>(writer: &mut T) -> Result<&mut T>
{
  Ok(writer)
}

fn save_context_internal<W: Write, T: X8664Writer<W>>(writer: &mut T) -> Result<&mut T>
{
  Ok(writer)
}
fn restore_context_internal<W: Write, T: X8664Writer<W>>(writer: &mut T)
  -> Result<&mut T>
{
  Ok(writer)
}

pub fn write_state<W: Write, T: X8664Writer<W>>(
  build_options: &BuildOptions,
  output: &BytecodeOutput,
  writer: &mut T,
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

  let mut name = String::new();

  if address >= bytecode.len() {
    return Ok((bytecode.len(), name));
  }

  if let Some((asm_state_name, ir_state_name)) = if predefined_name.is_some() {
    Some((predefined_name.unwrap().clone(), String::new()))
  } else if let Some(name) = offset_to_state_name.get(&(address as u32)) {
    Some((create_named_state_label(name), name.clone()))
  } else {
    None
  } {
    writer.label(&format!("{}", asm_state_name), false)?;

    name = ir_state_name.clone();

    if let Some(state) = output.ir_states.get(&ir_state_name) {
      match state.get_type() {
        IRStateType::ProductionStart
        | IRStateType::ScannerStart
        | IRStateType::ProductionGoto
        | IRStateType::ScannerGoto => {
          println!("{} {}", asm_state_name, ir_state_name);
          // TODO right checker for handling stack expansion.
          if state.get_stack_depth() > 0 {
            let needed_size = state.get_stack_depth() * 2 * 8;
            write_extend_stack_checker(writer, needed_size)?;
          }
        }
        _ => {}
      }

      is_scanner = state.is_scanner();
    }

    while address < bytecode.len() {
      match bytecode[address] & INSTRUCTION_HEADER_MASK {
        INSTRUCTION::I00_PASS => {
          break;
        }

        INSTRUCTION::I01_CONSUME => {
          address += 1;
        }

        INSTRUCTION::I02_GOTO => {
          address += 1;
        }

        INSTRUCTION::I03_SET_PROD => {
          address += 1;
        }

        INSTRUCTION::I04_REDUCE => {
          address += 1;
        }

        INSTRUCTION::I05_TOKEN => {
          address += 1;
        }

        INSTRUCTION::I06_FORK_TO => {
          break;
        }

        INSTRUCTION::I07_SCAN => {}

        INSTRUCTION::I08_NOOP => {}

        INSTRUCTION::I09_VECTOR_BRANCH | INSTRUCTION::I10_HASH_BRANCH => {}

        INSTRUCTION::I11_SET_FAIL_STATE => {}

        INSTRUCTION::I12_REPEAT => {}

        INSTRUCTION::I13_NOOP => {}

        INSTRUCTION::I14_ASSERT_CONSUME => {}
        INSTRUCTION::I15_FAIL => {
          break;
        }
        _ => {
          address += 1;
          writer.code("nop")?;
        }
      }
    }
  } else {
    writer.code("nop")?;
    address += 1;
  }

  Ok((address, name))
}

fn create_table_skip_label(table_name: &String) -> String
{
  format!("t_{}_skip", table_name)
}

fn create_table_branch_label(table_name: &String, address: &usize) -> String
{
  format!("t_{}_{}", table_name, address)
}

fn write_default_table_jumps<'a, W: Write, T: X8664Writer<W>>(
  data: &BranchTableData,
  writer: &'a mut T,
  table_name: &String,
) -> Result<&'a mut T>
{
  Ok(writer)
}

fn write_extend_stack_checker<W: Write, T: X8664Writer<W>>(
  writer: &mut T,
  needed_size: u32,
) -> Result<&mut T>
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

pub fn compile_from_bytecode<W: Write, T: X8664Writer<W>>(
  build_options: &BuildOptions,
  output: &BytecodeOutput,
  writer: &mut T,
) -> Result<()>
{
  write_preamble(output.grammar, writer)?;

  let mut offset = FIRST_STATE_ADDRESS as usize;

  let mut addresses = output
    .ir_states
    .values()
    .filter(|s| !s.is_scanner())
    .map(|s| output.state_name_to_offset.get(&s.get_name()).unwrap())
    .cloned()
    .collect::<VecDeque<_>>();

  // let mut seen = BTreeSet::new();

  // while let Some(address) = addresses.pop_front() {
  // if (seen.insert(address)) {
  // eprintln!("{:X}", address);
  // let mut referenced_addresses = Vec::new();
  //
  // write_state(
  // build_options,
  // output,
  // writer,
  // address as usize,
  // None,
  // false,
  // &mut referenced_addresses,
  // )?;
  //
  // for address in referenced_addresses {
  // addresses.push_front(address);
  // }
  // }
  // }
  Ok(())
}
