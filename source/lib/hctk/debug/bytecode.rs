use core::num;
use std::any::Any;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt::format;

use crate::bytecode::BytecodeOutput;
use crate::grammar;
use crate::types::*;

pub fn header(idx: usize) -> String {
  format!("{}| ", address_string(idx))
}

pub fn address_string(idx: usize) -> String {
  format!("{:0>6X}", idx)
}

pub struct BytecodeGrammarLookups<'a> {
  pub bc_to_prod:   BTreeMap<u32, &'a Production>,
  pub bc_to_body:   BTreeMap<u32, &'a Body>,
  pub bc_to_symbol: BTreeMap<u32, &'a Symbol>,
}

impl<'a> BytecodeGrammarLookups<'a> {
  pub fn new(g: &'a GrammarStore) -> Self {
    let bc_to_prod =
      g.productions.iter().map(|(_, p)| (p.bytecode_id, p)).collect::<BTreeMap<_, _>>();

    let bc_to_body = g.bodies.iter().map(|(_, p)| (p.bc_id, p)).collect::<BTreeMap<_, _>>();

    let bc_to_symbol =
      g.symbols.iter().map(|(_, p)| (p.bytecode_id, p)).collect::<BTreeMap<_, _>>();

    BytecodeGrammarLookups { bc_to_prod, bc_to_body, bc_to_symbol }
  }
}
//

pub fn disassemble_state(
  bc: &[u32],
  idx: usize,
  lu: Option<&BytecodeGrammarLookups>,
) -> (String, usize) {
  use super::disassemble_state as ds;
  use super::header as dh;

  let so = idx;

  if idx >= bc.len() {
    ("".to_string(), so)
  } else {
    let instruction = bc[idx] & INSTRUCTION_CONTENT_MASK;
    match bc[idx] & INSTRUCTION_HEADER_MASK {
      INSTRUCTION::I00_PASS => (format!("\n{}PASS", dh(so)), so + 1),
      INSTRUCTION::I01_CONSUME => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}SHFT", dh(so)) + &string, offset + 1)
      }
      INSTRUCTION::I02_GOTO => {
        let (string, offset) = ds(bc, so + 1, lu);
        if instruction & FAIL_STATE_FLAG > 0 {
          (
            format!(
              "\n{}RCVR {}",
              dh(so),
              address_string((bc[so] & GOTO_STATE_ADDRESS_MASK) as usize)
            ) + &string,
            offset,
          )
        } else {
          (
            format!(
              "\n{}GOTO {}",
              dh(so),
              address_string((bc[so] & GOTO_STATE_ADDRESS_MASK) as usize)
            ) + &string,
            offset,
          )
        }
      }
      INSTRUCTION::I03_SET_PROD => {
        let production_id = instruction & INSTRUCTION_CONTENT_MASK;
        let (string, offset) = ds(bc, so + 1, lu);

        if let Some(lu) = lu {
          let name = &lu.bc_to_prod.get(&production_id).unwrap().guid_name;
          (format!("\n{}PROD SET TO {}     // {}", dh(so), production_id, name,) + &string, offset)
        } else {
          (format!("\n{}PROD SET TO {}", dh(so), production_id,) + &string, offset)
        }
      }
      INSTRUCTION::I04_REDUCE => {
        let (string, offset) = ds(bc, so + 1, lu);
        let symbol_count = instruction >> 16 & 0x0FFF;
        let body_id = instruction & 0xFFFF;

        if symbol_count == 0xFFF {
          (format!("\n{}REDU accumulated symbols to {}", dh(so), body_id) + &string, offset)
        } else {
          let pluralized = if symbol_count == 1 { "SYMBOL" } else { "SYMBOLS" };
          (
            format!("\n{}REDU {} {} TO {}", dh(so), symbol_count, pluralized, body_id) + &string,
            offset,
          )
        }
      }
      INSTRUCTION::I05_TOKEN => {
        let (string, offset) = ds(bc, so + 1, lu);
        if (instruction & TOKEN_ASSIGN_FLAG) > 0 {
          (format!("\n{}TOKN ASSIGN TO {}", dh(so), instruction & 0x00FF_FFFF) + &string, offset)
        } else {
          (format!("\n{}TOKV", dh(so)) + &string, offset)
        }
      }
      INSTRUCTION::I06_FORK_TO => {
        let target_production = instruction & 0xFFFF;
        let num_of_states = (instruction >> 16) & 0xFFFF;
        let end = (so + 1 + num_of_states as usize);
        let (string, offset) = ds(bc, end, lu);
        let mut state_strings = vec![];

        for offset in (so + 1)..end {
          state_strings.push(format!(
            "{} -- FORK TO {}",
            dh(offset),
            address_string((bc[offset] & GOTO_STATE_ADDRESS_MASK) as usize,),
          ));
        }

        (
          format!(
            "\n{}FORK TO COMPLETE {}\n{}",
            dh(so),
            target_production,
            state_strings.join("\n"),
          ) + &string,
          offset,
        )
      }
      INSTRUCTION::I07_SCAN => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}SCAN", dh(so)) + &string, offset)
      }
      INSTRUCTION::I08_NOOP => (format!("\n{}NOOP", dh(so)), so + 1),
      INSTRUCTION::I09_VECTOR_BRANCH => generate_table_string(
        bc,
        so,
        lu,
        "VECT",
        |states: &[u32], table_entry_offset: usize, state_offset: usize| {
          (
            states[table_entry_offset] as usize,
            (table_entry_offset - (4 + state_offset)) as u32 + (states[state_offset + 2] & 0xFFFF),
            states[table_entry_offset] == 0xFFFF_FFFF,
            0,
          )
        },
      ),
      INSTRUCTION::I10_HASH_BRANCH => generate_table_string(
        bc,
        so,
        lu,
        "HASH",
        |states: &[u32], table_entry_offset: usize, state_offset: usize| {
          (
            (states[table_entry_offset] >> 11 & 0x7FF) as usize,
            (states[table_entry_offset] & 0x7FF),
            (states[table_entry_offset] >> 11 & 0x7FF) == 0x7FF,
            ((states[table_entry_offset] >> 22) & 0x3FF) as i64 - 512,
          )
        },
      ),
      INSTRUCTION::I11_SET_FAIL_STATE => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}FSET", dh(so)) + &string, offset)
      }
      INSTRUCTION::I12_REPEAT => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}REPT", dh(so)) + &string, offset)
      }
      INSTRUCTION::I13_NOOP => (format!("\n{}NOOP", dh(so)), so + 1),
      INSTRUCTION::I14_ASSERT_CONSUME => (format!("\n{}ASTC", dh(so)), so + 1),
      INSTRUCTION::I15_FAIL => (format!("\n{}FAIL", dh(so)), so + 1),
      _ => (format!("\n{}UNDF", dh(so)), so + 1),
    }
  }
}

type GetOffsetTokenIdPair =
  fn(states: &[u32], table_entry_offset: usize, state_offset: usize) -> (usize, u32, bool, i64);

pub fn generate_table_string(
  bc: &[u32],
  idx: usize,
  lu: Option<&BytecodeGrammarLookups>,
  table_name: &str,
  get_offset_token_id_pair: GetOffsetTokenIdPair,
) -> (String, usize) {
  let states = bc;
  let instruction = states[idx];
  let scanner_pointer = states[idx + 1];
  let table_len = states[idx + 2] >> 16 & 0xFFFF;
  let table_meta = states[idx + 2] & 0xFFFF;
  let input_type = (instruction >> 22) & 0x7;
  let mut strings = vec![];
  let default_offset = states[idx + 3] as usize;
  let mut delta_offsets = BTreeSet::new();

  for entry_offset in (4 + idx)..(idx + 4 + table_len as usize) {
    let (delta_offset, token_id, IS_SKIP, meta) =
      get_offset_token_id_pair(states, entry_offset, idx);
    let goto_offset = delta_offset + idx;
    if delta_offset == default_offset {
      strings.push(create_failure_entry(entry_offset, goto_offset));
    } else if IS_SKIP {
      strings.push(create_skip_entry(lu, token_id, input_type, entry_offset, meta));
    } else {
      delta_offsets.insert(delta_offset);
      strings.push(create_normal_entry(lu, token_id, input_type, entry_offset, goto_offset, meta));
    }
  }

  for delta_offset in delta_offsets {
    strings.push(disassemble_state(bc, idx + delta_offset, lu).0);
  }

  let (default_string, offset) = disassemble_state(bc, idx + default_offset, lu);

  let string =
    format!("\n{}{} JUMP | TYPE {}", header(idx), table_name, input_type_to_name(input_type))
      + &(if scanner_pointer > 0 {
        format!("\n{}SCANNER OFFSET {}", header(idx + 1), address_string(scanner_pointer as usize))
      } else {
        format!("\n{}NO SCANNER", header(idx + 1))
      })
      + &format!("\n{}LENGTH: {} META: {}", header(idx + 2), table_len, table_meta)
      + &create_failure_entry(idx + 3, idx + default_offset)
      + &strings.join("")
      + &default_string;
  (string, offset)
}

fn create_failure_entry(entry_offset: usize, goto_offset: usize) -> String {
  format!("\n{}---- JUMP TO {} ON FAIL", header(entry_offset), address_string(goto_offset))
}
fn create_normal_entry(
  lu: Option<&BytecodeGrammarLookups>,
  token_id: u32,
  input_type: u32,
  idx: usize,
  goto_idx: usize,
  meta: i64,
) -> String {
  let token_string = get_input_id(lu, token_id, input_type);
  format!(
    "\n{}---- JUMP TO {} ON {} ( {} ) [{}]",
    header(idx),
    address_string(goto_idx),
    input_type_to_name(input_type),
    token_string,
    meta
  )
}

fn create_skip_entry(
  lu: Option<&BytecodeGrammarLookups>,
  token_id: u32,
  input_type: u32,
  idx: usize,
  meta: i64,
) -> String {
  let token_string = get_input_id(lu, token_id, input_type);
  format!(
    "\n{}---- SKIP ON {} ( {} ) [ {} ]",
    header(idx),
    input_type_to_name(input_type),
    token_string,
    meta
  )
}

fn input_type_to_name(input_type: u32) -> &'static str {
  match input_type {
    INPUT_TYPE::T01_PRODUCTION => "PRODUCTION",
    INPUT_TYPE::T02_TOKEN => "TOKEN",
    INPUT_TYPE::T03_CLASS => "CLASS",
    INPUT_TYPE::T04_CODEPOINT => "CODEPOINT",
    INPUT_TYPE::T05_BYTE => "BYTE",
    _ => "TOKEN",
  }
}

fn get_input_id(lu: Option<&BytecodeGrammarLookups>, token_id: u32, input_type: u32) -> String {
  if let Some(lu) = lu {
    match input_type {
      INPUT_TYPE::T01_PRODUCTION => {
        let production = &lu.bc_to_prod.get(&token_id).unwrap().guid_name;
        format!("{} [{}]", token_id, production)
      }
      INPUT_TYPE::T02_TOKEN => {
        if let Some(symbol) = lu.bc_to_symbol.get(&token_id) {
          format!("{} [{}]", token_id, symbol.friendly_name)
        } else {
          token_id.to_string()
        }
      }
      INPUT_TYPE::T03_CLASS => token_id.to_string(),
      INPUT_TYPE::T04_CODEPOINT => token_id.to_string(),
      INPUT_TYPE::T05_BYTE => token_id.to_string(),
      _ => token_id.to_string(),
    }
  } else {
    token_id.to_string()
  }
}

pub fn generate_disassembly(
  output: &BytecodeOutput,
  lu: Option<&BytecodeGrammarLookups>,
) -> String {
  let mut states_strings = vec![];
  let mut offset: usize = 0;

  while offset < output.bytecode.len() {
    if offset >= FIRST_STATE_ADDRESS as usize {
      states_strings.push("\n".to_string());
      states_strings
        .push(output.offset_to_state_name.get(&(offset as u32)).cloned().unwrap_or_default())
    }
    let (string, next) = disassemble_state(&output.bytecode, offset, lu);

    offset = next;
    states_strings.push(string);
  }

  states_strings.join("\n")
}

pub fn print_states(output: &BytecodeOutput, lu: Option<&BytecodeGrammarLookups>) {
  eprintln!("{}", generate_disassembly(output, lu));
}

pub fn print_state(idx: usize, output: &BytecodeOutput, lu: Option<&BytecodeGrammarLookups>) {
  let string = disassemble_state(&output.bytecode, idx, lu).0;
  eprintln!("{}", string);
}

mod bytecode_debugging_tests {
  use std::collections::HashMap;

  use crate::bytecode::compile::build_byte_code_buffer;
  use crate::bytecode::compile::compile_ir_state_to_bytecode;
  use crate::bytecode::compile_bytecode;
  use crate::debug::bytecode::BytecodeGrammarLookups;
  use crate::debug::compile_test_grammar;
  use crate::debug::disassemble_state;
  use crate::grammar::get_production_id_by_name;
  use crate::grammar::parse::compile_ir_ast;
  use crate::intermediate::state::generate_production_states;

  use super::generate_disassembly;

  #[test]
  pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production() {
    let g = compile_test_grammar("<> A > \\h ? \\e ? \\l \\l \\o");

    let prod_id = get_production_id_by_name("A", &g).unwrap();

    let output = compile_bytecode(&g, 1);

    let result = generate_production_states(&prod_id, &g);

    let states = result
      .into_iter()
      .map(|s| {
        let string = s.get_code();
        let result = compile_ir_ast(Vec::from(string.as_bytes()));
        assert!(result.is_ok());
        *result.unwrap()
      })
      .collect::<Vec<_>>();

    let state_refs = states.iter().collect::<Vec<_>>();

    let (bytecode, state_lookup) = build_byte_code_buffer(state_refs);

    let mut offset: usize = 0;
    let lu = BytecodeGrammarLookups::new(&g);

    eprintln!("{}", generate_disassembly(&output, Some(&lu)));
  }
}
