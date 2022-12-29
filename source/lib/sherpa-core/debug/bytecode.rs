use std::{
  collections::{BTreeMap, BTreeSet},
  sync::Arc,
};

use crate::{compile::BytecodeOutput, types::*, Journal};

fn header(bc_address: usize) -> String {
  format!("{}| ", address_string(bc_address))
}

pub(crate) fn address_string(bc_address: usize) -> String {
  format!("{:0>6X}", bc_address)
}

pub(crate) struct BytecodeGrammarLookups {
  g: Arc<GrammarStore>,
  bc_to_prod: BTreeMap<u32, ProductionId>,
  bc_to_rule: BTreeMap<u32, RuleId>,
  bc_to_symbol: BTreeMap<u32, SymbolID>,
}

impl BytecodeGrammarLookups {
  pub fn new(g: Arc<GrammarStore>) -> Self {
    let bc_to_prod =
      g.productions.iter().map(|(id, p)| (p.bytecode_id, *id)).collect::<BTreeMap<_, _>>();

    let bc_to_rule = g.rules.iter().map(|(id, r)| (r.bytecode_id, *id)).collect::<BTreeMap<_, _>>();

    let bc_to_symbol =
      g.symbols.iter().map(|(id, s)| (s.bytecode_id, *id)).collect::<BTreeMap<_, _>>();

    BytecodeGrammarLookups { g, bc_to_prod, bc_to_rule, bc_to_symbol }
  }

  pub fn get_prod(&self, bytecode: u32) -> Option<&Production> {
    self.g.productions.get(self.bc_to_prod.get(&bytecode)?)
  }

  pub fn get_rule(&self, bytecode: u32) -> Option<&Rule> {
    self.g.rules.get(self.bc_to_rule.get(&bytecode)?)
  }

  pub fn get_sym(&self, bytecode: u32) -> Option<&Symbol> {
    self.g.symbols.get(self.bc_to_symbol.get(&bytecode)?)
  }

  pub fn get_grammar(&self) -> &GrammarStore {
    &self.g
  }
}
//

pub(crate) fn disassemble_state(
  bc: &[u32],
  idx: usize,
  lu: Option<&BytecodeGrammarLookups>,
) -> (String, usize) {
  use disassemble_state as ds;
  use header as dh;

  let so = idx;

  if idx >= bc.len() {
    ("".to_string(), so)
  } else {
    let instruction = bc[idx] & INSTRUCTION_CONTENT_MASK;
    match bc[idx] & INSTRUCTION_HEADER_MASK {
      INSTRUCTION::I00_PASS => (format!("\n{}PASS", dh(so)), so + 1),
      INSTRUCTION::I01_SHIFT => {
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
              "\n{}PUSH {}",
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
          let name = &lu.get_prod(production_id).unwrap().guid_name;
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
      INSTRUCTION::I08_EAT_CRUMBS => (format!("\n{}NOOP", dh(so)), so + 1),
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
      INSTRUCTION::I14_ASSERT_SHIFT => (format!("\n{}ASTC", dh(so)), so + 1),
      INSTRUCTION::I15_FAIL => (format!("\n{}FAIL", dh(so)), so + 1),
      _ => (format!("\n{}UNDF", dh(so)), so + 1),
    }
  }
}

type GetOffsetTokenIdPair =
  fn(states: &[u32], table_entry_offset: usize, state_offset: usize) -> (usize, u32, bool, i64);

pub(crate) fn generate_table_string(
  bc: &[u32],
  idx: usize,
  lu: Option<&BytecodeGrammarLookups>,
  table_name: &str,
  get_offset_token_id_pair: GetOffsetTokenIdPair,
) -> (String, usize) {
  let TableHeaderData {
    input_type,
    lexer_type,
    table_length,
    table_meta,
    scan_state_entry_instruction: scan_index,
  } = TableHeaderData::from_bytecode(idx, bc);

  let states = bc;
  let mut strings = vec![];
  let default_offset = states[idx + 3] as usize;
  let mut delta_offsets = BTreeSet::new();

  for entry_offset in (4 + idx)..(idx + 4 + table_length as usize) {
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

  let string = format!(
    "\n{}{} JUMP | TYPE {} | PEEK {}",
    header(idx),
    table_name,
    input_type_to_name(input_type),
    lexer_type == LEXER_TYPE::PEEK
  ) + &(if scan_index.get_address() > 0 {
    format!("\n{}SCANNER ADDRESS {}", header(idx + 1), address_string(scan_index.get_address()))
  } else {
    format!("\n{}NO SCANNER", header(idx + 1))
  }) + &format!("\n{}LENGTH: {} META: {}", header(idx + 2), table_length, table_meta)
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
  bc_address: usize,
  meta: i64,
) -> String {
  let token_string = get_input_id(lu, token_id, input_type);
  format!(
    "\n{}---- JUMP TO {} ON {} ( {} ) [{}]",
    header(idx),
    address_string(bc_address),
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
        let production = &lu.get_prod(token_id).unwrap().name;
        format!("{:<3} [{:^1}]", token_id, production)
      }
      INPUT_TYPE::T02_TOKEN => {
        if let Some(symbol) = lu.get_sym(token_id) {
          format!("{:<3} [{:^1}]", token_id, symbol.friendly_name)
        } else {
          token_id.to_string()
        }
      }
      INPUT_TYPE::T03_CLASS => token_id.to_string(),
      INPUT_TYPE::T04_CODEPOINT => token_id.to_string(),
      INPUT_TYPE::T05_BYTE => {
        if token_id < 128 {
          format!(
            "{:<3} [{:^3}]",
            token_id,
            String::from_utf8(vec![token_id as u8])
              .unwrap()
              .replace("\n", "\\n")
              .replace(" ", "\\s")
          )
        } else {
          token_id.to_string()
        }
      }
      _ => token_id.to_string(),
    }
  } else {
    token_id.to_string()
  }
}
// Making the Journal reference an optional requirement allows this function to
// work independent of any grammar that may been input for the bytecode generation.
// This allows the use of this function in situations where the bytecode is the only
// thing a user may have access to but they still want to get some insight in to the
// operations of the state machine.
pub fn generate_disassembly(output: &BytecodeOutput, j: Option<&mut Journal>) -> String {
  let lu = j.map(|j| BytecodeGrammarLookups::new(j.grammar().unwrap()));

  let mut states_strings = vec![];
  let mut offset: usize = 0;

  while offset < output.bytecode.len() {
    if offset >= FIRST_STATE_ADDRESS as usize {
      states_strings.push("\n".to_string());
      states_strings
        .push(output.offset_to_state_name.get(&(offset as u32)).cloned().unwrap_or_default())
    }

    let (string, next) = disassemble_state(&output.bytecode, offset, lu.as_ref());

    offset = next;
    states_strings.push(string);
  }

  states_strings.join("\n")
}

pub(crate) fn print_bytecode_states(output: &BytecodeOutput, j: Option<&mut Journal>) {
  eprintln!("{}", generate_disassembly(output, j));
}

pub(crate) fn print_bytecode_state(
  idx: usize,
  output: &BytecodeOutput,
  lu: Option<&BytecodeGrammarLookups>,
) {
  let string = disassemble_state(&output.bytecode, idx, lu).0;
  eprintln!("{}", string);
}

mod bytecode_debugging_tests {
  use std::collections::HashMap;

  use crate::{
    bytecode::{
      compile::{build_byte_code_buffer, compile_ir_state_to_bytecode},
      compile_bytecode,
    },
    debug::{bytecode::BytecodeGrammarLookups, disassemble_state},
    grammar::parse::compile_ir_ast,
    intermediate::{
      compile::{compile_production_states, compile_states},
      optimize::optimize_ir_states,
    },
    journal::Journal,
    types::{GrammarStore, SherpaResult},
  };

  use super::generate_disassembly;

  #[test]
  pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production(
  ) -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(&mut j, "<> A > \\h ? \\e ? \\l \\l \\o").unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let states = compile_states(&mut j, 1)?;

    let results = optimize_ir_states(&mut j, states);

    let output = compile_bytecode(&mut j, results);

    let result = compile_production_states(&mut j, prod_id)?;

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

    let _ = build_byte_code_buffer(state_refs);

    eprintln!("{}", generate_disassembly(&output, Some(&mut j)));

    SherpaResult::Ok(())
  }
}
