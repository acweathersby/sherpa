use crate::{bytecode::BytecodeOutput, types::*, Journal};
use std::collections::BTreeSet;

fn header(bc_address: usize) -> String {
  format!("{}| ", address_string(bc_address))
}

pub(crate) fn address_string(bc_address: usize) -> String {
  format!("{:0>6X}", bc_address)
}
pub(crate) fn disassemble_state(
  bc: &[u32],
  state_address: usize,
  lu: Option<&GrammarStore>,
) -> (String, usize) {
  use disassemble_state as ds;
  use header as dh;

  let so = state_address;

  if state_address >= bc.len() {
    ("".to_string(), so)
  } else {
    let instruction = bc[state_address] & INSTRUCTION_CONTENT_MASK;
    match bc[state_address] & INSTRUCTION_HEADER_MASK {
      Instruction::I00_PASS => (format!("\n{}PASS", dh(so)), so + 1),
      Instruction::I01_SHIFT_TOKEN => {
        let (string, offset) = ds(bc, so + 1, lu);
        if instruction & 1 == 0 {
          (format!("\n{}SHFT", dh(so)) + &string, offset + 1)
        } else {
          (format!("\n{}SHFT-SCAN", dh(so)) + &string, offset)
        }
      }
      Instruction::I02_GOTO => {
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
      Instruction::I03_SET_PROD => {
        let production_id = instruction & INSTRUCTION_CONTENT_MASK;
        let (string, offset) = ds(bc, so + 1, lu);

        if let Some(lu) = lu {
          let name = &lu.get_production_by_bytecode_id(production_id).unwrap().guid_name;
          (format!("\n{}PROD SET TO {}     // {}", dh(so), production_id, name,) + &string, offset)
        } else {
          (format!("\n{}PROD SET TO {}", dh(so), production_id,) + &string, offset)
        }
      }
      Instruction::I04_REDUCE => {
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
      Instruction::I05_TOKEN => {
        let (string, offset) = ds(bc, so + 1, lu);
        if (instruction & TOKEN_ASSIGN_FLAG) > 0 {
          (format!("\n{}TOKN ASSIGN TO {}", dh(so), instruction & 0x00FF_FFFF) + &string, offset)
        } else {
          (format!("\n{}TOKV", dh(so)) + &string, offset)
        }
      }
      Instruction::I06_FORK_TO => {
        let target_production = instruction & 0xFFFF;
        let num_of_states = (instruction >> 16) & 0xFFFF;
        let end = so + 1 + num_of_states as usize;
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
      Instruction::I07_PEEK_RESET => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}PKRST", dh(so)) + &string, offset)
      }
      Instruction::I08_POP => (format!("\n{}POP", dh(so)), so + 1),
      Instruction::I09_VECTOR_BRANCH => generate_table_string(
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
      Instruction::I10_HASH_BRANCH => generate_table_string(
        bc,
        so,
        lu,
        "HASH",
        |states: &[u32], table_entry_offset: usize, _: usize| {
          (
            (states[table_entry_offset] >> 11 & 0x7FF) as usize,
            (states[table_entry_offset] & 0x7FF),
            (states[table_entry_offset] >> 11 & 0x7FF) == 0x7FF,
            ((states[table_entry_offset] >> 22) & 0x3FF) as i64 - 512,
          )
        },
      ),
      Instruction::I11_SET_CATCH_STATE => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}FSET", dh(so)) + &string, offset)
      }
      Instruction::I12_SKIP => {
        let (string, offset) = ds(bc, so + 1, lu);
        if instruction & 1 == 0 {
          (format!("\n{}SKIP", dh(so)) + &string, offset)
        } else {
          (format!("\n{}SKIP-CHAR", dh(so)) + &string, offset)
        }
      }
      Instruction::I13_SHIFT_SCANNER => {
        let (string, offset) = ds(bc, so + 1, lu);
        (format!("\n{}SHFS", dh(so)) + &string, offset)
      }
      Instruction::I14_PEEK_TOKEN => {
        let (string, offset) = ds(bc, so + 1, lu);
        if instruction & 1 == 0 {
          (format!("\n{}PEEK", dh(so)) + &string, offset)
        } else {
          (format!("\n{}PEEK-TOK", dh(so)) + &string, offset)
        }
      }
      Instruction::I15_FAIL => (format!("\n{}FAIL", dh(so)), so + 1),
      _ => (format!("\n{}UNDF", dh(so)), so + 1),
    }
  }
}

type GetOffsetTokenIdPair =
  fn(states: &[u32], table_entry_offset: usize, state_offset: usize) -> (usize, u32, bool, i64);

pub(crate) fn generate_table_string(
  bc: &[u32],
  idx: usize,
  lu: Option<&GrammarStore>,
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
    lexer_type == LexerType::PEEK
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
  lu: Option<&GrammarStore>,
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
  lu: Option<&GrammarStore>,
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
    InputType::T01_PRODUCTION => "PRODUCTION",
    InputType::T02_TOKEN => "TOKEN",
    InputType::T03_CLASS => "CLASS",
    InputType::T04_CODEPOINT => "CODEPOINT",
    InputType::T05_BYTE => "BYTE",
    _ => "TOKEN",
  }
}

fn get_input_id(g: Option<&GrammarStore>, token_id: u32, input_type: u32) -> String {
  if let Some(g) = g {
    match input_type {
      InputType::T01_PRODUCTION => {
        let production = &g.get_production_by_bytecode_id(token_id).unwrap().name;
        format!("{:<3} [{:^1}]", token_id, production)
      }
      InputType::T02_TOKEN => {
        if let SherpaResult::Ok(sym_id) = g.get_symbol_id_by_bytecode_id(token_id) {
          format!("{:<3} [{:^1}]", token_id, sym_id.debug_string(g))
        } else {
          token_id.to_string()
        }
      }
      InputType::T03_CLASS => token_id.to_string(),
      InputType::T04_CODEPOINT => token_id.to_string(),
      InputType::T05_BYTE => {
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
/// Making the Journal reference an optional requirement allows this function to
/// work independent of any grammar that may been input for the bytecode generation.
/// This allows the use of this function in situations where the bytecode is the only
/// thing a user may have access to but they still want to get some insight in to the
/// operations of the state machine.
pub fn generate_disassembly(output: &BytecodeOutput, j: &mut Journal) -> String {
  let g = j.grammar().unwrap();

  let mut states_strings = vec![];
  let mut offset: usize = 0;

  while offset < output.bytecode.len() {
    if offset >= FIRST_STATE_ADDRESS as usize {
      states_strings.push("\n".to_string());
      states_strings
        .push(output.offset_to_state_name.get(&(offset as u32)).cloned().unwrap_or_default())
    }

    let (string, next) = disassemble_state(&output.bytecode, offset, Some(&g));

    offset = next;
    states_strings.push(string);
  }

  states_strings.join("\n")
}
