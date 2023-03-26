use crate::{
  bytecode::BytecodeOutput,
  grammar::new::types::ParserDatabase,
  types::*,
  Journal,
};
use sherpa_runtime::types::bytecode::{
  InputType,
  Instruction,
  FIRST_PARSE_BLOCK_ADDRESS,
};
use std::collections::BTreeSet;

fn header<'a>(address: usize) -> String {
  format!("{}| ", address_string(address))
}

pub(crate) fn address_string(bc_address: usize) -> String {
  format!("{:0>6X}", bc_address)
}

pub fn disassemble_parse_block<'a>(
  i: Option<Instruction<'a>>,
  db: Option<&ParserDatabase>,
  bc: &[u8],
) -> (String, Option<Instruction<'a>>) {
  use disassemble_parse_block as ds;
  use header as dh;

  let Some(i) = i  else {
    return ("".to_string(), None)
  };

  use bytecode::Opcode::*;

  if !i.is_valid() {
    ("".to_string(), None)
  } else {
    match i.get_opcode() {
      VectorBranch | HashBranch => generate_table_string(i, db, bc),
      Goto => {
        let mut iter = i.iter();
        let _state_mode = iter.next_u8().unwrap();
        let address = iter.next_u32_le().unwrap() as usize;
        if let Some(db) = db {
          (
            format!(
              "\n{}GOTO {} [ {} ]",
              dh(i.address()),
              address_string(address),
              get_state_name_from_address(db, address),
            ),
            i.next(),
          )
        } else {
          (
            format!("\n{}GOTO {}", dh(i.address()), address_string(address)),
            i.next(),
          )
        }
      }
      PopGoto => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}POP{string}", dh(i.address())), i_last)
      }
      PushGoto => {
        let (string, i_last) = ds(i.next(), db, bc);
        let mut iter = i.iter();
        let _state_mode = iter.next_u8().unwrap();
        let address = iter.next_u32_le().unwrap() as usize;
        if let Some(db) = db {
          (
            format!(
              "\n{}PUSH {} [ {} ]{string}",
              dh(i.address()),
              address_string(address),
              get_state_name_from_address(db, address),
            ),
            i_last,
          )
        } else {
          (
            format!(
              "\n{}PUSH {}{string}",
              dh(i.address()),
              address_string(address)
            ),
            i_last,
          )
        }
      }
      PushExceptionHandler => {
        let (string, i_last) = ds(i.next(), db, bc);
        let mut iter = i.iter();
        let _state_mode = iter.next_u8().unwrap();
        let address = iter.next_u32_le().unwrap() as usize;
        if let Some(db) = db {
          (
            format!(
              "\n{}PUSH-CATCH {} [ {} ]{string}",
              dh(i.address()),
              address_string(address),
              get_state_name_from_address(db, address),
            ),
            i_last,
          )
        } else {
          (
            format!(
              "\n{}PUSH-CATCH {}{string}",
              dh(i.address()),
              address_string(address)
            ),
            i_last,
          )
        }
      }
      Reduce => {
        let (string, i_last) = ds(i.next(), db, bc);
        let mut iter = i.iter();
        let prod_id = iter.next_u32_le().unwrap();
        let rule_id = iter.next_u32_le().unwrap();
        let symbol_count = iter.next_u16_le().unwrap() as u32;

        let pluralized = if symbol_count == 1 { "SYMBOL" } else { "SYMBOLS" };

        if let Some(db) = db {
          let name = &db.prod_name(prod_id.into()).to_string(db.string_store());
          (
            format!(
              "\n{}REDUCE-RULE {} TO [ {} ] ( {} {} ){string} ",
              dh(i.address()),
              rule_id,
              name,
              symbol_count,
              pluralized,
            ),
            i_last,
          )
        } else {
          (
            format!(
              "\n{}REDUCE-RULE {} TO [ {} ] ( {} {} ){string} ",
              dh(i.address()),
              rule_id,
              prod_id,
              symbol_count,
              pluralized,
            ),
            i_last,
          )
        }
      }
      AssignToken => {
        let (string, i_last) = ds(i.next(), db, bc);
        let mut iter = i.iter();
        let tok_id = iter.next_u32_le().unwrap();

        if let Some(db) = db {
          let tok_str = db.tok_data(tok_id.into()).sym_id.debug_string(db);

          (
            format!(
              "\n{}ASSIGN-TK [{} = {}]{string}",
              dh(i.address()),
              tok_id,
              tok_str
            ),
            i_last,
          )
        } else {
          (
            format!("\n{}ASSIGN-TK [{}]{string}", dh(i.address()), tok_id),
            i_last,
          )
        }
      }
      NoOp => {
        let (string, i_last) = ds(i.next(), db, bc);
        (
          format!(
            "\n{}NOOP [ASCII: {} 0x{:X}]{string}",
            dh(i.address()),
            char::from_u32((i.bytecode()[i.address()] & 127) as u32).unwrap(),
            i.bytecode()[i.address()]
          ),
          i_last,
        )
      }
      ScanShift => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SCAN-SHFT{string}", dh(i.address())), i_last)
      }
      ShiftToken => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SHFT-TK{string}", dh(i.address())), i_last)
      }
      ShiftTokenScanless => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SHFT-TK-NO-SCAN{string}", dh(i.address())), i_last)
      }
      PeekToken => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SHFT-PEEK-TK{string}", dh(i.address())), i_last)
      }
      PeekTokenScanless => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SHFT-PEEK-TK-NO-SCAN{string}", dh(i.address())), i_last)
      }
      SkipToken => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SKIP{string}", dh(i.address())), i_last)
      }
      SkipTokenScanless => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SKIP-NO-SCAN{string}", dh(i.address())), i_last)
      }
      PeekSkipToken => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SKIP-PEEK{string}", dh(i.address())), i_last)
      }
      PeekSkipTokenScanless => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}SKIP-PEEK-NO-SCAN{string}", dh(i.address())), i_last)
      }
      PeekReset => {
        let (string, i_last) = ds(i.next(), db, bc);
        (format!("\n{}PEEK-RESET{string}", dh(i.address())), i_last)
      }
      Fail => (format!("\n{}FAIL", dh(i.address())), i.next()),
      Pass => (format!("\n{}PASS", dh(i.address())), i.next()),
      Accept => (format!("\n{}ACCEPT", dh(i.address())), i.next()),
    }
  }
}

pub(crate) fn generate_table_string<'a>(
  i: Instruction<'a>,
  db: Option<&ParserDatabase>,
  bc: &[u8],
) -> (String, Option<Instruction<'a>>) {
  let TableHeaderData {
    input_type,
    table_length,
    table_meta,
    scan_block_instruction: scan_index,
    default_block,
    table_start,
    mut table_start_iter,
    ..
  } = i.into();
  let table_name = matches!(i.get_opcode(), bytecode::Opcode::HashBranch)
    .then_some("HASH")
    .unwrap_or("VECT");

  let mut strings = vec![];
  let mut delta_offsets = BTreeSet::new();

  let states = (0..table_length)
    .into_iter()
    .map(|_| table_start_iter.next_u32_le().unwrap())
    .collect::<Vec<_>>();

  for entry_offset in 0..table_length as usize {
    let entry = states[entry_offset];
    let (val_id, address_offset, meta) = match i.get_opcode() {
      bytecode::Opcode::VectorBranch => {
        let val_id = table_meta + entry_offset as u32;
        let address_offset = entry as usize;
        (val_id, address_offset, 0)
      }
      bytecode::Opcode::HashBranch => {
        let val_id = (entry & 0x7FF) as u32;
        let address_offset = ((entry >> 11) & 0x7FF) as usize;
        let meta = ((entry >> 22) & 0x3FF) as i64 - 512;
        (val_id, address_offset, meta)
      }
      _ => unreachable!(),
    };
    let address = i.address() + address_offset;

    if address == default_block.address() {
      strings.push(create_failure_entry(entry_offset, default_block.address()));
    } else {
      delta_offsets.insert(address);
      strings.push(create_normal_entry(
        db,
        val_id,
        input_type,
        entry_offset * 4 + table_start,
        address,
        meta,
      ));
    }
  }
  strings.push(create_default_entry(default_block.address()));

  for address in delta_offsets {
    strings.push(
      disassemble_parse_block(Some((i.bytecode(), address).into()), db, bc).0,
    );
  }
  let (default_string, offset) =
    disassemble_parse_block(Some(default_block), db, bc);

  let mut string = format!(
    "\n{}{} JUMP \n{: >7} TYPE {} ",
    header(i.address()),
    table_name,
    "",
    InputType::from(input_type).to_string(),
  );

  string += &(if scan_index.address() > 0 {
    format!(
      "\n{: >7} SCANNER ADDRESS {}",
      "",
      address_string(scan_index.address())
    )
  } else {
    format!("\n{: >7} NO SCANNER", "")
  });

  string +=
    &format!("\n{: >7} LENGTH: {} META: {}", "", table_length, table_meta);

  string += &strings.join("");

  string += &default_string;

  (string, offset)
}

fn create_failure_entry(entry_offset: usize, goto_offset: usize) -> String {
  format!(
    "\n{}---- JUMP TO {} ON FAIL",
    header(entry_offset),
    address_string(goto_offset)
  )
}
fn create_default_entry(goto_offset: usize) -> String {
  format!("\nDEFAULT ---- JUMP TO {} ON FAIL", address_string(goto_offset))
}

fn create_normal_entry(
  lu: Option<&ParserDatabase>,
  token_id: u32,
  input_type: InputType,
  idx: usize,
  bc_address: usize,
  meta: i64,
) -> String {
  let token_string = get_input_id(lu, token_id, input_type);
  format!(
    "\n{: >6}---- JUMP TO {} ON {} ( {} ) [{}]",
    header(idx),
    address_string(bc_address),
    InputType::from(input_type).to_string(),
    token_string,
    meta
  )
}

fn get_input_id(
  db: Option<&ParserDatabase>,
  token_id: u32,
  input_type: InputType,
) -> String {
  if let Some(db) = db {
    match input_type {
      InputType::Production => {
        let prod_name = db.prod_name(token_id.into());
        format!(
          "{:<3} [{:^1}]",
          token_id,
          prod_name.to_string(db.string_store())
        )
      }
      InputType::Token => {
        let sym_id = db.sym(token_id.into());
        format!("{:<3} [{:^1}]", token_id, sym_id.debug_string(db))
      }
      InputType::EndOfFile => token_id.to_string(),
      InputType::Class => token_id.to_string(),
      InputType::Codepoint => token_id.to_string(),
      InputType::Byte => {
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
      _ => "DEFAULT".into(),
    }
  } else {
    token_id.to_string()
  }
}
/// Returns a "disassembly"  representation of a bytecode parser's opcodes.
pub fn generate_disassembly(
  bc: &[u8],
  db: Option<&ParserDatabase>,
  j: &Journal,
) -> String {
  let mut states_strings = vec![];
  let i: Instruction = (bc, 0).into();
  let mut next = Some(i);

  while let Some(i) = next {
    if i.address() >= FIRST_PARSE_BLOCK_ADDRESS as usize {
      states_strings.push("\n".to_string());
      if let Some(db) = db.as_ref() {
        states_strings.push(get_state_name_from_address(db, i.address()))
      }
    }

    let (string, n) = disassemble_parse_block(next, db, bc);

    states_strings.push(string);

    next = n;
  }

  states_strings.join("\n")
}

fn get_state_name_from_address(
  output: &ParserDatabase,
  address: usize,
) -> String {
  "TODO".to_string()
  /* output
  .offset_to_state_name
  .get(&(address as u32))
  .cloned()
  .unwrap_or_default() */
}
