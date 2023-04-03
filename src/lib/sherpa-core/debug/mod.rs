use super::types::ParserDatabase;
use crate::types::*;
use sherpa_runtime::{bytecode::*, types::bytecode::Opcode};

#[derive(Debug, Clone)]
pub struct PrintConfig {
  pub display_scanner_output: bool,
  pub display_input_data:     bool,
  pub display_instruction:    bool,
  pub display_state:          bool,
  pub input_window_size:      usize,
}

impl Default for PrintConfig {
  fn default() -> Self {
    Self {
      display_scanner_output: false,
      display_input_data:     true,
      display_instruction:    false,
      display_state:          true,
      input_window_size:      74,
    }
  }
}

pub fn console_debugger(
  db: ParserDatabase,
  PrintConfig {
    display_scanner_output,
    display_input_data,
    input_window_size,
    display_instruction,
    display_state,
  }: PrintConfig,
) -> Option<Box<dyn FnMut(&DebugEvent)>> {
  let mut stack = vec![];
  Some(Box::new(move |event| match event {
    DebugEvent::ShiftToken { offset_end, offset_start, string } => {
      let string = string[*offset_start..(*offset_end).min(string.len())]
        .replace("\n", "\\n");
      stack.push(string.clone());
      println!(
          "
  [Shift] --------------------------------------------------------------------
  
  Pushing token [{string}] to stack
  
  Stack:\n    {}\n
  -------------------------------------------------------------------------------",
          stack
            .iter()
            .enumerate()
            .map(|(i, s)| format!("{}: {s}", i + 1))
            .collect::<Vec<_>>()
            .join("\n    ")
        );
    }
    DebugEvent::Reduce { rule_id } => {
      let item = Item::from_rule((*rule_id).into(), &db);
      let prod_name = item.prod_name().to_string(db.string_store());
      let prod_name = prod_name.split("____").last().unwrap();

      let items = stack.drain((stack.len() - item.len as usize)..);
      let symbols = items.collect::<Vec<_>>();
      stack.push(format!("({prod_name}: {})", symbols.join(",")));
      println!(
            "
  [REDUCE] ----------------------------------------------------------------------
  
    Reduce to {prod_name} with rule: 
    {}
  
    Stack:\n    {}\n
  -------------------------------------------------------------------------------",
            item.to_complete().debug_string(),
            stack
              .iter()
              .enumerate()
              .map(|(i, s)| format!("{}: {s}", i + 1))
              .collect::<Vec<_>>()
              .join("\n    ")
          )
    }

    DebugEvent::Failure { .. } => {
      println!(
          "
  [Failed] --------------------------------------------------------------------
  
    Failed to recognize input.
  -------------------------------------------------------------------------------",
        )
    }
    DebugEvent::Complete { production_id, .. } => {
      println!(
            "
  [Complete] --------------------------------------------------------------------
  
    Accepted on production {}.
  -------------------------------------------------------------------------------",
            db.prod_name((*production_id).into()).to_string(db.string_store())
          )
    }

    DebugEvent::TokenValue { input_value, start, end, string }
      if display_input_data =>
    {
      println!(
          "
  [Token Input]------------------------------------------------------------------------
  
  ║{}║
  Input Value: {input_value}
  Symbol Length: {}
  -------------------------------------------------------------------------------",
          &string[(*start)..(*end).min(string.len())].replace("\n", "\\n"),
          end - start
        )
    }
    DebugEvent::ByteValue { input_value, start, end, string }
      if display_input_data =>
    {
      println!(
          "
  [Byte Input]------------------------------------------------------------------------
  
  ║{}║
  Input Value: {input_value}
  Symbol Length: {}
  -------------------------------------------------------------------------------",
          &string[(*start)..(*end).min(string.len())].replace("\n", "\\n"),
          end - start
        )
    }
    DebugEvent::CodePointValue { input_value, start, end, string }
      if display_input_data && display_scanner_output =>
    {
      println!(
          "
  [CodePoint Input]------------------------------------------------------------------------
  
  ║{}║
  Input Value: {input_value}
  Symbol Length: {}
  -------------------------------------------------------------------------------",
          &string[(*start)..(*end).min(string.len())].replace("\n", "\\n"),
          end - start
        )
    }
    DebugEvent::ClassValue { input_value, start, end, string }
      if display_input_data && display_scanner_output =>
    {
      println!(
          "
  [Class Input]------------------------------------------------------------------------
  
  ║{}║
  Input Value: {input_value}
  Symbol Length: {}
  -------------------------------------------------------------------------------",
          &string[(*start)..(*end).min(string.len())].replace("\n", "\\n"),
          end - start
        )
    }
    DebugEvent::GotoValue { production_id }
      if display_input_data && display_scanner_output =>
    {
      println!(
        "
  [GOTO Input]-------------------------------------------------------------------
  
    Production_name: {}
    BytcodeID: {}
  -------------------------------------------------------------------------------",
      db.prod_name((*production_id).into()).to_string(db.string_store()),
        production_id
      )
    }
    DebugEvent::ExecuteState { base_instruction, .. } if display_state => {
      println!(
          "
  [State]------------------------------------------------------------------
  
  {}
  -------------------------------------------------------------------------------",
          disassemble_parse_block(Some(*base_instruction),base_instruction.bytecode()).0
        );
      println!("");
    }
    DebugEvent::ExecuteInstruction {
      instruction,
      string,
      sym_len,
      is_scanner,
      scan_ptr,
      tok_id,
      tok_len,
      anchor_ptr,
      base_ptr,
      end_ptr,
      head_ptr,
      ..
    } if display_instruction => {
      let active_ptr = if *is_scanner { scan_ptr } else { head_ptr };
      if !is_scanner || display_scanner_output {
        if !matches!(
          instruction.get_opcode(),
          Opcode::VectorBranch | Opcode::HashBranch
        ) {
          return;
        }
        println!(
            "
  [Instruction]------------------------------------------------------------------
  
    address:{:0>6X}; tok_len: {} sym_len: {}; tok_id: {};  
    anchor: {}; base: {}; head: {}; tail: {};  end: {}; 
  
    ║{: <74}║
  
  {}
  -------------------------------------------------------------------------------",
            instruction.address(),
            tok_len,
            sym_len,
            tok_id,
            anchor_ptr,
            base_ptr,
            head_ptr,
            scan_ptr,
            end_ptr,
            &string[(*active_ptr)..(active_ptr + input_window_size).min(string.len())]
              .replace("\n", "\\n"),
            disassemble_parse_block(Some(*instruction), instruction.bytecode()).0
          );
        println!("");
      }
    }
    _ => {}
  }))
}
