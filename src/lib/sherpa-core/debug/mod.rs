use super::types::ParserDatabase;
use crate::types::*;
use sherpa_rust_runtime::{
  bytecode::{DebugFn, *},
  types::{bytecode::Opcode, ByteReader, ParseContext, UTF8Reader},
};
#[derive(Debug, Clone, Copy)]
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

#[derive(Default)]

pub struct Node {
  string:   String,
  offset:   u32,
  children: Vec<Node>,
}

impl Node {
  pub fn draw(&self, offset: u32) -> String {
    let mut string = format!("{}{}", " ".repeat(offset as usize), self.string);

    if self.is_single() {
      let child_strings = self.children.iter().map(|c| c.draw(0)).collect::<Vec<_>>().join("");
      if !child_strings.is_empty() {
        string += &child_strings;
      }
    } else {
      let child_strings = self.children.iter().map(|c| c.draw(offset + self.offset)).collect::<Vec<_>>().join("\n");
      if !child_strings.is_empty() {
        string += &("\n".to_string() + &child_strings);
      }
    }
    string
  }

  pub fn is_single(&self) -> bool {
    self.children.len() <= 1 && self.children.iter().all(|c| c.is_single())
  }
}

#[cfg(all(debug_assertions))]
#[allow(unused)]
pub fn file_debugger<R: ByteReader + UTF8Reader, M>(
  db: ParserDatabase,
  print_config: PrintConfig,
  state_lu: Map<u32, IString>,
) -> Option<Box<DebugFn<R, M>>> {
  let mut stack = vec![];
  crate::test::utils::write_debug_file(&db, "parser_output.tmp", "    ", false);
  Some(Box::new(move |event, ctx| {
    let string = diagram_constructor(event, ctx, &mut stack, &db, &print_config, &state_lu);

    if !string.is_empty() {
      crate::test::utils::write_debug_file(&db, "parser_output.tmp", string, true);
    }
  }))
}

#[cfg(debug_assertions)]
#[allow(unused)]
pub fn console_debugger<R: ByteReader + UTF8Reader, M>(
  db: ParserDatabase,
  print_config: PrintConfig,
  state_lu: Map<u32, IString>,
) -> Option<Box<DebugFn<R, M>>> {
  let mut stack = vec![];
  Some(Box::new(move |event, ctx| {
    let string = diagram_constructor(event, ctx, &mut stack, &db, &print_config, &state_lu);

    if !string.is_empty() {
      println!("{string}");
    }
  }))
}

#[cfg(debug_assertions)]
#[allow(unused)]
fn diagram_constructor<R: ByteReader + UTF8Reader, M>(
  event: &DebugEvent<'_>,
  ctx: &ParseContext<R, M>,
  stack: &mut Vec<Node>,
  db: &ParserDatabase,
  pc: &PrintConfig,
  state_lu: &Map<u32, IString>,
) -> String {
  let PrintConfig {
    display_scanner_output,
    display_input_data,
    input_window_size,
    display_instruction,
    display_state,
  } = *pc;
  match event {
    DebugEvent::ExecuteState { base_instruction } => {
      let i = base_instruction.address() as u32;
      if let Some(state_name) = state_lu.get(&i) {
        let name = state_name.to_str(db.string_store());
        let name = name.as_str();

        format!(
          "
  [STATE] --------------------------------------------------------------------
  {}
  -------------------------------------------------------------------------------",
          name
        )
      } else {
        Default::default()
      }
    }
    DebugEvent::ActionShift { offset_end, offset_start, token_id } => {
      let string = ctx.get_str()[*offset_start..(*offset_end).min(ctx.get_str().len())].replace("\n", "\\n");
      stack.push(Node { string: " ".to_string() + &string, offset: 0, ..Default::default() });
      format!(
        "
[Shift] --------------------------------------------------------------------

Pushing token [{}] to stack

Stack:\n{}\n
-------------------------------------------------------------------------------",
        db.token((*token_id).into()).name.to_string(db.string_store()),
        stack
          .iter()
          .enumerate()
          .map(|(i, s)| format!("{: >4}: {}", i + 1, s.draw(0).split("\n").collect::<Vec<_>>().join("\n       ")))
          .collect::<Vec<_>>()
          .join("\n")
      )
    }
    DebugEvent::ActionReduce { rule_id } => {
      let item = Item::from_rule((*rule_id).into(), &db);
      let nterm_name = item.nonterm_name().to_string(db.string_store());
      let nterm_name = nterm_name.split("____").last().unwrap();

      match item.reduction_type() {
        /* ReductionType::LeftRecursive => {
          let mut items = stack.drain((stack.len() - item.len as usize)..);
          let mut first = items.next().unwrap();
          first.children.extend(items);
          stack.push(first);
        } */
        t @ _ | t @ ReductionType::SemanticAction | t @ ReductionType::Mixed | t @ ReductionType::SingleTerminal => {
          let items = stack.drain((stack.len() - item.len as usize)..);
          let symbols = items.collect::<Vec<_>>();

          stack.push(Node {
            string:   format!("({})", nterm_name.to_string()),
            children: symbols,
            offset:   2,
          });
        }
        _ => {}
      }

      format!(
        "
[REDUCE] ----------------------------------------------------------------------

Reduce to {nterm_name} with rule: 
{}

Stack:\n{}\n
-------------------------------------------------------------------------------",
        item.to_complete()._debug_string_(),
        stack
          .iter()
          .enumerate()
          .map(|(i, s)| format!("{: >4}: {}", i + 1, s.draw(0).split("\n").collect::<Vec<_>>().join("\n       ")))
          .collect::<Vec<_>>()
          .join("\n")
      )
    }

    DebugEvent::Complete { nonterminal_id, .. } => {
      format!(
        "
  [Complete] --------------------------------------------------------------------
  
    Accepted on non-terminal {}.
  
    Stack:\n{}\n
  -------------------------------------------------------------------------------",
        db.nonterm_guid_name((*nonterminal_id).into()).to_string(db.string_store()),
        stack
          .iter()
          .enumerate()
          .map(|(i, s)| format!("{: >4}: {}", i + 1, s.draw(0).split("\n").collect::<Vec<_>>().join("\n       ")))
          .collect::<Vec<_>>()
          .join("\n")
      )
    }

    DebugEvent::ExecuteState { base_instruction, .. } if display_state => {
      format!(
        "
[State]------------------------------------------------------------------

{}{}
-------------------------------------------------------------------------------",
        disassemble_parse_block(Some(*base_instruction), true).0,
        disassemble_parse_block(base_instruction.next(), true).0
      )
    }
    DebugEvent::ExecuteInstruction { instruction } if display_instruction => {
      let active_ptr = if ctx.is_scanner() { ctx.tok_ptr } else { ctx.sym_ptr };
      if ctx.is_scanner() || display_scanner_output {
        if !matches!(instruction.get_opcode(), Opcode::VectorBranch | Opcode::HashBranch) {
          Default::default()
        }
        format!(
          "
[Instruction]------------------------------------------------------------------

  address:{:0>6X}; tok_len: {} sym_len: {}; tok_id: {};  
  anchor: {}; base: {}; head: {}; tail: {};  end: {}; 

  ║{: <74}║

{}
-------------------------------------------------------------------------------",
          instruction.address(),
          ctx.tok_len,
          ctx.sym_len,
          ctx.tok_id,
          ctx.anchor_ptr,
          ctx.base_ptr,
          ctx.sym_ptr,
          ctx.tok_ptr,
          ctx.end_ptr,
          &ctx.get_str()[(active_ptr)..(active_ptr + input_window_size).min(ctx.get_str().len())].replace("\n", "\\n"),
          disassemble_parse_block(Some(*instruction), true).0
        )
      } else {
        Default::default()
      }
    }
    _ => Default::default(),
  }
}
