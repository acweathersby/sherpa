use super::parse_functions::get_next_action;
use crate::{
  bytecode,
  bytecode::{
    compile::{build_byte_code_buffer, compile_ir_state_to_bytecode},
    compile_ir_states_into_bytecode,
  },
  debug::{disassemble_state, generate_disassembly},
  grammar::parse::compile_ir_ast,
  runtime::parse_functions::{dispatch, hash_jump, vector_jump},
  types::*,
  Journal,
};
use std::collections::HashMap;

#[test]
fn test_fork() {
  let (bytecode, mut reader, mut ctx) = setup_states(
    vec![
      "
  state [test_start]
      
      fork to ( state [X]  state [Y]  state [Z] ) to complete prod 2 then pass
      ",
      "
  state [X]
      
      set prod to 2 then reduce 0 symbols to body 1
      ",
      "
  state [Y]
      
      set prod to 2 then reduce 0 symbols to body 2
      ",
      "
  state [Z]
      
      set prod to 2 then reduce 0 symbols to body 3
      ",
    ],
    "",
  );

  ctx.init_normal_state(NORMAL_STATE_FLAG | FIRST_STATE_ADDRESS);

  match get_next_action(&mut reader, &mut ctx, &bytecode) {
    ParseAction::Fork { states_start_offset, num_of_states, target_production } => {
      assert_eq!(states_start_offset, FIRST_STATE_ADDRESS + 1);
      assert_eq!(num_of_states, 3);
      assert_eq!(target_production, 2);
    }
    _ => panic!("Could not complete parse"),
  }
}

#[test]
fn test_goto() {
  let (bytecode, mut reader, mut state) = setup_states(
    vec![
      "
  state [test_start]
      
      goto state [test_end]
      ",
      "
  state [test_end]
      
      set prod to 444 then pass
      ",
    ],
    "",
  );
  state.init_normal_state(NORMAL_STATE_FLAG | FIRST_STATE_ADDRESS);

  match get_next_action(&mut reader, &mut state, &bytecode) {
    ParseAction::Accept { production_id } => {
      assert_eq!(production_id, 444);
    }
    _ => panic!("Could not complete parse"),
  }
}

#[test]
fn test_set_production() {
  let (bytecode, mut reader, mut state) = setup_state(
    "
  state [test]
      
      set prod to 222 then pass
      ",
    "0",
  );

  state.set_production_to(3);

  dispatch(&mut reader, &mut state, &bytecode);

  assert_eq!(state.get_production(), 222);
}

#[test]
fn test_reduce() {
  let (bytecode, mut reader, mut state) = setup_state(
    "
  state [test]
      
      reduce 2 symbols to body 1
      ",
    "0",
  );

  state.set_production_to(3);

  match dispatch(&mut reader, &mut state, &bytecode) {
    ParseAction::Reduce { rule_id, production_id, symbol_count } => {
      assert_eq!(rule_id, 1);
      assert_eq!(symbol_count, 2);
      assert_eq!(production_id, 3);
    }
    _ => panic!("Incorrect value returned"),
  }
}
#[test]
fn test_shift_nothing() {
  let (bytecode, mut reader, mut state) = setup_state(
    "
  state [test]
      
      shift nothing
      ",
    "123456781234567812345678",
  );

  reader.next(10);

  state.assert = ParseToken {
    byte_length: 5,
    byte_offset: 10,
    cp_length: 5,
    cp_offset: 20,
    ..Default::default()
  };

  match dispatch(&mut reader, &mut state, &bytecode) {
    ParseAction::Shift { skipped_characters, token } => {
      assert_eq!(skipped_characters.cp_length, 20);
      assert_eq!(skipped_characters.byte_length, 10);

      assert_eq!(token.cp_offset, 20);
      assert_eq!(token.byte_offset, 10);

      assert_eq!(token.byte_length, 0);
      assert_eq!(token.cp_length, 0);

      // assert_eq!(reader.cursor(), 15);
      assert_eq!(state.anchor, state.assert);
    }
    _ => panic!("Incorrect value returned"),
  }
}

#[test]
fn test_shift() {
  let (bytecode, mut reader, mut state) = setup_state(
    "
  state [test]
      
      shift
      ",
    "123456781234567812345678",
  );

  state.assert = ParseToken {
    byte_length: 5,
    byte_offset: 10,
    cp_length: 5,
    cp_offset: 20,
    ..Default::default()
  };

  match dispatch(&mut reader, &mut state, &bytecode) {
    ParseAction::Shift { skipped_characters, token } => {
      assert_eq!(skipped_characters.cp_length, 20);
      assert_eq!(skipped_characters.byte_length, 10);

      assert_eq!(token.cp_offset, 20);
      assert_eq!(token.byte_offset, 10);

      assert_eq!(token.byte_length, 5);
      assert_eq!(token.cp_length, 5);

      // assert_eq!(reader.cursor(), 15);
      assert_eq!(state.anchor, state.assert);
    }
    _ => panic!("Incorrect value returned"),
  }
}

#[test]
fn test_hash_table() {
  let (bytecode, mut reader, mut state) = setup_state(
    "
  state [test]
  assert PRODUCTION [1] (pass)
  assert PRODUCTION [2] (pass)
  assert PRODUCTION [3] (pass)",
    "AB",
  );

  state.set_production_to(1);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 7);
  state.set_production_to(2);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 8);
  state.set_production_to(3);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 9);
  state.set_production_to(4);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 10);
  state.set_production_to(0);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 10);
}

#[test]
fn test_hash_table_skip() {
  let (bytecode, mut reader, mut state) = setup_state(
    "
  state [test] scanner [none]
      skip [1] 
      assert TOKEN [0] ( set prod to 44 then pass)",
    "",
  );

  state.set_production_to(1);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 6);
  state.set_production_to(2);
  assert_eq!(hash_jump(0, &mut reader, &mut state, &bytecode), 8);
}

#[test]
fn test_jump_table() {
  use IRStateType::*;

  let mut ir_state = IRState {
    code: "
  assert PRODUCTION [1] (pass)
  assert PRODUCTION [2] (pass)
  assert PRODUCTION [3] (pass)
  "
    .to_string(),
    name: "test".to_string(),
    ..Default::default()
  };

  let ir_ast = ir_state.compile_ast();

  assert!(ir_ast.is_ok());

  let ir_ast = (ir_ast.unwrap()).clone();
  let mut j = Journal::new(None);
  let grammar = GrammarStore::default();

  let output =
    compile_ir_states_into_bytecode(&mut j, vec![(ir_state.get_name(), Box::new(ir_state))], vec![
      ir_ast,
    ]);

  let bc = &output.bytecode;

  let mut r = UTF8StringReader::from_string("test");

  let mut s = ParseContext::bytecode_context();

  eprintln!("{}", generate_disassembly(&output, None));
  let off = FIRST_STATE_ADDRESS;
  s.set_production_to(1);
  assert_eq!(vector_jump(off, &mut r, &mut s, bc), off + 7);
  s.set_production_to(2);
  assert_eq!(vector_jump(off, &mut r, &mut s, bc), off + 8);
  s.set_production_to(3);
  assert_eq!(vector_jump(off, &mut r, &mut s, bc), off + 9);
  s.set_production_to(4);
  assert_eq!(vector_jump(off, &mut r, &mut s, bc), off + 10);
  s.set_production_to(0);
  assert_eq!(vector_jump(off, &mut r, &mut s, bc), off + 10);
}
#[ignore]
#[test]
fn test_jump_table_skip() {
  let val = "
              skip [1] 
              assert PRODUCTION [0] ( set prod to 44 then pass)";
  let mut j = Journal::new(None);
  let output = create_output(&mut j, val);
  let bytecode = &output.bytecode;
  let index: u32 = 0;
  let mut reader = UTF8StringReader::from_string("AB");
  let mut state = ParseContext::bytecode_context();

  eprintln!("{}", disassemble_state(&output.bytecode, 0, None).0);

  state.set_production_to(2);
  assert_eq!(vector_jump(index, &mut reader, &mut state, &bytecode), 6);

  state.set_production_to(0);
  assert_eq!(vector_jump(index, &mut reader, &mut state, &bytecode), 8);
}

fn setup_states<'a>(
  state_ir: Vec<&str>,
  reader_input: &'a str,
) -> (Vec<u32>, UTF8StringReader<'a>, ParseContext<UTF8StringReader<'a>>) {
  let is_asts = state_ir
    .into_iter()
    .map(|s| {
      let result = compile_ir_ast(Vec::from(s.to_string()));

      match result {
        Ok(ast) => *ast,
        Err(err) => {
          eprintln!("{}", err);
          panic!("Could not build state:\n{}", s);
        }
      }
    })
    .collect::<Vec<_>>();

  let (bytecode, _) = build_byte_code_buffer(is_asts.iter().collect());

  let mut reader = UTF8StringReader::from_string(reader_input);
  let mut ctx = ParseContext::bytecode_context();

  (bytecode, reader, ctx)
}
fn setup_state<'a>(
  state_ir: &str,
  reader_input: &'a str,
) -> (Vec<u32>, UTF8StringReader<'a>, ParseContext<UTF8StringReader<'a>>) {
  let ir_ast = compile_ir_ast(Vec::from(state_ir.to_string()));

  assert!(ir_ast.is_ok());

  let ir_ast = ir_ast.unwrap();

  let bytecode = compile_ir_state_to_bytecode(
    &ir_ast.instructions,
    |_, _, _| BranchSelector::Hash,
    &HashMap::new(),
    &"".to_string(),
    "",
  );

  let mut reader = UTF8StringReader::from_string(reader_input);
  let mut ctx = ParseContext::bytecode_context();

  (bytecode, reader, ctx)
}

fn create_output(j: &mut Journal, val: &str) -> bytecode::BytecodeOutput {
  let mut ir_state =
    IRState { code: val.to_string(), name: "test".to_string(), ..Default::default() };

  let ir_ast = ir_state.compile_ast();

  assert!(ir_ast.is_ok());

  let ir_ast = ir_ast.unwrap().clone();

  let output =
    compile_ir_states_into_bytecode(j, vec![(ir_state.get_name(), Box::new(ir_state))], vec![
      ir_ast,
    ]);

  output
}
