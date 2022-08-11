use std::collections::BTreeMap;

use crate::grammar::data::ast::IR_STATE;
use crate::grammar::parse::compile_ir_ast;
use crate::intermediate::state::compile_states;
use crate::types::GrammarStore;
use crate::types::IRState;
use crate::types::Symbol;

use self::compile::build_byte_code_buffer;

pub mod compile;
pub mod optimize;

pub struct BytecodeOutput<'a>
{
  pub grammar: &'a GrammarStore,
  /// The bytecode.
  pub bytecode: Vec<u32>,
  /// The intermediate representation states that the bytecode
  /// is based on.
  pub states: Vec<IR_STATE>,
  /// Maps plain state names to the offset within the bytecode
  /// vector.
  pub state_name_to_offset: BTreeMap<String, u32>,
  pub offset_to_state_name: BTreeMap<u32, String>,
  pub bytecode_id_to_symbol_lookup: BTreeMap<u32, &'a Symbol>,
  /// The original [IRStates](IRState) produced during the
  pub ir_states: BTreeMap<String, IRState>,
}

pub fn compile_bytecode(grammar: &GrammarStore, threads: usize) -> BytecodeOutput
{
  let mut ir_states = compile_states(grammar, threads);

  let ir_ast_states = ir_states
    .values_mut()
    .map(|s| match s.compile_ast() {
      Ok(ast) => (*ast).clone(),
      Err(err) => {
        panic!("\n{}", err);
      }
    })
    .collect::<Vec<_>>();

  compile_ir_states_into_bytecode(grammar, ir_states, ir_ast_states)
}

pub(crate) fn compile_ir_states_into_bytecode<'a>(
  grammar: &'a GrammarStore,
  ir_states: BTreeMap<String, IRState>,
  ir_ast_states: Vec<IR_STATE>,
) -> BytecodeOutput<'a>
{
  let state_refs = ir_ast_states.iter().collect::<Vec<_>>();

  let (bytecode, state_lookups) = build_byte_code_buffer(state_refs);

  BytecodeOutput {
    grammar,
    bytecode,
    ir_states,
    states: ir_ast_states,
    offset_to_state_name: state_lookups
      .iter()
      .map(|(a, b)| (*b, a.clone()))
      .collect::<BTreeMap<_, _>>(),
    state_name_to_offset: state_lookups,
    bytecode_id_to_symbol_lookup: grammar
      .symbols_table
      .values()
      .chain(Symbol::Generics)
      .map(|s| (s.bytecode_id, s))
      .collect::<BTreeMap<_, _>>(),
  }
}

#[cfg(test)]
mod byte_code_creation_tests
{

  use std::collections::HashMap;

  use crate::bytecode::compile::compile_ir_state_to_bytecode;
  use crate::debug::compile_test_grammar;
  use crate::debug::BytecodeGrammarLookups;
  use crate::debug::{self};
  use crate::grammar::data::ast::ASTNode;
  use crate::grammar::get_production_id_by_name;
  use crate::grammar::parse::compile_ir_ast;
  use crate::intermediate::state::generate_production_states;
  use crate::types::default_get_branch_selector;

  use super::compile_bytecode;

  #[test]
  pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production()
  {
    let grammar = compile_test_grammar("<> A > \\h");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    let state = result[0].get_code();

    let result = compile_ir_ast(Vec::from(state.as_bytes()));

    println!("{:#?}", result);

    assert!(result.is_ok());

    let result = compile_ir_state_to_bytecode(
      &result.unwrap(),
      default_get_branch_selector,
      &HashMap::new(),
    );

    println!("{:#?}", result);
  }

  #[test]
  pub fn test_production_of_bytecode_for_simple_expression_grammar()
  {
    let grammar = compile_test_grammar(
      "
      @IGNORE g:sp

      @EXPORT statement as entry
      
      @NAME llvm_language_test
      
      <> statement > expression
      
      <> expression > sum 
      
      <> sum > mul \\+ sum
          | mul
      
      <> mul > term \\* expression
          | term
      
      <> term > g:num
          | \\( expression \\)
      
      
",
    );

    let output = compile_bytecode(&grammar, 1);

    // output.ir_states.iter().for_each(|s| println!("{}", s.1.to_string()));

    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&grammar)))
    );
  }
}
