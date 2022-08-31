use std::collections::BTreeMap;

use crate::grammar::data::ast::IR_STATE;
use crate::grammar::parse::compile_ir_ast;
use crate::intermediate::state::compile_states;
use crate::types::GrammarStore;
use crate::types::IRState;
use crate::types::Symbol;
use crate::types::SymbolID;

use self::compile::build_byte_code_buffer;

pub mod compile;
pub mod optimize;

#[derive(Debug)]
pub struct BytecodeOutput
{
  /// The bytecode.
  pub bytecode: Vec<u32>,
  /// The intermediate representation states that the bytecode
  /// is based on.
  pub states: Vec<IR_STATE>,
  /// Maps plain state names to the offset within the bytecode
  /// vector.
  pub state_name_to_offset: BTreeMap<String, u32>,
  pub offset_to_state_name: BTreeMap<u32, String>,
  pub bytecode_id_to_symbol_lookup: BTreeMap<u32, Symbol>,
  /// The original [IRStates](IRState) produced during the
  pub ir_states: BTreeMap<String, IRState>,
}

pub fn compile_bytecode<'a>(grammar: &'a GrammarStore, threads: usize) -> BytecodeOutput
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
) -> BytecodeOutput
{
  let state_refs = ir_ast_states.iter().collect::<Vec<_>>();

  let (bytecode, state_lookups) = build_byte_code_buffer(state_refs);

  println!("{:#?}", grammar.symbols_table);

  BytecodeOutput {
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
      .map(|s| (s.bytecode_id, s.clone()))
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
      &result.unwrap().instructions,
      default_get_branch_selector,
      &HashMap::new(),
      &("".to_string()),
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
      
      <> statement > expression       f:ast { { t_Stmt, v:$1 } }

      <> expression > sum             
      
      <> sum > sum \\+ mul             f:ast { { t_Sum, l:$1, r:$3 } }
          | mul
      
      <> mul > mul \\* term     f:ast { { t_Mul, l:$1, r:$3 } }
          | term
      
      <> term > \\2                f:ast { { t_Num, v: u16($1) } }
      
          | \\( expression \\)          f:ast { { t_Paren, v: $2 } }
      
      
",
    );

    let output = compile_bytecode(&grammar, 1);

    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&grammar)))
    );
  }

  #[test]
  pub fn generate_production_with_a_recursion()
  {
    let grammar = compile_test_grammar(
      "    
      <> element_block > \\< component_identifier
      ( t:tested )? 
      ( element_block | t:test )(*) 
      \\>
      
  <> component_identifier > 
      identifier

  <> identifier > tk:tok_identifier 
  
  <> tok_identifier > ( g:id ) ( g:id | g:num )(+)
",
    );

    let output = compile_bytecode(&grammar, 1);
    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&grammar)))
    );
  }

  #[test]
  pub fn production_with_multiple_sub_productions()
  {
    let grammar = compile_test_grammar(
      "    
<> test > t:d A | B | C | D
<> A > t:a id
<> B > t:b id
<> C > t:c id
<> D > t:d id

<> id > g:id
",
    );

    let prod_id = get_production_id_by_name("test", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    for state in result {
      println!("{:#?}", state.get_code());
    }
    // println!(
    //   "dD: {}",
    //   debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&grammar)))
    // );
  }
}
