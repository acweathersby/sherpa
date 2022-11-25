use std::collections::BTreeMap;

use crate::grammar::data::ast::IR_STATE;
use crate::grammar::parse::compile_ir_ast;
use crate::intermediate::state::compile_states;
use crate::types::GrammarStore;
use crate::types::IRState;
use crate::types::IRStateType;
use crate::types::Symbol;
use crate::types::SymbolID;

use self::compile::build_byte_code_buffer;

pub mod compile;

/// Store metadata for each state present in the bytecode.
#[derive(Debug)]
pub struct StateData {
  state_type:  IRStateType,
  stack_depth: u32,
  _is_scanner: bool,
  name:        String,
}

impl StateData {
  pub fn from_ir_state(state: &IRState) -> Self {
    Self {
      state_type:  state.state_type,
      stack_depth: state.stack_depth,
      _is_scanner: state.is_scanner(),
      name:        state.get_name(),
    }
  }

  pub fn get_type(&self) -> IRStateType {
    self.state_type
  }

  pub fn get_stack_depth(&self) -> u32 {
    self.stack_depth
  }

  pub fn is_scanner(&self) -> bool {
    self._is_scanner
  }

  pub fn get_name(&self) -> String {
    self.name.clone()
  }
}

#[derive(Debug)]
pub struct BytecodeOutput {
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
  pub state_data: BTreeMap<String, StateData>,
}

pub fn compile_bytecode<'a>(
  g: &'a GrammarStore,
  ir_states: &mut Vec<(String, Box<IRState>)>,
) -> BytecodeOutput {
  let ir_ast_states = ir_states
    .iter_mut()
    .map(|(_, s)| match s.compile_ast() {
      Ok(ast) => (*ast).clone(),
      Err(err) => {
        panic!("\n{}", err);
      }
    })
    .collect::<Vec<_>>();

  compile_ir_states_into_bytecode(g, ir_states, ir_ast_states)
}

pub(crate) fn compile_ir_states_into_bytecode<'a>(
  g: &'a GrammarStore,
  ir_states: &Vec<(String, Box<IRState>)>,
  ir_ast_states: Vec<IR_STATE>,
) -> BytecodeOutput {
  let state_refs = ir_ast_states.iter().collect::<Vec<_>>();

  let (bytecode, state_lookups) = build_byte_code_buffer(state_refs);

  BytecodeOutput {
    bytecode,
    states: ir_ast_states,
    offset_to_state_name: state_lookups
      .iter()
      .map(|(a, b)| (*b, a.clone()))
      .collect::<BTreeMap<_, _>>(),
    state_name_to_offset: state_lookups,
    bytecode_id_to_symbol_lookup: g
      .symbols
      .values()
      .chain(Symbol::Generics)
      .map(|s| (s.bytecode_id, s.clone()))
      .collect::<BTreeMap<_, _>>(),
    state_data: ir_states
      .iter()
      .map(|(s, state)| (s.clone(), StateData::from_ir_state(state)))
      .collect(),
  }
}

#[cfg(test)]
mod byte_code_creation_tests {

  use std::collections::BTreeSet;
  use std::collections::HashMap;

  use crate::bytecode::compile::compile_ir_state_to_bytecode;
  use crate::debug::BytecodeGrammarLookups;
  use crate::debug::{self};
  use crate::grammar::data::ast::ASTNode;
  use crate::grammar::parse::compile_ir_ast;
  use crate::intermediate::optimize::optimize_ir_states;
  use crate::intermediate::state::compile_states;
  use crate::intermediate::state::generate_production_states;
  use crate::types::default_get_branch_selector;
  use crate::types::GrammarStore;

  use super::compile_bytecode;

  #[test]
  pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production() {
    let g = GrammarStore::from_str("<> A > \\h").unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let result = generate_production_states(&prod_id, g).states;

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
      "",
    );

    println!("{:#?}", result);
  }

  #[test]
  pub fn test_production_of_bytecode_for_simple_expression_grammar() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();
    let (mut ir_states, _) = compile_states(g.clone(), 1);

    let output = compile_bytecode(&g, &mut optimize_ir_states(ir_states, &g));

    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&g)))
    );
  }

  #[test]
  pub fn generate_production_with_a_recursion() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();

    let (mut states, _) = compile_states(g.clone(), 1);
    let output = compile_bytecode(&g, &mut optimize_ir_states(states, &g));
    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&g)))
    );
  }

  #[test]
  pub fn production_with_multiple_sub_productions() {
    let g = GrammarStore::from_str(
      "    
<> test > t:d A | B | C | D
<> A > t:a id
<> B > t:b id
<> C > t:c id
<> D > t:d id

<> id > g:id
",
    )
    .unwrap();

    let prod_id = g.get_production_id_by_name("test").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    for state in result {
      println!("{:#?}", state.get_code());
    }
    // println!(
    //   "dD: {}",
    //   debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&g)))
    // );
  }
  #[test]
  pub fn temp_test() {
    let g = GrammarStore::from_str(
      "@NAME wick_element

      @IGNORE g:sp g:nl
      
      <> element_block > \\< component_identifier
          ( element_attribute(+)  f:r { { t_Attributes, c_Attribute, attributes: $1 } } )? 
          ( element_attributes | general_data | element_block | general_binding )(*) 
          \\>
      
                                                                      f:ast { { t_Element, id:$2, children: [$3, $4], tok } }
      <> component_identifier > 
          identifier ( \\: identifier )?
                                                                      f:ast { { t_Ident, name:str($1), sub_name:str($2), tok } }
      
      <> element_attributes >g:nl element_attribute(+)               
                                                                      f:ast { { t_Attributes, c_Attribute, attributes: $2 } }
      
      <> element_attribute > \\- identifier attribute_chars g:sp
      
                                                                      f:ast { { t_GeneralAttr, c_Attribute, key:str($2), val1: str($3) } }
      
          | \\- identifier \\: identifier 
                                                                      f:ast { { t_BindingAttr, c_Attribute, key:str($2), val2: str($4) } }
      
          | \\- t:store \\{ local_values? \\} 
                                                                      f:ast { { t_StoreAttr, c_Attribute, children: $4 } }
          | \\- t:local \\{ local_values? \\} 
                                                                      f:ast { { t_LocalAttr, c_Attribute, children: $4 } }
          | \\- t:param \\{ local_values? \\} 
                                                                      f:ast { { t_ParamAttr, c_Attribute, children: $4 } }
          | \\- t:model \\{ local_values? \\} 
                                                                      f:ast { { t_ModelAttr, c_Attribute, children: $4 } }
      
      <> general_binding > \\: identifier               
                                                                      f:ast { { t_OutputBinding, val3:str($2) } }
      
      <> local_values > local_value(+)
      
      <> local_value > identifier ( \\` identifier )? ( \\=  g:num f:r{ $2 } )? ( \\, )(*)
      
                                                                      f:ast { { t_Var, c_Attribute, name:str($1), meta:str($2), value:$3 } }
      
      <> attribute_chars > ( g:id | g:num | g:sym  )(+)
                                                                      f:ast { { t_AttributeData, tok } }
      <> general_data > ( g:id | g:num  | g:nl  )(+)
                                                                      f:ast { { t_GeneralData, tok } }
      
      <> identifier > tk:tok_identifier 
      
      <> tok_identifier > ( g:id | g:num )(+)                     ",
    ).unwrap();

    let (mut states, _) = compile_states(g.clone(), 1);
    let output = compile_bytecode(&g, &mut optimize_ir_states(states, &g));
    return;
    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&g)))
    );
  }
}
