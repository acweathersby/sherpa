  use std::collections::{HashMap};
  use crate::{
    bytecode::compile::compile_ir_state_to_bytecode,
    debug::{
      BytecodeGrammarLookups,
      {self},
    },
    grammar::{parse::compile_ir_ast},
    intermediate::{
      compile::{compile_production_states, compile_states},
      optimize::optimize_ir_states,
    },
    journal::Journal,
    types::{default_get_branch_selector, GrammarStore, SherpaResult},
  };
  use super::compile_bytecode;


  #[test]
  pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production() -> SherpaResult<()>
  {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(&mut j, "<> A > \\h").unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let mut result = compile_production_states(&mut j, prod_id)?;

    println!("{:#?}", result);

    let ir_state = result.remove(0);

    let state = ir_state.get_code();

    let result = compile_ir_ast(Vec::from(state.as_bytes()));

    println!("{:#?}", result);

    assert!(result.is_ok());

    let result = compile_ir_state_to_bytecode(
      &result.unwrap().instructions,
      default_get_branch_selector,
      &HashMap::new(),
      &ir_state.get_scanner_state_name()?,
      "",
    );

    println!("{:#?}", result);

    SherpaResult::Ok(())
  }

  #[test]
  pub fn temp_test1() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(
      &mut j,
      "
    @NAME test

    @IGNORE g:sp 
    
    <> Term  >  Num     f:ast { [ $1 ] }
        | \\( Num \\)   f:ast { [ $2 ] }
    
    
    <> Num > g:num

",
    )
    .unwrap();

    let states = compile_states(&mut j, 1)?;

    let mut results = optimize_ir_states(&mut j, states);

    compile_bytecode(&mut j, results);

    SherpaResult::Ok(())
  }

  #[test]
  pub fn test_production_of_bytecode_for_simple_expression_grammar() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(
      &mut j,
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
    let mut states = compile_states(&mut j, 1)?;

    let mut results = optimize_ir_states(&mut j, states);

    let output = compile_bytecode(&mut j, results);

    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&mut j))
    );
    SherpaResult::Ok(())
  }

  #[test]
  pub fn generate_production_with_a_recursion() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(
      &mut j,
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

    let states = compile_states(&mut j, 1)?;
    let mut results = optimize_ir_states(&mut j, states);

    let output = compile_bytecode(&mut j, results);
    println!(
      "dD: {}",
      debug::generate_disassembly(&output, Some(&mut j))
    );

    SherpaResult::Ok(())
  }

  #[test]
  pub fn production_with_multiple_sub_productions() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(
      &mut j,
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

    let result = compile_production_states(&mut j, prod_id)?;

    for state in result {
      println!("{:#?}", state.get_code());
    }
    // println!(
    //   "dD: {}",
    //   debug::generate_disassembly(&output, Some(&BytecodeGrammarLookups::new(&g)))
    // );

    SherpaResult::Ok(())
  }
  #[test]
  pub fn temp_test() -> SherpaResult<()> {
    let mut j = Journal::new(None);
    let g = GrammarStore::from_str(&mut j, 
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

    let states = compile_states(&mut j, 1)?;

    let mut results = optimize_ir_states(&mut j, states);

    compile_bytecode(&mut j, results);

    SherpaResult::Ok(())
  }

