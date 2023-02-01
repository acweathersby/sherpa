use crate::{
  bytecode::{compile::build_byte_code_buffer, compile_bytecode},
  debug::{
    generate_disassembly,
    {self},
  },
  grammar::compile::compile_ir_ast,
  intermediate::{
    compile::{compile_production_states, compile_states},
    optimize::optimize_ir_states,
  },
  journal::Journal,
  types::{GrammarStore, SherpaResult},
};

#[test]
pub fn temp_test1() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    r#"
  NAME test

  IGNORE { c:sp }

  <> Term  >  Num     :ast [ $1 ]
      | "(" Num ")"   :ast [ $2 ]

  <> Num > c:num

"#,
  )
  .unwrap();

  let states = compile_states(&mut j, 1)?;

  let results = optimize_ir_states(&mut j, states);

  compile_bytecode(&mut j, results);

  SherpaResult::Ok(())
}

#[test]
pub fn test_production_of_bytecode_for_simple_expression_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    r##"
IGNORE { c:sp }
EXPORT statement as entry
NAME llvm_language_test

<> statement > expression :ast { t_Stmt, v:$1 }

<> expression > sum             

<> sum > sum "+" mul      :ast { t_Sum, l:$1, r:$3 }
    | mul

<> mul > mul "*" term     :ast { t_Mul, l:$1, r:$3 }
    | term

<> term > "2"             :ast { t_Num, v: u16($1) }

    | "(" expression ")"  :ast { t_Paren, v: $2 }
"##,
  )
  .unwrap();
  let states = compile_states(&mut j, 1)?;

  let results = optimize_ir_states(&mut j, states);

  let output = compile_bytecode(&mut j, results);

  eprintln!("dD: {}", debug::generate_disassembly(&output, Some(&mut j)));
  SherpaResult::Ok(())
}

#[test]
pub fn generate_production_with_a_recursion() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "    
    <> element_block > '<' component_identifier
    ( 'tested' )? 
    ( element_block | 'test' )(*) 
    '>'

<> component_identifier > 
    identifier

<> identifier > tk:tok_identifier 

<> tok_identifier > ( c:id ) ( c:id | c:num )(+)
",
  )
  .unwrap();

  let states = compile_states(&mut j, 1)?;

  j.flush_reports();

  assert!(!j.debug_error_report());

  for (_, state) in &states {
    println!("{}", state.get_code());
  }

  let results = optimize_ir_states(&mut j, states);

  let output = compile_bytecode(&mut j, results);
  eprintln!("dD: {}", debug::generate_disassembly(&output, Some(&mut j)));

  SherpaResult::Ok(())
}

#[test]
pub fn production_with_multiple_sub_productions() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    r##"    
<> test > "d" A | B | C | D
<> A > "a" id
<> B > "b" id
<> C > "c" id
<> D > "d" id

<> id > c:id
"##,
  )
  .unwrap();

  let prod_id = g.get_production_id_by_name("test").unwrap();

  let result = compile_production_states(&mut j, prod_id)?;

  for state in result {
    eprintln!("{}", state.get_code());
  }

  SherpaResult::Ok(())
}
#[test]
pub fn temp_test() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(&mut j,
    r#"
NAME wick_element

IGNORE { c:sp c:nl }

<> element_block > '<' component_identifier
    ( element_attribute(+)  :ast { t_Attributes, c_Attribute, attributes: $1 } )? 
    ( element_attributes | general_data | element_block | general_binding )(*) 
    '>'

                                                                :ast { t_Element, id:$2, children: [$3, $4], tok }
<> component_identifier > 
    identifier ( ':' identifier )?
                                                                :ast { t_Ident, name:str($1), sub_name:str($2), tok }

<> element_attributes >c:nl element_attribute(+)               
                                                                :ast { t_Attributes, c_Attribute, attributes: $2 }

<> element_attribute > '-' identifier attribute_chars c:sp

                                                                :ast { t_GeneralAttr, c_Attribute, key:str($2), val1: str($3) }

    | '-' identifier ':' identifier 
                                                                :ast { t_BindingAttr, c_Attribute, key:str($2), val2: str($4) }

    | '-' "store" '{' local_values? '}' 
                                                                :ast { t_StoreAttr, c_Attribute, children: $4 }
    | '-' "local" '{' local_values? '}' 
                                                                :ast { t_LocalAttr, c_Attribute, children: $4 }
    | '-' "param" '{' local_values? '}' 
                                                                :ast { t_ParamAttr, c_Attribute, children: $4 }
    | '-' "model" '{' local_values? '}' 
                                                                :ast { t_ModelAttr, c_Attribute, children: $4 }

<> general_binding > ':' identifier               
                                                                :ast { t_OutputBinding, val3:str($2) }

<> local_values > local_value(+)

<> local_value > identifier ( '`' identifier )? ( '='  c:num )? ( ',' )(*)

                                                                :ast { t_Var, c_Attribute, name:str($1), meta:str($2), value:$3 }

<> attribute_chars > ( c:id | c:num | c:sym  )(+)
                                                                :ast { t_AttributeData, tok }
<> general_data > ( c:id | c:num  | c:nl  )(+)
                                                                :ast { t_GeneralData, tok }

<> identifier > tk:tok_identifier 

<> tok_identifier > ( c:id | c:num )(+)
"#,
  ).unwrap();

  let states = compile_states(&mut j, 1)?;

  let results = optimize_ir_states(&mut j, states);

  compile_bytecode(&mut j, results);

  SherpaResult::Ok(())
}

#[test]
pub fn test_produce_a_single_ir_ast_from_a_single_state_of_a_trivial_production() -> SherpaResult<()>
{
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > 'h'? 'e'? 'l' 'l' 'o'").unwrap();

  let prod_id = g.get_production_id_by_name("A").unwrap();

  let states = compile_states(&mut j, 1)?;

  let results = optimize_ir_states(&mut j, states);

  let output = compile_bytecode(&mut j, results);

  let result = compile_production_states(&mut j, prod_id)?;

  let states = result
    .into_iter()
    .map(|s| {
      let string = s.get_code();
      let result = compile_ir_ast(&string);
      assert!(result.is_ok());
      result.unwrap()
    })
    .collect::<Vec<_>>();

  let state_refs = states.iter().collect::<Vec<_>>();

  let _ = build_byte_code_buffer(state_refs);

  eprintln!("{}", generate_disassembly(&output, Some(&mut j)));

  SherpaResult::Ok(())
}
