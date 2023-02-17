use crate::{
  bytecode::compile_bytecode,
  debug::{self},
  journal::Journal,
  parser::{compile_parse_states, optimize_parse_states},
  types::{GrammarStore, SherpaResult},
};

use super::utils::{console_debugger, test_runner, TestConfig};

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

  let states = compile_parse_states(&mut j, 1)?;

  let results = optimize_parse_states(&mut j, states);

  compile_bytecode(&mut j, &results);

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
  let states = compile_parse_states(&mut j, 1)?;

  let results = optimize_parse_states(&mut j, states);

  let output = compile_bytecode(&mut j, &results);

  println!("dD: {}", debug::generate_disassembly(&output, &mut j));
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

  let states = compile_parse_states(&mut j, 1)?;

  j.flush_reports();

  assert!(!j.debug_error_report());

  for (_, state) in &states {
    println!("{}", state.get_code());
  }

  let results = optimize_parse_states(&mut j, states);

  let output = compile_bytecode(&mut j, &results);
  println!("dD: {}", debug::generate_disassembly(&output, &mut j));

  SherpaResult::Ok(())
}

#[test]
pub fn temp_test() -> SherpaResult<()> {
  let input = r#"
NAME wick_element

IGNORE { c:sp c:nl }

<> element_block > '<' component_identifier
    ( element_attribute(+)  :ast { t_Attributes, c_Attribute, attributes: $1 } )? 
    ( element_attributes | general_data | element_block | general_binding )(*) 
    ">"

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
"#;

  test_runner(
    &[("element_block", "<i -test : soLongMySwanSong - store { test } <i>>", true).into()],
    None,
    TestConfig {
      grammar_string: Some(input),
      bytecode_parse: true,
      debugger_handler: Some(&|g| console_debugger(g, Default::default())),
      print_parse_reports: &["local_values_list_1"],
      ..Default::default()
    },
  )?;

  SherpaResult::Ok(())
}
