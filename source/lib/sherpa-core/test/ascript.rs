use crate::{
  ascript::{
    compile::{compile_struct_props, compile_struct_type, verify_property_presence},
    output_base::AscriptWriter,
    rust::{create_rust_writer_utils, write_rust_ast},
    types::{AScriptStore, AScriptTypeVal},
  },
  grammar::compile::{
    compile_ascript_struct,
    compile_grammar_ast,
    parser::sherpa::{self, ASTNode},
  },
  journal::*,
  types::*,
  writer::code_writer::CodeWriter,
};
use std::path::PathBuf;

#[test]
fn test_grammar_imported_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/script_base.sg")
      .canonicalize()
      .unwrap(),
  )
  .unwrap();

  let store = AScriptStore::new(&mut j)?;

  assert_eq!(store.prod_types.len(), g.parse_productions.len());

  assert!(store
    .prod_types
    .iter()
    .all(|p| { p.1.iter().all(|t| !matches!(t.0.type_, AScriptTypeVal::Undefined)) }));

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output()).unwrap());

  SherpaResult::Ok(())
}

#[test]
fn test_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
IGNORE { c:sp }
EXPORT statement as entry
NAME llvm_language_test

<> statement > expression    :ast { t_Stmt, v:$1 }

<> expression > sum num

<> sum > mul '+' sum         :ast { t_Sum, l:$1, r:$3 }
    | mul

<> mul > term '*' expression :ast { t_Mul, l:$1, r:$3 }
    | term

<> term > num                :ast { t_Num, v:$1 }

    | '(' expression ')'     :ast { t_Paren, v: $2 }

<> num > c:num '.' c:num     :ast [$1, $3]
",
  )?;

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output()).unwrap());

  SherpaResult::Ok(())
}

#[test]
fn test_add_hoc_vector_prop_merged_with_vector_production() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
<> statement > adhoc        :ast { t_Expr, v:[$1] }
    | '{' adhoc(+) '}'      :ast { t_Expr, v:$2 }

<> adhoc > 'test'           :ast tok
",
  );

  assert!(!j.debug_error_report(), "Should not have grammar errors");

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output())?);

  SherpaResult::Ok(())
}

#[test]
fn handles_multipart_arrays() -> SherpaResult<()> {
  use SherpaResult::*;
  let mut j = Journal::new(Option::None);
  GrammarStore::from_str(
    &mut j,
    r##"     
      <> A > B(+) | C 

      <> B > 'tok'

      <> C > D(+ "t" ) 
             ( "x" "y" "z" )?
             ( "x" "y" "z" :ast tok )?

              :ast [ $1, $2, $3 ]

        | ( "ggg" "rrr" )

              :ast{ [$1] }

      <> D > 'xxx'
  "##,
  )
  .unwrap();

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  write_rust_ast(w)?;

  Ok(())
}

#[test]
fn rust_vector_return_types_print_correctly() -> SherpaResult<()> {
  use SherpaResult::*;
  let mut j = Journal::new(Option::None);
  GrammarStore::from_str(
    &mut j,
    " 
        <> A > B :ast { t_A, r:$1 }

        <> B > 'z'? ( 'd' )(*)  :ast { [$1, $2] }
        ",
  )
  .unwrap();

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output())?);

  Ok(())
}

#[test]
fn group_productions_get_correct_type_information() -> SherpaResult<()> {
  let mut j = Journal::new(Option::None);
  GrammarStore::from_str(
    &mut j,
    r##"
NAME hc_symbol

IGNORE { c:sp c:nl }

<> annotated_symbol > 

        symbol^s [unordered tk:reference?^r "?"?^o ]

            :ast { t_AnnotatedSymbol, symbol:$s, is_optional:bool($o), reference:str($r), tok  }

        | symbol

<> symbol > class

<> class >

        "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' )

            :ast { t_Class, c_Symbol , c_Terminal, val:str($2),  tok }

<> reference > 

        "^" tk:identifier_syms

<> identifier > 

        tk:identifier_syms 

<> identifier_syms >  

        identifier_syms c:id

        | identifier_syms '_'

        | identifier_syms '-'

        | identifier_syms c:num

        | '_'

        | '-' 

        | c:id
        "##,
  )
  .unwrap();

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output())?);

  SherpaResult::Ok(())
}

// pri

#[test]
fn test_parse_errors_when_production_has_differing_return_types3() -> SherpaResult<()> {
  let mut j = Journal::new(Option::None);

  GrammarStore::from_str(&mut j, "<> B > c:id(+)").unwrap();

  let store = AScriptStore::new(&mut j).unwrap();

  println!("{:#?}", store);

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output()).unwrap());

  SherpaResult::Ok(())
}

fn create_dummy_body(id: RuleId) -> Rule {
  Rule { id, ..Default::default() }
}

#[test]
fn test_parse_errors_when_struct_prop_type_is_redefined() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  j.set_active_report("test", ReportType::AScriptCompile);

  let astA = compile_ascript_struct(" { t_TestA, apple: u32 }")?;
  let astB = compile_ascript_struct(" { t_TestA, apple: i64 }")?;

  let mut ast = AScriptStore::default();

  let rule = create_dummy_body(RuleId(0));

  let id = compile_struct_type(&mut j, &mut ast, &astA, &rule);
  compile_struct_props(&mut j, &mut ast, &id, &astA, &rule);

  assert!(!j.report().debug_error());

  let id = compile_struct_type(&mut j, &mut ast, &astB, &rule);
  compile_struct_props(&mut j, &mut ast, &id, &astB, &rule);

  assert!(j.report().debug_error());

  SherpaResult::Ok(())
}

#[test]
fn test_prop_is_made_optional_when_not_present_or_introduced_in_subsequent_definitions(
) -> SherpaResult<()> {
  let mut j = Journal::new(None);
  j.set_active_report("test", ReportType::AScriptCompile);
  let mut ast = AScriptStore::default();

  for (i, struct_) in [
    " { t_TestA, apple: u32, beetle:bool }",
    " { t_TestA, beetle:bool }",
    " { t_TestB }",
    " { t_TestB, apple: u32 }",
  ]
  .iter()
  .map(|input| compile_ascript_struct(input))
  .enumerate()
  {
    assert!(struct_.is_ok());

    let struct_ = struct_.unwrap();

    let rule = create_dummy_body(RuleId(i as u64));

    let id = compile_struct_type(&mut j, &mut ast, &struct_, &rule);

    compile_struct_props(&mut j, &mut ast, &id, &struct_, &rule);

    assert!(!j.report().debug_error());
  }

  for struct_id in ast.structs.keys().cloned().collect::<Vec<_>>() {
    verify_property_presence(&mut ast, &struct_id);
  }

  for prop in &ast.props {
    if prop.0.name == "beetle" {
      assert!(
        !prop.1.optional,
        "Expected {}~{} to not be optional",
        ast.structs.get(&prop.0.struct_id).unwrap().type_name,
        prop.0.name
      );
    } else {
      assert!(
        prop.1.optional,
        "Expected {}~{} to be optional",
        ast.structs.get(&prop.0.struct_id).unwrap().type_name,
        prop.0.name
      );
    }
  }

  SherpaResult::Ok(())
}

#[test]
fn test_parse_errors_when_production_has_differing_return_types() {
  let mut j = Journal::new(Option::None);
  GrammarStore::from_str(
    &mut j,
    r#"
<> A > "1" :ast { t_Test }
| 'a'
"#,
  )
  .unwrap();

  match AScriptStore::new(&mut j) {
    SherpaResult::Err(_) => {
      assert!(j.report().debug_error());
    }
    _ => unreachable!("This should have generated an error"),
  }
}

#[test]
fn test_ASTs_are_defined_for_ascript_return_functions() -> SherpaResult<()> {
  let grammar = "<> A > '1' :ast { t_Test, val: str($1) } ";

  let grammar_ast = compile_grammar_ast(grammar)?;

  let box sherpa::Production { rules, .. } = &grammar_ast.productions[0];
  let box sherpa::Rule { ast_definition, .. } = &rules[0];

  if let Some(box sherpa::Ascript { ast, .. }) = &ast_definition {
    if let ASTNode::AST_Struct(box sherpa::AST_Struct { typ, props, .. }) = ast {
      assert_eq!(props.len(), 1);

      assert_eq!(typ.to_string(), "t_Test");

      if let ASTNode::AST_Property(box sherpa::AST_Property { id, value, .. }) = &props[0] {
        assert_eq!(id, "val");

        if let Some(ASTNode::AST_STRING(..)) = value {
        } else {
          panic!("Prop is not a string");
        }
      } else {
        panic!("Incorrect prop");
      }
    } else {
      panic!("Script value is not a struct.")
    }
  } else {
    panic!("AScripT expression not found.")
  }

  SherpaResult::Ok(())
}

#[test]
fn test_rust_render() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
<> statement > adhoc        :ast { t_Expr, v:[$1], t:str(tok<1,1>) }
    | '{' adhoc(+) '}'      :ast { t_Expr, v:$2 }

<> adhoc > 'test'           :ast tok
",
  );
  assert!(!j.debug_error_report(), "Should not have grammar errors");

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output())?);

  /*   let writer = build_c(&store, CodeWriter::new(vec![]))?;

  println!("{}", String::from_utf8(writer.into_writer().into_output())?); */

  SherpaResult::Ok(())
}

#[test]
fn eof_symbols_should_not_contribute_anything_to_ast() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
    <> A > B $ 

    <> B > \"2\" :ast u32($1)
    
    ",
  );
  assert!(!j.debug_error_report(), "Should not have grammar errors");

  let store = AScriptStore::new(&mut j)?;

  let u = create_rust_writer_utils(&store);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast(w)?;

  println!("{}", String::from_utf8(writer.into_writer().into_output())?);

  let g = &j.grammar()?;

  let t: AScriptTypeVal =
    store.prod_types.get(&g.get_production_id_by_name("A")?)?.iter().next()?.0.into();

  assert!(matches!(t, AScriptTypeVal::U32(_)), "{:?} should be a u32 type", t);

  SherpaResult::Ok(())
}
