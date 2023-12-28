use radlr_ascript::{output_base::AscriptWriter, types::AScriptStore};
use radlr_core::{
  test::utils::{build_parse_db_from_source_str, build_parse_states_from_source_str},
  *,
};

use crate::builder::{create_rust_writer_utils, write_rust_ast};

#[test]
fn temp_test_grammar() -> RadlrResult<()> {
  build_parse_states_from_source_str(
    "
  IGNORE { c:sp }
  EXPORT statement as entry
  NAME llvm_language_test
  
  <> statement > \"test\"    :ast { t_Stmt, v:$1 }
  ",
    "/test.sg".into(),
    Default::default(),
    &|TestPackage { mut journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      journal.flush_reports();

      assert!(!journal.debug_error_report());

      let u = create_rust_writer_utils(&store, &db);

      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));

      let writer = write_rust_ast(w)?;
      eprintln!("{}", String::from_utf8(writer.into_writer().into_output()).unwrap());

      RadlrResult::Ok(())
    },
  )
}
#[test]
fn test_grammar() -> RadlrResult<()> {
  build_parse_states_from_source_str(
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
    "/test.sg".into(),
    Default::default(),
    &|TestPackage { mut journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      journal.flush_reports();

      assert!(!journal.debug_error_report());

      let u = create_rust_writer_utils(&store, &db);

      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));

      let writer = write_rust_ast(w)?;
      eprintln!("{}", String::from_utf8(writer.into_writer().into_output()).unwrap());

      RadlrResult::Ok(())
    },
  )
}

#[test]
fn test_add_hoc_vector_prop_merged_with_vector_nonterminal() -> RadlrResult<()> {
  build_parse_states_from_source_str(
    "
  <> statement > adhoc        :ast { t_Expr, v:[$1] }
      | '{' adhoc(+) '}'      :ast { t_Expr, v:$2 }

  <> adhoc > 'test'           :ast tok
  ",
    "/test.sg".into(),
    Default::default(),
    &|TestPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )?;

  RadlrResult::Ok(())
}

#[test]
fn handles_multipart_arraysd() -> RadlrResult<()> {
  build_parse_states_from_source_str(
    r##"
        <> A > B(+) 
        <> B > 'tok'
    "##,
    "/test.sg".into(),
    Default::default(),
    &|TestPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      write_rust_ast(w)?;
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn handles_multipart_arrays() -> RadlrResult<()> {
  build_parse_states_from_source_str(
    r##"
        <> A > B(+) | C

        <> B > 'tok'

        <> C > D(+ "t" )
               ( "x" "y" "z" )?
               ( "x" "y" "t" :ast tok )?

                :ast [ $1, $2, $3 ]

          | ( "ggg" "rrr" )

                :ast{ [$1] }

        <> D > 'xxx'
    "##,
    "/test.sg".into(),
    Default::default(),
    &|TestPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      write_rust_ast(w)?;
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn rust_vector_return_types_print_correctly() -> RadlrResult<()> {
  build_parse_states_from_source_str(
    "
          <> A > B :ast { t_A, r:$1 }

          <> B >  [ 'z'? ( 'd' )(*) ]  :ast { [$1, $2] }
          ",
    "/test.sg".into(),
    Default::default(),
    &|TestPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn group_nonterminals_get_correct_type_information() -> RadlrResult<()> {
  build_parse_db_from_source_str(
    r##"
  NAME hc_symbol

  IGNORE { c:sp c:nl }

  <> annotated_symbol >

          symbol^s [ tk:reference?^r "?"?^o ]

              :ast { t_AnnotatedSymbol, symbol:$s, is_optional:bool($o), reference:str($r), tok  }

          | symbol

  <> symbol > class

  <> class >

          "c:" ( 'num' | 'nl' | 'sp' | 'id' | 'sym' | 'any' )

              :ast { t_Class, val:str($2),  tok }

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
    "/test.sg".into(),
    &|DBPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn token_range_slice() -> RadlrResult<()> {
  build_parse_db_from_source_str(
    r#"<> A > "1234" :ast { t_R, d:str(tok<1,2>) }"#,
    "/test.sg".into(),
    &|DBPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn reference_nonterminal_and_reference_names_when_using_valueless_props() -> RadlrResult<()> {
  build_parse_db_from_source_str(
    r#"      <> A > R "1234"^num :ast { t_A, num }

    <> R > A :ast { t_R, A }"#,
    "/test.sg".into(),
    &|DBPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn convert_str_to_numeric() -> RadlrResult<()> {
  build_parse_db_from_source_str(
    r#"
    <> A > "1234" :ast { t_R, d:str($1) }

    <> B > "1234" :ast str($1)

    <> C > c:id(+)"#,
    "/test.sg".into(),
    &|DBPackage { journal, db, .. }| {
      let store = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )
}

#[test]
fn temp() -> RadlrResult<()> {
  build_parse_db_from_source_str(
    r#"
    <> non_branch_statement > A (+) "t" :ast $1

    <> A > c:num  :ast u32($1)
    "#,
    "/test.sg".into(),
    &|DBPackage { journal, db, .. }| {
      let store: AScriptStore = AScriptStore::new(journal.transfer(), &db)?;

      let u = create_rust_writer_utils(&store, &db);
      let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
      let writer = write_rust_ast(w)?;

      eprintln!("{}", String::from_utf8(writer.into_writer().into_output())?);
      RadlrResult::Ok(())
    },
  )
}
