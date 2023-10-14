use std::collections::hash_map::HashMap;

use crate::types::*;
use sherpa_core::{CachedString, IStringStore, SherpaError, SherpaResult};

use crate::parser;

#[test]
fn formatter_parser_constructs_ast_for_simpler_formatter() -> SherpaResult<()> {
  parser::ast::default_from("readers".into()).map(|_| Ok(()))?
}

#[test]
fn construct_formatter_interpreter() -> SherpaResult<()> {
  match "result".into() {
    FormatterResult::Ok(formatter) => Ok(()),
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_script() -> SherpaResult<()> {
  match "test".into() {
    FormatterResult::Ok(mut formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!("test", formatter.build(&mut context)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_script_indent_spaces() -> SherpaResult<()> {
  match r#"test@+\ntest@+\ntest@-\ntest\n@-@-@-"#.into() {
    FormatterResult::Ok(mut formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!(
        "test
  test
    test
  test
",
        formatter.build(&mut context)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_function() -> SherpaResult<()> {
  match r###"#t { 
  {{ This is a complex string that comes up frequently.  }}
}
  
#t() \n #t()
  
  
"###
    .into()
  {
    FormatterResult::Ok(mut formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!(
        "This is a complex string that comes up frequently.\nThis is a complex string that comes up frequently.",
        formatter.build(&mut context)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_function_with_match_statement() -> SherpaResult<()> {
  match r###"

#two { two }

#nums @n:num {
  match @n {
    4 { four }
  }
}

#t @val:num { 
   match @val { 1 { {{ one }} } 2 { #two() } 3 { [[1 + 2]] } 1+3 { 4 } { zero } }
}

#t(1) @1 #t(2) @2 #t(3) @3 #t(4) @4 #t(1000)

"###
    .into()
  {
    FormatterResult::Ok(mut formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!("one two  3   4    zero", formatter.build(&mut context)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function() -> SherpaResult<()> {
  match r###"

#type_obj @t:obj { 
    @t.name
}

@test

"###
    .into()
  {
    FormatterResult::Ok(mut formatter) => {
      let s_store = IStringStore::default();
      let mut test_obj = HashMap::new();
      test_obj.insert("name".to_token(), Value::Str("test_printer".intern(&s_store)));

      let mut values = HashMap::new();
      values.insert("@test".to_token(), Value::Obj("obj".to_token(), &test_obj));

      let mut context = FormatterContext::new_with_values(&values, s_store);
      assert_eq!("test_printer", formatter.build(&mut context)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}
