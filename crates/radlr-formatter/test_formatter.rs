use std::collections::hash_map::HashMap;

use crate::{formatter::FormatterResult, types::*};
use radlr_core::{CachedString, IStringStore, RadlrResult};

use crate::parser;

#[test]
fn test_trivial_formatter_script() -> RadlrResult<()> {
  parser::ast::default_from("readers".into()).map(|_| Ok(()))?
}

#[test]
fn construct_formatter_interpreter() -> RadlrResult<()> {
  match "result".into() {
    FormatterResult::Ok(_formatter) => Ok(()),
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_script() -> RadlrResult<()> {
  match "test".into() {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new("test", IStringStore::default());
      assert_eq!("test", formatter.write_to_string(&mut context, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_script_indent_spaces() -> RadlrResult<()> {
  match r#"test@+\ntest@+\ntest@-\ntest\n@-@-@-"#.into() {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new("test", IStringStore::default());
      assert_eq!(
        "test
  test
    test
  test
",
        formatter.write_to_string(&mut context, 1024)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_function() -> RadlrResult<()> {
  match r###"#t { 
   @" This is a complex string that comes up frequently. "
}
  
#t() \n #t()
  
  
"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new("test", IStringStore::default());
      assert_eq!(
        "This is a complex string that comes up frequently.\nThis is a complex string that comes up frequently.",
        formatter.write_to_string(&mut context, 1024)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_function_with_match_statement() -> RadlrResult<()> {
  match r###"

#two { two }

#nums @n:num {
  match @n {
    4 { four }
  }
}

#t @val:num { 
   match @val { 1 { @" one " } 2 { #two() } 3 { @[1 + 2] } 1+3 { 4 } { zero } }
}

#t(1) @1 #t(2) @2 #t(3) @3 #t(4) @4 #t(1000)

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new("test", IStringStore::default());
      assert_eq!("one two  3   4    zero", formatter.write_to_string(&mut context, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function_for_map() -> RadlrResult<()> {
  match r###"

#type_obj { 
    @self.name
}

@test

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let s_store = IStringStore::default();
      let mut test_obj = HashMap::new();
      test_obj.insert("name".to_string(), Value::Str("test_printer".intern(&s_store)));

      let mut ctx = FormatterContext::new("test", s_store);
      ctx.set_val("test", Value::Obj(&test_obj));

      assert_eq!("test_printer", formatter.write_to_string(&mut ctx, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function_for_list() -> RadlrResult<()> {
  match r###"

#read_list @t:obj @i:int {
  match @t.len - @i {
    @t.len  { @t.[@i] #read_list( @t, @i + 1 ) }
            { @1 @t.[@i] #read_list( @t, @i + 1 ) }
    0       { } 
  }
}

#type_list { #read_list(self, 0) }

@test

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut list = vec![];

      for i in 0..20 {
        list.push(Value::Int(i + 1))
      }
      let s_store = IStringStore::default();

      let mut ctx = FormatterContext::new("test", s_store);
      ctx.set_val("test", Value::Obj(&list));

      assert_eq!("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20", formatter.write_to_string(&mut ctx, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function_for_list_with_tail_call_optimization() -> RadlrResult<()> {
  match r###"

#read_list @t:obj @i:int {
  match @t.len - @i {
    @t.len  { @t.[@i] #read_list( @t, @i + 1 ) }
            { @1 @t.[@i] #read_list( @t, @i + 1 ) }
    0       { } 
  }
}

#type_list { #read_list(self, 0) }

@test

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut list = vec![];
      let mut string = vec![];

      for i in 1..=200000 {
        list.push(Value::Int(i));
        string.push(i.to_string());
      }
      let s_store = IStringStore::default();

      let mut ctx = FormatterContext::new("test", s_store);
      ctx.set_val("test", Value::Obj(&list));

      assert_eq!(string.join(" "), formatter.write_to_string(&mut ctx, 1 << 20)?);
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn construct_length_limited_block() -> RadlrResult<()> {
  match r###"

#read_list t:obj i:int {
  match t.len - i {
    t.len   { @t.[i] @; #read_list( @t, @i + 1 ) }
            { @t.[i] @; #read_list( @t, @i + 1 ) }
    1       { @t.[i] }
  }
}

#type_list { #read_list(self, 0) }

\n@+
{ { @test  } }
@-

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut list = vec![];
      let mut string = vec![];

      for i in 1..=10 {
        list.push(Value::Int(i));
        string.push(i.to_string());
      }

      let mut fm = FormatterContext::new("test", IStringStore::default());
      fm.max_width = 4;
      fm.tab_size = 2;
      fm.set_val("test", Value::Obj(&list));

      assert_eq!(
        "
  {
    {
      1
      2
      3
      4
      5
      6
      7
      8
      9
      10
    }
  }",
        formatter.write_to_string(&mut fm, 1 << 20)?,
        "{}",
        formatter.write_to_string(&mut fm, 1 << 20)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
fn iterator_syntax() -> RadlrResult<()> {
  match r###"

#read_list  {
  @self.[i] match iter_last { false { @; } }
}

\n@+
{ { @test.iter#read_list()  } }
@-

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut list = vec![];
      let mut string = vec![];

      for i in 1..=10 {
        list.push(Value::Int(i));
        string.push(i.to_string());
      }

      let mut fm = FormatterContext::new("test", IStringStore::default());
      fm.max_width = 4;
      fm.tab_size = 2;
      fm.set_val("test", Value::Obj(&list));

      assert_eq!(
        "
  {
    {
      1
      2
      3
      4
      5
      6
      7
      8
      9
      10
    }
  }",
        formatter.write_to_string(&mut fm, 1 << 20)?,
        "{}",
        formatter.write_to_string(&mut fm, 1 << 20)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}

#[test]
#[ignore = "not a real test"]
fn playground() -> RadlrResult<()> {
  match r###"

#fn val:int {
  #fn val:int {
    test__@val
  }

  #fn(val)
}

#type_list i:int {
  match i { { Rock[@self.#len] } 1 { List[@self.#len] } }
}

#fn(1)
\n
@test.(11)


"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      formatter._debug_print_();
      let mut list = vec![];
      let mut string = vec![];

      for i in 1..=10 {
        list.push(Value::Int(i));
        string.push(i.to_string());
      }
      let s_store = IStringStore::default();

      let mut ctx = FormatterContext::new("test", s_store);
      ctx.set_val("test", Value::Obj(&list));

      println!("{}", formatter.write_to_string(&mut ctx, 1024)?);

      Ok(())
    }
    FormatterResult::Err(err) => RadlrResult::Err(err),
  }
}
