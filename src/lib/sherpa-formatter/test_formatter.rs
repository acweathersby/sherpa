use std::collections::hash_map::HashMap;

use crate::{formatter::FormatterResult, types::*};
use sherpa_core::{CachedString, IStringStore, SherpaResult};

use crate::parser;

#[test]
fn test_trivial_formatter_script() -> SherpaResult<()> {
  parser::ast::default_from("readers".into()).map(|_| Ok(()))?
}

#[test]
fn construct_formatter_interpreter() -> SherpaResult<()> {
  match "result".into() {
    FormatterResult::Ok(_formatter) => Ok(()),
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_script() -> SherpaResult<()> {
  match "test".into() {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!("test", formatter.write_to_string(&mut context, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_script_indent_spaces() -> SherpaResult<()> {
  match r#"test@+\ntest@+\ntest@-\ntest\n@-@-@-"#.into() {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new();
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
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_interpret_trivial_function() -> SherpaResult<()> {
  match r###"#t { 
   @" This is a complex string that comes up frequently. "
}
  
#t() \n #t()
  
  
"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!(
        "This is a complex string that comes up frequently.\nThis is a complex string that comes up frequently.",
        formatter.write_to_string(&mut context, 1024)?
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
   match @val { 1 { @" one " } 2 { #two() } 3 { @[1 + 2] } 1+3 { 4 } { zero } }
}

#t(1) @1 #t(2) @2 #t(3) @3 #t(4) @4 #t(1000)

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let mut context = FormatterContext::new();
      assert_eq!("one two  3   4    zero", formatter.write_to_string(&mut context, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function_for_map() -> SherpaResult<()> {
  match r###"

#type_obj @t:obj { 
    @t.name
}

@test

"###
    .into()
  {
    FormatterResult::Ok(formatter) => {
      let s_store = IStringStore::default();
      let mut test_obj = HashMap::new();
      test_obj.insert("name".to_string(), Value::Str("test_printer".intern(&s_store)));

      let mut values = HashMap::new();
      values.insert("test".to_string(), Value::Obj(&test_obj));

      let mut context = FormatterContext::new_with_values(&values, s_store);
      assert_eq!("test_printer", formatter.write_to_string(&mut context, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function_for_list() -> SherpaResult<()> {
  match r###"

#read_list @t:obj @i:int {
  match @t.len - @i {
    @t.len  { @t.[@i] #read_list( @t, @i + 1 ) }
            { @1 @t.[@i] #read_list( @t, @i + 1 ) }
    0       { } 
  }
}

#type_list @u:obj { #read_list(@u, 0) }

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

      let mut values = HashMap::new();
      values.insert("test".to_string(), Value::Obj(&list));

      let mut context = FormatterContext::new_with_values(&values, s_store);
      assert_eq!("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20", formatter.write_to_string(&mut context, 1024)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_and_use_a_type_function_for_list_with_tail_call_optimization() -> SherpaResult<()> {
  match r###"

#read_list @t:obj @i:int {
  match @t.len - @i {
    @t.len  { @t.[@i] #read_list( @t, @i + 1 ) }
            { @1 @t.[@i] #read_list( @t, @i + 1 ) }
    0       { } 
  }
}

#type_list @u:obj { #read_list(@u, 0) }

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

      let mut values = HashMap::new();
      values.insert("test".to_string(), Value::Obj(&list));

      assert_eq!(string.join(" "), formatter.write_to_string(&mut FormatterContext::new_with_values(&values, s_store), 1 << 20)?);
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
fn construct_length_limited_block() -> SherpaResult<()> {
  match r###"

#read_list t:obj i:int {
  match t.len - i {
    t.len   { @t.[i] @; #read_list( @t, @i + 1 ) }
            { @2 @t.[i] @; #read_list( @t, @i + 1 ) }
    1       { @2 @t.[i] }
  }
}

#type_list @u:obj { #read_list(@u, 0) }


\n@+
{ @+ { @+ @test @- } @- }
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
      let s_store = IStringStore::default();

      let mut values = HashMap::new();
      values.insert("test".to_string(), Value::Obj(&list));

      let mut fm = FormatterContext::new_with_values(&values, s_store);
      fm.max_width = 4;

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
        formatter.write_to_string(&mut fm, 1 << 20)?
      );
      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}

#[test]
#[ignore = "not a real test"]
fn playground() -> SherpaResult<()> {
  match r###"

#fn b:flt p:flt l:int sep:str { 
  @j={ b ^ p } 
  @{- @b^@p = @j@sep\n}
  match p {
    l {}
    { #fn(b, p + 1, l, sep) }
  }
}

@num={ 8 }

@{
Powers of 2 from 1 to @num 

> Note: this includes all numbers that are transient
```rust
#fn(2, 1, num, ";")
```

Powers of 2 from 16 to 2 ^ 8
```rust
#fn(2, 16, 2 ^ 8, ".")
```
}

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
      let s_store = IStringStore::default();

      let mut values = HashMap::new();
      values.insert("test".to_string(), Value::Obj(&list));

      println!("{}", formatter.write_to_string(&mut FormatterContext::new_with_values(&values, s_store), 1024)?);

      Ok(())
    }
    FormatterResult::Err(err) => SherpaResult::Err(err),
  }
}
