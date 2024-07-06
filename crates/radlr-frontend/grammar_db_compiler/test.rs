use crate::grammar_db_compiler::*;

#[test]
fn test_compile_grammar() -> Result<(), GrammarCompilerError> {
  let source_data = parse_grammar_source(
    r###"
IGNORE { \s \n }

EXPORT A as t

IMPORT ../../grammars/json/json as A
<> A > "hello" "world" 

  "###,
    Some(std::env::current_dir().unwrap()),
  )?;

  dbg!(&source_data);

  let imports = resolve_grammar_imports(&source_data)?;

  let data = extract_grammar_components(&source_data)?;

  dbg!(&imports, &data);

  Ok(())
}

#[test]
fn throws_on_invalid_cwd_when_importing() -> Result<(), GrammarCompilerError> {
  let source_data = parse_grammar_source(
    r###"
IMPORT A as A
<> A > "hello" "world" 
  "###,
    None,
  )?;

  assert!(resolve_grammar_imports(&source_data).is_err());

  Ok(())
}

#[test]
fn throws_on_invalid_cwd_when_file_not_found_importing() -> Result<(), GrammarCompilerError> {
  let source_data = parse_grammar_source(
    r###"
IMPORT A as A
<> A > "hello" "world"
  "###,
    Some(std::env::current_dir().unwrap()),
  )?;

  assert!(resolve_grammar_imports(&source_data).is_err());

  Ok(())
}

#[test]
fn generates_name_for_grammar() -> Result<(), GrammarCompilerError> {
  let source_data = parse_grammar_source(
    r###"
NAME mecha_grammar
<> A > "hello" "world"+
  "###,
    Some(std::env::current_dir().unwrap()),
  )?;

  let data = extract_grammar_components(&source_data)?;

  assert_eq!(data.ids[0].local_name, "mecha_grammar".intern());

  Ok(())
}

#[test]
fn handles_ignore_scopes() -> Result<(), GrammarCompilerError> {
  todo!("handles_ignore_scopes")
}

#[test]
fn regex_expressions() -> Result<(), GrammarCompilerError> {
  let source_data = parse_grammar_source(
    r###"
NAME mecha_grammar

<> B > "a"? "b"?
  "###,
    Some(std::env::current_dir().unwrap()),
  )?;

  let mut data = extract_grammar_components(&source_data)?;

  let parser_db = merge_grammars(&[&mut data]);

  dbg!(parser_db);

  assert_eq!(data.ids[0].local_name, "mecha_grammar".intern());

  //dbg!(data);

  Ok(())
}

pub struct T {
  pub r#pub: u32,
}

pub static t: T = T { r#pub: 2 };
