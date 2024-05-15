use crate::parser_core::parse_grammar_input;

#[test]
fn simple_nonterminal() {
  const INPUT: &'static str = r##"

<> test > "hello" "world"

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");

  std::hint::black_box(result);
}

#[test]
fn simple_nonterminal_regex() {
  const INPUT: &'static str = r##"

<> test > /aaan *[^ \sd]+/

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");

  std::hint::black_box(result);
}

#[test]
fn class_symols() {
  const INPUT: &'static str = r##"

<> test > \n \sp \is \sym \any \tab \vtab \test \wildtest
  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");

  std::hint::black_box(result);
}

#[test]
fn optional_repeat() {
  const INPUT: &'static str = r##"

<> test > "test"( 8, 9 "|") "test"(*"|") "test"(*) "test"*

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  std::hint::black_box(result);
}

#[test]
fn at_least_once_repeat() {
  const INPUT: &'static str = r##"

<> test > "test"(1 ",") "test"(+",") "test"(+) "test"+

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  std::hint::black_box(result);
}

#[test]
fn ignore_scope() {
  const INPUT: &'static str = r##"

  EXPORT A as A IGNORE { \s } IGNORE { \s } { <> A > \n |\s }

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");

  std::hint::black_box(result);
}

#[test]
fn ignore_scope_panicked() {
  const INPUT: &'static str = r##"

  IGNORE { \s } { <> test > "test"(1 ",") "test"(+",") "test"(+) "test"+ }

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  std::hint::black_box(result);
}

#[test]
fn equivalent_token_expressions() {
  const INPUT: &'static str = r##"

  <> A > "\n "  tk:( \n \s )  /\n\s/

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");

  std::hint::black_box(result);
}

mod ir {
  use crate::parser_core::parse_ir_input;

  #[test]
  fn simple_ir_match_expression() {
    const INPUT: &'static str = r##"

  test => match : PRODUCTION {
    ( tiger ) { pass }
  }"##;
    let result = parse_ir_input::<String>(INPUT).expect("Did not parse input");

    dbg!(&result);

    std::hint::black_box(result);
  }
}
