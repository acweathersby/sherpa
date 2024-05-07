use crate::parser::parse_grammar_input;

#[test]
fn simple_nonterminal() {
  const INPUT: &'static str = r##"

<> test > "hello" "world"

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  dbg!(result);
}

#[test]
fn simple_nonterminal_regex() {
  const INPUT: &'static str = r##"

<> test > /aaan *[^ \sd]+/

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  dbg!(result);
}

#[test]
fn class_symols() {
  const INPUT: &'static str = r##"

<> test > \n \s \is \sym \any \tab \vtab \test \wildtest
  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");

  dbg!(result);
}

#[test]
fn optional_repeat() {
  const INPUT: &'static str = r##"

<> test > "test"( 8, 9 "|") "test"(*"|") "test"(*) "test"*

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  dbg!(result);
}

#[test]
fn at_least_once_repeat() {
  const INPUT: &'static str = r##"

<> test > "test"(+",") "test"(+) "test"+

  "##;
  let result = parse_grammar_input::<String>(INPUT).expect("Did not parse input");
  dbg!(result);
}
