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
