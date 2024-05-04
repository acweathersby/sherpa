use crate::parser::parse_grammar_input;

#[test]
fn hello_world() {
  parse_grammar_input::<String>("hello world").expect("Did not parse input");
}
