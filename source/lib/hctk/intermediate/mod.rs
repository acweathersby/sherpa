pub mod optimize;
pub mod state;
pub mod transition_graph;

#[cfg(test)]

mod transition_tree_tests
{

  use crate::debug::compile_test_grammar;
  use crate::grammar::get_production_id_by_name;
  use crate::grammar::get_production_plain_name;
  use crate::grammar::get_production_start_items;
  use crate::intermediate::transition_graph::construct_recursive_descent;

  #[test]
  pub fn construct_descent_on_basic_grammar()
  {
    let grammar = compile_test_grammar(
      "<> A > \\h \\e
         \\l \\l \\o",
    );

    let production_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = construct_recursive_descent(
      &grammar,
      false,
      &get_production_start_items(&production_id, &grammar),
    );

    assert_eq!(result.get_node_len(), 7);

    assert_eq!(result.leaf_nodes.len(), 1);
  }

  #[test]
  pub fn construct_descent_on_scanner_symbol()
  {
    let grammar = compile_test_grammar(
      "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
    );

    for p in grammar.production_table.values() {
      eprintln!("{}", p.original_name);
    }

    let production = grammar
      .production_table
      .iter()
      .find(|p| p.1.original_name == "scan_tok_test_9AD7F26F987E3173_GUID_B__")
      .unwrap();

    let production_id = production.0;

    let result = construct_recursive_descent(
      &grammar,
      true,
      &get_production_start_items(production_id, &grammar),
    );

    assert_eq!(result.get_node_len(), 9);

    assert_eq!(result.leaf_nodes.len(), 2);
  }
}

#[cfg(test)]
mod state_constructor_tests
{

  use std::collections::BTreeSet;

  use crate::debug::compile_test_grammar;
  use crate::grammar::get_production_id_by_name;
  use crate::grammar::get_production_start_items;
  use crate::intermediate::state::generate_production_states;
  use crate::intermediate::state::generate_scanner_intro_state;
  use crate::types::GrammarId;
  use crate::types::SymbolID;

  #[test]
  pub fn generate_production_states_with_basic_grammar()
  {
    let grammar = compile_test_grammar("<> A > \\h \\e \\l \\l \\o");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 7);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar_with_one_optional_token()
  {
    let grammar = compile_test_grammar("<> A > \\h ? \\e ? \\l \\l \\o");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 21);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar_with_left_recursion()
  {
    let grammar = compile_test_grammar("<> A > A \\1 | \\2 ");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 5);
  }

  #[test]
  pub fn generate_production_states_with_synthesized_scanner_state()
  {
    let grammar = compile_test_grammar("<> A > \\1 | \\2 | \\3 ");

    let symbols = grammar
      .symbols_table
      .iter()
      .filter_map(|(id, sym)| if sym.scanner_only { None } else { Some(id) })
      .cloned()
      .collect::<BTreeSet<_>>();

    println!(
      "{:#?}",
      symbols.iter().map(|s| grammar.symbols_string_table.get(s)).collect::<Vec<_>>()
    );

    let result = generate_scanner_intro_state(symbols, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 5);
  }

  #[test]
  pub fn generate_production_state_with_scanner_function()
  {
    let grammar = compile_test_grammar(
      "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
    );

    let token_production = grammar
      .symbols_table
      .keys()
      .find(|p| matches!(p, SymbolID::TokenProduction(..)))
      .unwrap();

    let result = generate_scanner_intro_state(
      BTreeSet::from_iter(vec![*token_production]),
      &grammar,
    );

    println!("{:#?}", result);

    assert_eq!(result.len(), 6);
  }

  #[test]
  pub fn generate_production_with_ambiguity()
  {
    let grammar = compile_test_grammar(
      "
<> A > B | C

<> B > \\a \\b \\c (*)

<> C > \\a \\b \\c (*)

",
    );

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 12);
  }
  #[test]
  pub fn generate_production_with_recursion()
  {
    let grammar = compile_test_grammar(
      "
      @IGNORE g:sp

      @EXPORT statement as entry
      
      @NAME llvm_language_test
      
      <> statement > expression
      
      <> expression > sum 
      
      <> sum > mul \\+ sum
          | mul
      
      <> mul > term \\* expression
          | term
      
      <> term > g:num
          | \\( expression \\)
      
      
",
    );

    let prod_id = get_production_id_by_name("term", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 6);
  }
}
