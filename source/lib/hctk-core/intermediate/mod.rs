pub mod optimize;
pub mod state;
pub mod transition;

#[cfg(test)]

mod transition_tree_tests {

  use std::collections::BTreeSet;

  use crate::debug::compile_test_grammar;
  use crate::debug::debug_items;
  use crate::grammar::get_production_id_by_name;
  use crate::grammar::get_production_plain_name;
  use crate::grammar::get_production_start_items;
  use crate::intermediate::transition::deconflict_starts;

  use crate::intermediate::transition::construct_recursive_descent;

  #[test]
  pub fn construct_descent_on_basic_grammar() {
    let grammar = compile_test_grammar("<> A > \\h \\e \\l \\l \\o");

    let production_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = construct_recursive_descent(
      &grammar,
      false,
      &get_production_start_items(&production_id, &grammar),
      BTreeSet::from_iter(vec![production_id]),
    );

    assert_eq!(result.get_node_len(), 7);

    assert_eq!(result.leaf_nodes.len(), 1);
  }

  #[test]
  pub fn construct_descent_on_scanner_symbol() {
    let g = compile_test_grammar(
      "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
    );

    for p in g.productions.values() {
      eprintln!("{}", p.original_name);
    }

    let production = g
      .productions
      .iter()
      .find(|p| p.1.original_name == "scan_tok_test_9AD7F26F987E3173_GUID_B__")
      .unwrap();

    let production_id = production.0;

    let result = construct_recursive_descent(
      &g,
      true,
      &get_production_start_items(production_id, &g),
      BTreeSet::from_iter(vec![*production_id]),
    );

    assert_eq!(result.get_node_len(), 8);

    assert_eq!(result.leaf_nodes.len(), 2);
  }
}

#[cfg(test)]
mod state_constructor_tests {

  use std::any::Any;
  use std::collections::BTreeSet;
  use std::iter::FromIterator;

  use crate::debug::compile_test_grammar;
  use crate::debug::debug_items;
  use crate::grammar::get_production_by_name;
  use crate::grammar::get_production_id_by_name;
  use crate::grammar::get_production_start_items;
  use crate::intermediate::state::generate_production_states;
  use crate::intermediate::state::generate_scanner_intro_state;
  use crate::intermediate::transition::deconflict_starts;
  use crate::types::GrammarId;
  use crate::types::SymbolID;

  use super::state::compile_states;

  #[test]
  pub fn production_reduction_decisions() {
    let g = compile_test_grammar(
      "
<> A > B | C | R 
     | \\g

<> C > \\c c

<> c >  \\a | \\b 

<> B > C \\d
     | \\a \\c

<> R > G \\o 
    | C \\x

<> G > \\xx

  ",
    );

    let prod_id = get_production_id_by_name("A", &g).unwrap();

    let result = generate_production_states(&prod_id, &g);

    println!("{:#?}", result);

    assert_eq!(result.len(), 25);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar() {
    let grammar = compile_test_grammar("<> A > \\h \\e \\l \\l \\o");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 8);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar_with_one_optional_token() {
    let grammar = compile_test_grammar("<> A > \\h ? \\e ? \\l \\l \\o");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 26);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar_with_left_recursion() {
    let grammar = compile_test_grammar("<> A > A \\1 | \\2 ");

    let prod_id = get_production_id_by_name("A", &grammar).unwrap();

    let result = generate_production_states(&prod_id, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 8);
  }

  #[test]
  pub fn generate_production_states_with_synthesized_scanner_state() {
    let grammar = compile_test_grammar("<> A > \\1 | \\2 | \\3 ");

    let symbols = grammar
      .symbols
      .iter()
      .filter_map(|(id, sym)| if sym.scanner_only { None } else { Some(id) })
      .cloned()
      .collect::<BTreeSet<_>>();

    println!("{:#?}", symbols.iter().map(|s| grammar.symbol_strings.get(s)).collect::<Vec<_>>());

    let result = generate_scanner_intro_state(symbols, &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 11);
  }

  #[test]
  pub fn generate_production_state_with_scanner_function() {
    let grammar = compile_test_grammar(
      "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
    );

    let token_production =
      grammar.symbols.keys().find(|p| matches!(p, SymbolID::TokenProduction(..))).unwrap();

    let result =
      generate_scanner_intro_state(BTreeSet::from_iter(vec![*token_production]), &grammar);

    println!("{:#?}", result);

    assert_eq!(result.len(), 7);
  }

  #[test]
  pub fn handle_moderate_scanner_token_combinations() {
    let g = compile_test_grammar(
      "
<> A > \\CC  | tk:id_syms

<> id_syms >  

    id_syms g:id

    |   id_syms \\_

    |   id_syms \\-

    |   id_syms g:num      

    |   \\_ 

    |   \\- 

    |   g:id
",
    );

    let p = get_production_id_by_name("A", &g).unwrap();

    let syms =
      get_production_start_items(&p, &g).iter().map(|i| i.get_symbol(&g)).collect::<BTreeSet<_>>();

    let result = generate_scanner_intro_state(syms, &g);

    println!("{:#?}", result);

    assert_eq!(result.len(), 7);
  }

  #[test]
  pub fn generate_production_with_ambiguity() {
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
  pub fn generate_production_with_recursion() {
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

    assert_eq!(result.len(), 10);
  }

  #[test]
  pub fn generate_scanner_production_with_recursion() {
    let grammar = compile_test_grammar(
      "
      @IGNORE g:sp

      @EXPORT statement as entry
      
      @NAME llvm_language_test
      
      <> statement > tk:test
      
      <> test > V test?
          | A test \\t

      <> V > V g:num | \\dd

      <> A > \\a \\- \\b
      
",
    );

    let result = generate_scanner_intro_state(
      BTreeSet::from_iter(vec![SymbolID::from_string("V", Some(&grammar))]),
      &grammar,
    );

    println!("{:#?}", result);

    assert_eq!(result.len(), 11);
  }

  #[test]
  pub fn generate_production_with_recursiond() {
    let grammar = compile_test_grammar(
      "
        @EXPORT markdown as md

        <> markdown > lines
        
            f:ast { {t_Markdown, lines:$1 } }
        
        <> lines > g:nl? line
        
            f:ast { [$2] }
            
            | lines g:nl line
            
            f:ast { [$1, $3] }
        
            | lines ( g:nl f:ast{ { t_EmptyLine, c_Line } } )
        
            f:ast {  [$1, $2] }
        
        <> line >
        
            header_token content
        
            f:ast { { t_Header, c_Line, length:f64($1), content:$2 } }
        
            | 
            
            tk:spaces? tk:ol_token content
        
            f:ast { { t_OL, c_Line, spaces:str($1), content:$3 } }
        
            |
        
            tk:spaces? tk:ul_token content
        
            f:ast { { t_UL, c_Line, spaces:str($1), content:$3 } }
        
            |
        
            tk:spaces? tk:quote_token content
        
            f:ast { { t_Quote, c_Line, spaces:str($1), content:$3 } }
        
            | 
        
            tk:spaces? content
        
            f:ast { { t_Paragraph, c_Line, spaces:str($1), content:$2 } }
        
            |
             
            tk:code_block_delimiter code_line_text? code_line(*) cb_sentinel
        
            f:ast { { t_CodeBlock, c_Line, syntax:str($2), data:$3 } }
        
        <> ol_token > g:num \\. 
        
        <> spaces > g:sp(+\\\" )
        
        <> header_token > \\% (+)
        
        <> ul_token > \\- 
            | \\+ 
        
        <> quote_token > \\>           
        
        <> code_line >
        
            g:nl code_line_text?
        
            f:ast { { t_Text, c_Content, value: str($2) } }
        
        <> code_block_delimiter > \\```
        
        <> code_block_delimiter_with_nl > g:nl \\```
        
        <> cb_sentinel > tk:code_block_delimiter_with_nl
        
        <[ recover cb_sentinel_1 ] 
        
            shift nothing then set prod to cb_sentinel
        >
        
        <> code_line_text > 
            (   g:num 
            |   g:sp
            |   g:id 
            |   g:sym
            )(+\\\" )
        
        <> code_block_sentinel >
        
            g:nl \\``` 
        
        <> content > ( text | format_symbol )(+)
        
        <> text > text_symbol(+\\\" )
            f:ast { { t_Text, c_Content, value: str($1) } }
        
        <> text_symbol > 
                g:sym
            |   g:sp
            |   tk:word
            |   tk:num
        
        <> word > g:id 
            | word g:id
        
        <> num > g:num
            | num g:num
        
        <> format_symbol > 
            \\` 
            f:ast { { t_InlineCode, c_Content } }
            | \\* 
            f:ast { { t_MarkerA, c_Content } }
            | \\_ 
            f:ast { { t_MarkerB, c_Content } }
            | \\{
            f:ast { { t_QueryStart, c_Content } }
            | \\}
            f:ast { { t_QueryEnd, c_Content } }
            | \\[ 
            f:ast { { t_AnchorStart, c_Content } }
            | \\![
            f:ast { { t_AnchorImageStart, c_Content } }
            | \\]
            f:ast { { t_AnchorEnd, c_Content } }
            | \\](
            f:ast { { t_AnchorMiddle, c_Content } }
            | \\)
            f:ast { { t_AnchorEnd, c_Content, c_Meta } }
            | \\(
            f:ast { { t_MetaStart, c_Content, c_Meta } }              
        ",
    );

    // compile_states(&grammar, 1);

    let result = generate_scanner_intro_state(
      BTreeSet::from_iter(vec![
        SymbolID::from_string("g:nl", Some(&grammar)),
        SymbolID::from_string("code_block_delimiter_with_nl", Some(&grammar)),
      ]),
      &grammar,
    );
    // if let Some(prod) = get_production_id_by_name("line", &grammar) {
    //   let result = generate_production_states(&prod, &grammar);
    //   println!("{:#?}", result);
    // }
  }
}
