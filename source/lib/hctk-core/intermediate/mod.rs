pub mod optimize;
pub mod state;
pub mod transition;

#[cfg(test)]

mod transition_tree_tests {

  use std::collections::BTreeSet;

  use crate::debug::debug_items;
  use crate::grammar::get_production_start_items;
  use crate::intermediate::transition::get_valid_starts;

  use crate::intermediate::transition::construct_recursive_descent;
  use crate::types::GrammarStore;

  #[test]
  pub fn construct_descent_on_basic_grammar() {
    let g = GrammarStore::from_str("<> A > \\h \\e \\l \\l \\o").unwrap();

    let production_id = g.get_production_id_by_name("A").unwrap();

    let result = construct_recursive_descent(
      g.clone(),
      false,
      &get_production_start_items(&production_id, &g),
      BTreeSet::from_iter(vec![production_id]),
    )
    .0;

    assert_eq!(result.get_node_len(), 7);

    assert_eq!(result.leaf_nodes.len(), 1);
  }

  #[test]
  pub fn construct_descent_on_scanner_symbol() {
    let g = GrammarStore::from_str(
      "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
    )
    .unwrap();

    for p in g.productions.values() {
      eprintln!("{}", p.name);
    }

    let production =
      g.productions.iter().find(|p| p.1.name == "scan_tok_test_9AD7F26F987E3173_GUID_B__").unwrap();

    let production_id = production.0;

    let result = construct_recursive_descent(
      g.clone(),
      true,
      &get_production_start_items(production_id, &g),
      BTreeSet::from_iter(vec![*production_id]),
    )
    .0;

    assert_eq!(result.get_node_len(), 8);

    assert_eq!(result.leaf_nodes.len(), 2);
  }
}

#[cfg(test)]
mod state_constructor_tests {

  use std::any::Any;
  use std::collections::BTreeSet;
  use std::iter::FromIterator;
  use std::path::PathBuf;

  use crate::debug::debug_items;
  use crate::grammar::get_production_start_items;
  use crate::intermediate::state::generate_production_states;
  use crate::intermediate::state::generate_scanner_intro_state;
  use crate::intermediate::state::IROutput;
  use crate::intermediate::transition::get_valid_starts;
  use crate::types::GrammarId;
  use crate::types::GrammarStore;
  use crate::types::SymbolID;

  use super::state::compile_states;

  #[test]
  pub fn production_reduction_decisions() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 25);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar() {
    let g = GrammarStore::from_str("<> A > \\h \\e \\l \\l \\o").unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 8);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar_with_one_optional_token() {
    let g = GrammarStore::from_str("<> A > \\h ? \\e ? \\l \\l \\o").unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 26);
  }

  #[test]
  pub fn generate_production_states_with_basic_grammar_with_left_recursion() {
    let g = GrammarStore::from_str("<> A > A \\1 | \\2 ").unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 8);
  }

  #[test]
  pub fn generate_production_states_with_synthesized_scanner_state() {
    let g = GrammarStore::from_str("<> A > \\1 | \\2 | \\3 ").unwrap();

    let symbols = g
      .symbols
      .iter()
      .filter_map(|(id, sym)| if sym.scanner_only { None } else { Some(id) })
      .cloned()
      .collect::<BTreeSet<_>>();

    println!("{:#?}", symbols.iter().map(|s| g.symbol_strings.get(s)).collect::<Vec<_>>());

    let result = generate_scanner_intro_state(symbols, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 11);
  }

  #[test]
  pub fn generate_production_state_with_scanner_function() {
    let grammar = GrammarStore::from_str(
      "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
    )
    .unwrap();

    let token_production =
      grammar.symbols.keys().find(|p| matches!(p, SymbolID::TokenProduction(..))).unwrap();

    let result =
      generate_scanner_intro_state(BTreeSet::from_iter(vec![*token_production]), grammar).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 7);
  }

  #[test]
  pub fn generate_conflicts() {
    let grammar = GrammarStore::from_path(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../test/e2e/bootstrap/grammar/comment.hcg")
        .canonicalize()
        .unwrap(),
    )
    .unwrap();

    let IROutput { errors, states } =
      generate_production_states(&grammar.get_production_id_by_name("comment").unwrap(), grammar);

    for error in &errors {
      println!("{}", error);
    }

    println!("{:#?}", states);
    // assert_eq!(errors.len(), 1);
  }

  #[test]
  pub fn generate_A_state_of_a_merged_grammar_with_extended_production() {
    let grammar = GrammarStore::from_path(
      PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../test/grammars/merge_conflict_host.hcg")
        .canonicalize()
        .unwrap(),
    )
    .unwrap();

    let IROutput { errors, states } = generate_production_states(
      &grammar.get_production_id_by_name("( mcc::B | C )(+)").unwrap(),
      grammar,
    );

    for error in &errors {
      println!("{}", error);
    }

    // assert_eq!(errors.len(), 1);

    assert!(matches!(errors[0], crate::types::HCError::transition_err_ambiguous_production { .. }));
  }

  #[test]
  pub fn handle_moderate_scanner_token_combinations() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();

    let p = g.get_production_id_by_name("A").unwrap();

    let syms =
      get_production_start_items(&p, &g).iter().map(|i| i.get_symbol(&g)).collect::<BTreeSet<_>>();

    let result = generate_scanner_intro_state(syms, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 7);
  }

  #[test]
  pub fn generate_production_with_ambiguity() {
    let g = GrammarStore::from_str(
      "
<> A > B | C

<> B > \\a \\b \\c (*)

<> C > \\a \\b \\c (*)

",
    )
    .unwrap();

    let prod_id = g.get_production_id_by_name("A").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 12);
  }

  #[test]
  pub fn generate_annotated_symbol() {
    let g = GrammarStore::from_str(
      "
      @NAME hc_symbol
      @NAME ascript

      @IGNORE g:sp g:nl
      
      <> body >  
      
          struct 
          
          | expression(+\\, )
              f:ast { { t_AST_Statements, statements:$1, tok } }
      
      <> expression > 
          
          member
      
          | string_convert
      
          | numeric_convert
      
          | bool_convert
      
          | literal
      
          | vector
      
          | token
      
      
      <> struct > 
      
          \\{ struct_prop(+\\, ) \\}
              f:ast { { t_AST_Struct, props:$2, tok } }
      
      
      <> struct_prop >  
          
          identifier \\: expression
              f:ast { { t_AST_Property, id:str($1), value:$3, tok } }
          
          |  identifier \\: struct
              f:ast { { t_AST_Property, id:str($1), value:$3, tok } }
          
          |  identifier
              f:ast { { t_AST_Property, id:str($1), named_reference: str($1), tok } }
          
          | type_identifier
              f:ast { { t_AST_TypeId,  value:str($1), tok } }
          
          | class_identifier
              f:ast { { t_AST_ClassId, value:str($1), tok } }
      
          | token
      
      
      <> type_identifier > 
      
          t:t_ identifier
      
      
      <> class_identifier >
      
          t:c_ identifier
      
      
      <> vector >
      
          \\[ expression(*\\, ) \\]
              f:ast { { t_AST_Vector, initializer: $2, tok  } }
      
      
      <> add > 
          
          member \\+ expression
          
              f:ast { { t_AST_Add, left: $1, right: $3, tok } }
      
      
      <> member > 
          
          reference
      
          | reference \\. identifier
              f:ast { { t_AST_Member, reference:$1, property:$3 } }
      
      
      <> string_convert > 
          
          t:str convert_initializer?
              f:ast { { t_AST_STRING, value: $2, tok  } }
      
      
      <> bool_convert > 
          
          t:bool convert_initializer?
              f:ast { { t_AST_BOOL,  initializer: $2, tok  } }
      
      
      <> numeric_convert > 
          
          t:u8  convert_initializer?
              f:ast { { t_AST_U8,  initializer: $2, tok  } }
      
          | t:u16 convert_initializer?
              f:ast { { t_AST_U16, initializer: $2, tok  } }
      
          | t:u32 convert_initializer?
              f:ast { { t_AST_U32, initializer: $2, tok  } }
      
          | t:u64 convert_initializer?
              f:ast { { t_AST_U64, initializer: $2, tok  } }
      
          | t:i8  convert_initializer?
              f:ast { { t_AST_I8,  initializer: $2, tok  } }
      
          | t:i16 convert_initializer?
              f:ast { { t_AST_I16, initializer: $2, tok  } }
      
          | t:i32 convert_initializer?
              f:ast { { t_AST_I32, initializer: $2, tok  } }
      
          | t:i64 convert_initializer?
              f:ast { { t_AST_I64, initializer: $2, tok  } }
      
          | t:f32 convert_initializer?
              f:ast { { t_AST_F32, initializer: $2, tok  } }
      
          | t:f64 convert_initializer?
              f:ast { { t_AST_F64, initializer: $2, tok  } }
      
      
      <> convert_initializer > 
      
          t:( init_objects t:)       
              f:ast { { t_Init, expression: $2 } }
      
      <> init_objects > member | token 
      
      
      <> literal > 
          
          t:true 
              f:ast { { t_AST_BOOL, value: true } }
      
          | t:false
              f:ast { { t_AST_BOOL, value: false } }
      
          | tk:integer
              f:ast { { t_AST_NUMBER, value:f64($1) } }
      
      
      
      <> reference > 
          
          t:$ tk:identifier 
              f:ast { { t_AST_NamedReference, value: str($2), tok } }
      
          | t:$ tk:integer         
              f:ast { { t_AST_IndexReference, value: i64($2), tok } }
      
      
      <> integer > 
          
          g:num(+)
      
      
      <> identifier > 
      
          tk:identifier_syms 
      
      
      <> identifier_syms >  
      
          identifier_syms g:id
      
          | identifier_syms \\_
      
          | identifier_syms \\-
      
          | identifier_syms g:num      
      
          | \\_ 
      
          | \\- 
      
          | g:id
      
      
      <> token > 
      
          t:tok 
              f:ast { { t_AST_Token } }
      
          | t:token 
              f:ast { { t_AST_Token } }
      
      
",
    )
    .unwrap();

    let prod_id = g.get_production_id_by_name("struct_prop").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 10);
  }

  #[test]
  pub fn generate_production_with_recursion() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();

    let prod_id = g.get_production_id_by_name("term").unwrap();

    let result = generate_production_states(&prod_id, g).states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 10);
  }

  #[test]
  pub fn generate_scanner_production_with_recursion() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();

    let result = generate_scanner_intro_state(
      BTreeSet::from_iter(vec![SymbolID::from_string("V", Some(&g))]),
      g,
    )
    .states;

    println!("{:#?}", result);

    assert_eq!(result.len(), 11);
  }

  #[test]
  pub fn generate_production_with_recursiond() {
    let g = GrammarStore::from_str(
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
    )
    .unwrap();

    // compile_states(&grammar, 1);

    let result = generate_scanner_intro_state(
      BTreeSet::from_iter(vec![
        SymbolID::from_string("g:nl", Some(&g)),
        SymbolID::from_string("code_block_delimiter_with_nl", Some(&g)),
      ]),
      g,
    );
    // if let Some(prod) = get_production_id_by_name("line", &grammar) {
    //   let result = generate_production_states(&prod, &grammar);
    //   println!("{:#?}", result);
    // }
  }
}
