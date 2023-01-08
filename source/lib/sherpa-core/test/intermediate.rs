#[cfg(test)]
use crate::intermediate::{construct_recursive_descent, utils::generate_recursive_descent_items};
use crate::{
  bytecode::compile_bytecode,
  compile::{
    compile_production_states,
    compile_scanner_states,
    compile_states,
    compile_token_production_states,
    optimize_ir_states,
  },
  debug::{collect_shifts_and_skips, generate_disassembly},
  errors::SherpaErrorSeverity,
  grammar::get_production_start_items,
  journal::{config::Config, report::ReportType, Journal},
  types::*,
  util::get_num_of_available_threads,
};
use std::{
  collections::{BTreeMap, BTreeSet},
  iter::FromIterator,
  path::PathBuf,
};

#[test]
pub fn construct_descent_on_basic_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > \\h \\e \\l \\l \\o").unwrap();

  let production_id = g.get_production_id_by_name("A").unwrap();

  let items = generate_recursive_descent_items(&mut j, production_id);

  let (result, _) = construct_recursive_descent(&mut j, ScanType::None, &items)?;

  assert_eq!(result._get_node_len(), 7);

  assert_eq!(result.leaf_nodes.len(), 1);
  SherpaResult::Ok(())
}

#[test]
pub fn construct_descent_on_scanner_symbol() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config { allow_occluding_symbols: false, ..Default::default() }));
  let g = GrammarStore::from_str(
    &mut j,
    "
<> A > tk:B

<> B > C | D

<> C > \\a D \\c

<> D > \\a \\b
",
  )?;

  let grammar = j.grammar()?;

  for p in g.productions.values() {
    eprintln!("{}", p.name);
  }

  let production = g.productions.iter().find(|p| p.1.name == "tk:B").unwrap();

  let prod_id = production.0;

  let items = generate_recursive_descent_items(&mut j, *prod_id)
    .into_iter()
    .map(|i| {
      i.to_origin(crate::types::OriginData::Symbol(grammar.get_production(prod_id).unwrap().sym_id))
    })
    .collect();

  let (result, _) = construct_recursive_descent(&mut j, ScanType::ScannerEntry, &items)?;

  result._print_nodes();
  j.flush_reports();
  j.debug_print_reports(ReportType::Any);

  assert_eq!(result._get_node_len(), 8);

  assert_eq!(result.leaf_nodes.len(), 2);
  SherpaResult::Ok(())
}

#[test]
pub fn production_reduction_decisions() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
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

  let result = compile_production_states(&mut j, prod_id)?;

  j.flush_reports();
  j.debug_print_reports(ReportType::ProductionCompile(prod_id));

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 11);
  SherpaResult::Ok(())
}

#[test]
pub fn compile_production_states_with_basic_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > \\h \\e \\l \\l \\o").unwrap();

  let prod_id = g.get_production_id_by_name("A").unwrap();

  let result = compile_production_states(&mut j, prod_id)?;

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 7);
  SherpaResult::Ok(())
}

#[test]
pub fn compile_production_states_with_basic_grammar_with_one_optional_token() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > \\h ? \\e ? \\l \\l \\o").unwrap();

  let prod_id = g.get_production_id_by_name("A").unwrap();

  let result = compile_production_states(&mut j, prod_id)?;

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 17);
  SherpaResult::Ok(())
}

#[test]
pub fn compile_production_states_with_basic_grammar_with_left_recursion() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > A \\1 | \\2 ").unwrap();

  let prod_id = g.get_production_id_by_name("A").unwrap();

  match compile_production_states(&mut j, prod_id) {
    SherpaResult::Ok(result) => {
      eprintln!("{:#?}", result);

      j.flush_reports();
      j.debug_print_reports(ReportType::Any);

      assert_eq!(result.len(), 5);
    }
    _ => {
      j.flush_reports();
      j.debug_print_reports(ReportType::Any);
      return SherpaResult::None;
    }
  }

  SherpaResult::Ok(())
}

#[test]
pub fn compile_production_states_with_synthesized_scanner_state() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > \\1 | \\2 | \\3 ").unwrap();

  let symbols = g
    .symbols
    .iter()
    .filter_map(|(id, sym)| if sym.scanner_only { None } else { Some(id) })
    .cloned()
    .collect::<BTreeSet<_>>();

  eprintln!("{:#?}", symbols.iter().map(|s| g.symbol_strings.get(s)).collect::<Vec<_>>());

  let result = compile_scanner_states(&mut j, symbols)?;

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 4);
  SherpaResult::Ok(())
}

#[test]
pub fn generate_production_state_with_scanner_function() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar = GrammarStore::from_str(
    &mut j,
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

  let result = compile_scanner_states(&mut j, BTreeSet::from_iter(vec![*token_production]))?;

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 5);
  SherpaResult::Ok(())
}

#[test]
pub fn generate_conflicts() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar = GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/e2e/bootstrap/grammar/comment.hcg")
      .canonicalize()
      .unwrap(),
  )
  .unwrap();

  let states =
    compile_production_states(&mut j, grammar.get_production_id_by_name("line_comment").unwrap())?;

  eprintln!("{:#?}", states);
  // assert_eq!(errors.len(), 1);
  SherpaResult::Ok(())
}

#[test]
pub fn generate_A_state_of_a_merged_grammar_with_extended_production() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar = GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/merge_conflict_host.hcg")
      .canonicalize()
      .unwrap(),
  )
  .unwrap();

  compile_production_states(&mut j, grammar.get_production_id_by_name("A_list_1").unwrap());

  // assert_eq!(errors.len(), 1);

  j.flush_reports();
  j.debug_print_reports(ReportType::Any);

  // eprintln!("{}", report.debug_string());

  // assert!(report.errors()[0].is(WarnTransitionAmbiguousProduction::friendly_name));

  SherpaResult::Ok(())
}

#[test]
pub fn handle_moderate_scanner_token_combinations() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
<> A > \\C | tk:id_syms

<> id_syms >  

    id_syms g:id

    |   id_syms \\_

    |   \\_ 

    |   g:id
",
  )
  .unwrap();

  let p = g.get_production_id_by_name("A").unwrap();

  let syms =
    get_production_start_items(&p, &g).iter().map(|i| i.get_symbol(&g)).collect::<BTreeSet<_>>();

  let result = compile_scanner_states(&mut j, syms)?;

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 7);
  SherpaResult::Ok(())
}

#[test]
pub fn generate_production_with_ambiguity() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
<> A > B | C

<> B > \\a \\b \\c (*)

<> C > \\a \\b \\c (*)
",
  )
  .unwrap();

  let prod_id = g.get_production_id_by_name("A").unwrap();

  match compile_production_states(&mut j, prod_id) {
    SherpaResult::Ok(_) => {
      panic!("Expected error to be produced")
    }
    _ => {
      j.flush_reports();
      j.debug_print_reports(ReportType::Any);
    }
  }

  SherpaResult::Ok(())
}

#[test]
pub fn generate_annotated_symbol() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
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

          | struct_prop(+\\, ) \\}
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

  let result = compile_production_states(&mut j, prod_id)?;

  j.flush_reports();
  j.debug_print_reports(ReportType::ProductionCompile(prod_id));

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 10);
  SherpaResult::Ok(())
}

#[test]
pub fn generate_production_with_recursion() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
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

  let result = compile_production_states(&mut j, prod_id)?;

  j.flush_reports();
  j.debug_print_reports(ReportType::ProductionCompile(prod_id));

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 6);
  SherpaResult::Ok(())
}

#[test]
pub fn generate_scanner_production_with_recursion() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
      @IGNORE g:sp

      @EXPORT statement as entry

      @NAME llvm_language_test

      <> statement > tk:test tk:V

      <> test > V test?
          | A test \\t

      <> V > V g:num | \\dd

      <> A > \\a \\- \\b

",
  )
  .unwrap();

  let symbols = SymbolSet::from_iter(vec![SymbolID::from_string("V", Some(&g))]);

  let result = compile_scanner_states(&mut j, symbols.clone())?;

  eprintln!("{:#?}", result);

  j.flush_reports();
  j.debug_print_reports(ReportType::ScannerCompile(ScannerStateId::new(&symbols)));

  assert_eq!(result.len(), 3);

  SherpaResult::Ok(())
}

#[test]
pub fn generate_production_with_recursiond() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
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

  let result = compile_scanner_states(
    &mut j,
    BTreeSet::from_iter(vec![
      SymbolID::from_string("g:nl", Some(&g)),
      SymbolID::from_string("code_block_delimiter_with_nl", Some(&g)),
    ]),
  )?;

  assert_eq!(result.len(), 7);
  // if let Some(prod) = get_production_id_by_name("line", &grammar) {
  //   let result = compile_production_states(&prod, &grammar);
  //   eprintln!("{:#?}", result);
  // }
  SherpaResult::Ok(())
}

#[test]
fn test_construct_LR() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    " @IGNORE g:sp 

        <> A > X\\c
             | Y \\d

        <> X > \\x X?

        <> Y > \\x Y?
      ",
  )?;

  let prod_id = g.get_production_id_by_name("A")?;

  let states = compile_production_states(&mut j, prod_id)?;

  let report = j.report();
  if report.have_errors_of_type(SherpaErrorSeverity::Critical) {
    for error in report.errors() {
      eprintln!("{}", error);
    }
  }

  for state in states {
    eprintln!("{}", state.to_string())
  }
  SherpaResult::Ok(())
}

#[test]
fn test_peek() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config {
    build_disassembly: true,
    allow_occluding_symbols: true,
    ..Default::default()
  }));
  let g = GrammarStore::from_str(
    &mut j,
    r##"
    @IGNORE g:sp

    <> term >  tk:ident \= value_list

    <> value_list > \" formal_value_list(+g:sp) \"

    <> formal_value_list > ident

    <> ident > g:id(+) 

    "##,
  )
  .unwrap();

  let states = compile_states(&mut j, 10)?;
  let pre_opt_length = states.len();

  let mut states = optimize_ir_states(&mut j, states);
  let post_opt_length = states.len();

  compile_bytecode(&mut j, states);

  j.flush_reports();

  // j.debug_report(ReportType::ProductionCompile(g.get_production_id_by_name("A").unwrap()));
  let report_type = ReportType::ProductionCompile(ProductionId::default());
  j.get_reports(report_type, |report| {
    let ReportType::ProductionCompile(prod_id) = report.report_type else {return};

    if let Some(note) = report.get_note("RD Graph Nodes") {
      eprintln!(
        "Production [ {} ] Recursive Descent Graph =>\n{}",
        g.get_production_plain_name(&prod_id),
        note
      );
    }

    if let Some(note) = report.get_note("RA Graph Nodes") {
      eprintln!(
        "Production [ {} ] Recursive Ascent Graph =>\n{}",
        g.get_production_plain_name(&prod_id),
        note
      );
    }
  });
  //  j.get_reports(ReportType::Disassembly, |report| {
  // if let Some(note) = report.get_note("Output") {
  // eprintln!("{}", note);
  // }
  // });
  SherpaResult::Ok(())
}

#[test]
fn test_peek3() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config {
    build_disassembly: true,
    allow_occluding_symbols: true,
    debug_add_ir_states_note: true,
    enable_breadcrumb_parsing: true,
    ..Default::default()
  }));
  eprintln!("Item State Size {}", std::mem::size_of::<ItemState>());
  eprintln!("Item Size {}", std::mem::size_of::<Item>());
  let g = GrammarStore::from_str(
    &mut j,
    r##"
    @IGNORE g:sp

    <> term >  \x A \( g:id? \)  f:ast { { t_Function_Definition } }
            |  \x B \;           f:ast { { t_Type_Definition } }

    <> A > Adent \x

    <> B > Bdent

    <> Adent > Cdent

    <> Cdent > g:id

    <> Bdent > g:id

    "##,
  )
  .unwrap();

  // compile_production_states_LR(&mut j, g.get_production_id_by_name("term")?);

  compile_production_states(&mut j, g.get_production_id_by_name("term")?);

  j.flush_reports();

  j.debug_print_reports(ReportType::AnyProductionCompile);

  SherpaResult::Ok(())
}

//#[test]
// fn string_terminators_should_not_be_overridden_by_sym_class() -> SherpaResult<()> {
// let input = " <> str >  \" g:sym(*) \" ";
// let production_name = "str";
//
// let (j, states) = build_production_states(input, production_name)?;
//
// let report = j.report();
// if report.have_errors_of_type(SherpaErrorSeverity::Critical) {
// for error in report.errors() {
// eprintln!("{}", error);
// }
// }
//
// for state in states {
// eprintln!("{}", state.to_string())
// }
//
// SherpaResult::Ok(())
// }
#[test]
fn grammar_with_exclusive_symbols() -> SherpaResult<()> {
  let input = r##" 
  @IGNORE g:sp

  <> A >   B t:d
       |   B t:g

  <> B >   C t:r
       |   D t:x

  <> C > t:c t:g

  <> D > t:d t:g
  "##;

  "d g g";

  let (mut j, states) = build_states(input)?;

  let states = optimize_ir_states(&mut j, states);

  for (_, state) in &states {
    eprintln!("{}", state.get_code());
  }

  let bc = compile_bytecode(&mut j, states);

  eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  j.flush_reports();
  j.debug_print_reports(ReportType::Optimize);

  SherpaResult::Ok(())
}

#[test]
fn scientific_numeric_token() -> SherpaResult<()> {
  let input = r##"

  <> num > tk:number

  <> number > ( \+ | \- )? g:num(+) ( \. g:num(+) )? ( ( \E | \e ) ( \+ | \- )? g:num(+) )?

"##;
  let mut j = Journal::new(Some(Config { ..Default::default() }));

  let g = GrammarStore::from_str(&mut j, input).unwrap();

  let prod_id = g.get_production_id_by_name("tk:number")?;

  let states = compile_token_production_states(&mut j, prod_id)?;

  for (state) in &states {
    eprintln!("{}", state.get_code());
  }

  j.flush_reports();
  j.debug_print_reports(ReportType::TokenProductionCompile(Default::default()));

  SherpaResult::Ok(())
}

#[test]
fn string_token() -> SherpaResult<()> {
  let input = r##"

  <> str > tk:string

  <> string > \" ( g:id | g:num | g:sym | g:sp )(+) \"

"##;
  let mut j = Journal::new(Some(Config { ..Default::default() }));

  /*   let g = GrammarStore::from_str(&mut j, input).unwrap();

   //

   let prod_id = g.get_production_id_by_name("scan_tok_C62B306E7774ADEF_3DE072630D545EFC")?;

   let states = compile_token_production_states(&mut j, prod_id)?;

   for (state) in &states {
     eprintln!("{}", state.get_code());
   }
   j.flush_reports();
   j.debug_print_reports(ReportType::TokenProductionCompile(Default::default()));
  */
  let (mut j, states) = build_states(input)?;
  for (_, state) in &states {
    eprintln!("{}", state.get_code());
  }
  let states = optimize_ir_states(&mut j, states);

  j.flush_reports();
  //j.debug_print_reports(ReportType::TokenProductionCompile(Default::default()));

  let bc = compile_bytecode(&mut j, states);

  eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  SherpaResult::Ok(())
}

#[test]
fn raddler() -> SherpaResult<()> {
  let input = r##" 
  @IGNORE g:sp

  <> p > r g:id
         | p g:id
         | g:id

  <> r > p g:id
       |   g:num
  "##;

  let (mut j, states) = build_states(input)?;

  let states = optimize_ir_states(&mut j, states);

  for (_, state) in &states {
    eprintln!("{}", state.get_code());
  }

  let bc = compile_bytecode(&mut j, states);

  eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  j.flush_reports();
  j.debug_print_reports(ReportType::ProductionCompile(Default::default()));

  SherpaResult::Ok(())
}

fn build_states(input: &str) -> SherpaResult<(Journal, BTreeMap<String, Box<IRState>>)> {
  let mut j = Journal::new(Some(Config { enable_breadcrumb_parsing: false, ..Default::default() }));
  let g = GrammarStore::from_str(&mut j, input)?;

  let states = compile_states(&mut j, get_num_of_available_threads())?;

  SherpaResult::Ok((j, states))
}

fn build_production_states(
  input: &str,
  production_names: &[&str],
) -> SherpaResult<(Journal, Vec<Vec<Box<IRState>>>)> {
  let mut j = Journal::new(Some(Config { enable_breadcrumb_parsing: false, ..Default::default() }));
  let g = GrammarStore::from_str(&mut j, input)?;

  let mut states = vec![];

  for name in production_names {
    let prod_id = g.get_production_id_by_name(name)?;
    states.push(compile_production_states(&mut j, prod_id)?);
  }

  SherpaResult::Ok((j, states))
}

// fn build_states(input: &str) -> SherpaResult<(Journal, Vec<Box<IRState>>)> {
// let mut j = Journal::new(Some(Config { enable_breadcrumb_parsing: true, ..Default::default() }));
// let g = GrammarStore::from_str(&mut j, input)?;
// let prod_id = g.get_production_id_by_name(production_name)?;
// let states = compile_production_states(&mut j, prod_id)?;
//
// SherpaResult::Ok((j, states))
// }

#[test]
fn test_compile_json_parser() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    r##"
    @IGNORE g:sp g:nl

    @EXPORT json as entry

    @NAME llvm_language_test

    <> json > 
            object                              f:ast { { t_Json, v: $1 } }
            | 
            array                               f:ast { { t_Json, v: $1 } }

    <> array > \[  value(*\, )  \]              f:ast { { t_Array, entries: $2 } }

    <> object > \{ key_value(*\, ) \}           f:ast { { t_Object, entries: $2 } }

    <> key_value > str \: value              f:ast { { t_KeyVal, k:$1, v:$3 } }

    <> value > num | bool | str | null

    <> null > t:null                            f:ast { { t_Null, v:false } }

    <> bool > 
        t:false                                 f:ast { { t_Bool, v:false } }
        |   
        t:true                                  f:ast { { t_Bool, v:true } }

    <> num > tk:number                          f:ast { { t_Number } }

    <> number > ( \+ | \- )? g:num(+) ( \. g:num(+) )? ( ( \e | \E ) ( \+ | \i ) g:num(+) )?

    <> str > tk:string f:ast { { t_String } }

    <> string > \" ( g:id | g:sym | g:num | g:sp )(*) \"  
"##,
  )
  .unwrap();

  let mut states = compile_states(&mut j, 1)?;

  /* for (_, state) in &states {
    eprintln!("{}", state.get_code());
  } */
  //return SherpaResult::Ok(());

  let ir_states = optimize_ir_states(&mut j, states);

  j.debug_print_reports(ReportType::ScannerCompile(ScannerStateId::default()));

  let bc = compile_bytecode(&mut j, ir_states);

  eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  SherpaResult::Ok(())
}
