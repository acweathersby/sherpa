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

  assert_eq!(result._get_node_len(), 7);

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

  assert_eq!(result.len(), 8);
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

  let token_production = grammar.get_production_by_name("tk:B").unwrap();

  let result = compile_scanner_states(&mut j, BTreeSet::from_iter(vec![token_production.sym_id]))?;

  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 4);
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

  assert_eq!(result.len(), 6);
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

  let production = g.get_production_by_name("tk:V")?;

  let symbols = SymbolSet::from_iter(vec![production.sym_id]);

  let result = compile_scanner_states(&mut j, symbols.clone())?;

  eprintln!("{:#?}", result);

  j.flush_reports();
  j.debug_print_reports(ReportType::ScannerCompile(ScannerStateId::new(&symbols)));

  assert_eq!(result.len(), 3);

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

fn build_states(input: &str) -> SherpaResult<(Journal, BTreeMap<String, Box<IRState>>)> {
  let mut j = Journal::new(Some(Config { enable_breadcrumb_parsing: false, ..Default::default() }));
  let g = GrammarStore::from_str(&mut j, input)?;

  let states = compile_states(&mut j, get_num_of_available_threads())?;

  SherpaResult::Ok((j, states))
}
