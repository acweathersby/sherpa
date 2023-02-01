pub(crate) mod create_store;
pub mod finalize;
pub mod merge;
pub mod parse;
pub mod parser;
use sherpa_runtime::functions::DebugEvent;

use self::{finalize::finalize_grammar, merge::merge_grammars, parser::sherpa};
use crate::{
  compile::{compile_bytecode, compile_scanner_states, compile_states, optimize_ir_states},
  debug::{
    collect_shifts_and_skips,
    disassemble_state,
    generate_disassembly,
    BytecodeGrammarLookups,
  },
  grammar::compile::{
    create_store::create_store,
    parse::{grammar_from_string, load_from_path},
  },
  types::*,
  util::get_num_of_available_threads,
  Config,
  Journal,
  ReportType,
};
use std::{
  collections::{BTreeSet, HashMap},
  ffi::OsStr,
  path::PathBuf,
  sync::Arc,
};

fn compile_grammars(
  j: &mut Journal,
  grammars: &Vec<(PathBuf, ImportedGrammarReferences, Box<sherpa::Grammar>)>,
) -> SherpaResult<()> {
  if grammars.is_empty() {
    j.report_mut().add_error("No grammars were generated.".into());
    SherpaResult::None
  } else {
    let results = std::thread::scope(|s| {
      grammars
        .chunks(
          (grammars.len() as f64 / get_num_of_available_threads() as f64).ceil().max(1.0) as usize
        )
        .into_iter()
        .map(|chunk| {
          let mut j = j.transfer();
          s.spawn(move || {
            chunk
              .iter()
              .map(|(absolute_path, import_refs, grammar)| {
                let grammar =
                  create_store(&mut j, &grammar, absolute_path.clone(), import_refs.clone());
                grammar
              })
              .collect::<Vec<_>>()
          })
        })
        .map(|s| s.join().unwrap())
        .collect::<Vec<_>>()
    });
    j.flush_reports();

    if j.have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    let mut grammars: Vec<Arc<GrammarStore>> = results.into_iter().flatten().collect();

    let rest = grammars.drain(1..).collect::<Vec<_>>();

    let mut grammar = Arc::try_unwrap(grammars.pop().unwrap()).unwrap();

    merge_grammars(j, &mut grammar, &rest);

    if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    let grammar = finalize_grammar(j, grammar);

    if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    j.set_grammar(Arc::new(grammar));

    SherpaResult::Ok(())
  }
}

#[test]
fn grammar_name_from_preamble() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast = grammar_from_string(
    &mut j,
    r#"
    NAME test
    IGNORE { "tacos" }
    <> A > "hello" 
    "world" "#,
    Default::default(),
  );

  compile_grammars(&mut j, &ast)?;

  assert_eq!(j.grammar()?.id.name, "test");

  SherpaResult::Ok(())
}

#[test]
fn grammar_with_optional_list() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast = grammar_from_string(&mut j, r#"<> A > "hello"(*) c:nl"#, Default::default());

  compile_grammars(&mut j, &ast);

  j.flush_reports();
  assert!(!j.debug_error_report());

  let grammar = j.grammar()?;

  let prod_id = grammar.get_production_id_by_name("A")?;

  let rules = grammar.production_rules.get(&prod_id)?;

  assert_eq!(rules.len(), 2);

  SherpaResult::Ok(())
}

#[test]
fn grammar_name_from_path() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast =
    grammar_from_string(&mut j, r#" IGNORE { "a" } <> A > "h"  "w" "#, "/test_path.test".into());

  compile_grammars(&mut j, &ast)?;

  assert_eq!(j.grammar()?.id.name, "test_path");

  SherpaResult::Ok(())
}

#[test]
fn missing_append_host_error() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  let ast = grammar_from_string(
    &mut j,
    r#"
  NAME test
  IGNORE { "tacos" }
  +> A > "hello"{1} 
       | "goodby"{1} 
  
  "#,
    "/test_path.test".into(),
  );

  compile_grammars(&mut j, &ast);

  j.flush_reports();

  assert!(j.debug_error_report());

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    let error = &r.errors[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "missing-append-host");

    true
  }));

  SherpaResult::Ok(())
}

#[test]
fn compile_grammar_with_syntax_definitions() -> SherpaResult<()> {
  let mut j = Journal::new(Some(Config::default()));

  j.set_active_report("Grammar Parse", crate::ReportType::GrammarCompile(Default::default()));

  let ast = grammar_from_string(
    &mut j,
    r#"<> A > B :syn { $1: type rgb(r255 g33 b22) } "#,
    "/test_path.test".into(),
  );

  j.flush_reports();

  assert!(!j.debug_error_report());

  let syntax = ast[0].2.productions[0].rules[0].syntax_definition.as_ref()?;
  let color = &syntax.specs[0].spec.rgb.as_ref()?;
  let id = &syntax.specs[0].spec.id;

  assert_eq!(syntax.specs[0].reference.to_string(), "$1");
  assert_eq!(color.r, 255);
  assert_eq!(color.g, 33);
  assert_eq!(color.b, 22);
  assert_eq!(id.to_string(), "type");

  SherpaResult::Ok(())
}

#[test]
pub fn generate_block_comment() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar_data = grammar_from_string(
    &mut j,
    r#"
<> A > tk:comment

<> comment > tk:block  | tk:line  | c:sym 
   
<> block > "/*"  ( c:sym )(*) "*/"

<> line > "//"  ( c:sym )(*) c:nl

"#,
    "/test_path.test".into(),
  );

  j.flush_reports();
  j.debug_error_report();

  compile_grammars(&mut j, &grammar_data);

  j.flush_reports();
  j.debug_error_report();

  let grammar = j.grammar()?;

  let token_production = grammar.get_production_by_name("tk:block")?;

  let result = compile_scanner_states(
    &mut j,
    BTreeSet::from_iter(vec![
      grammar.get_production_by_name("tk:block")?.sym_id,
      grammar.get_production_by_name("tk:line")?.sym_id,
      SymbolID::from_string("g:sym", Some(&grammar)),
    ]),
  )?;
  j.flush_reports();
  j.debug_print_reports(ReportType::ScannerCompile(Default::default()));
  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 11);
  SherpaResult::Ok(())
}

// Compile v1.0.0 grammar with v1.0.0_strap parser
#[test]
fn compile_latest_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let path = crate::test::utils::path_from_source("grammar/v1_0_0/grammar.sp")?;

  let grammar_data = load_from_path(&mut j, path);
  assert!(!j.debug_error_report());

  compile_grammars(&mut j, &grammar_data);

  j.flush_reports();
  assert!(!j.debug_error_report());

  let states = compile_states(&mut j, get_num_of_available_threads());

  //j.debug_print_reports(crate::ReportType::ScannerCompile(ScannerStateId::default()));

  j.flush_reports();
  assert!(!j.debug_error_report());

  let states = optimize_ir_states(&mut j, states?);

  let bc = compile_bytecode(&mut j, states);

  println!("{}", generate_disassembly(&bc, Some(&mut j)));

  j.flush_reports();
  assert!(!j.debug_error_report());

  let entry_state_name = j.grammar()?.get_production_by_name("grammar").unwrap().guid_name.clone();
  let target_production_id = j.grammar()?.get_production_by_name("grammar").unwrap().bytecode_id;
  let entry_point = *bc.state_name_to_offset.get(&entry_state_name).unwrap();
  let grammar_pack = BytecodeGrammarLookups::new(j.grammar()?);

  let (shifts, skips) = collect_shifts_and_skips(
    r#"
    /* test */
    // This is a comment
      <>grammar>
        "Hello" 'World'
"#,
    entry_point,
    target_production_id,
    &bc.bytecode,
    Some(&move |event| match event {
      DebugEvent::ExecuteInstruction { bc, address, instruction, ctx } => {
        if true || !ctx.is_scanner() {
          return;
          println!(
            "[{} : {}] {}",
            ctx.get_reader().get_char(),
            ctx.get_reader().cursor(),
            disassemble_state(bc, address, Some(&grammar_pack)).0
          )
        }
      }
      _ => {}
    }),
  )?;

  dbg!(shifts, skips);

  SherpaResult::Ok(())
}
