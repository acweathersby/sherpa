use crate::{
  ascript::types::AScriptStore,
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore},
  debug::{
    collect_shifts_and_skips,
    disassemble_state,
    generate_disassembly,
    BytecodeGrammarLookups,
  },
  grammar::compile::parser::sherpa::Grammar,
  llvm::{compile_module_from_bytecode, construct_module},
  test::utils::path_from_source,
  util::get_num_of_available_threads,
  writer::code_writer::StringBuffer,
  Config,
  Journal,
  SherpaResult,
};
use inkwell::context::Context;
use sherpa_runtime::{
  functions::DebugEvent,
  types::{ByteReader, UTF8Reader},
};

#[test]
fn test_compile_of_sherpa_grammar_bytecode() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar_path = path_from_source("grammar/v1_0_0_strap/grammar.hcg")?;

  let SherpaResult::Ok(g) = GrammarStore::from_path(&mut j, grammar_path) else {
    j.debug_error_report();
    return SherpaResult::None;
  };

  let states = compile_states(&mut j, get_num_of_available_threads());

  j.get_report(
    crate::ReportType::ProductionCompile(g.get_production_id_by_name("terminal_list_1")?),
    |r| {
      println!("{}", r.debug_string());
      false
    },
  );

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let ir_states = optimize_ir_states(&mut j, states?);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let bc = compile_bytecode(&mut j, ir_states);

  /// eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));
  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let entry_state_name = &g.get_production_by_name("grammar").unwrap().guid_name;

  let entry_point = *bc.state_name_to_offset.get(entry_state_name).unwrap();

  let target_production_id = g.get_production_by_name("grammar").unwrap().bytecode_id;

  let lu = BytecodeGrammarLookups::new(g.clone());

  let (shifts, skips) = collect_shifts_and_skips(
    r#"
NAME sherpa

<> AA > "dd" AA

<> BB > "cc"
"#,
    entry_point,
    target_production_id,
    &bc.bytecode,
    None,
  )?;

  dbg!(shifts, skips);

  SherpaResult::Ok(())
}

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn test_compile_of_sherpa_grammar_llvm() -> SherpaResult<()> {
  let grammar_path = path_from_source("grammar/v1_0_0_strap/grammar.hcg")?;

  dbg!(&grammar_path);

  let mut j = Journal::new(Some(Config { ..Default::default() }));

  let SherpaResult::Ok(g) = GrammarStore::from_path(&mut j, grammar_path) else {
    assert!(!j.debug_error_report());
    return SherpaResult::None;
  };

  assert!(!j.debug_error_report());

  // Compile parse from grammar

  let states = compile_states(&mut j, get_num_of_available_threads())?;

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let ir_states = optimize_ir_states(&mut j, states);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let bc = compile_bytecode(&mut j, ir_states);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  // Build Ascript data

  let ascript = AScriptStore::new(g.clone())?;

  let mut writer = StringBuffer::new(vec![]);

  crate::ascript::rust::write(&ascript, &mut writer)?;

  //eprintln!("{}", String::from_utf8(writer.into_output())?);

  //eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  // Build LLVM Data

  let ctx = Context::create();

  let mut module = construct_module(&mut j, "test", &ctx);
  compile_module_from_bytecode(&mut module, &mut j, &bc)?;

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  // Perform a parsing pass on some simple input.

  let (.., shifts, _) = collect_shifts_and_skips(
    r##"<> t > "t" :ast { $2 + $3 + tok<1,2> } <> B > [unordered "d" "b" "c" ] "##,
    *bc.state_name_to_offset.get(g.get_exported_productions()[0].guid_name)?,
    g.get_exported_productions()[0].production.bytecode_id,
    &bc.bytecode,
    None,
  )?;

  dbg!(shifts);

  SherpaResult::Ok(())
}
