use crate::{
  ascript::types::AScriptStore,
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore},
  debug::collect_shifts_and_skips,
  llvm::{compile_module_from_bytecode, construct_module},
  util::get_num_of_available_threads,
  writer::code_writer::StringBuffer,
  Config,
  Journal,
  SherpaResult,
};
use inkwell::context::Context;
use std::{path::PathBuf, str::FromStr};

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn test_compile_of_sherpa_grammar() -> SherpaResult<()> {
  let grammar_path = std::env::var("CARGO_MANIFEST_DIR")
    .map(|val| PathBuf::from_str(&val).unwrap().join("../../grammar/"))
    .unwrap_or_else(|_| {
      std::env::current_dir()
        .unwrap()
        .join(PathBuf::from_str(concat!("./", file!())).unwrap().parent().unwrap())
    })
    .join("v1_0_0_strap/grammar.hcg");

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

  // eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  // Build LLVM Data

  let ctx = Context::create();

  let mut module = construct_module(&mut j, "test", &ctx);
  compile_module_from_bytecode(&mut module, &mut j, &bc)?;

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  // Perform a parsing pass on some simple input.

  let (.., shifts, _) = collect_shifts_and_skips(
    r##"<> t > "t" => { $2 + $3 + tok<1,2> } <> B > [unordered "d" "b" "c" ] "##,
    *bc.state_name_to_offset.get(g.get_exported_productions()[0].guid_name)?,
    g.get_exported_productions()[0].production.bytecode_id,
    &bc.bytecode,
  )?;

  dbg!(shifts);

  SherpaResult::Ok(())
}
