use std::{
  path::{Path, PathBuf},
  str::FromStr,
};

use inkwell::context::Context;

use crate::{
  ascript::{self, types::AScriptStore},
  build::disassembly,
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore},
  debug::{collect_shifts_and_skips, generate_disassembly},
  llvm::compile_from_bytecode,
  util::get_num_of_available_threads,
  writer::code_writer::StringBuffer,
  Config,
  Journal,
  SherpaResult,
};

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn test_compile_of_sherpa_grammar() -> SherpaResult<()> {
  let grammar_path = std::env::var("CARGO_MANIFEST_DIR")
    .map(|val| PathBuf::from_str(&val).unwrap().join("test/bootstrap/"))
    .unwrap_or_else(|_| {
      std::env::current_dir()
        .unwrap()
        .join(PathBuf::from_str(concat!("./", file!())).unwrap().parent().unwrap())
    })
    .join("grammar/grammar.hcg");

  let mut j = Journal::new(Some(Config { ..Default::default() }));

  let SherpaResult::Ok(g) = GrammarStore::from_path(&mut j, grammar_path) else {

    j.debug_error_report();

    return SherpaResult::None;
  };

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

  let mut ascript = AScriptStore::new(g.clone())?;

  let mut writer = StringBuffer::new(vec![]);

  crate::ascript::rust::write(&ascript, &mut writer);

  // eprintln!("{}", String::from_utf8(writer.into_output())?);

  //eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  // Build LLVM Data

  let ctx = Context::create();

  let module = compile_from_bytecode("test", &mut j, &ctx, &bc)?;

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  // Perform a parsing pass on some simple input.

  let (.., shifts, _) = collect_shifts_and_skips(
    r##" <> test > "test" "##,
    *bc.state_name_to_offset.get(g.get_exported_productions()[0].guid_name)?,
    g.get_exported_productions()[0].production.bytecode_id,
    bc.bytecode,
  );

  dbg!(shifts);

  SherpaResult::Ok(())
}
