use std::path::PathBuf;

use crate::{
  ascript::types::AScriptStore,
  build::{
    ascript::build_ascript_types_and_functions,
    pipeline::{BuildPipeline, SourceType},
  },
  bytecode::compile::build_byte_code_buffer,
  debug::collect_shifts_and_skips,
  intermediate::compile::compile_states,
  types::{GrammarStore, *},
  util::get_num_of_available_threads,
  Journal,
};

#[test]
fn test_pipeline() -> SherpaResult<()> {
  let threads = get_num_of_available_threads();

  let mut j = Journal::new(None);

  let g = GrammarStore::from_str(
    &mut j,
    "
@IGNORE g:sp g:tab

<> start > \\hello \\world 
",
  )
  .unwrap();

  let mut states = compile_states(&mut j, threads)?;

  for state in states.values_mut() {
    if state.get_ast().is_none() {
      eprintln!("--FAILED: {:?}", state.compile_ast())
    }
  }

  let entry_state_name = &g.get_production_by_name("start").unwrap().guid_name;

  let (bytecode, state_lookup) =
    build_byte_code_buffer(states.iter().map(|(_, s)| s.get_ast().unwrap()).collect::<Vec<_>>());

  let entry_point = *state_lookup.get(entry_state_name).unwrap();

  let target_production_id = g.get_production_by_name("start").unwrap().bytecode_id;

  let (shifts, skips) =
    collect_shifts_and_skips("hello    \tworld", entry_point, target_production_id, &bytecode)?;

  assert_eq!(shifts, ["hello", "world"]);

  assert_eq!(skips, ["    \t"]);

  j.flush_reports();

  j.debug_print_reports(crate::ReportType::Any);

  SherpaResult::Ok(())
}

#[test]
fn test_compile_pipeline() {
  let mut pipeline = BuildPipeline::from_string(
    "
    <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
    | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
    ",
    &PathBuf::from("/tmp"),
    Default::default(),
  );

  pipeline
    .set_source_output_dir(&std::env::temp_dir())
    .add_task(build_ascript_types_and_functions(SourceType::Rust))
    .run(|e| {});
}

#[test]
fn test_output_rust_on_trivial_grammar() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
        <> A >\\1 f:ast { { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true } } 
        | \\a \\b A f:ast { { t_Banana, value: u32($1), dd:u32($3), tok, useful:false } }
        ",
  )
  .unwrap();

  let mut ascript = AScriptStore::new(g).unwrap();

  assert_eq!(ascript.structs.len(), 1);
}
