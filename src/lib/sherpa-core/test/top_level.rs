use crate::{
  ascript::types::AScriptStore,
  build::{
    ascript::build_ascript_types_and_functions,
    pipeline::{BuildPipeline, SourceType},
  },
  types::{GrammarStore, *},
  Journal,
};
use std::path::PathBuf;

/* #[test]
fn test_pipeline() -> SherpaResult<()> {
  let threads = get_num_of_available_threads();

  let mut j = Journal::new(None);

  let g = GrammarStore::from_str(
    &mut j,
    "
IGNORE { c:sp c:tab }

<> start > 'hello' 'world'
",
  );

  assert!(!j.debug_error_report());

  let g = g?;

  let mut states = compile_parse_states(&mut j, threads)?;

  for state in states.values_mut() {
    if state.get_ast().is_none() {
      eprintln!("--FAILED: {:?}", state.get_cached_ast())
    }
  }

  let entry_state_name = &g.get_production_by_name("start").unwrap().guid_name;

  let (bytecode, state_lookup) =
    build_byte_code_buffer(states.iter().map(|(_, s)| s.get_ast().unwrap()).collect::<Vec<_>>());

  let entry_point = *state_lookup.get(entry_state_name).unwrap();

  let target_production_id = g.get_production_by_name("start").unwrap().bytecode_id?;

  let (shifts, skips) = collect_shifts_and_skips(
    "hello    \tworld",
    entry_point,
    target_production_id,
    &bytecode,
    None,
  )?;

  assert_eq!(shifts, ["hello", "world"]);

  assert_eq!(skips, ["    \t"]);

  j.flush_reports();

  j.debug_print_reports(crate::ReportType::Any);

  SherpaResult::Ok(())
} */

#[test]
fn test_compile_pipeline() {
  let mut pipeline = BuildPipeline::from_string(
    "
<> A > '1' :ast { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true }
| 'a' 'b' A :ast { t_Banana, value: u32($1), dd:u32($3), tok, useful:false }
    ",
    &PathBuf::from("/tmp"),
    Default::default(),
  );

  pipeline
    .set_source_output_dir(&std::env::temp_dir())
    .add_task(build_ascript_types_and_functions(SourceType::Rust))
    .run(|_| {});
}

#[test]
fn test_output_rust_on_trivial_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    "
<> A > '1' :ast { t_Banana, c_Mobius, value:u32($1), string:str($1), useful:true }
| 'a' 'b' A :ast { t_Banana, value: u32($1), dd:u32($3), tok, useful:false }
        ",
  )
  .unwrap();

  let ascript = AScriptStore::new(&mut j)?;

  assert_eq!(ascript.structs.len(), 1);

  SherpaResult::Ok(())
}
