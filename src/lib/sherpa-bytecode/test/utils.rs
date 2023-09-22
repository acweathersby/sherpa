use crate::*;
use sherpa_core::{proxy::OrderedMap, test::utils::build_parse_states_from_multi_sources, *};
use sherpa_rust_runtime::{
  bytecode::ByteCodeParserNew,
  types::{AstObjectNew, ParserInput, ParserProducer, ReducerNew, RuntimeDatabase, StringInput},
};

pub type TestParser = ByteCodeParserNew;

pub fn compile_and_run_grammars(source: &[&str], inputs: &[(&str, &str, bool)], config: ParserConfig) -> SherpaResult<()> {
  build_parse_states_from_multi_sources(
    source,
    "".into(),
    true,
    &|tp| {
      #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
      tp._write_states_to_temp_file_()?;

      let pkg = compile_bytecode(&tp, true)?;
      let TestPackage { db, .. } = tp;

      pkg._write_disassembly_to_temp_file_(&db)?;

      let mut parser = pkg.get_parser().unwrap();

      for (entry_name, input, should_pass) in inputs {
        let (bc_offset, e): (u32, &EntryPoint) = db.get_entry_data(entry_name, &pkg).expect(&format!(
          "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
          db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
        ));

        assert!(bc_offset != 0);

        let entry = pkg.get_entry_data_from_name(entry_name);

        parser.set_debugger(sherpa_core::file_debugger(
          db.to_owned(),
          PrintConfig {
            display_scanner_output: false,
            display_instruction: false,
            display_input_data: true,
            display_state: true,
            ..Default::default()
          },
          pkg.address_to_state_name.clone(),
        ));

        parser.init(entry.unwrap().nonterm_id)?;

        let result = parser.as_mut().completes(&mut StringInput::from(*input), e.nonterm_key.to_val());

        if result.is_ok() != *should_pass {
          if result.is_err() {
            result?;
          }
          panic!(
            "\n\nParsing of input\n   \"{input}\"\nthrough entry point [{entry_name}] should {}.\n",
            if *should_pass { "pass" } else { "fail" }
          );
        }
      }

      SherpaResult::Ok(())
    },
    config,
  )
}

// Sorts reduce functions according to their respective
// rules. This assumes the number of rules in the array
// matches the number of rules in the parser.
pub fn map_reduce_function<I: ParserInput, ASTNode>(
  db: &ParserDatabase,
  fns: Vec<(&str, usize, ReducerNew<I, ASTNode>)>,
) -> Vec<ReducerNew<I, ASTNode>>
where
  ASTNode: AstObjectNew,
{
  fns
    .into_iter()
    .filter_map(|(name, rule_number, b)| {
      let nterm = db.nonterm_from_name(name);
      if nterm != Default::default() {
        let rule_id = db.nonterm_rules(nterm).unwrap()[rule_number];
        Some((Into::<usize>::into(rule_id), b))
      } else {
        None
      }
    })
    .collect::<OrderedMap<_, _>>()
    .into_values()
    .collect::<Vec<_>>()
}
