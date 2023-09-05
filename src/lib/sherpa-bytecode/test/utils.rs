use crate::*;
use sherpa_core::{proxy::OrderedMap, test::utils::build_parse_states_from_multi_sources, *};
use sherpa_rust_runtime::{
  bytecode::ByteCodeParser,
  types::{
    ast::{AstObject, AstSlot, AstStackSlice, Reducer},
    ByteReader,
    MutByteReader,
    ParseContext,
    SherpaParser,
    UTF8StringReader,
  },
};

pub type TestParser<'a, Bytecode> = ByteCodeParser<UTF8StringReader<'a>, u32, Bytecode>;

pub fn compile_and_run_grammars(source: &[&str], inputs: &[(&str, &str, bool)]) -> SherpaResult<()> {
  build_parse_states_from_multi_sources(source, "".into(), true, &|tp| {
    #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
    tp.write_states_to_temp_file()?;

    let pkg = compile_bytecode(&tp, true)?;
    let TestPackage { db, .. } = tp;

    for (entry_name, input, should_pass) in inputs {
      TestParser::new(&mut ((*input).into()), &pkg).collect_shifts_and_skips(
        db.get_entry_offset(entry_name, &pkg.state_name_to_address).expect(&format!(
          "\nCan't find entry offset for entry point [default].\nValid entry names are\n    {}\n",
          db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
        )) as u32,
        4,
        &mut sherpa_core::file_debugger(db.to_owned(), PrintConfig {
          display_scanner_output: false,
          display_instruction: false,
          display_input_data: true,
          display_state: true,
          ..Default::default()
        })
        .as_deref_mut(),
      );

      let ok = TestParser::new(&mut ((*input).into()), &pkg)
        .completes(
          db.get_entry_offset(entry_name, &pkg.state_name_to_address).expect(&format!(
            "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
            db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
          )) as u32,
          &mut None,
        )
        .is_ok();

      if ok != *should_pass {
        panic!(
          "\n\nParsing of input\n   \"{input}\"\nthrough entry point [{entry_name}] should {}.\n",
          if *should_pass { "pass" } else { "fail" }
        );
      }
    }

    SherpaResult::Ok(())
  })
}

// Sorts reduce functions according to their respective
// rules. This assumes the number of rules in the array
// matches the number of rules in the parser.
pub fn map_reduce_function<'a, R, ExtCTX, ASTNode>(
  db: &ParserDatabase,
  fns: Vec<(&str, usize, fn(*mut ParseContext<R, ExtCTX>, &AstStackSlice<AstSlot<ASTNode>, true>))>,
) -> Vec<Reducer<R, ExtCTX, ASTNode, true>>
where
  R: ByteReader + MutByteReader,
  ASTNode: AstObject,
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
