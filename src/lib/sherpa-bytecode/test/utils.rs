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

pub type TestParser<'a> = ByteCodeParser<'a, UTF8StringReader<'a>, u32>;

pub fn compile_and_run_grammars(source: &[&str], inputs: &[(&str, &str, bool)]) -> SherpaResult<()> {
  build_parse_states_from_multi_sources(source, "".into(), true, &|tp| {
    // states.iter().for_each(|(_, s)| println!("{}\n\n",
    // s.source_string(db.string_store())));

    let (bc, state_map) = compile_bytecode(&tp, true)?;

    let TestPackage { db, .. } = tp;

    for (entry_name, input, should_pass) in inputs {
      let ok = TestParser::new(&mut ((*input).into()), &bc)
        .completes(db.get_entry_offset(entry_name, &state_map).expect(&format!(
          "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
          db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
        )) as u32)
        .is_ok();

      let mut cd = console_debugger(db.to_owned(), PrintConfig {
        display_scanner_output: true,
        display_instruction: true,
        ..Default::default()
      });

      if ok != *should_pass {
        TestParser::new(&mut ((*input).into()), &bc).collect_shifts_and_skips(
          db.get_entry_offset(entry_name, &state_map).expect(&format!(
            "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
            db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
          )) as u32,
          0,
          &mut cd.as_deref_mut(),
        );
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
      let prod = db.prod_from_name(name);
      if prod != Default::default() {
        let rule_id = db.prod_rules(prod).unwrap()[rule_number];
        Some((Into::<usize>::into(rule_id), b))
      } else {
        None
      }
    })
    .collect::<OrderedMap<_, _>>()
    .into_values()
    .collect::<Vec<_>>()
}
