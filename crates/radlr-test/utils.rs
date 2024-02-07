use crate::debug::{file_debugger, PrintConfig};
use radlr_bytecode::compile_bytecode;
use radlr_core::{
  proxy::OrderedMap,
  test::utils::{build_parse_states_from_multi_sources, build_parse_states_from_multi_sources_beta},
  *,
};
use radlr_rust_runtime::{
  kernel::{disassemble_bytecode, ByteCodeParserNew},
  types::{AstObjectNew, BytecodeParserDB, ParserInput, ParserProducer, ReducerNew, RuntimeDatabase, StringInput},
};
use std::fmt::format;
pub type TestParser = ByteCodeParserNew;

/// Writes to a debug file for testing
pub fn _write_debug_file_<FileName: AsRef<std::path::Path>, Data: AsRef<[u8]>>(
  db: &ParserDatabase,
  file_name: FileName,
  data: Data,
  append: bool,
  beta: bool,
) -> RadlrResult<()> {
  use std::{env::*, fs::*, io::Write};

  let mut file_dir = temp_dir().join("radlr_testing").join(db.name_string());

  if beta {
    file_dir = file_dir.join("beta");
  } else {
    file_dir = file_dir.join("alpha");
  }

  let file_path = file_dir.join(file_name);

  create_dir_all(file_dir)?;

  let mut file = OpenOptions::new().append(append).truncate(!append).write(true).create(true).open(file_path)?;
  file.write_all(data.as_ref())?;
  file.flush()?;

  Ok(())
}

/// Writes the parser IR states to a file in the temp directory
pub fn _write_states_to_temp_file_(builder: &impl ParserStore) -> RadlrResult<()> {
  #[cfg(all(debug_assertions))]
  {
    let db = builder.get_db();
    _write_debug_file_(
      db,
      "ir_states.tmp",
      format!(
        "
===============================================================================
{}
{}  
===============================================================================
\n\n",
        builder.get_classification().to_string(),
        builder.report().to_string()
      ),
      false,
      builder.get_config().is_beta(),
    )?;

    for (i, state) in builder.get_states().enumerate() {
      _write_debug_file_(
        db,
        "ir_states.tmp",
        format!("{i:0>5}: [{:X}] \n\n {}", state.1.get_canonical_hash(db, true)?, state.1.print(db, true)? + "\n"),
        true,
        builder.get_config().is_beta(),
      )?;
    }
  }

  Ok(())
}

/// Writes the parser IR states to a file in the temp directory
pub fn _write_disassembly_to_temp_file_(pkg: &BytecodeParserDB, db: &ParserDatabase, config: ParserConfig) -> RadlrResult<()> {
  #[cfg(all(debug_assertions))]
  {
    _write_debug_file_(db, "bc_disassembly.tmp", disassemble_bytecode(&pkg.bytecode), false, config.is_beta())?;
  }
  Ok(())
}

pub fn compile_and_run_grammars(source: &[&str], inputs: &[(&str, &str, bool)], config: ParserConfig) -> RadlrResult<()> {
  build_parse_states_from_multi_sources(
    source,
    "".into(),
    false,
    &|tp| {
      #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
      _write_states_to_temp_file_(&tp)?;

      let pkg = compile_bytecode(&tp, true)?;
      let TestPackage { db, .. } = tp;

      _write_disassembly_to_temp_file_(&pkg, &db, config)?;

      let mut parser = pkg.get_parser().unwrap();

      for (entry_name, input, should_pass) in inputs {
        let entry = pkg.get_entry_data_from_name(entry_name).expect(&format!(
          "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
          db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
        ));

        parser.set_debugger(file_debugger(
          db.as_ref().clone(),
          PrintConfig {
            display_scanner_output: true,
            display_instruction: true,
            display_input_data: true,
            display_state: true,
            ..Default::default()
          },
          pkg.address_to_state_name.clone(),
          false,
        ));

        let result = parser.as_mut().recognize(&mut StringInput::from(*input), entry);

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

      RadlrResult::Ok(())
    },
    config,
  )
}

pub fn compile_and_run_grammars_beta(source: &[&str], inputs: &[(&str, &str, bool)], config: ParserConfig) -> RadlrResult<()> {
  let config = config.beta();
  build_parse_states_from_multi_sources_beta(
    source,
    "".into(),
    false,
    &|tp| {
      #[cfg(all(debug_assertions, not(feature = "wasm-target")))]
      _write_states_to_temp_file_(&tp)?;

      let pkg = compile_bytecode(&tp, true)?;
      let TestPackage { db, .. } = tp;

      _write_disassembly_to_temp_file_(&pkg, &db, config)?;

      let mut parser = pkg.get_parser().unwrap();

      for (entry_name, input, should_pass) in inputs {
        let entry = pkg.get_entry_data_from_name(entry_name).expect(&format!(
          "\nCan't find entry offset for entry point [{entry_name}].\nValid entry names are\n    {}\n",
          db.entry_points().iter().map(|e| { e.entry_name.to_string(db.string_store()) }).collect::<Vec<_>>().join(" | ")
        ));

        parser.set_debugger(file_debugger(
          db.as_ref().clone(),
          PrintConfig {
            display_scanner_output: true,
            display_instruction: true,
            display_input_data: true,
            display_state: true,
            ..Default::default()
          },
          pkg.address_to_state_name.clone(),
          true,
        ));

        let result = parser.as_mut().recognize(&mut StringInput::from(*input), entry);

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

      RadlrResult::Ok(())
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
