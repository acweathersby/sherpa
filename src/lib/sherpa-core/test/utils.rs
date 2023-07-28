use crate::*;
use std::{path::PathBuf, str::FromStr, sync::Arc};

#[cfg(test)]
/// Return the full filepath of a grammar stored in
/// ./tests/grammars/
pub(super) fn get_test_grammar_path(partial_path: &str) -> PathBuf {
  let path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../test/grammars/").join(partial_path);
  dbg!(&path);
  path.canonicalize().unwrap()
}

/* pub struct TestConfig<'a> {
  pub llvm_parse: bool,
  pub bytecode_parse: bool,
  pub print_llvm_ir: bool,
  pub build_llvm: bool,
  pub build_bytecode: bool,
  pub print_states: bool,
  pub build_ascript: bool,
  pub print_ascript: bool,
  pub build_parse_states: bool,
  /// Run state reduction and llvm optimization passes. Defaults to `true`.
  pub optimize: bool,
  /// Assert that reports do not contain error at every compilation point.
  /// Defaults to `true`.
  pub assert_clean_reports: bool,
  pub print_disassembly: bool,
  /// Prints the parse report for the givin productions
  pub print_parser_states_compile_reports: &'static [&'static str],
  /// If `grammar_string` is assigned a value, then this is treated
  /// as a base dir path for file imports declared in the grammar.
  ///
  /// Other wise, this is expected to be an absolute file path to
  /// a grammar file.
  pub grammar_path: Option<PathBuf>,
  /// A grammar declaration string.
  pub grammar_string: Option<&'static str>,
  pub journal: Option<Journal>,
  pub num_of_threads: usize,
  pub debugger_handler: Option<
    &'a dyn Fn(Arc<GrammarStore>) -> Option<Box<dyn FnMut(&DebugEvent)>>,
  >,
} */

/* impl<'a> Default for TestConfig<'a> {
  fn default() -> Self {
    Self {
      llvm_parse: false,
      bytecode_parse: false,
      build_llvm: false,
      build_bytecode: true,
      print_states: false,
      build_ascript: false,
      print_llvm_ir: false,
      print_ascript: false,
      optimize: true,
      build_parse_states: false,
      assert_clean_reports: true,
      print_disassembly: false,
      grammar_path: None,
      journal: None,
      grammar_string: None,
      debugger_handler: None,
      print_parser_states_compile_reports: &[],
      num_of_threads: get_num_of_available_threads(),
    }
  }
}

pub struct TestInput<'a> {
  pub input:          &'a str,
  pub entry_name:     &'a str,
  pub should_succeed: bool,
}

impl<'a> From<&(&'a str, &'a str, bool)> for TestInput<'a> {
  fn from(
    (entry_name, input, should_parse): &(&'a str, &'a str, bool),
  ) -> Self {
    TestInput { entry_name, input, should_succeed: *should_parse }
  }
}

impl<'a> From<(&'a str, &'a str, bool)> for TestInput<'a> {
  fn from((entry_name, input, should_parse): (&'a str, &'a str, bool)) -> Self {
    TestInput { entry_name, input, should_succeed: should_parse }
  }
} */
/*
/// Runs an input grammar through a series of stages determined by
/// the TestConfig, and reports failure points when they occur.
///
/// # Arguments
///
/// * `parse_inputs` - A slice of TestInput structs that can be used
/// to evaluate generated parsers. A tuple of `(entry_name: &str, input: &str,
/// should_succeed:bool)` can be used instead of declaring TestInput structs.
pub fn test_runner<'a>(
  parse_inputs: &[TestInput],
  config: Option<Config>,
  test_cfg: TestConfig<'a>,
) -> SherpaResult<Journal> {
  let TestConfig {
    print_llvm_ir,
    build_bytecode,
    print_states,
    build_ascript,
    print_ascript,
    optimize,
    assert_clean_reports,
    print_disassembly,
    grammar_path,
    grammar_string,
    journal,
    debugger_handler,
    build_llvm,
    bytecode_parse,
    llvm_parse,
    num_of_threads,
    print_parser_states_compile_reports: report_parse_states,
    build_parse_states,
  } = test_cfg;

  let run_llvm_parser = llvm_parse;
  let run_bytecode_parser = bytecode_parse;
  let build_llvm_parser = build_llvm || run_llvm_parser;
  let build_bytecode = build_llvm_parser
    || run_bytecode_parser
    || print_disassembly
    || build_bytecode;
  let build_states = build_parse_states
    || build_bytecode
    || print_states
    || !report_parse_states.is_empty();

  let mut j = if let Some(j) = journal { j } else { Journal::new(config) };

  let g = if let Some(g) = j.grammar() {
    SherpaResult::Ok(g)
  } else if let Some(grammar_string) = grammar_string {
    if let Some(grammar_path) = grammar_path {
      GrammarStore::from_str_with_base_dir(
        &mut j,
        grammar_string,
        &grammar_path,
      )
    } else {
      GrammarStore::from_str(&mut j, grammar_string)
    }
  } else if let Some(grammar_path) = grammar_path {
    GrammarStore::from_path(&mut j, grammar_path)
  } else {
    return SherpaResult::Err(
      "
Neither test_cfg.grammar_string nor test_cfg.grammar_path are have been assigned a value.
Cannot create a GrammarStore without one of these values present. "
        .into(),
    );
  };

  assert_reports(assert_clean_reports, &mut j)?;

  let g = g?;

  if build_ascript {
    let store = AScriptStore::new(&mut j)?;
    let u = create_rust_writer_utils(&store);
    let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
    let w = write_rust_ast(w)?;

    assert_reports(assert_clean_reports, &mut j)?;

    if print_ascript {
      println!("{}", String::from_utf8(w.into_writer().into_output())?);
    }
  }

  if !build_states {
    return SherpaResult::Ok(j);
  }

  let states = compile_parse_states(&mut j, num_of_threads);

  print_parse_state_reports(report_parse_states, &mut j);

  assert_reports(assert_clean_reports, &mut j)?;

  let states = states?;

  let states = if optimize {
    optimize_parse_states(&mut j, states)
  } else {
    states.into_iter().collect()
  };

  if print_states {
    for state in &states {
      println!("{}", state.1.to_string())
    }
  }

  if !build_bytecode {
    return SherpaResult::Ok(j);
  }

  let bc = compile_bytecode(&mut j, &states)?;

  if print_disassembly {
    println!("{}", generate_disassembly(&bc, &mut j));
  }

  if !(run_bytecode_parser || build_llvm_parser) {
    return SherpaResult::Ok(j);
  }

  if run_bytecode_parser {
    for TestInput { input, entry_name, should_succeed } in parse_inputs {
      let Some(prod) = g.get_entry_prod_id_from_name(entry_name)?.into_prod(&g) else {
    return SherpaResult::Err(format!("could not locate production [{}]", entry_name).into())
  };
      let entry_point = *bc
        .state_name_to_offset
        .get(&g.get_entry_name_from_prod_id(&prod.id)?)?;
      let target_production_id = prod.bytecode_id?;
      let mut reader = UTF8StringReader::from_string(input);
      let mut parser = ByteCodeParser::<_, u32>::new(&mut reader, &bc.bytecode);
      let mut debugger = debugger_handler.and_then(|s| s(g.clone()));
      let result = parser.collect_shifts_and_skips(
        entry_point,
        target_production_id,
        &mut debugger,
      );
      resolve_shifts_and_skips(result, should_succeed, input, &g, "BYTECODE")?;
    }
  }
  #[cfg(feature = "llvm")]
  if build_llvm_parser {
    use super::test_reader::TestUTF8StringReader;
    use inkwell::context::Context;

    let ctx = Context::create();
    let mut jit_parser = JitParser::<_, u32, u32>::new(&mut j, states, &ctx)?;
    if assert_clean_reports {
      j.flush_reports();
      assert!(!j.debug_error_report());
    }

    if print_llvm_ir {
      jit_parser.print_code()
    }

    if run_llvm_parser {
      for TestInput { input, entry_name, should_succeed } in parse_inputs {
        let Some(prod) = g.get_entry_prod_id_from_name(entry_name)?.into_prod(&g) else {
        return SherpaResult::Err(format!("could not locate production [{}]", entry_name).into())
      };
        let entry_point = prod.export_id? as u32;
        let target_production_id = prod.bytecode_id?;
        let mut r = TestUTF8StringReader::new(input);
        let mut debugger = debugger_handler.and_then(|s| s(g.clone()));

        jit_parser.set_reader(&mut r);

        let result = jit_parser.collect_shifts_and_skips(
          entry_point,
          target_production_id,
          &mut debugger,
        );

        resolve_shifts_and_skips(
          result,
          should_succeed,
          input,
          &g,
          "LLVM-JIT",
        )?;
      }
    }
  }

  SherpaResult::Ok(j)
}

fn resolve_shifts_and_skips(
  result: ShiftsAndSkipsResult,
  should_parse: &bool,
  input: &str,
  g: &Arc<GrammarStore>,
  parse_type: &str,
) -> SherpaResult<()> {
  match result {
    ShiftsAndSkipsResult::Accepted { shifts, .. } => {
      if !should_parse {
        return SherpaResult::Err(
          format!("[{parse_type}] The input [ {} ] should have failed to parse", input).into(),
        );
      }
      dbg!(shifts);
    }
    ShiftsAndSkipsResult::IncorrectProduction { expected_prod_id, actual_prod_id, .. } => {
      return SherpaResult::Err(
        format!(
          "[{parse_type}] The resulting production [{}] does not match expected production [{}]",
          g.get_production_plain_name(g.bytecode_production_lookup.get(&actual_prod_id)?),
          g.get_production_plain_name(g.bytecode_production_lookup.get(&expected_prod_id)?)
        )
        .into(),
      )
    }
    ShiftsAndSkipsResult::FailedParse(err) => {
      if *should_parse {
        return SherpaResult::Err(
          format!("[{parse_type}] The input [{}] should have been parsed\n {}", input, err).into(),
        );
      }
    }
  }
  SherpaResult::Ok(())
}

fn print_parse_state_reports(report_parse_states: &[&str], j: &mut Journal) {
  j.flush_reports();
  let g = &(j.grammar().unwrap());
  if !report_parse_states.is_empty() {
    j.flush_reports();
    for name in report_parse_states {
      let mut printed = false;
      if let Some(prod_id) = g.get_production_id_by_name(name) {
        printed |=
          j.debug_print_reports(crate::ReportType::ProductionCompile(prod_id));
        printed |= j.debug_print_reports(
          crate::ReportType::TokenProductionCompile(prod_id),
        );
      }
      // Try building a symbol name
      let scanner_id = ScannerStateId::from_string(name, g);

      printed |=
        j.debug_print_reports(crate::ReportType::ScannerCompile(scanner_id));

      if !printed {
        println!(
          "[Warning] Failed to find parser compile reports for [{}]",
          name
        );
      }
    }
  }
}

fn assert_reports(
  assert_clean_reports: bool,
  j: &mut Journal,
) -> SherpaResult<()> {
  if assert_clean_reports {
    j.flush_reports();
    if let Some(reports) = j.get_faulty_reports() {
      return SherpaResult::Err((&reports[0]).into());
    }
  }

  SherpaResult::Ok(())
}

#[derive(Debug, Clone)]
pub struct PrintConfig {
  pub display_scanner_output: bool,
  pub display_input_data:     bool,
  pub display_instruction:    bool,
  pub display_state:          bool,
  pub input_window_size:      usize,
}

impl Default for PrintConfig {
  fn default() -> Self {
    Self {
      display_scanner_output: false,
      display_input_data:     true,
      display_instruction:    false,
      display_state:          true,
      input_window_size:      74,
    }
  }
}
 */
