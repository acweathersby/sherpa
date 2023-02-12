use crate::{
  ascript::{rust_2, types::AScriptStore},
  bytecode::compile_bytecode,
  compile::{optimize_ir_states, GrammarStore},
  debug::{
    collect_shifts_and_skips,
    disassemble_state,
    generate_disassembly,
    BytecodeGrammarLookups,
  },
  intermediate::algorithm_2::{self},
  journal::{config::Config, Journal},
  types::*,
  writer::code_writer::StringBuffer,
  SherpaResult,
};
use inkwell::context::Context;
use sherpa_runtime::functions::DebugEvent;
use std::{path::PathBuf, str::FromStr, sync::Arc};

use super::test_reader::TestUTF8StringReader;

#[cfg(test)]
/// Return the full filepath of a grammar stored in
/// ./tests/grammars/
pub(super) fn get_test_grammar_path(partial_path: &str) -> PathBuf {
  let path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../test/grammars/").join(partial_path);
  dbg!(&path);
  path.canonicalize().unwrap()
}

/// Creates and compiles a Journal and GrammarStore from a file path and configuration object.
pub(super) fn build_grammar_from_file(
  input: PathBuf,
  config: Config,
) -> (Journal, SherpaResult<Arc<GrammarStore>>) {
  let mut j = Journal::new(Some(config));
  let g = GrammarStore::from_path(&mut j, input);
  (j, g)
}

/// Return a path relative to `<sherpa_repo>/source/`
pub(crate) fn path_from_source(source_path: &str) -> SherpaResult<PathBuf> {
  let grammar_path = std::env::var("CARGO_MANIFEST_DIR")
    .map(|val| PathBuf::from_str(&val).unwrap().join("../../"))
    .unwrap_or_else(|_| {
      std::env::current_dir().unwrap().join(PathBuf::from_str("./source/").unwrap())
    })
    .join(source_path);

  if let Ok(grammar_path) = grammar_path.canonicalize() {
    SherpaResult::Ok(grammar_path)
  } else {
    panic!("File path {} not found in file system", grammar_path.to_str().unwrap());
  }
}

#[derive(Clone)]
pub struct TestConfig<'a> {
  pub llvm_parse: bool,
  pub bytecode_parse: bool,
  pub print_llvm_ir: bool,
  pub build_llvm: bool,
  pub build_bytecode: bool,
  pub print_states: bool,
  pub build_ascript: bool,
  pub print_ascript: bool,
  pub optimize: bool,
  pub assert_clean_reports: bool,
  pub print_disassembly: bool,
  /// If `grammar_string` is assigned a value, then this is treated
  /// as a base dir path for file imports declared in the grammar.
  ///
  /// Other wise, this is expected to be an absolute file path to
  /// a grammar file.
  pub grammar_path: Option<PathBuf>,
  /// A grammar declaration string.
  pub grammar_string: Option<&'static str>,

  pub bytecode_parse_reporter:
    Option<&'a dyn Fn(Arc<GrammarStore>) -> Option<Box<dyn Fn(DebugEvent<UTF8StringReader, u32>)>>>,
}

impl<'a> Default for TestConfig<'a> {
  fn default() -> Self {
    Self {
      llvm_parse: false,
      bytecode_parse: false,
      build_llvm: false,
      build_bytecode: false,
      print_states: false,
      build_ascript: false,
      print_llvm_ir: false,
      print_ascript: false,
      optimize: true,
      assert_clean_reports: true,
      print_disassembly: false,
      grammar_path: None,
      grammar_string: None,
      bytecode_parse_reporter: None,
    }
  }
}

pub fn test_runner<'a>(
  input_str: &str,
  entry_name: &str,
  config: Option<Config>,
  test_cfg: TestConfig<'a>,
) -> SherpaResult<Journal> {
  let run_llvm_parser = test_cfg.llvm_parse;
  let run_bytecode_parser = test_cfg.bytecode_parse;
  let build_llvm_parser = test_cfg.build_llvm || run_llvm_parser;
  let build_bytecode = build_llvm_parser || run_bytecode_parser;
  let build_states = build_bytecode;
  let mut j = Journal::new(config);

  let g = if let Some(grammar_string) = test_cfg.grammar_string {
    if let Some(grammar_path) = test_cfg.grammar_path {
      GrammarStore::from_str_with_base_dir(&mut j, grammar_string, &grammar_path)
    } else {
      GrammarStore::from_str(&mut j, grammar_string)
    }
  } else if let Some(grammar_path) = test_cfg.grammar_path {
    GrammarStore::from_path(&mut j, grammar_path)
  } else {
    return SherpaResult::Err(
      "
Neither test_cfg.grammar_string nor test_cfg.grammar_path are have been assigned a value. 
Cannot create a GrammarStore without one of these values present. "
        .into(),
    );
  };

  if test_cfg.assert_clean_reports {
    j.flush_reports();
    assert!(!j.debug_error_report(), "Errors encountered while creating grammar");
  }

  let g = g?;

  if test_cfg.build_ascript {
    let writer = rust_2::build_rust(&AScriptStore::new(g.clone())?, StringBuffer::new(vec![]))?;

    if test_cfg.print_ascript {
      eprintln!("{}", String::from_utf8(writer.into_output())?);
    }
  }

  if !build_states {
    return SherpaResult::Ok(j);
  }

  let states = algorithm_2::compile::compile_states(&mut j, 1)?;

  if test_cfg.assert_clean_reports {
    j.flush_reports();
    assert!(!j.debug_error_report());
  }

  let states = if test_cfg.optimize {
    optimize_ir_states(&mut j, states)
  } else {
    states.into_iter().collect()
  };

  if test_cfg.print_states {
    for state in &states {
      println!("{}", state.1.to_string())
    }
  }

  if !build_bytecode {
    return SherpaResult::Ok(j);
  }

  let bc = compile_bytecode(&mut j, states);

  if test_cfg.print_disassembly {
    eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));
  }
  let prod = g.get_production_by_name(entry_name).unwrap();
  let entry_point = *bc.state_name_to_offset.get(&prod.guid_name)?;
  let target_production_id = prod.bytecode_id;

  if run_bytecode_parser {
    let mut reader = UTF8StringReader::from_string(input_str);
    let mut parser = ByteCodeParser::new(&mut reader, &bc.bytecode);
    let results = parser.collect_shifts_and_skips(
      entry_point,
      target_production_id,
      &test_cfg.bytecode_parse_reporter.and_then(|s| s(g.clone())),
    )?;

    dbg!(results);
  }

  if build_llvm_parser {
    let ctx = Context::create();
    let mut r = TestUTF8StringReader::new(input_str);

    let mut jit_parser = JitParser::<TestUTF8StringReader, u32, u32>::new(&mut j, &bc, &ctx)?;

    jit_parser.set_reader(&mut r);

    if test_cfg.assert_clean_reports {
      j.flush_reports();
      assert!(!j.debug_error_report());
    }

    if test_cfg.print_llvm_ir {
      jit_parser.print_code()
    }
    if run_llvm_parser {
      let shifts_and_skips =
        jit_parser.collect_shifts_and_skips(entry_point, target_production_id, &None)?;

      dbg!(shifts_and_skips);
    }
  }

  SherpaResult::Ok(j)
}

pub fn debug_print_states<'a>(
  g: Arc<GrammarStore>,
) -> Box<dyn Fn(DebugEvent<UTF8StringReader, u32>)> {
  let grammar_pack = BytecodeGrammarLookups::new(g);
  Box::new(move |event| match event {
    DebugEvent::ExecuteState { ctx, address, bc, .. } => {
      if !ctx.is_scanner() || true {
        println!(
          "\n[{} : \"{}\" : \"{}\"] tk: {} {}",
          ctx.token_off,
          &ctx.get_str()[ctx.token_off as usize..(ctx.token_len + ctx.token_off) as usize],
          &ctx.get_str()[ctx.token_off as usize..],
          ctx.tok_id,
          disassemble_state(bc, address, Some(&grammar_pack)).0
        )
      }
    }
    _ => {}
  })
}
