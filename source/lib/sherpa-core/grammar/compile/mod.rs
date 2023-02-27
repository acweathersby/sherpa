pub(crate) mod create_store;
pub mod finalize;
pub mod merge;
pub mod parse;
pub mod parser;
use sherpa_runtime::types::ast::AstObject;

use self::{
  finalize::finalize_grammar,
  merge::merge_grammars,
  parser::sherpa::{self},
};
use crate::{
  grammar::compile::create_store::create_store,
  types::*,
  util::get_num_of_available_threads,
  Journal,
};
use std::{path::PathBuf, sync::Arc};

/// Used to calculate the max size of a
/// AST node
#[derive(Debug, Clone)]
#[repr(C, u32)]
pub(crate) enum DummyASTEnum {
  _None,
  _U64(u64),
  _NODE(Box<u64>),
  _VEC(Vec<u64>),
  _STRING(String),
  _TOKEN(Token),
}

impl Default for DummyASTEnum {
  fn default() -> Self {
    DummyASTEnum::_None
  }
}

impl AstObject for DummyASTEnum {}

pub(crate) fn compile_ir_ast(buffer: &str) -> SherpaResult<sherpa::IR_STATE> {
  SherpaResult::Ok(*(sherpa::ast::ir_from(buffer.into())?))
}

#[allow(dead_code)]
pub(crate) fn compile_ascript_struct(buffer: &str) -> SherpaResult<sherpa::AST_Struct> {
  SherpaResult::Ok(*(sherpa::ast::ast_struct_from(buffer.into())?))
}

#[allow(dead_code)]
pub(crate) fn compile_grammar_ast(buffer: &str) -> SherpaResult<sherpa::Grammar> {
  SherpaResult::Ok(*(sherpa::ast::grammar_from(buffer.into())?))
}

pub(crate) fn compile_grammars(
  j: &mut Journal,
  grammars: &Vec<(PathBuf, ImportedGrammarReferences, Box<sherpa::Grammar>)>,
) -> SherpaResult<()> {
  if grammars.is_empty() {
    j.report_mut().add_error("No grammars were generated.".into());
    SherpaResult::None
  } else {
    let results = std::thread::scope(|s| {
      grammars
        .chunks(
          (grammars.len() as f64 / get_num_of_available_threads() as f64).ceil().max(1.0) as usize
        )
        .into_iter()
        .map(|chunk| {
          let mut j = j.transfer();
          s.spawn(move || {
            chunk
              .iter()
              .map(|(absolute_path, import_refs, grammar)| {
                let grammar =
                  create_store(&mut j, &grammar, absolute_path.clone(), import_refs.clone());
                grammar
              })
              .collect::<Vec<_>>()
          })
        })
        .map(|s| s.join().unwrap())
        .collect::<Vec<_>>()
    });
    j.flush_reports();

    if j.have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    let mut grammars: Vec<Arc<GrammarStore>> = results.into_iter().flatten().collect();

    let rest = grammars.drain(1..).collect::<Vec<_>>();

    let mut grammar = Arc::try_unwrap(grammars.pop().unwrap()).unwrap();

    merge_grammars(j, &mut grammar, &rest);

    if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    let grammar = finalize_grammar(j, grammar);

    if j.report().have_errors_of_type(SherpaErrorSeverity::Critical) {
      return SherpaResult::None;
    }

    j.set_grammar(Arc::new(grammar));

    SherpaResult::Ok(())
  }
}
