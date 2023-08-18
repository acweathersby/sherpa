#![feature(box_patterns)]
#![feature(drain_filter)]
#![feature(btree_drain_filter)]

#[cfg(test)]
mod test;

mod builder;

use builder::{
  add_ascript_functions_for_rust,
  create_rust_writer_utils,
  write_rust_ast,
  write_rust_ast2,
  write_rust_bytecode_parser_file,
};
use sherpa_ascript::{output_base::AscriptWriter, types::AScriptStore};

use sherpa_core::{
  proxy::Map,
  CodeWriter,
  IString,
  Journal,
  ParserDatabase,
  SherpaResult,
};

use crate::builder::write_rust_llvm_parser_file;

pub fn build_rust(mut j: Journal, db: &ParserDatabase) -> SherpaResult<String> {
  j.set_active_report("Rust AST Compile", sherpa_core::ReportType::Any);

  let store = AScriptStore::new(j.transfer(), &db)?;
  let u = create_rust_writer_utils(&store, &db);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));

  let writer = write_rust_ast2(w)?;

  String::from_utf8(writer.into_writer().into_output()).into()
}

pub async fn compile_rust_bytecode_parser(
  mut j: Journal,
  db: &ParserDatabase,
  bytecode: &Vec<u8>,
  state_lookups: &Map<IString, usize>,
) -> SherpaResult<String> {
  let store = async { AScriptStore::new(j.transfer(), &db) };

  let store: AScriptStore = store.await?;

  j.flush_reports();

  assert!(!j.have_errors_of_type(sherpa_core::SherpaErrorSeverity::Critical));

  let state_lookups =
    state_lookups.into_iter().map(|(name, offset)| (name.to_string(db.string_store()), *offset as u32)).collect();

  let u = create_rust_writer_utils(&store, &db);

  let mut writer = AscriptWriter::new(&u, CodeWriter::new(vec![]));

  writer.stmt(
    r###"/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta2
/// - **SOURCE**: /home/work/projects/lib_sherpa/src/grammar/v2_0_0/grammar.sg
///
/// #### WARNING:
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### License:
/// Copyright (c) 2023 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE.

// 
use std::hash::Hash;
use sherpa_rust_runtime::{
  llvm_parser::*,
  types::{ast::*, *},
};
"###
      .to_string(),
  )?;

  add_ascript_functions_for_rust(&mut writer, db)?;

  let writer = write_rust_ast(writer)?;

  let writer = write_rust_bytecode_parser_file(writer, &state_lookups, bytecode)?;

  SherpaResult::Ok(writer.into_writer().to_string())
}

pub async fn compile_rust_llvm_parser(
  mut j: Journal,
  db: &ParserDatabase,
  grammar_name: &str,
  parser_name: &str,
) -> SherpaResult<String> {
  let j2 = j.transfer();

  let store = async move { AScriptStore::new(j2, &db) };

  let store: AScriptStore = store.await?;

  j.flush_reports();

  assert!(!j.have_errors_of_type(sherpa_core::SherpaErrorSeverity::Critical));

  let u = create_rust_writer_utils(&store, &db);

  let mut writer = AscriptWriter::new(&u, CodeWriter::new(vec![]));

  writer.stmt(
    r###"/// ### `sherpa` Rust Parser
///
/// - **GENERATOR**: sherpa 1.0.0-beta2
/// - **SOURCE**: /home/work/projects/lib_sherpa/src/grammar/v2_0_0/grammar.sg
///
/// #### WARNING:
///
/// This is a generated file. Any changes to this file may be **overwritten
/// without notice**.
///
/// #### License:
/// Copyright (c) 2023 Anthony Weathersby
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to
/// deal in the Software without restriction, including without limitation the
/// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
/// sell copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in
/// all copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
/// IN THE SOFTWARE.

// 
use std::hash::Hash;
use sherpa_rust_runtime::{
  llvm_parser::*,
  types::{ast::*, *},
};
"###
      .to_string(),
  )?;

  add_ascript_functions_for_rust(&mut writer, db)?;

  let writer = write_rust_ast(writer)?;

  let writer = write_rust_llvm_parser_file(writer, grammar_name, parser_name)?;

  SherpaResult::Ok(writer.into_writer().to_string())
}
