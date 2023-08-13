#![feature(box_patterns)]
#![feature(drain_filter)]
#![feature(btree_drain_filter)]

#[cfg(test)]
mod test;

mod builder;

use builder::{
  create_rust_writer_utils,
  write_rust_ast,
  write_rust_ast2,
  write_rust_bytecode_parser_file,
};
use sherpa_ascript::{output_base::AscriptWriter, types::AScriptStore};
use sherpa_bytecode::compile_bytecode;
use sherpa_core::{
  compile_parse_states,
  optimize,
  CodeWriter,
  Journal,
  ParserDatabase,
  SherpaResult,
};

pub fn build_rust(mut j: Journal, db: &ParserDatabase) -> SherpaResult<String> {
  j.set_active_report("Rust AST Compile", sherpa_core::ReportType::Any);

  let store = AScriptStore::new(&mut j, &db)?;
  let u = create_rust_writer_utils(&store, &db);
  let w = AscriptWriter::new(&u, CodeWriter::new(vec![]));
  let writer = write_rust_ast2(w)?;

  String::from_utf8(writer.into_writer().into_output()).into()
}

pub async fn compile_rust_bytecode_parser(
  j: &mut Journal,
  db: &ParserDatabase,
) -> SherpaResult<String> {
  let j1 = j.transfer();
  let mut j2 = j.transfer();

  let a = async move {
    let states = compile_parse_states(j1, &db)?;

    let states = optimize::<Vec<_>>(&db, states)?;

    compile_bytecode(&db, states.iter())
  };

  let store = async move { AScriptStore::new(&mut j2, &db) };

  let (bytecode, state_lookups) = a.await?;
  let mut store: AScriptStore = store.await?;

  j.flush_reports();

  assert!(!j.have_errors_of_type(sherpa_core::SherpaErrorSeverity::Critical));

  let state_lookups = state_lookups
    .into_iter()
    .map(|(name, offset)| (name.to_string(db.string_store()), offset as u32))
    .collect();

  dbg!(&state_lookups);

  let u = create_rust_writer_utils(&store, &db);
  let writer = write_rust_ast(AscriptWriter::new(&u, CodeWriter::new(vec![])))?;
  let w = write_rust_bytecode_parser_file(writer, &state_lookups, &bytecode)?;

  SherpaResult::Ok(w.into_writer().to_string())
}
