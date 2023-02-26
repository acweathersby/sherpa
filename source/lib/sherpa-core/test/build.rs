use crate::{
  ascript::{
    output_base::AscriptWriter,
    rust::{create_rust_writer_utils, write_rust_ast, write_rust_bytecode_parser_file},
    types::AScriptStore,
  },
  bytecode::{compile_bytecode, BytecodeOutput},
  compile::GrammarStore,
  parser::{compile_parse_states, optimize_parse_states},
  util::get_num_of_available_threads,
  writer::code_writer::CodeWriter,
  Journal,
  SherpaResult,
};

#[test]
fn construct_rust_bytecode_parser_file() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let j = &mut j;
  GrammarStore::from_str(
    j,
    r##"

IGNORE { c:sp }

<> main > "hello" "," "world" 
  "##,
  )?;

  let ascript = AScriptStore::new(j)?;
  let utils = create_rust_writer_utils(&ascript);
  let writer = AscriptWriter::new(&utils, CodeWriter::new(vec![]));

  let states = compile_parse_states(j, get_num_of_available_threads())?;
  let states = optimize_parse_states(j, states);
  let BytecodeOutput { bytecode, state_name_to_offset, .. } = compile_bytecode(j, &states)?;

  let w = write_rust_bytecode_parser_file(writer, &state_name_to_offset, &bytecode)?;
  //let w = write_rust_ast(w)?;

  println!("{}", String::from_utf8(w.into_writer().into_output()).unwrap());

  SherpaResult::Ok(())
}
