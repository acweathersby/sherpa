use crate::{
  ascript::{
    output_base::AscriptWriter,
    rust::{
      create_rust_writer_utils,
      write_rust_ast,
      write_rust_bytecode_parser_file,
      write_rust_llvm_parser_file,
    },
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

<> main > "hello" "," "," "world" B

    :ast { t_Test, t:u32($2), d:$5 }

<> B > "test" "test" :ast [ str($1), str($2) ]
  "##,
  )?;

  let ascript = AScriptStore::new(j)?;
  let utils = create_rust_writer_utils(&ascript);
  let w = AscriptWriter::new(&utils, CodeWriter::new(vec![]));

  let states = compile_parse_states(j, get_num_of_available_threads())?;
  let states = optimize_parse_states(j, states);
  let BytecodeOutput { bytecode, state_name_to_offset, .. } =
    compile_bytecode(j, &states)?;

  let w = write_rust_bytecode_parser_file(w, &state_name_to_offset, &bytecode)?;
  let w = write_rust_ast(w)?;

  println!("{}", String::from_utf8(w.into_writer().into_output()).unwrap());

  SherpaResult::Ok(())
}

#[test]
fn construct_rust_llvm_parser_file() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let j = &mut j;
  GrammarStore::from_str(
    j,
    r##"

IGNORE { c:sp }

<> main > "hello" "," "," "world" B

    :ast { t_Test, t:u32($2), d:$5 }

<> B > "test" "test" :ast [ str($1), str($2) ]
  "##,
  )?;

  let ascript = AScriptStore::new(j)?;
  let utils = create_rust_writer_utils(&ascript);
  let w = AscriptWriter::new(&utils, CodeWriter::new(vec![]));

  let w = write_rust_llvm_parser_file(w, "test", "test")?;
  let w = write_rust_ast(w)?;

  println!("{}", String::from_utf8(w.into_writer().into_output()).unwrap());

  SherpaResult::Ok(())
}
