#[cfg(test)]
mod test_parser_build {
  use std::{io::Write, path::PathBuf};

  use crate::{
    compile::{
      compile_bytecode,
      compile_states,
      compile_token_production_states,
      optimize_ir_states,
    },
    debug::generate_disassembly,
    GrammarStore,
    Journal,
    SherpaResult,
  };

  #[test]
  fn build() -> SherpaResult<()> {
    let input_file = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/e2e/bootstrap/grammar/grammar.hcg")
      .canonicalize()
      .unwrap();

    let mut output_file = input_file.clone();
    output_file.pop();
    output_file.push("grammar.sherpa.bytecode");

    let mut j = Journal::new(None);

    GrammarStore::from_path(&mut j, input_file).unwrap();

    if let SherpaResult::Ok(states) = compile_states(&mut j, 10) {
      let pre_opt_length = states.len();
      //
      let mut states = optimize_ir_states(&mut j, states);
      let post_opt_length = states.len();
      //
      let output = compile_bytecode(&mut j, states);

      let dissasembly = generate_disassembly(&output, Some(&mut j));

      println!("{}", dissasembly);

      if let Ok(mut parser_data_file) = std::fs::File::create(&output_file) {
        parser_data_file.write_all(&dissasembly.as_bytes()).unwrap();
        parser_data_file.flush().unwrap();
      }
    } else {
      j.flush_reports();
      j.debug_error_report();
    }

    SherpaResult::Ok(())
  }
}
