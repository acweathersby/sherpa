use crate::{
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore, ScannerStateId},
  debug::{collect_shifts_and_skips, generate_disassembly},
  Journal,
  SherpaResult,
};

#[test]
fn escaped_string() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
        <> string > tk:string_tk
        
        <> string_tk > \" ( g:sym | g:num | g:sp | g:id | escape )(*) t:"
        
        <> escape > t:\  ( g:sym | g:num | g:sp | g:id )
        "##,
    &[(r##""""##, true), (r##""1234""##, true), (r##""12\"34""##, true)],
  )
}

#[test]
fn scientific_number() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
    <> sci_number > tk:number
    
    <> number > ( \+ | \- )? g:num(+) ( t:. g:num(+) )? ( ( \e | \E ) ( \+ | \- )? g:num(+) )?
    "##,
    &[(r##"2.3e-22"##, true), (r##"0.3e-22"##, true)],
  )
}

#[test]
fn json_object_with_specialized_key() -> SherpaResult<()> {
  compile_and_run_grammar(
    r##"
    @IGNORE g:sp g:nl
    
    <> json > \{  value(*,) \}

    <> value > tk:string \: tk:string

        | t:"test" \: g:num

    <> string > \" ( g:sym | g:num | g:sp | g:id | escape )(*) t:"
        
    <> escape > t:\  ( g:sym | g:num | g:sp | g:id )
    "##,
    &[
      (r##"{ "test" : 2  }"##, true),
      (r##"{ "tester" : "mango"  }"##, true),
      (r##"{ "tester" : 2  }"##, false),
      (r##"{ "test" : "mango"  }"##, false),
    ],
  )
}

fn compile_and_run_grammar(grammar: &str, test_inputs: &[(&str, bool)]) -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, grammar).unwrap();

  let states = compile_states(&mut j, 1)?;

  //j.debug_print_reports(crate::ReportType::ScannerCompile(ScannerStateId::default()));

  let ir_states = optimize_ir_states(&mut j, states);

  let bc = compile_bytecode(&mut j, ir_states);

  // eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  for (input, should_complete) in test_inputs {
    match collect_shifts_and_skips(
      input,
      *bc.state_name_to_offset.get(g.get_exported_productions()[0].guid_name)?,
      g.get_exported_productions()[0].production.bytecode_id,
      &bc.bytecode,
    ) {
      SherpaResult::Ok((shifts, _)) => {
        if !should_complete {
          return SherpaResult::Err(
            format!("The input [ {} ] should have failed to parse", input).into(),
          );
        }
        dbg!(shifts);
      }
      _ => {
        if *should_complete {
          return SherpaResult::Err(
            format!("The input [{}] should have been parsed", input).into(),
          );
        }
      }
    };
  }

  SherpaResult::Ok(())
}
