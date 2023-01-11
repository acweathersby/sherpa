use crate::{
  compile::{
    compile_bytecode,
    compile_production_states,
    compile_states,
    optimize_ir_states,
    GrammarStore,
    ScannerStateId,
  },
  debug::generate_disassembly,
  types::IRState,
  Config,
  Journal,
  ReportType,
  SherpaResult,
};

#[test]
fn test_compile_json_parser() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  GrammarStore::from_str(
    &mut j,
    r##"
    @IGNORE g:sp g:nl

    @EXPORT json as entry

    @NAME llvm_language_test

    <> json > 
            object                              f:ast { { t_Json, v: $1 } }
            | 
            array                               f:ast { { t_Json, v: $1 } }

    <> array > \[  value(*\, )  \]              f:ast { { t_Array, entries: $2 } }

    <> object > \{ key_value(*\, ) \}           f:ast { { t_Object, entries: $2 } }

    <> key_value > str \: value              f:ast { { t_KeyVal, k:$1, v:$3 } }

    <> value > num | bool | str | null

    <> null > t:null                            f:ast { { t_Null, v:false } }

    <> bool > 
        t:false                                 f:ast { { t_Bool, v:false } }
        |   
        t:true                                  f:ast { { t_Bool, v:true } }

    <> num > tk:number                          f:ast { { t_Number } }

    <> number > ( \+ | \- )? g:num(+) ( \. g:num(+) )? ( ( \e | \E ) ( \+ | \i ) g:num(+) )?

    <> str > tk:string f:ast { { t_String } }

    <> string > \" ( g:id | g:sym | g:num | g:sp )(*) t:"  
"##,
  )
  .unwrap();

  let states = compile_states(&mut j, 1)?;

  /* for (_, state) in &states {
    eprintln!("{}", state.get_code());
  } */
  //return SherpaResult::Ok(());

  let ir_states = optimize_ir_states(&mut j, states);

  j.debug_print_reports(ReportType::ScannerCompile(ScannerStateId::default()));

  let bc = compile_bytecode(&mut j, ir_states);

  eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  SherpaResult::Ok(())
}
