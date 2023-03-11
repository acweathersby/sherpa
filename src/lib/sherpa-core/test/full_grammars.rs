use crate::{
  bytecode::compile_bytecode,
  compile::{GrammarStore, ScannerStateId},
  debug::generate_disassembly,
  parser::{compile_parse_states, optimize_parse_states},
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
IGNORE { c:sp c:nl }
EXPORT json as entry
NAME llvm_language_test

<> json > 
        object                              :ast { t_Json, v: $1 }
        | 
        array                               :ast { t_Json, v: $1 }

<> array > '['  value(*',' )  ']'              :ast { t_Array, entries: $2 }

<> object > '{' key_value(*',' ) '}'           :ast { t_Object, entries: $2 }

<> key_value > str ':' value              :ast { t_KeyVal, k:$1, v:$3 }

<> value > num | bool | str | null

<> null > "null"                            :ast { t_Null, v:false }

<> bool > 
    "false"                                 :ast { t_Bool, v:false }
    |   
    "true"                                  :ast { t_Bool, v:true }

<> num > tk:number                          :ast { t_Number }

<> number > ( '+' | '-' )? c:num(+) ( '.' c:num(+) )? ( ( 'e' | 'E' ) ( '+' | 'i' ) c:num(+) )?

<> str > tk:string :ast { t_String }

<> string > '"' ( c:id | c:sym | c:num | c:sp )(*) "\""
"##,
  )
  .unwrap();

  let states = compile_parse_states(&mut j, 1)?;

  let ir_states = optimize_parse_states(&mut j, states);

  j.debug_print_reports(ReportType::ScannerCompile(ScannerStateId::default()));

  let bc = compile_bytecode(&mut j, &ir_states)?;

  println!("{}", generate_disassembly(&bc, &mut j));

  SherpaResult::Ok(())
}
