use crate::{
  ascript::types::AScriptStore,
  compile::{compile_bytecode, compile_states, optimize_ir_states, GrammarStore},
  debug::collect_shifts_and_skips,
  llvm::{compile_module_from_bytecode, construct_module},
  test::utils::path_from_source,
  util::get_num_of_available_threads,
  writer::code_writer::StringBuffer,
  Config,
  Journal,
  SherpaResult,
};
use inkwell::context::Context;

#[test]
fn test_compile_of_sherpa_grammar_bytecode() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar_path = path_from_source("grammar/v1_0_0/grammar.sg")?;

  let SherpaResult::Ok(g) = GrammarStore::from_path(&mut j, grammar_path) else {
    j.flush_reports();
    j.debug_error_report();
    return SherpaResult::None;
  };

  let states = compile_states(&mut j, get_num_of_available_threads());

  j.get_report(
    crate::ReportType::ProductionCompile(g.get_production_id_by_name("terminal_list_1")?),
    |r| {
      println!("{}", r.debug_string());
      false
    },
  );

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let ir_states = optimize_ir_states(&mut j, states?);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let bc = compile_bytecode(&mut j, ir_states);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let entry_state_name = &g.get_production_by_name("grammar").unwrap().guid_name;

  let entry_point = *bc.state_name_to_offset.get(entry_state_name).unwrap();

  let target_production_id = g.get_production_by_name("grammar").unwrap().bytecode_id;

  let (shifts, skips) = collect_shifts_and_skips(
    r#"
    NAME sherpa

    IMPORT ./script.hcg as ast 
    IMPORT ./symbol.hcg as sym
    
    IGNORE { c:sp c:nl }
    
    EXPORT grammar as grammar
    
    <> grammar > 
    
            preamble(*) ( production | append_production )(+)
    
                :ast { t_Grammar, c_Version_1_0, preamble:$1, productions:$2, tok }
    
    <> preamble >
    
            export_clause 
    
            | import_clause
    
            | name_clause
    
            | ignore_clause
    
    <> export_clause > 
    
            "EXPORT" sym::non_terminal (( "AS" | "as" ) sym::identifier)?
    
                :ast { t_Export, c_Preamble, production:$2, reference:$3 } 
    
    <> import_clause > 
    
            "IMPORT" ( g:id | g:sym  )(+)^test ( "AS" | "as" ) sym::identifier
    
                :ast { t_Import, c_Preamble, uri: str($2), reference:str($4), tok }
    
    <> ignore_clause >
    
            "IGNORE" "{"  ( sym::terminal | sym::class )(+) "}"
    
                :ast { t_Ignore, c_Preamble, symbols: $3 }
    
    <> name_clause >
    
            "NAME" sym::identifier
    
                :ast { t_Name, c_Preamble, name: str($2) }
    
    <> production > 
    
            "<"  (template_name)(*\, )^t \> \lazy?^l sym::priority?^p sym::non_terminal^n \> rules^r
    
                :ast { t_Production, is_lazy:bool($l), priority:$p, name:str($n), name_sym:$n, rules: $r, template_names:$t, tok }
    
    <> append_production > 
    
            "+>" sym::priority?^p sym::non_terminal^n \> rules^r
    
                :ast { t_Production, is_append: true, is_lazy:false, priority:$p, name:str($n), name_sym:$n, rules: $r, tok }
    
    <> template_name >  
    
        sym::identifier
    
               :ast str(tok)
    
    <> rules > 
    
            rule(+"|")
    
    <> rule > 
    
            \!?^p ( sym::annotated_symbol | any_group )(+)^s ast_definition?^a
    
                :ast { t_Rule, is_priority:bool($p), symbols:$s, ast_definition:$a, tok }
    
    <> ast_definition > 
    
            ":ast" ast::body^ast
    
                :ast  { t_Ascript, c_Function, ast:$ast, tok }
    
    
    +> sym::symbol > group
    
    +> sym::non_terminal > sym::non_terminal^p "<" sym::production_symbol^t ">"
    
            :ast { t_TemplateProductionSymbol, prod_sym:$p, template_productions:$t }
    
    
    <> group > 
    
            "(" rules ")"{1}
    
                :ast { t_Group_Production, c_Symbol, rules:$2,  tok }
    
    <> any_group > 
    
            \[ "unordered"? sym::annotated_symbol(+)^s \]
    
                :ast { t_AnyGroup, unordered: bool($2), symbols:$s, tok }
    
    
"#,
    entry_point,
    target_production_id,
    &bc.bytecode,
    None,
  )?;

  dbg!(shifts, skips);

  SherpaResult::Ok(())
}

/// Test component module wide compilation of the sherpa grammar.
#[test]
fn test_compile_of_sherpa_grammar_llvm() -> SherpaResult<()> {
  let grammar_path = path_from_source("grammar/v1_0_0/grammar.sg")?;

  dbg!(&grammar_path);

  let mut j = Journal::new(Some(Config { ..Default::default() }));

  let SherpaResult::Ok(g) = GrammarStore::from_path(&mut j, grammar_path) else {
    assert!(!j.debug_error_report());
    return SherpaResult::None;
  };

  assert!(!j.debug_error_report());

  // Compile parse from grammar

  let states = compile_states(&mut j, get_num_of_available_threads())?;

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let ir_states = optimize_ir_states(&mut j, states);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  let bc = compile_bytecode(&mut j, ir_states);

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  // Build Ascript data

  let ascript = AScriptStore::new(g.clone())?;

  let mut writer = StringBuffer::new(vec![]);

  crate::ascript::rust::write(&ascript, &mut writer)?;

  //eprintln!("{}", String::from_utf8(writer.into_output())?);

  //eprintln!("{}", generate_disassembly(&bc, Some(&mut j)));

  // Build LLVM Data

  let ctx = Context::create();

  let mut module = construct_module(&mut j, "test", &ctx);
  compile_module_from_bytecode(&mut module, &mut j, &bc)?;

  if j.debug_error_report() {
    return SherpaResult::None;
  }

  // Perform a parsing pass on some simple input.

  let (.., shifts, _) = collect_shifts_and_skips(
    r##"<> t > "t" :ast { $2 + $3 + tok<1,2> } <> B > [unordered "d" "b" "c" ] "##,
    *bc.state_name_to_offset.get(g.get_exported_productions()[0].guid_name)?,
    g.get_exported_productions()[0].production.bytecode_id,
    &bc.bytecode,
    None,
  )?;

  dbg!(shifts);

  SherpaResult::Ok(())
}
