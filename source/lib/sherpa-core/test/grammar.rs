use crate::{
  compile::{compile_bytecode, compile_scanner_states, compile_states, optimize_ir_states},
  debug::{collect_shifts_and_skips, debug_items, generate_disassembly},
  grammar::{
    compile::{
      compile_ascript_struct,
      compile_grammar_ast,
      compile_grammars,
      compile_ir_ast,
      finalize::convert_left_recursion_to_right,
      parse::{load_from_path, load_from_string, resolve_grammar_path},
    },
    get_production_start_items,
  },
  journal::Journal,
  types::{RecursionType, *},
  util::get_num_of_available_threads,
};
use lazy_static::__Deref;
use sherpa_runtime::functions::DebugEvent;
use std::{collections::BTreeSet, path::PathBuf};

#[test]
fn test_load_all() {
  let mut j = Journal::new(None);

  GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.sg")
      .canonicalize()
      .unwrap(),
  );

  assert!(!j.debug_error_report());

  let mut j = Journal::new(None);

  GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/invalid_load.sg")
      .canonicalize()
      .unwrap(),
  );
  assert!(j.debug_error_report());
}

#[test]
fn test_load_grammar_from_non_existing_directory() {
  let mut j = Journal::new(None);
  GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test_grammars/load.sg"),
  );
  assert!(j.debug_error_report());
}

#[test]
fn test_resolve_cargo_file() {
  let result = resolve_grammar_path(&Default::default(), &Default::default(), &[]);

  assert!(result.is_faulty());

  let result = resolve_grammar_path(
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml"),
    &Default::default(),
    &[],
  );

  assert!(result.is_ok());

  let result = resolve_grammar_path(
    &PathBuf::from("./Cargo.toml"),
    &PathBuf::from(env!("CARGO_MANIFEST_DIR")),
    &[],
  );

  assert!(result.is_ok());

  let result =
    resolve_grammar_path(&PathBuf::from("./Cargo"), &PathBuf::from(env!("CARGO_MANIFEST_DIR")), &[
      "toml",
    ]);

  assert!(result.is_ok());
}

#[test]
fn test_ir_trivial_state() {
  let result = compile_ir_ast("state [ A ] \n pass");

  print!("{:#?}", result);

  assert!(result.is_ok());
}

#[test]
fn test_ir_goto_state() {
  let result = compile_ir_ast(
    "state [ A ] shift then goto state [ test ] then goto state [ test ] then repeat state",
  );

  print!("{:#?}", result);

  assert!(result.is_ok());
}

#[test]

fn test_ir_trivial_branch_state() {
  let result = compile_ir_ast("state [ A ] assert peek TOKEN [ 1 ] ( pass )");

  print!("{:#?}", result);

  assert!(result.is_ok());
}

#[test]

fn test_trivial_ascript_struct() {
  let result = compile_ascript_struct(" { t_Type, t_Class, value: u32 } ");

  print!("{:#?}", result);

  assert!(result.is_ok());
}

#[test]

fn test_production_minimum() {
  let result = compile_grammar_ast("\n<> a > b\n");

  assert!(result.is_ok());
}

#[test]

fn test_production_with_generic_symbol() {
  let result = compile_grammar_ast("\n<> a > g:sp\n");

  assert!(result.is_ok());
}

#[test]
fn test_merge_productions_file() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

  path.push("../../../test/grammars/merge_root.sg");

  let g = GrammarStore::from_path(&mut j, path);
  assert!(!j.debug_error_report());
  g?;

  SherpaResult::Ok(())
}

#[test]
fn test_trivial_file_compilation() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

  path.push("../../../test/grammars/trivial.sg");

  let g = GrammarStore::from_path(&mut j, path);
  assert!(!j.debug_error_report());
  g?;

  SherpaResult::Ok(())
}

#[test]
fn test_trivial_file_compilation_with_single_import() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

  path.push("../../../test/grammars/trivial_importer.sg");

  let g = GrammarStore::from_path(&mut j, path);
  assert!(!j.debug_error_report());
  g?;

  SherpaResult::Ok(())
}

#[test]
fn conversion_of_left_to_right_recursive() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("../../../test/grammars/left_recursive_token_production.sg");
  let g = GrammarStore::from_path(&mut j, path);
  assert!(!j.debug_error_report());
  let g = g?;

  let left_recursive_prod = g.get_production_by_name("tk:left_recursive").unwrap();

  assert!(!left_recursive_prod
    .recursion_type
    .contains(RecursionType::LEFT_INDIRECT | RecursionType::LEFT_DIRECT));

  SherpaResult::Ok(())
}

#[test]
fn left_to_right_recursive_conversion() -> SherpaResult<()> {
  let mut j = Journal::new(None);

  let g = GrammarStore::from_str(&mut j, "<> B > tk:A  <> A > A 't' 'y' | A 'u' | 'CCC' | 'R' A ");

  assert!(!j.debug_error_report());

  let g = g?;

  let prod = g.get_production_id_by_name("A").unwrap();

  assert!(g.get_production_recursion_type(prod).contains(RecursionType::LEFT_DIRECT));

  let mut g2 = g.deref().clone();

  convert_left_recursion_to_right(&mut g2, prod);

  assert!(g2.get_production_recursion_type(prod,).contains(RecursionType::RIGHT));
  assert!(!g2.get_production_recursion_type(prod).contains(RecursionType::LEFT_DIRECT));

  SherpaResult::Ok(())
}

#[test]
fn processing_of_any_groups() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<> A > [ unordered 'g' ? ( 'r' 'l' ) ? ] '\\\\' 'd'");
  assert!(!j.debug_error_report());
  let g = g?;
  let prod = g.get_production_id_by_name("A").unwrap();
  debug_items("A", get_production_start_items(&prod, &g), &g);
  SherpaResult::Ok(())
}

#[test]
fn test_compile_grammars_into_store() {
  let mut j = Journal::new(None);

  let g = GrammarStore::from_path(
    &mut j,
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../../../test/grammars/load.sg")
      .canonicalize()
      .unwrap(),
  );

  assert!(!j.debug_error_report());
  assert!(g.is_ok());

  match g {
    SherpaResult::Ok(grammar) => {
      dbg!(grammar);
    }
    _ => {}
  }
}

#[test]
fn test_get_default_production() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
EXPORT start as test

<> start > 'hello' 'and' end

<> end > 'goodby'
",
  );

  assert!(!j.debug_error_report());
  assert!(g.is_ok());

  let g = g?;

  let exported_productions = g.get_exported_productions();
  let first = exported_productions.first()?;

  assert_eq!(first.production.name, "start");

  assert_eq!(first.export_name, "test");
  SherpaResult::Ok(())
}

#[test]

fn test_get_production_plain_name() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(&mut j, "<>billofolious_tantimum>'o'").unwrap();

  let prod = g.get_production_id_by_name("billofolious_tantimum").unwrap();

  assert_eq!(g.get_production_plain_name(&prod), "billofolious_tantimum");

  assert_ne!(g.get_production(&prod).unwrap().guid_name, "billofolious_tantimum");
}

#[test]

fn test_get_production_by_name() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
      <> Apple > 'o'
      <> Bad_Cakes > 'b'
      ",
  )
  .unwrap();

  assert!(g.get_production_id_by_name("Apple").is_some());

  assert!(g.get_production_id_by_name("Bad_Cakes").is_some());

  assert!(g.get_production_id_by_name("Bandible").is_none());
}

#[test]

fn test_is_production_recursive() {
  let mut j = Journal::new(None);
  let g = GrammarStore::from_str(
    &mut j,
    "
      <> A > B 
      <> B > C
      <> C > D
      <> D > E
      <> E > B A R | 'e'
      <> R > R A | R B O | 'r'
      <> O > 'o'
      ",
  )
  .unwrap();

  let production = g.get_production_id_by_name("A").unwrap();

  assert_eq!(g.get_production_recursion_type(production), RecursionType::RIGHT);

  let production = g.get_production_id_by_name("R").unwrap();

  assert!(g
    .get_production_recursion_type(production)
    .contains(RecursionType::LEFT_DIRECT | RecursionType::RIGHT));

  let production = g.get_production_id_by_name("B").unwrap();

  assert!(g
    .get_production_recursion_type(production)
    .contains(RecursionType::LEFT_INDIRECT | RecursionType::RIGHT));

  let production = g.get_production_id_by_name("C").unwrap();

  assert!(g
    .get_production_recursion_type(production)
    .contains(RecursionType::LEFT_INDIRECT | RecursionType::RIGHT));

  let production = g.get_production_id_by_name("O").unwrap();

  assert_eq!(g.get_production_recursion_type(production), RecursionType::NONE);
}

#[test]
fn grammar_name_from_preamble() -> SherpaResult<()> {
  let mut j = Journal::new(None);

  let ast = load_from_string(
    &mut j,
    r#"
    NAME test
    IGNORE { "tacos" }
    <> A > "hello" 
    "world" "#,
    Default::default(),
  );

  compile_grammars(&mut j, &ast)?;

  assert_eq!(j.grammar()?.id.name, "test");

  SherpaResult::Ok(())
}

#[test]
fn grammar_with_optional_list() -> SherpaResult<()> {
  let mut j = Journal::new(None);

  let ast = load_from_string(&mut j, r#"<> A > "hello"(*) c:nl"#, Default::default());

  compile_grammars(&mut j, &ast);

  j.flush_reports();
  assert!(!j.debug_error_report());

  let grammar = j.grammar()?;

  let prod_id = grammar.get_production_id_by_name("A")?;

  let rules = grammar.production_rules.get(&prod_id)?;

  assert_eq!(rules.len(), 2);

  SherpaResult::Ok(())
}

#[test]
fn grammar_name_from_path() -> SherpaResult<()> {
  let mut j = Journal::new(None);

  let ast =
    load_from_string(&mut j, r#" IGNORE { "a" } <> A > "h"  "w" "#, "/test_path.test".into());

  compile_grammars(&mut j, &ast)?;

  assert_eq!(j.grammar()?.id.name, "test_path");

  SherpaResult::Ok(())
}

#[test]
fn missing_append_host_error() -> SherpaResult<()> {
  let mut j = Journal::new(None);

  let ast = load_from_string(
    &mut j,
    r#"
  NAME test
  IGNORE { "tacos" }
  +> A > "hello"{1} 
       | "goodby"{1} 
  
  "#,
    "/test_path.test".into(),
  );

  compile_grammars(&mut j, &ast);

  j.flush_reports();

  assert!(j.debug_error_report());

  assert!(j.get_report(crate::ReportType::GrammarCompile(Default::default()), |r| {
    let error = &r.errors[0];

    assert!(matches!(error, SherpaError::SourceError { .. }));

    let SherpaError::SourceError { id, .. } = error else {
        panic!("Expected a SourceError");
    };

    assert_eq!(*id, "missing-append-host");

    true
  }));

  SherpaResult::Ok(())
}

#[test]
fn compile_grammar_with_syntax_definitions() -> SherpaResult<()> {
  let mut j = Journal::new(None);

  j.set_active_report("Grammar Parse", crate::ReportType::GrammarCompile(Default::default()));

  let ast = load_from_string(
    &mut j,
    r#"<> A > B :syn { $1: type rgb(r255 g33 b22) } "#,
    "/test_path.test".into(),
  );

  j.flush_reports();

  assert!(!j.debug_error_report());

  let syntax = ast[0].2.productions[0].rules[0].syntax_definition.as_ref()?;
  let color = &syntax.specs[0].spec.rgb.as_ref()?;
  let id = &syntax.specs[0].spec.id;

  assert_eq!(syntax.specs[0].reference.to_string(), "$1");
  assert_eq!(color.r, 255);
  assert_eq!(color.g, 33);
  assert_eq!(color.b, 22);
  assert_eq!(id.to_string(), "type");

  SherpaResult::Ok(())
}

#[test]
pub fn generate_block_comment() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let grammar_data = load_from_string(
    &mut j,
    r#"
<> A > tk:comment

<> comment > tk:block  | tk:line  | c:sym 
   
<> block > "/*"  ( c:sym )(*) "*/"

<> line > "//"  ( c:sym )(*) c:nl

"#,
    "/test_path.test".into(),
  );

  j.flush_reports();
  j.debug_error_report();

  compile_grammars(&mut j, &grammar_data);

  j.flush_reports();
  j.debug_error_report();

  let grammar = j.grammar()?;

  let result = compile_scanner_states(
    &mut j,
    BTreeSet::from_iter(vec![
      grammar.get_production_by_name("tk:block")?.sym_id,
      grammar.get_production_by_name("tk:line")?.sym_id,
      SymbolID::from_string("g:sym", Some(&grammar)),
    ]),
  )?;
  j.flush_reports();
  // j.debug_print_reports(ReportType::ScannerCompile(Default::default()));
  eprintln!("{:#?}", result);

  assert_eq!(result.len(), 13);
  SherpaResult::Ok(())
}

// Compile v1.0.0 grammar with v1.0.0_strap parser
#[test]
fn compile_latest_grammar() -> SherpaResult<()> {
  let mut j = Journal::new(None);
  let path = crate::test::utils::path_from_source("grammar/v1_0_0/grammar.sg")?;

  let grammar_data = load_from_path(&mut j, path);
  assert!(!j.debug_error_report());

  compile_grammars(&mut j, &grammar_data);

  j.flush_reports();
  assert!(!j.debug_error_report());

  let states = compile_states(&mut j, get_num_of_available_threads());

  //j.debug_print_reports(crate::ReportType::ScannerCompile(ScannerStateId::default()));

  j.flush_reports();
  assert!(!j.debug_error_report());

  let states = optimize_ir_states(&mut j, states?);

  let bc = compile_bytecode(&mut j, states);

  println!("{}", generate_disassembly(&bc, Some(&mut j)));

  j.flush_reports();
  assert!(!j.debug_error_report());

  let entry_state_name = j.grammar()?.get_production_by_name("grammar").unwrap().guid_name.clone();
  let target_production_id = j.grammar()?.get_production_by_name("grammar").unwrap().bytecode_id;
  let entry_point = *bc.state_name_to_offset.get(&entry_state_name).unwrap();
  //let grammar_pack = BytecodeGrammarLookups::new(j.grammar()?);
  let (shifts, skips) = collect_shifts_and_skips(
    r#"
    /* test */
    // This is a comment
      <>grammar>
        "Hello" 'World'
"#,
    entry_point,
    target_production_id,
    &bc.bytecode,
    Some(&move |event| match event {
      DebugEvent::ExecuteInstruction { ctx, .. } => {
        if true || !ctx.is_scanner() {
          return;
          /*      println!(
            "[{} : {}] {}",
            ctx.get_reader().get_char(),
            ctx.get_reader().cursor(),
            disassemble_state(bc, address, Some(&grammar_pack)).0
          ) */
        }
      }
      _ => {}
    }),
  )?;

  dbg!(shifts, skips);

  SherpaResult::Ok(())
}
