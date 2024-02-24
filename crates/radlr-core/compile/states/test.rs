#![allow(unused)]
use radlr_rust_runtime::types::StringInput;
use std::path::PathBuf;

use crate::{
  compile::{
    ir::{build_ir_concurrent, optimize},
    states::build_states::compile_parser_states,
  },
  ParserConfig,
  RadlrGrammar,
  RadlrResult,
  TestPackage,
};

#[test]
fn build_json_graph() -> RadlrResult<()> {
  let mut config = ParserConfig::default();
  config.ALLOW_BYTE_SEQUENCES = true;
  let db = RadlrGrammar::new()
    .add_source_from_string(include_str!("../../../../grammars/json/json.sg"), "", false)
    .unwrap()
    .build_db("", config)
    .unwrap()
    .into_internal();

  #[cfg(not(feature = "wasm-target"))]
  let pool = crate::types::worker_pool::StandardPool::new(20).unwrap();

  #[cfg(feature = "wasm-target")]
  let pool = crate::types::worker_pool::SingleThreadPool {};

  let graph = compile_parser_states(db.clone(), config, &pool)?;

  let mut ir = build_ir_concurrent(&pool, graph.clone(), config, &db)?;

  for (_, ir) in &mut ir.1 {
    ir.build_ast(&db)?;
    // println!("{}", ir.print(&db, true)?);
  }

  let ir: (Vec<_>, _) = optimize(&db, &config, ir.1, false, &pool)?;

  println!("{}", ir.1.to_string());

  for (_, ir) in &ir.0 {
    println!("{} {}", ir.get_canonical_hash(&db, false)?, ir.print(&db, true)?);
  }

  Ok(())
}

#[test]
pub fn peek_hybrid_graph() -> RadlrResult<()> {
  let mut config = ParserConfig::default().enable_fork(false);
  config.ALLOW_BYTE_SEQUENCES = true;
  config.ALLOW_SCANNER_INLINING = false;
  let db = RadlrGrammar::new()
    .add_source_from_string(
      r#"
        IGNORE { c:sp c:nl }

        <> element_block > '<' component_identifier
            ( element_attribute(+) )?
            ( element_attributes | general_data | element_block | general_binding )(*)
            ">"
        
        <> component_identifier >
            identifier ( ':' identifier )?
        
        <> element_attributes >c:nl element_attribute(+)
        
        <> element_attribute > '-' identifier attribute_chars c:sp
        
        
            | '-' identifier ':' identifier
        
            | '-' "store" '{' local_values? '}'
            | '-' "local" '{' local_values? '}'
            | '-' "param" '{' local_values? '}'
            | '-' "model" '{' local_values? '}'
        
        <> general_binding > ':' identifier
        
        <> local_values > local_value(+)
        
        <> local_value > identifier ( '`' identifier )? ( '='  c:num )? ( ',' )(*)
        
        
        <> attribute_chars > ( c:id | c:num | c:sym  )(+)
        <> general_data > ( c:id | c:num  | c:nl  )(+)
        
        <> identifier > tk:tok_identifier
        
        <> tok_identifier > ( c:id | c:num )(+)"#,
      "",
      false,
    )
    .unwrap()
    .build_db("", config)
    .unwrap()
    .into_internal();

  #[cfg(not(feature = "wasm-target"))]
  let pool = crate::types::worker_pool::StandardPool::new(20).unwrap();

  #[cfg(feature = "wasm-target")]
  let pool = crate::types::worker_pool::SingleThreadPool {};

  let graph = compile_parser_states(db.clone(), config, &pool)?;

  let mut ir = build_ir_concurrent(&pool, graph.clone(), config, &db)?;

  //return Ok(());

  for (_, ir) in &ir.1 {
    println!("{}", ir.print(&db, true)?);
  }

  let ir: (Vec<_>, _) = optimize(&db, &config, ir.1, false, &pool)?;

  println!("{}", ir.1.to_string());

  for (_, ir) in &ir.0 {
    println!("{}", ir.print(&db, true)?);
  }

  Ok(())
}
