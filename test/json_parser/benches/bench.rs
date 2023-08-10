use criterion::{criterion_group, criterion_main, Criterion};
use json_test_parser::*;
use sherpa_rust_runtime::types::{
  ParseAction,
  ParseActionType,
  SherpaParseError,
  UTF8StringReader,
};
use std::time::Instant;

pub fn json_ast_build(input: &str) -> Result<Box<Json>, SherpaParseError> {
  Json::from_str(input)
}

pub fn json_ast_parse(input: &str) -> ParseActionType {
  let parser = Parser::<_, u32>::new_entry_parser(UTF8StringReader::from(input));
  let mut action = ParseActionType::None;
  for a in parser.into_iter() {
    action = a;
  }
  action
}

fn criterion_benchmark(c: &mut Criterion) {
  let str_ref = include_str!("../spirv.core.grammar.json");

  c.bench_function("x86_64 json_ast_build", |b| b.iter_with_large_drop(|| json_ast_build(str_ref)));
  c.bench_function("x86_64 json_parse", |b| b.iter(|| json_ast_parse(str_ref)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
