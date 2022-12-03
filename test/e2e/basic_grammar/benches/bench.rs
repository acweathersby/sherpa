use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sherpa::types::{UTF8StringReader, *};
use test_basic_grammar::*;

pub fn bench(input: &str) {
  let mut updates = 0;

  for action in Context::new_banner_parser(&mut UTF8StringReader::new(input.to_string())) {
    match action {
      ParseAction::Shift { .. } => {
        updates += 1;
      }
      ParseAction::Accept { .. } => {
        updates += 1;
        break;
      }
      _ => {}
    }
  }

  assert!(updates == 3);
}

fn criterion_benchmark(c: &mut Criterion) {
  c.bench_function("bytecode run", |b| b.iter(|| bench("hello world")));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
