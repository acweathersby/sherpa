use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use hctk::types::UTF8StringReader;
use hctk::types::*;
use test_basic_grammar_nasm::*;

pub fn bench(input: &str)
{
    let mut reader = UTF8StringReader::new(input.to_string());
    let mut iter = Context::new(&mut reader);
    iter.set_start_point(StartPoint::BANNER);

    let mut updates = 0;

    for action in iter {
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

fn criterion_benchmark(c: &mut Criterion)
{
    c.bench_function("x86_64 run", |b| b.iter(|| bench("hello world")));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
