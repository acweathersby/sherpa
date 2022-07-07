use hctk::types::UTF8StringReader;
use hctk::types::*;
use std::time::Instant;
use test_basic_grammar_nasm::*;
pub fn main()
{
    let mut reader = UTF8StringReader::new("hello world".to_string());
    let mut iter = Context::new(&mut reader);
    let mut messages = Vec::<String>::with_capacity(10);

    let start = Instant::now();

    iter.set_start_point(StartPoint::BANNER);

    for action in iter {
        match action {
            ParseAction::Shift {
                skipped_characters: skip,
                token,
            } => {
                messages.push(format!(
                    "Skip {:? } & Extract token {:?} ",
                    skip, token
                ));
            }
            ParseAction::Accept { production_id } => {
                messages.push(format!("Accept production {}", production_id));
                break;
            }
            _ => {}
        }
    }
    let duration = start.elapsed();

    messages.iter().for_each(|s| println!("{}", s));

    println!("-- dur: {:?}", duration)
}
