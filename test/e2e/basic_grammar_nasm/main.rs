mod nasm_test_parser;

use hctk::types::UTF8StringReader;
use hctk::types::*;
use std::time::Duration;
use std::time::Instant;
pub fn main()
{
    let mut reader = UTF8StringReader::new("hello world".to_string());
    let mut iter = nasm_test_parser::Context::new(&mut reader);
    let mut messages = Vec::<String>::with_capacity(10);

    let start = Instant::now();

    iter.set_start_point(nasm_test_parser::StartPoint::BANNER);

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

#[cfg(test)]
mod test
{
    use crate::nasm_test_parser;
    use hctk::types::*;

    #[test]
    pub fn test_build()
    {
        let mut reader = UTF8StringReader::new("hello world".to_string());
        let mut ctx = nasm_test_parser::Context::new(&mut reader);

        ctx.set_start_point(nasm_test_parser::StartPoint::BANNER);

        loop {
            match ctx.next() {
                Some(ParseAction::Shift {
                    skipped_characters: skip,
                    token,
                }) => {
                    println!("Skip {:? } & Extract token {:?} ", skip, token);
                }
                Some(ParseAction::Accept { production_id }) => {
                    println!("Accept production {}", production_id);
                    break;
                }
                _ => {
                    break;
                }
            }
        }
    }
}
