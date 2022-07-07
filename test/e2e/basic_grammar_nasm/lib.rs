mod nasm_test_parser;
pub use nasm_test_parser::*;

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
