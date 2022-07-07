mod nasm_test_parser;
pub use nasm_test_parser::*;

#[cfg(test)]
mod test
{
    use crate::Context;
    use hctk::types::*;

    #[test]
    pub fn test_build()
    {
        for action in Context::new_banner_parser(&mut UTF8StringReader::new(
            "hello world".to_string(),
        )) {
            match action {
                ParseAction::Shift {
                    skipped_characters: skip,
                    token,
                } => {
                    println!("Skip {:?} & Extract token {:?} ", skip, token);
                }
                ParseAction::Reduce {
                    production_id,
                    body_id,
                    symbol_count,
                } => {
                    println!(
                        "Reduce {} symbols to production {} from completion of body {}",
                         symbol_count,production_id, body_id,
                    );
                }
                ParseAction::Accept { production_id } => {
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
