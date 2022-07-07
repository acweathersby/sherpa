mod ast;
mod nasm_test_parser;
pub use ast::*;
pub use nasm_test_parser::*;

#[cfg(test)]
mod test
{
    use crate::ast::ASTNode;
    use crate::Context;
    use hctk::types::*;
    use std::sync::Arc;
    #[test]
    pub fn test_build()
    {
        let mut nodes: Vec<HCObj<ASTNode>> = Vec::with_capacity(8);

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

        // if let HCObj::NODE(ASTNode::Tested(data)) = nodes.pop().unwrap() {
        //     assert_eq!(data.first, "hello");
        //     assert_eq!(data.second, "world");
        //     assert_eq!(data.third, "hello");
        // } else {
        //     panic!("Parsing failed!")
        // }
    }
}
