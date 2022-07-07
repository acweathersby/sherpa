mod ast;
mod parser_data;
pub use ast::*;
pub use parser_data::*;

#[cfg(test)]
mod test
{

    use crate::ast::ASTNode;
    use crate::ast::REDUCE_FUNCTIONS;
    use crate::parser_data::EntryPoint_default;
    use crate::parser_data::BYTECODE;
    use hctk::types::*;
    use std::sync::Arc;
    #[test]
    pub fn test_build()
    {
        let reader = UTF8StringReader::from_string("hello world");

        let iter =
            ReferenceParseIterator::new(reader, &BYTECODE, EntryPoint_default);

        let mut nodes: Vec<HCObj<ASTNode>> = Vec::with_capacity(8);

        for action in iter {
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
