mod ast;
mod parser_data;

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
            use ParseAction::*;

            match action {
                Shift { token, .. } => {
                    let mut token = Token::from_kernel_token(&token);
                    token.set_source(Arc::new(
                        "hello world".as_bytes().to_vec(),
                    ));
                    nodes.push(HCObj::TOKEN(token));
                }
                Reduce { body_id, .. } => {
                    let node = REDUCE_FUNCTIONS[body_id as usize](
                        &mut nodes,
                        Token::new(),
                    );
                    nodes.push(node);
                }
                Error {
                    message,
                    last_input,
                } => {
                    let mut token = Token::from_kernel_token(&last_input);
                    token.set_source(Arc::new(
                        "hello world".as_bytes().to_vec(),
                    ));
                    println!("{}", token.blame(1, 1, message).unwrap());
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        if let HCObj::NODE(ASTNode::Tested(data)) = nodes.pop().unwrap() {
            assert_eq!(data.first, "hello");
            assert_eq!(data.second, "world");
            assert_eq!(data.third, "hello");
        } else {
            panic!("Parsing failed!")
        }
    }
}
