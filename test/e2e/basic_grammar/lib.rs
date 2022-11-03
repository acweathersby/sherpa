mod ast;
pub use ast::*;

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

    let actions =
      ParseContext::new_banner_parser(&mut UTF8StringReader::new("hello world"))
        .collect::<Vec<_>>();

    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Shift { .. }));
    assert!(matches!(actions[2], ParseAction::Reduce { production_id, .. } if production_id == 1));
    assert!(matches!(actions[3], ParseAction::Accept { production_id } if production_id == 1));

    // if let HCObj::NODE(ASTNode::Tested(data)) = nodes.pop().unwrap() {
    //     assert_eq!(data.first, "hello");
    //     assert_eq!(data.second, "world");
    //     assert_eq!(data.third, "hello");
    // } else {
    //     panic!("Parsing failed!")
    // }
  }
}
