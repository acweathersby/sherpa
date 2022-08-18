mod llvm_language_test_parser;
mod llvm_language_test_parser_ast;
pub use llvm_language_test_parser::*;

#[cfg(test)]
mod test
{

  use crate::llvm_language_test_parser_ast::*;
  use crate::Context;
  use hctk::types::*;

  #[test]
  pub fn test_build()
  {
    let mut stack = Vec::new();
    for action in Context::new_entry_parser(&mut UTF8StringReader::new("(2+(2*2))+1+1+1"))
    {
      match action {
        ParseAction::Error { last_input, .. } => {
          println!("Error: failed at {}", last_input.cp_offset);
        }
        ParseAction::Shift { skipped_characters: skip, token } => {
          stack.push(HCO::TOKEN(Token::from_kernel_token(&token)));

          println!("Skip {:?} & Extract token {:?} ", skip, token);
        }
        ParseAction::Reduce { production_id, body_id, symbol_count } => {
          REDUCE_FUNCTIONS[production_id as usize](&mut stack, Token::new());

          println!(
            "Reduce {} symbols to production {} from completion of body {}",
            symbol_count, production_id, body_id,
          );
        }
        ParseAction::Accept { production_id } => {
          if let Some(top) = stack.pop() {
            println!("{:#?}", top);
          }

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
