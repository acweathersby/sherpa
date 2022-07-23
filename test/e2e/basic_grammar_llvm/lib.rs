mod llvm_test_parser;
pub use llvm_test_parser::*;

#[cfg(test)]
mod test
{
  use crate::Context;
  use hctk::types::*;

  #[test]
  pub fn test_build()
  {
    for action in
      Context::new_banner_parser(&mut UTF8StringReader::new("hello world".to_string()))
    {
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
            symbol_count, production_id, body_id,
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

    let actions =
      Context::new_banner_parser(&mut UTF8StringReader::new("hello world".to_string()))
        .collect::<Vec<_>>();

    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Shift { .. }));
    assert!(
      matches!(actions[2], ParseAction::Reduce { production_id, .. } if production_id == 8)
    );
    assert!(
      matches!(actions[3], ParseAction::Accept { production_id } if production_id == 8)
    );
  }

  #[test]
  pub fn should_fail_on_second_erroneous_token()
  {
    let actions =
      Context::new_banner_parser(&mut UTF8StringReader::new("hello wold".to_string()))
        .collect::<Vec<_>>();
    println!("{:?}", actions);
    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Error { .. }));
  }

  #[test]
  pub fn should_emit_EndOfInputAction()
  {
    let actions =
      Context::new_banner_parser(&mut UTF8StringReader::new("hello world".to_string()))
        .collect::<Vec<_>>();

    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Shift { .. }));
    assert!(
      matches!(actions[2], ParseAction::Reduce { production_id, .. } if production_id == 8)
    );
    assert!(
      matches!(actions[3], ParseAction::Accept { production_id } if production_id == 8)
    );
  }
}
