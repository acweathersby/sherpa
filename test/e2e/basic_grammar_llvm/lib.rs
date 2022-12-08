pub mod llvm_test {
  include!(concat!(env!("OUT_DIR"), "/llvm_test.rs"));
}

#[cfg(test)]
mod test {

  use sherpa_runtime::types::*;

  use crate::llvm_test;

  #[test]
  pub fn test_build() {
    for action in llvm_test::Parser::new_banner_parser(UTF8StringReader::from("hello world")) {
      match action {
        ParseAction::Shift { skipped_characters: skip, token } => {
          println!("Skip {:?} & Extract token {:?} ", skip, token);
        }
        ParseAction::Reduce { production_id, rule_id, symbol_count } => {
          println!(
            "Reduce {} symbols to production {} from completion of body {}",
            symbol_count, production_id, rule_id,
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

    let actions = llvm_test::Parser::new_banner_parser(UTF8StringReader::from("hello world"))
      .collect::<Vec<_>>();

    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Shift { .. }));
    assert!(matches!(actions[2], ParseAction::Reduce { production_id, .. } if production_id == 0));
    assert!(matches!(actions[3], ParseAction::Accept { production_id } if production_id == 0));
  }

  #[test]
  pub fn should_fail_on_second_erroneous_token() {
    let actions = llvm_test::Parser::new_banner_parser(UTF8StringReader::from("hello wold"))
      .collect::<Vec<_>>();
    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Error { .. }));
  }
}
