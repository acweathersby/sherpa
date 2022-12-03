pub mod llvm_test {
  include!(concat!(env!("OUT_DIR"), "/llvm_test.rs"));
}

#[cfg(test)]
mod test {

  use crate::llvm_test;
  use sherpa::types::*;

  #[test]
  pub fn test_build() {
    for action in llvm_test::Context::new_banner_parser(UTF8StringReader::new("hello world")) {
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

    let actions = llvm_test::Context::new_banner_parser(UTF8StringReader::new("hello world"))
      .collect::<Vec<_>>();

    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Shift { .. }));
    assert!(matches!(actions[2], ParseAction::Reduce { production_id, .. } if production_id == 8));
    assert!(matches!(actions[3], ParseAction::Accept { production_id } if production_id == 8));
  }

  #[test]
  pub fn should_fail_on_second_erroneous_token() {
    let actions = llvm_test::Context::new_banner_parser(UTF8StringReader::new("hello wold"))
      .collect::<Vec<_>>();
    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::Error { .. }));
  }

  #[test]
  pub fn should_emit_end_of_input_action() {
    let mut reader = TestUTF8StringReader::new("hello world");

    reader.len = 5; // Artificially truncating the readers input window

    let actions = llvm_test::Context::new_banner_parser(reader).collect::<Vec<_>>();

    assert!(matches!(actions[0], ParseAction::Shift { .. }));
    assert!(matches!(actions[1], ParseAction::EndOfInput { .. }));
    if let ParseAction::EndOfInput { current_cursor_offset } = actions[1] {
      assert_eq!(current_cursor_offset, 5);
      println!("Offset position: {}", current_cursor_offset)
    }
  }
}
