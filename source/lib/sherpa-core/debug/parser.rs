use crate::{runtime::get_next_action, types::*};
use std::sync::Arc;

pub fn collect_shifts_and_skips(
  input: &str,
  entry_point: u32,
  target_production_id: u32,
  bytecode: Vec<u32>,
) -> (UTF8StringReader, ParseContext<UTF8StringReader>, Vec<String>, Vec<String>) {
  let mut reader = UTF8StringReader::from_string(input);
  let source = reader.get_source();
  let mut state = ParseContext::bytecode_context();
  state.init_normal_state(entry_point);
  let mut shifts = vec![];
  let mut skips = vec![];

  loop {
    match get_next_action(&mut reader, &mut state, &bytecode) {
      ParseAction::Accept { production_id } => {
        assert_eq!(production_id, target_production_id);
        break;
      }
      ParseAction::Error { last_input, .. } => {
        let mut token = Token::from_parse_token(&last_input);
        token.set_source(Arc::new(Vec::from(input.to_string().as_bytes())));
        panic!("{} [{}]:\n{}", "message", token, token.blame(1, 1, "", None));
        break;
      }
      ParseAction::Fork { .. } => {
        panic!("No implementation of fork resolution is available")
      }
      ParseAction::Shift { skipped_characters, token } => {
        if skipped_characters.byte_offset > 0 {
          unsafe {
            skips.push(
              input
                .to_string()
                .get_unchecked(
                  skipped_characters.byte_offset as usize
                    ..(skipped_characters.byte_offset + skipped_characters.byte_length) as usize,
                )
                .to_owned(),
            );
          }
        }

        unsafe {
          shifts.push(
            input
              .to_string()
              .get_unchecked(
                token.byte_offset as usize..(token.byte_offset + token.byte_length) as usize,
              )
              .to_owned(),
          );
        }
      }
      ParseAction::Reduce { .. } => {}
      _ => panic!("Unexpected Action!"),
    }
  }
  (reader, state, shifts, skips)
}
