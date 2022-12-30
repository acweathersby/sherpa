use sherpa_runtime::functions::get_next_action;

use crate::types::*;
use std::sync::Arc;
/// Todo: Docs
pub fn collect_shifts_and_skips(
  input: &str,
  entry_point: u32,
  target_production_id: u32,
  bytecode: Vec<u32>,
) -> (UTF8StringReader, ParseContext<UTF8StringReader>, Vec<String>, Vec<String>) {
  let mut reader = UTF8StringReader::from_string(input);
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
        let mut token: Token = last_input.into();
        token.set_source(Arc::new(Vec::from(input.to_string().as_bytes())));
        panic!("{} [{}]:\n{}", "message", token, token.blame(1, 1, "", None));
      }
      ParseAction::Fork { .. } => {
        panic!("No implementation of fork resolution is available")
      }
      ParseAction::Shift { anchor_byte_offset, token_byte_length, token_byte_offset, .. } => {
        if anchor_byte_offset > 0 {
          unsafe {
            skips.push(
              input
                .to_string()
                .get_unchecked(
                  anchor_byte_offset as usize..(token_byte_offset - anchor_byte_offset) as usize,
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
                token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize,
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
