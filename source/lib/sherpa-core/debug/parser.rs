use sherpa_runtime::functions::get_next_action;

use crate::types::*;
use std::sync::Arc;
/// Collects all tokens that where shifted and skipped during parsing.
pub fn collect_shifts_and_skips(
  input: &str,
  entry_point: u32,
  target_production_id: u32,
  bytecode: Vec<u32>,
) -> (UTF8StringReader, LLVMParseContext<UTF8StringReader, u32>, Vec<String>, Vec<String>) {
  let mut ctx = LLVMParseContext::new();
  let mut reader = UTF8StringReader::from_string(input);

  let mut stack = vec![0, entry_point | NORMAL_STATE_FLAG];
  let mut shifts = vec![];
  let mut skips = vec![];

  loop {
    match get_next_action(&mut reader, &mut ctx, &mut stack, &bytecode) {
      ParseAction::Accept { production_id } => {
        assert_eq!(
          production_id, target_production_id,
          "Expected the accepted production id to match target_production_id"
        );
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
        if (token_byte_offset - anchor_byte_offset) > 0 {
          skips.push(input[anchor_byte_offset as usize..(token_byte_offset) as usize].to_string());
        }

        shifts.push(
          input[token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize]
            .to_string(),
        );
      }
      ParseAction::Reduce { .. } => {}
      _ => panic!("Unexpected Action!"),
    }
  }
  (reader, ctx, shifts, skips)
}
