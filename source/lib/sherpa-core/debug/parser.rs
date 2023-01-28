use sherpa_runtime::functions::{get_next_action, DebugFn};

use crate::types::*;
use std::sync::Arc;
/// Collects all tokens that where shifted and skipped during parsing.
pub fn collect_shifts_and_skips<'a>(
  input: &'a str,
  entry_point: u32,
  target_production_id: u32,
  bytecode: &[u32],
  debug: Option<&DebugFn<UTF8StringReader<'a>, u32>>,
) -> SherpaResult<(Vec<String>, Vec<String>)> {
  let mut reader = UTF8StringReader::from_string(input);
  let mut ctx = ParseContext::<_, u32>::new(&mut reader);

  let mut stack = vec![0, entry_point | NORMAL_STATE_FLAG];
  let mut shifts = vec![];
  let mut skips = vec![];

  loop {
    match get_next_action(&mut ctx, &mut stack, bytecode, debug) {
      ParseAction::Accept { production_id } => {
        assert_eq!(
          production_id, target_production_id,
          "Expected the accepted production id to match target_production_id"
        );
        break SherpaResult::Ok((shifts, skips));
      }
      ParseAction::Error { last_input, .. } => {
        let mut token: Token = last_input.to_token(ctx.get_reader());

        token.set_source(Arc::new(Vec::from(input.to_string().as_bytes())));
        break SherpaResult::Err(token.blame(1, 1, "", None).into());
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
}
