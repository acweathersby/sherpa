//! A parser that produce the tokens, including skipped tokens, of an input.
use crate::types::*;

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Tokens {
  pub shifts: Vec<String>,
  pub skips:  Vec<String>,
}

pub trait TokenProducer<I: ParserInput>: ParserIterator<I> + ParserInitializer {
  fn collect_shifts_and_skips(&mut self, input: &mut I, entry: EntryPoint) -> Result<Tokens, ParserError> {
    let mut shifts = vec![];
    let mut skips = vec![];

    let mut ctx = self.init(entry)?;

    while let Some(action) = self.next(input, &mut ctx) {
      match action {
        ParseAction::Accept { nonterminal_id, final_offset } => {
          return if final_offset != input.len() {
            Err(ParserError::InputError {
              inline_message: format!("Failed to read entire input {} {}", input.len(), final_offset),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "Failed to read entire input".to_string(),
            })
          } else if nonterminal_id != entry.nonterm_id {
            Err(ParserError::InputError {
              inline_message: "Top symbol did not match the target nonterminal".to_string(),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "CST is incorrect".to_string(),
            })
          } else {
            return Ok(Tokens { shifts, skips });
          };
        }
        ParseAction::Error { last_nonterminal, .. } => {
          let last_input = TokenRange {
            len:      ctx.tok_byte_len as u32,
            off:      ctx.sym_ptr as u32,
            line_num: 0,
            line_off: 0,
          };
          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionError {}, self.get_ctx());
          //}
          let token: Token = last_input.to_token_from_ref(input.get_owned_ref());

          return Err(ParserError::InputError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_nonterminal,
          });
        }
        ParseAction::Fork { .. } => {
          panic!("No implementation of fork resolution is available")
        }
        ParseAction::Skip { byte_offset: token_byte_offset, byte_length: token_byte_length, .. } => {
          skips.push(input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize));
        }
        ParseAction::Shift {
          byte_length: token_byte_length,
          byte_offset: token_byte_offset,
          token_id,
          ..
        } => {
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;

          #[cfg(debug_assertions)]
          if let Some(debug) = self.get_debugger() {
            //debug(&DebugEventNew::ActionShift { offset_start, offset_end,
            // token_id }, input);
          }
          shifts.push(input.string_range(offset_start..offset_end));
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } => {
          #[cfg(debug_assertions)]
          if let Some(debug) = self.get_debugger() {
            // debug(&DebugEventNew::ActionReduce { rule_id: _rule_id }, input);
          }
        }
        _ => panic!("Unexpected Action!"),
      }
    }
    return Err(ParserError::Unexpected);
  }
}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> TokenProducer<I> for T {}
