use super::*;
use crate::bytecode::{DebugEventNew, DebugFnNew};

pub trait ParserInitializer {
  /// Resets the parser and prepares it to accept in input that should yield the
  /// given non-terminal. A parser should be initialized before attempting to
  /// iterate.
  ///
  /// An error is returned if `nonterminal_goal_id` is an invalid entry point
  /// of the parser.
  fn init(&mut self, nonterminal_goal_id: u32) -> Result<(), ParseError>;
  /// Resets the parser and prepares it to accept in input that should yield the
  /// given non-terminal. A parser should be initialized before attempting to
  /// iterate.
  ///
  /// An error is returned if `nonterminal_goal_id` is an invalid entry point
  /// of the parser.
  fn init_range(&mut self, nonterminal_goal_id: u32, start: usize, end: usize) -> Result<(), ParseError>;

  fn get_debugger(&mut self) -> &mut Option<Box<DebugFnNew>>;

  fn set_debugger(&mut self, debugger: Option<Box<DebugFnNew>>);
}

pub trait ParserIterator2<'input>: 'input {
  /// Returns the next ParseAction from the parser. Returns None if the parser
  /// has already returned a ParseAction::Complete or ParseAction::Error
  fn next(&mut self) -> Option<ParseAction>;
}

pub trait ParserIterator<T: ParserInput> {
  /// Returns the next ParseAction from the parser. Returns None if the parser
  /// has already returned a ParseAction::Complete or ParseAction::Error
  fn next(&mut self, input: &mut T) -> Option<ParseAction>;
}

pub trait ParserCompleter<T: ParserInput>: ParserIterator<T> + ParserInitializer {
  fn completes(&mut self, input: &mut T, target_nonterminal_id: u32) -> Result<(), ParseError> {
    while let Some(action) = self.next(input) {
      match action {
        ParseAction::Accept { nonterminal_id, final_offset } => {
          return if final_offset != input.len() {
            Err(ParseError::InputError {
              inline_message: format!(
                "\nFailed to read entire input \"{}\" \n     end pos: {} \n     expected end pos: {}",
                input.string_range(0..input.len()),
                final_offset,
                input.len(),
              ),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "Failed to read entire input".to_string(),
            })
          } else if nonterminal_id != target_nonterminal_id {
            Err(ParseError::InputError {
              inline_message: "Top symbol did not match the target nonterminal".to_string(),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "CST is incorrect".to_string(),
            })
          } else {
            Ok(())
          };
        }
        ParseAction::Shift { token_byte_length, token_byte_offset, token_id, .. } => {
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;
          #[cfg(debug_assertions)]
          if let Some(debug) = self.get_debugger() {
            debug(&DebugEventNew::ActionShift { offset_start, offset_end, token_id }, input);
          }
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } =>
        {
          #[cfg(debug_assertions)]
          if let Some(debug) = self.get_debugger() {
            debug(&DebugEventNew::ActionReduce { rule_id: _rule_id }, input);
          }
        }
        ParseAction::Error { last_nonterminal, last_input, .. } => {
          let mut token: Token = last_input.to_token_from_ref(input.get_owned_ref());
          return Err(ParseError::InputError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_nonterminal,
          });
        }
        _ => {}
      }
    }

    Err(ParseError::Unexpected)
  }
}

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct ShiftsAndSkips {
  pub shifts: Vec<String>,
  pub skips:  Vec<String>,
}

pub trait ArtifactProducer<T: ParserInput>: ParserIterator<T> {
  fn collect_shifts_and_skips<'debug>(
    &mut self,
    input: &mut T,
    target_nonterminal_id: u32,
  ) -> Result<ShiftsAndSkips, ParseError> {
    let mut shifts = vec![];
    let mut skips = vec![];

    while let Some(action) = self.next(input) {
      match action {
        ParseAction::Accept { nonterminal_id, final_offset } => {
          return if final_offset != input.len() {
            Err(ParseError::InputError {
              inline_message: format!("Failed to read entire input {} {}", input.len(), final_offset),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "Failed to read entire input".to_string(),
            })
          } else if nonterminal_id != target_nonterminal_id {
            Err(ParseError::InputError {
              inline_message: "Top symbol did not match the target nonterminal".to_string(),
              last_nonterminal: nonterminal_id,
              loc: Default::default(),
              message: "CST is incorrect".to_string(),
            })
          } else {
            return Ok(ShiftsAndSkips { shifts, skips });
          };
        }
        ParseAction::Error { last_input, last_nonterminal, .. } => {
          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionError {}, self.get_ctx());
          //}
          let mut token: Token = last_input.to_token_from_ref(input.get_owned_ref());

          return Err(ParseError::InputError {
            message: "Could not recognize the following input:".to_string(),
            inline_message: "".to_string(),
            loc: token,
            last_nonterminal,
          });
        }
        ParseAction::Fork { .. } => {
          panic!("No implementation of fork resolution is available")
        }
        ParseAction::Skip { token_byte_offset, token_byte_length, .. } => {
          skips.push(input.string_range(token_byte_offset as usize..(token_byte_offset + token_byte_length) as usize));
        }
        ParseAction::Shift { token_byte_length, token_byte_offset, token_id, .. } => {
          let offset_start = token_byte_offset as usize;
          let offset_end = (token_byte_offset + token_byte_length) as usize;

          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionShift { offset_start, offset_end, token_id },
          // self.get_ctx());
          //}
          shifts.push(input.string_range(offset_start..offset_end));
        }
        ParseAction::Reduce { rule_id: _rule_id, .. } => {
          //#[cfg(debug_assertions)]
          //if let Some(debug) = debug {
          //  debug(&DebugEvent::ActionReduce { rule_id: _rule_id },
          // self.get_ctx());
          //}
        }
        _ => panic!("Unexpected Action!"),
      }
    }
    return Err(ParseError::Unexpected);
  }
}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> ParserCompleter<I> for T {}
impl<T: ParserIterator<I>, I: ParserInput> ArtifactProducer<I> for T {}

pub trait Parser<T: ParserInput>: ParserIterator<T> + ParserCompleter<T> + ArtifactProducer<T> {}

impl<T: ParserIterator<I> + ParserInitializer, I: ParserInput> Parser<I> for T {}

/// Provides information on artifacts consumed and produced by a parser
pub trait RuntimeDatabase {
  fn get_entry_data_from_name(&self, entry_name: &str) -> Option<EntryPoint>;
}

/// An object capable of instantiating a parser
pub trait ParserProducer<T: ParserInput>: RuntimeDatabase {
  /// Creates a new parser or `Err` if there is an issue creating the parser.
  fn get_parser(&self) -> Result<Box<dyn Parser<T>>, ParseError> {
    Err(ParseError::NoData)
  }
}
