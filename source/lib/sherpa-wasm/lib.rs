use sherpa_core::{
  compile::{
    compile_bytecode,
    compile_parse_states,
    optimize_parse_states,
    BytecodeOutput,
    GrammarStore,
  },
  debug,
  Journal,
  SherpaResult,
};
use sherpa_runtime::types::bytecode;
use std::{cell::RefCell, rc::Rc};
use wasm_bindgen::{prelude::*, JsCast};

fn window() -> web_sys::Window {
  web_sys::window().expect("no global `window` exists")
}

fn document() -> web_sys::Document {
  window().document().expect("should have a document on window")
}

fn body() -> web_sys::HtmlElement {
  document().body().expect("document should have a body")
}

/// Compiles a sherpa grammar from a string value.
///
/// Returns an error if the `grammar` argument cannot be cast to a string.
#[wasm_bindgen]
pub fn compile_grammar(grammar: JsValue) -> Result<JournalWrap, JsError> {
  match grammar.as_string() {
    Some(grammar_source) => {
      let mut j = Journal::new(None);
      GrammarStore::from_str(&mut j, &grammar_source);
      let valid_grammar =
        !j.have_errors_of_type(sherpa_core::errors::SherpaErrorSeverity::Critical);
      Ok(JournalWrap {
        _internal_: Box::new(j),
        _states_: None,
        _bytecode_: None,
        valid_grammar,
      })
    }
    None => Err(JsError::new("Could not read grammar string")),
  }
}

#[wasm_bindgen]
/// A Grammar context created after the parsing of an
/// input value.
///
/// May contain errors and thus be invalid for further
/// processing.
pub struct JournalWrap {
  _internal_:    Box<Journal>,
  _states_:      Option<Vec<(String, Box<sherpa_core::compile::ParseState>)>>,
  _bytecode_:    Option<Box<BytecodeOutput>>,
  valid_grammar: bool,
}

#[wasm_bindgen]
impl JournalWrap {
  /// Returns `true` if the internal Grammar
  /// is free of critical errors.
  pub fn is_valid(&self) -> bool {
    self.valid_grammar
  }

  pub fn compile_states(&mut self, optimize: bool) {
    if self._states_.is_none() {
      let j = &mut self._internal_;

      match compile_parse_states(j, 1) {
        SherpaResult::Ok(states) => {
          if optimize {
            let states = optimize_parse_states(j, states);
            self._states_ = Some(states);
          } else {
            self._states_ = Some(states.into_iter().collect());
          }
        }
        SherpaResult::Err(err) => self._states_ = Some(vec![]),
        _ => unreachable!(),
      }
    }
  }

  pub fn compile_bytecode(&mut self, optimize: bool) {
    if self._states_.is_none() {
      self.compile_bytecode(optimize);
    }

    if self._bytecode_.is_none() {
      let bytecode = {
        let Self { _internal_, _states_, .. } = self;
        let Some(states) = &_states_ else {
        return;
      };

        if states.is_empty() {
          return;
        }

        compile_bytecode(_internal_, states).unwrap()
      };

      self._bytecode_ = Some(Box::new(bytecode));
    }
  }

  pub fn generate_disassembly(&mut self) -> Result<JsValue, JsError> {
    match &self._bytecode_ {
      Some(bytecode) => {
        let j = &mut self._internal_;
        let output = debug::generate_disassembly(&bytecode, &j);
        Ok(output.into())
      }
      None => Ok("Bytecode is not built or not valid".into()),
    }
  }
}
