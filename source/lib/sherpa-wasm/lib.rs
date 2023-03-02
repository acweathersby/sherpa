use sherpa_core::{
  compile::{compile_bytecode, compile_parse_states, optimize_parse_states, GrammarStore},
  debug,
  Journal,
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

//static active_grammar: Box<Option<Journal>> = Box::new(None);

#[wasm_bindgen]
pub fn parse_input(grammar: JsValue) -> Result<(), JsValue> {
  let div = document().query_selector("#output1")?;
  if let Some(div) = div {
    div.set_text_content(Some("all"));
    if let Some(grammar_source) = grammar.as_string() {
      div.set_text_content(Some(&grammar_source));
      let mut j = Journal::new(None);

      GrammarStore::from_str(&mut j, &grammar_source);

      if let Some(strings) = j.string_error_report() {
        div.set_text_content(Some(strings.as_str()));
      } else {
        let states = compile_parse_states(&mut j, 1);

        if let Some(strings) = j.string_error_report() {
          div.set_text_content(Some(strings.as_str()));
        } else {
          let states = optimize_parse_states(&mut j, states.unwrap());

          let bytecode = compile_bytecode(&mut j, &states).unwrap();

          let bytecode = debug::generate_disassembly(&bytecode, &mut j);

          div.set_text_content(Some(bytecode.as_str()));
        }
      }
    }
  }
  Ok(())
}
