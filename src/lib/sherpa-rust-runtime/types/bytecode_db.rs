use super::*;
use crate::{kernel::ByteCodeParserNew, parsers::Parser};
use std::{
  collections::{BTreeMap, HashMap},
  rc::Rc,
};

#[derive(Clone, Default)]
pub struct BytecodeParserDB {
  pub bytecode: Vec<u8>,
  pub ir_token_lookup: BTreeMap<u32, Token>,
  pub nonterm_name_to_id: HashMap<String, u32>,
  pub state_name_to_address: HashMap<String, u32>,
  pub address_to_state_name: HashMap<u32, String>,
  pub nonterm_id_to_address: HashMap<u32, u32>,
  pub state_to_token_ids_map: HashMap<u32, Vec<u32>>,
  pub token_id_to_str: HashMap<u32, String>,
}

impl AsRef<[u8]> for BytecodeParserDB {
  fn as_ref(&self) -> &[u8] {
    &self.bytecode
  }
}

impl AsRef<HashMap<String, u32>> for BytecodeParserDB {
  fn as_ref(&self) -> &HashMap<String, u32> {
    &self.state_name_to_address
  }
}

impl AsRef<HashMap<u32, String>> for BytecodeParserDB {
  fn as_ref(&self) -> &HashMap<u32, String> {
    &self.address_to_state_name
  }
}

impl RuntimeDatabase for BytecodeParserDB {
  fn get_entry_data_from_name(&self, entry_name: &str) -> Result<EntryPoint, ParseError> {
    if let Some(id) = self.nonterm_name_to_id.get(entry_name) {
      Ok(EntryPoint { nonterm_id: *id })
    } else {
      Err(ParseError::InvalidEntryName)
    }
  }

  fn get_expected_tok_ids_at_state(&self, state_id: u32) -> Option<&[u32]> {
    self.state_to_token_ids_map.get(&state_id).map(|s| s.as_slice())
  }

  fn token_id_to_str(&self, tok_id: u32) -> Option<&str> {
    self.token_id_to_str.get(&tok_id).map(|s| s.as_str())
  }
}

impl<T: ParserInput> ParserProducer<T> for BytecodeParserDB {
  fn get_parser(&self) -> Result<Box<dyn Parser<T>>, ParseError> {
    Ok(Box::new(ByteCodeParserNew::new(Rc::new(self.bytecode.clone()), self.nonterm_id_to_address.clone())))
  }
}
