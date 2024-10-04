use super::*;
use crate::{kernel::ByteCodeParserNew, parsers::Parser};
use std::{
  collections::{BTreeMap, HashMap},
  io::Write,
  rc::Rc,
};

/// Stores information, such as bytecode instructions and symbols to
/// non-terminal id maps,  used by parsers and other "runtime tools".
#[derive(Clone, Default, Debug)]
pub struct BytecodeParserDB {
  pub default_entry:          EntryPoint,
  pub bytecode:               Vec<u8>,
  pub ir_token_lookup:        BTreeMap<u32, Token>,
  pub nonterm_name_to_id:     HashMap<String, u32>,
  pub state_name_to_address:  HashMap<String, u32>,
  pub address_to_state_name:  HashMap<u32, String>,
  pub nonterm_id_to_address:  HashMap<u32, u32>,
  pub state_to_token_ids_map: HashMap<u32, Vec<u32>>,
  pub token_id_to_str:        HashMap<u32, String>,
  /// Start and end bytes of the Nonterminal definition in the original grammar
  pub rule_offsets:           HashMap<u32, (u32, u32)>,
  pub rule_diagram:           HashMap<u32, String>,
  /// Friendly names of all non-terminals
  pub nonterm_name:           HashMap<u32, String>,
}

impl BytecodeParserDB {
  /// Write the entire bytecode binary as disassembly to the writer.
  pub fn write_binary<W: Write>(&self, w: &mut W) -> std::io::Result<()> {
    w.write_all(&self.bytecode)
  }
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
  fn get_entry_data_from_name(&self, entry_name: &str) -> Result<EntryPoint, ParserError> {
    if let Some(id) = self.nonterm_name_to_id.get(entry_name) {
      Ok(EntryPoint { nonterm_id: *id })
    } else {
      Err(ParserError::InvalidEntryName)
    }
  }

  fn get_expected_tok_ids_at_state(&self, state_id: u32) -> Option<&[u32]> {
    self.state_to_token_ids_map.get(&state_id).map(|s| s.as_slice())
  }

  fn token_id_to_str(&self, tok_id: u32) -> Option<&str> {
    self.token_id_to_str.get(&tok_id).map(|s| s.as_str())
  }

  fn default_entrypoint(&self) -> EntryPoint {
    self.default_entry
  }

  fn entrypoints(&self) -> Vec<(String, u32)> {
    self
      .nonterm_name_to_id
      .iter()
      .map(|(name, id)| (name.to_owned(), self.nonterm_id_to_address.get(id).map(|address| *address).unwrap_or(0)))
      .collect()
  }
}

impl<T: ParserInput> ParserProducer<T> for BytecodeParserDB {
  fn get_parser(&self) -> Result<Box<dyn Parser<T>>, ParserError> {
    Ok(Box::new(ByteCodeParserNew::new(Rc::new(self.bytecode.clone()), self.nonterm_id_to_address.clone())))
  }
}
