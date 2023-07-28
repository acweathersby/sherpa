use super::*;
use crate::{utils::create_u64_hash, writer::code_writer::CodeWriter};
use sherpa_runtime::utf8::lookup_table::CodePointClass;

pub const DEFAULT_SYM_ID: u32 = 0xFDEFA017;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[repr(u8)]
pub enum SymbolId {
  Undefined,
  Default,
  EndOfFile { precedence: u16 },
  ClassSpace { precedence: u16 },
  ClassHorizontalTab { precedence: u16 },
  ClassNewLine { precedence: u16 },
  ClassIdentifier { precedence: u16 },
  ClassNumber { precedence: u16 },
  ClassSymbol { precedence: u16 },
  Token { precedence: u16, val: IString },
  NonTerminal { id: ProductionId },
  NonTerminalToken { precedence: u16, id: ProductionId },
  DBNonTerminalToken { precedence: u16, prod_key: DBProdKey, sym_key: Option<DBTokenKey> },
  DBNonTerminal { key: DBProdKey },
  DBToken { key: DBTokenKey },
  Char { char: u8, precedence: u16 },
  Codepoint { precedence: u16, val: u32 },
}

impl Default for SymbolId {
  fn default() -> Self {
    SymbolId::Undefined
  }
}

impl SymbolId {
  pub const GEN_SPACE_ID: u32 = 2;

  pub fn tok_db_key(&self) -> Option<DBTokenKey> {
    use SymbolId::*;
    match self {
      DBToken { key } => Some(*key),
      DBNonTerminalToken { sym_key, .. } => *sym_key,
      _ => None,
    }
  }

  pub fn is_term(&self) -> bool {
    use SymbolId::*;
    match self {
      NonTerminal { .. } | DBNonTerminal { .. } => false,
      _ => true,
    }
  }

  pub fn is_default(&self) -> bool {
    use SymbolId::*;
    match self {
      Default => true,
      _ => false,
    }
  }

  /// True if the SymbolId is a Generic class type
  pub fn is_class(&self) -> bool {
    use SymbolId::*;
    match *self {
      ClassSymbol { .. }
      | ClassSpace { .. }
      | ClassHorizontalTab { .. }
      | ClassNewLine { .. }
      | ClassIdentifier { .. }
      | ClassNumber { .. } => true,
      _ => false,
    }
  }

  pub fn is_linefeed(&self) -> bool {
    use SymbolId::*;
    match *self {
      Token { val, .. } if val == "\n".to_token() => true,
      ClassNewLine { .. } => true,
      _ => false,
    }
  }

  pub fn precedence(&self) -> u16 {
    use SymbolId::*;
    match *self {
      EndOfFile { precedence } => precedence,
      ClassSpace { precedence } => precedence,
      ClassHorizontalTab { precedence } => precedence,
      ClassNewLine { precedence } => precedence,
      ClassIdentifier { precedence } => precedence,
      ClassNumber { precedence } => precedence,
      ClassSymbol { precedence } => precedence,
      Token { precedence, .. } => precedence,
      NonTerminalToken { precedence, .. } => precedence,
      Codepoint { precedence, .. } => precedence,
      DBNonTerminalToken { precedence, .. } => precedence,
      Char { precedence, .. } => precedence,
      _ => 0,
    }
  }

  pub fn to_precedence(&self, precedence: u16) -> Self {
    use SymbolId::*;
    match *self {
      EndOfFile { .. } => EndOfFile { precedence },
      ClassSpace { .. } => ClassSpace { precedence },
      ClassHorizontalTab { .. } => ClassHorizontalTab { precedence },
      ClassNewLine { .. } => ClassNewLine { precedence },
      ClassIdentifier { .. } => ClassIdentifier { precedence },
      ClassNumber { .. } => ClassNumber { precedence },
      ClassSymbol { .. } => ClassSymbol { precedence },
      Token { val, .. } => Token { val, precedence },
      NonTerminal { id, .. } => NonTerminal { id },
      NonTerminalToken { id, .. } => NonTerminalToken { id, precedence },
      Codepoint { val, .. } => Codepoint { val, precedence },
      DBNonTerminal { key } => DBNonTerminal { key },
      DBNonTerminalToken { prod_key, sym_key, .. } => {
        DBNonTerminalToken { prod_key, sym_key, precedence }
      }
      DBToken { key } => DBToken { key },
      Char { char, .. } => Char { char, precedence },
      Default => Default,
      _ => Undefined,
    }
  }

  /// Returns a ProductionId for a token scanner derived from a standard symbol.
  pub fn to_scanner_prod_id(&self) -> ProductionId {
    use SymbolId::*;
    match self {
      NonTerminal { id } => id.as_scan_prod(),
      NonTerminalToken { id, .. } => id.as_scan_prod(),
      Token { .. } => {
        ProductionId::Standard(create_u64_hash(self), ProductionSubType::ScannerToken)
      }
      ClassSymbol { .. }
      | ClassSpace { .. }
      | ClassHorizontalTab { .. }
      | ClassNewLine { .. }
      | ClassIdentifier { .. }
      | ClassNumber { .. }
      | Codepoint { .. }
      | Char { .. } => ProductionId::Standard(create_u64_hash(self), ProductionSubType::ScannerSym),
      _ => {
        #[cfg(debug_assertions)]
        unimplemented!("{:?}", self);
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }
  }

  /// Retrieves the binary / bytecode form of the symbol.
  pub fn to_state_val(&self, db: &ParserDatabase) -> u32 {
    use SymbolId::*;
    match *self {
      Default => DEFAULT_SYM_ID,
      EndOfFile { .. } => CodePointClass::EndOfInput as u32,
      ClassSymbol { .. } => CodePointClass::Symbol as u32,
      ClassSpace { .. } => CodePointClass::Space as u32,
      ClassHorizontalTab { .. } => CodePointClass::HorizontalTab as u32,
      ClassNewLine { .. } => CodePointClass::NewLine as u32,
      ClassIdentifier { .. } => CodePointClass::Identifier as u32,
      ClassNumber { .. } => CodePointClass::Number as u32,
      Codepoint { val, .. } => val,
      Char { char, .. } => char as u32,
      DBNonTerminal { key } => (Into::<usize>::into(key)) as u32,
      DBToken { key, .. } => key.to_val(db),
      DBNonTerminalToken { sym_key, .. } => sym_key.map(|d| d.to_val(db)).unwrap_or(u32::MAX),
      _ => u32::MAX,
    }
  }

  /// Returns `true` if the symbol is `SymbolId::EndOfFile`
  pub fn is_eof(&self) -> bool {
    matches!(self, SymbolId::EndOfFile { .. })
  }

  /// Produce a human friendly string representation of this symbol
  pub fn name(&self, s_store: &IStringStore) -> String {
    use SymbolId::*;
    let mut w = CodeWriter::new(vec![]);
    match *self {
      Undefined => &mut w + "Undefine",
      Default => &mut w + "Default",
      EndOfFile { .. } => &mut w + "{EOF}",
      ClassSpace { .. } => &mut w + "c:sp",
      ClassHorizontalTab { .. } => &mut w + "c:tab",
      ClassNewLine { .. } => &mut w + "c:nl",
      ClassIdentifier { .. } => &mut w + "c:id",
      ClassNumber { .. } => &mut w + "c:num",
      ClassSymbol { .. } => &mut w + "c:sym",
      Token { val, precedence } => {
        let _ = &mut w + "[" + val.to_str(s_store) + "]";
        &mut w + "{ " + precedence.to_string() + " }"
      }
      NonTerminal { .. } => &mut w + "<non-term>",
      NonTerminalToken { precedence, .. } => {
        let _ = &mut w + "<non-term-token>";
        &mut w + "[" + precedence.to_string() + "]"
      }
      Codepoint { val, .. } => &mut w + "cp:" + val.to_string(),
      DBNonTerminal { key } => &mut w + "nt-idx:" + key.to_string(),
      DBNonTerminalToken { prod_key, precedence, .. } => {
        let _ = &mut w + "nt-ind-tok:" + prod_key.to_string();
        &mut w + "[" + precedence.to_string() + "]"
      }
      DBToken { key } => &mut w + "db-tok:" + key.to_string(),
      Char { char, .. } => {
        if char < 128 {
          &mut w + "char:" + char::from(char).to_string()
        } else {
          &mut w + "char:" + char.to_string()
        }
      }
    };

    String::from_utf8(w.into_output()).unwrap()
  }
}
