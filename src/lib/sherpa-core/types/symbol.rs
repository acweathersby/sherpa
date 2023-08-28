use super::*;
use crate::{utils::create_u64_hash, writer::code_writer::CodeWriter};
use sherpa_rust_runtime::utf8::lookup_table::CodePointClass;

pub const DEFAULT_SYM_ID: u32 = 0xFDEFA017;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(debug_assertions, derive(Debug))]
#[repr(u8)]
pub enum SymbolId {
  Undefined,
  Default,
  EndOfFile,
  ClassSpace,
  ClassHorizontalTab,
  ClassNewLine,
  ClassIdentifier,
  ClassNumber,
  ClassSymbol,
  Token { val: IString },
  NonTerminal { id: ProductionId },
  NonTerminalToken { id: ProductionId },
  DBNonTerminalToken { prod_key: DBProdKey, sym_key: Option<DBTokenKey> },
  DBNonTerminal { key: DBProdKey },
  DBToken { key: DBTokenKey },
  Char { char: u8 },
  Codepoint { val: u32 },
  NonTerminalState { id: ProductionId },
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

  /// True if the SymbolId is a Unicode code point. This is true
  /// if the code point is single character token, and it's Unicode value
  /// is greater than the ASCII code points.
  pub fn is_codepoint(&self, st: &IStringStore) -> bool {
    use SymbolId::*;
    match *self {
      Token { val, .. } => {
        let guard_string = val.to_str(st);
        let string = guard_string.as_str();

        if string.len() > 1 {
          false
        } else {
          string.chars().next().is_some_and(|v| v as u32 > 127)
        }
      }
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
      Char { char, .. } if char == "\n".chars().next().unwrap_or_default() as u8 => true,
      Codepoint { val, .. } if val == "\n".chars().next().unwrap_or_default() as u32 => true,
      _ => false,
    }
  }

  /// Returns a ProductionId for a token scanner derived from a standard symbol.
  pub fn to_prod_id(&self) -> ProductionId {
    use SymbolId::*;
    match self {
      NonTerminal { id } | NonTerminalToken { id, .. } => *id,
      _ => {
        #[cfg(debug_assertions)]
        unimplemented!("{:?}", self);
        #[cfg(not(debug_assertions))]
        unimplemented!()
      }
    }
  }

  /// Returns a ProductionId for a token scanner derived from a standard symbol.
  pub fn to_scanner_prod_id(&self) -> ProductionId {
    use SymbolId::*;
    match self {
      NonTerminal { id } | NonTerminalToken { id, .. } => id.as_scan_prod(),
      Token { .. } => ProductionId::Standard(create_u64_hash(self), ProductionSubType::ScannerToken),
      EndOfFile { .. }
      | ClassSymbol { .. }
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
      Token { val } => {
        let val: String = val.to_string(db.string_store());
        match val.chars().next() {
          Some(char) => char as u32,
          _ => u32::MAX,
        }
      }
      _ => u32::MAX,
    }
  }

  /// Returns `true` if the symbol is `SymbolId::EndOfFile`
  pub fn is_eof(&self) -> bool {
    matches!(self, SymbolId::EndOfFile { .. })
  }

  pub fn debug_string(&self, db: &ParserDatabase) -> String {
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
      Token { val } => &mut w + "[" + val.to_str(db.string_store()).as_str() + "]",
      NonTerminalState { .. } => &mut w + "non_term_state",
      NonTerminal { .. } => &mut w + "non_term",
      NonTerminalToken { .. } => &mut w + "tk:" + "non_term",
      Codepoint { val } => &mut w + "" + val.to_string(),
      DBNonTerminal { key } => {
        let guard_str = db.prod_friendly_name_string(key);
        let name = guard_str.as_str();
        &mut w + name
      }
      DBNonTerminalToken { prod_key, .. } => {
        let guard_str = db.prod_friendly_name_string(prod_key);
        &mut w + "tk:" + guard_str
      }
      DBToken { key: index } => &mut w + db.sym(index).debug_string(db),
      Char { char } => {
        if char < 128 {
          &mut w + "" + char::from(char).to_string()
        } else {
          &mut w + "[ char:" + char.to_string() + "]"
        }
      }
    };
    w.to_string()
  }
}
