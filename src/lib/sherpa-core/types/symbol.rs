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

  pub fn precedence(&self, db: &ParserDatabase) -> u16 {
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
      DBToken { key: index } => db.sym(index).precedence(db),
      Char { precedence, .. } => precedence,
      _ => 0,
    }
  }

  /// Returns an unprecedented version of the symbol
  pub fn to_plain(&self) -> Self {
    self.to_precedence(0)
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
      DBNonTerminalToken { prod_key, sym_key, .. } => DBNonTerminalToken { prod_key, sym_key, precedence },
      DBToken { key } => DBToken { key },
      Char { char, .. } => Char { char, precedence },
      Default => Default,
      _ => Undefined,
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
      Token { precedence: _, val } => {
        let val = val.to_string(db.string_store());
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
      Token { val, precedence } => &mut w + "[" + val.to_str(db.string_store()).as_str() + "]" + print_precedence(precedence),
      NonTerminalState { .. } => &mut w + "non_term_state",
      NonTerminal { .. } => &mut w + "non_term",
      NonTerminalToken { .. } => &mut w + "tk:" + "non_term",
      Codepoint { val, precedence } => &mut w + "" + val.to_string() + print_precedence(precedence),
      DBNonTerminal { key } => {
        let guard_str = db.prod_friendly_name_string(key);
        let name = guard_str.as_str();
        &mut w + name
      }
      DBNonTerminalToken { prod_key, precedence, .. } => {
        let guard_str = db.prod_friendly_name_string(prod_key);
        &mut w + "tk:" + guard_str + print_precedence(precedence)
      }
      DBToken { key: index } => &mut w + db.sym(index).debug_string(db),
      Char { char, precedence } => {
        if char < 128 {
          &mut w + "" + char::from(char).to_string() + print_precedence(precedence)
        } else {
          &mut w + "[ char:" + char.to_string() + "]{" + precedence.to_string() + "}"
        }
      }
    };
    w.to_string()
  }
}

fn print_precedence(precedence: u16) -> String {
  if precedence > 0 {
    "{".to_string() + &precedence.to_string() + "}"
  } else {
    "".to_string()
  }
}
