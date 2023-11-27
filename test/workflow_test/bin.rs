use sherpa_rust_runtime::{
  parsers::{
    ast::{AstDatabase, Tk},
    error_recovery::ErrorRecoveringDatabase,
  },
  types::{RuntimeDatabase, SharedSymbolBuffer, StringInput},
};
mod ast;
mod parser;

fn main() {
  let db: parser::ParserDB = parser::ParserDB::new();
  let entry = db.get_entry_data_from_name("default").unwrap();

  let result = db.build_ast::<Token, _, _>(&mut StringInput::from("Hello World"), entry, ast::ReduceRules::new()).unwrap();

  println!("{result:#?}");
}

#[derive(Debug, Clone, Default, Hash)]
struct Token {
  len:    usize,
  offset: usize,
  string: String,
}

impl Tk for Token {
  fn from_range(start: usize, end: usize, id: u32, source: SharedSymbolBuffer) -> Self {
    Self {
      len:    end - start,
      offset: start,
      string: unsafe { String::from_utf8_unchecked(source[start..end].to_vec()) },
    }
  }

  fn from_slice(slice: &[Self]) -> Self {
    let node = slice.iter().skip(1).fold(slice[0].clone(), |mut existing, other| {
      existing.string += &other.string;
      existing.len += other.len;
      existing
    });

    node
  }

  fn to_string(&self) -> String {
    self.string.clone()
  }
}
