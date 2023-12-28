use radlr_rust_runtime::{
  parsers::ast::{AstDatabase, Tk},
  types::{RuntimeDatabase, SharedSymbolBuffer, StringInput},
};
mod parser;
use crate::parser::{ASTNode, EntryAny, ValAny};

fn main() {
  let db: parser::ParserDB = parser::ParserDB::new();
  let entry = db.get_entry_data_from_name("default").unwrap();

  let result = db
    .build_ast::<Token, _, _>(
      &mut StringInput::from(
        r###"
{ 
  "ts": [1,2,3,{ "taco": "legendary" },5,6,7,8,9],
  "tree" : {  
    "rope" : null,
    "tree" : true
  }, 
  "test": 1,
  "vest" : "test sts"
}"###,
      ),
      entry,
      parser::ReduceRules::new(),
    )
    .unwrap();

  println!("{result:#?}");

  if let ASTNode::JSON(json) = result {
    match &json.body {
      EntryAny::Array(array) => {
        for val in &array.values {
          match val {
            ValAny::Array(array) => {}
            ValAny::Null => {}
            _ => {}
          }
        }
      }
      EntryAny::Object(body) => if body.values.contains_key("test") {},
      _ => {}
    }
  }
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

  fn trim(&self, start: usize, end: usize) -> Self {
    let offset = self.offset + start;
    let len = self.len - (start + end);
    Self { len, offset, string: self.string[start..start + len].to_string() }
  }

  fn len(&self) -> usize {
    self.len
  }
}
