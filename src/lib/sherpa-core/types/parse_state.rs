use sherpa_rust_runtime::types::bytecode::InputType;

use super::*;
use crate::{
  parser::{self, ASTNode, Matches, State, Statement},
  utils::create_u64_hash,
  writer::code_writer::CodeWriter,
};

pub type ParseStatesVec = Array<(IString, Box<ParseState>)>;
pub type ParseStatesMap = Map<IString, Box<ParseState>>;

#[allow(unused)]
#[cfg(debug_assertions)]
use std::fmt::Debug;

#[derive(Default, Clone)]
/// The IR of a sherpa
pub struct ParseState {
  pub name:     IString,
  pub comment:  String,
  pub code:     String,
  pub ast:      SherpaResult<Box<State>>,
  /// Collections of scanner based on TOKEN match statements
  pub scanners: Option<Map<IString, OrderedSet<DBTokenData>>>,
}

impl<'db> ParseState {
  pub fn build_scanners(
    &mut self,
    db: &'db ParserDatabase,
  ) -> Option<&Map<IString, OrderedSet<DBTokenData>>> {
    fn get_token_scanner_data(
      statement: &mut Statement,
      db: &ParserDatabase,
      scanners: &mut Map<IString, OrderedSet<DBTokenData>>,
    ) {
      match &mut statement.branch {
        Some(ASTNode::Matches(box Matches { mode, matches, meta }))
          if mode == InputType::TOKEN_STR =>
        {
          let mut scanner_data = OrderedSet::new();
          for m in matches {
            match m {
              ASTNode::IntMatch(m) => {
                for id in &m.vals {
                  scanner_data.insert(db.tok_data((*id as u32).into()).clone());
                }
                get_token_scanner_data(&mut m.statement, db, scanners);
              }
              _ => {}
            }
          }
          let name = ParseState::get_interned_scanner_name(&scanner_data, db.string_store());

          (*meta) = name.as_u64();

          scanners.insert(name, scanner_data);
        }
        _ => {}
      }
    }

    if self.scanners.is_none() {
      let mut scanners = Map::new();
      //Ensure we have build the ast of the IR code.
      self.build_ast(db.string_store());
      if let SherpaResult::Ok(ast) = self.get_ast_mut() {
        get_token_scanner_data(&mut ast.statement, db, &mut scanners);
      }
      self.scanners = Some(scanners);
    }
    self.scanners.as_ref()
  }

  pub fn get_interned_scanner_name(
    scanner_syms: &OrderedSet<DBTokenData>,
    string_store: &IStringStore,
  ) -> IString {
    Self::get_scanner_name(scanner_syms).intern(string_store)
  }

  pub fn get_scanner_name(scanner_syms: &OrderedSet<DBTokenData>) -> String {
    ("scan".to_string()
      + &create_u64_hash(scanner_syms.iter().map(|g| g.sym_id).collect::<OrderedSet<_>>())
        .to_string())
  }

  /// Should only be used on matches that read results from token scanners.
  pub fn get_scanner_name_from_matches(matches: &[ASTNode], db: &ParserDatabase) -> String {
    let mut vals = Array::new();

    for val in matches {
      match val {
        parser::ASTNode::IntMatch(int_match) => {
          vals.append(&mut int_match.vals.clone());
        }
        _ => {}
      }
    }

    Self::get_scanner_name(
      &vals.into_iter().map(|i| db.tok_data((i as usize).into()).clone()).collect(),
    )
  }

  /// Returns a reference to the AST.
  ///
  /// May be `None` if the ast
  /// has not yet been built through `build_ast`.
  ///
  /// May also be an `Err`
  /// if there was a problem building the ast.
  pub fn get_ast(&self) -> SherpaResult<&Box<State>> {
    self.ast.as_ref()
  }

  pub fn get_ast_mut(&mut self) -> SherpaResult<&mut Box<State>> {
    self.ast.as_mut()
  }

  /// Builds and returns a reference to the AST.
  ///
  /// May be an `Err` if there was a problem building the ast.
  pub fn build_ast(&mut self, s: &IStringStore) -> SherpaResult<&Box<State>> {
    if self.ast.is_none() {
      let code = String::from_utf8(self.source(s))?;

      self.ast = SherpaResult::from(parser::ast::ir_from((&code).into()));
    }

    self.get_ast()
  }

  pub fn source(&self, s: &IStringStore) -> Vec<u8> {
    let mut w = CodeWriter::new(vec![]);
    let name = self.name.to_string(s);

    let _ = &mut w + name.clone() + " =>\n" + self.code.as_str().replace("%%%%", name.as_str());

    let string = w.into_output();
    string
  }

  pub fn source_string(&self, s: &IStringStore) -> String {
    unsafe { String::from_utf8_unchecked(self.source(s)) }
  }
}

#[cfg(debug_assertions)]
impl ParseState {
  pub fn debug_string(&self, db: &ParserDatabase) -> String {
    format!(
      "ParseState{{
  name: {}
  code: [\n    {}\n  ]
  ast: {:#?}
  scan_fn: {:?}
}}",
      self.name.to_string(db.string_store()),
      &self.code.split("\n").collect::<Vec<_>>().join("\n    "),
      &self.get_ast(),
      self.scanners.as_ref().map(|s| s
        .iter()
        .map(|(name, _)| { name.to_string(db.string_store()) })
        .collect::<Vec<_>>())
    )
  }
}
