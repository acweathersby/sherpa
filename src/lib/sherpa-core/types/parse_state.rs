use sherpa_rust_runtime::types::bytecode::InputType;

use super::*;
use crate::{
  get_goto_target_name,
  parser::{self, ASTNode, DefaultMatch, Matches, State, Statement},
  utils::create_u64_hash,
  writer::code_writer::{self, CodeWriter},
};

pub type ParseStatesVec = Array<(IString, Box<ParseState>)>;
pub type ParseStatesMap = Map<IString, Box<ParseState>>;

#[allow(unused)]
#[cfg(debug_assertions)]
use std::fmt::Debug;
use std::io::Write;

#[derive(Default, Clone)]

/// The intermediate representation of a sherpa parser
pub struct ParseState {
  pub name:     IString,
  pub comment:  String,
  pub code:     String,
  pub ast:      SherpaResult<Box<State>>,
  /// Collections of scanner based on TOKEN match statements
  pub scanners: Option<Map<IString, OrderedSet<DBTokenData>>>,
}

impl<'db> ParseState {
  pub fn build_scanners(&mut self, db: &'db ParserDatabase) -> Option<&Map<IString, OrderedSet<DBTokenData>>> {
    fn get_token_scanner_data(
      statement: &mut Statement,
      db: &ParserDatabase,
      scanners: &mut Map<IString, OrderedSet<DBTokenData>>,
    ) {
      match &mut statement.branch {
        Some(ASTNode::Matches(box Matches { mode, matches, meta, .. })) if mode == InputType::TOKEN_STR => {
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
      self.build_ast(db);
      if let SherpaResult::Ok(ast) = self.get_ast_mut() {
        get_token_scanner_data(&mut ast.statement, db, &mut scanners);
      }
      self.scanners = Some(scanners);
    }
    self.scanners.as_ref()
  }

  pub fn get_interned_scanner_name(scanner_syms: &OrderedSet<DBTokenData>, string_store: &IStringStore) -> IString {
    Self::get_scanner_name(scanner_syms).intern(string_store)
  }

  pub fn get_scanner_name(scanner_syms: &OrderedSet<DBTokenData>) -> String {
    ("scan".to_string() + &create_u64_hash(scanner_syms.iter().map(|g| g.sym_id).collect::<OrderedSet<_>>()).to_string())
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

    Self::get_scanner_name(&vals.into_iter().map(|i| db.tok_data((i as usize).into()).clone()).collect())
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

  /// Returns the hash of the body of the state, ignore the state declaration
  /// header.
  pub fn get_canonical_hash(&self) -> u64 {
    match self.get_ast() {
      SherpaResult::Ok(box State { statement, .. }) => create_u64_hash(statement),
      _ => 0,
    }
  }

  /// Builds and returns a reference to the AST.
  ///
  /// May be an `Err` if there was a problem building the ast.
  pub fn build_ast(&mut self, db: &ParserDatabase) -> SherpaResult<&Box<State>> {
    if self.ast.is_none() {
      let code = String::from_utf8(self.source(db))?;

      self.ast = SherpaResult::from(parser::ast::ir_from((&code).into()));
    }

    self.get_ast()
  }

  pub fn source(&self, db: &ParserDatabase) -> Vec<u8> {
    let mut w = CodeWriter::new(vec![]);
    let name = self.name.to_string(db.string_store());

    let _ = &mut w + name.clone() + " =>\n" + self.code.as_str().replace("%%%%", name.as_str());

    let string = w.into_output();
    string
  }

  pub fn source_string(&self, db: &ParserDatabase) -> String {
    unsafe { String::from_utf8_unchecked(self.source(db)) }
  }

  pub fn print(&self, db: &ParserDatabase, print_header: bool) -> SherpaResult<String> {
    if let SherpaResult::Ok(ast) = self.get_ast() {
      let mut cw = CodeWriter::new(vec![]);

      renderIR(db, &mut cw, &ASTNode::State(ast.clone()), print_header)?;

      unsafe { SherpaResult::Ok(String::from_utf8_unchecked(cw.into_output())) }
    } else {
      Default::default()
    }
  }

  /// Creates a new version of this State with a source that matches the
  /// original state's AST.
  pub fn remap_source(&self, db: &'db ParserDatabase) -> SherpaResult<Self> {
    SherpaResult::Ok(Self {
      scanners: self.scanners.clone(),
      code: self.print(db, false)?,
      name: self.name,
      ..Default::default()
    })
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
      self.scanners.as_ref().map(|s| s.iter().map(|(name, _)| { name.to_string(db.string_store()) }).collect::<Vec<_>>())
    )
  }
}

/// Renders a string from an IR AST
fn renderIR<T: Write>(db: &ParserDatabase, mut w: &mut CodeWriter<T>, node: &ASTNode, add_header: bool) -> SherpaResult<()> {
  match node {
    ASTNode::State(box parser::State { catches, id, statement, .. }) => {
      if add_header {
        w = (w + &id.name + " =>");
      }
      renderIR(db, w, &ASTNode::Statement(statement.clone()), false);
    }
    ASTNode::Statement(box parser::Statement { branch, non_branch, transitive }) => {
      let mut nodes = vec![];

      if let Some(transitive) = transitive {
        nodes.push(transitive);
      }

      for stmt in non_branch {
        nodes.push(stmt);
      }

      if let Some(branch) = branch {
        nodes.push(branch);
      }

      for (i, node) in nodes.iter().enumerate() {
        renderIR(db, w, node, false)?;
        if i < nodes.len() - 1 {
          w = w + " then";
        }
      }
    }
    ASTNode::Matches(box parser::Matches { matches, mode, .. }) => {
      w = (w.indent().newline()? + "match: " + mode + " {").indent();

      for m in matches {
        w = w.newline()?;
        renderIR(db, w, m, false)?;
      }

      w = (w.dedent().newline()? + "}").dedent().newline()?;
    }

    ASTNode::DefaultMatch(box parser::DefaultMatch { statement, .. }) => {
      w = w + "default {";

      renderIR(db, w, &ASTNode::Statement(statement.clone()), false);

      w = w + " }";
    }

    ASTNode::IntMatch(box parser::IntMatch { vals, statement, .. }) => {
      w = w + "( " + vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" | ") + " )";
      w = w + " {";

      renderIR(db, w, &ASTNode::Statement(statement.clone()), false);

      w = w + " }";
    }
    ASTNode::Peek(..) => w.write(" peek")?,
    ASTNode::Shift(..) => w.write(" shift")?,
    ASTNode::Skip(..) => w.write(" skip")?,
    ASTNode::Pop(..) => w.write(" pop")?,
    ASTNode::Scan(..) => w.write(" scan")?,
    ASTNode::Reset(..) => w.write(" reset")?,
    ASTNode::Fail(..) => w.write(" fail")?,
    ASTNode::Pass(..) => w.write(" pass")?,
    ASTNode::Accept(..) => w.write(" accept")?,
    ASTNode::ReduceRaw(box parser::ReduceRaw { len, prod_id, rule_id, .. }) => {
      w = (w + " reduce " + len.to_string() + " symbols to " + prod_id.to_string() + " with rule " + rule_id.to_string());
    }
    ASTNode::SetTokenId(box parser::SetTokenId { id, .. }) => {
      w = (w + " set-tok " + id.to_string());
    }

    ASTNode::Gotos(box parser::Gotos { goto, pushes }) => {
      if pushes.len() > 1 {
        w.indent().newline()?;
        for push in pushes {
          w = (w + " push " + get_goto_target_name(&push.prod) + " then").newline()?;
        }
        w = (w + " goto " + get_goto_target_name(&goto.prod));
        w.dedent().newline()?;
      } else {
        for push in pushes {
          w = (w + " push " + get_goto_target_name(&push.prod) + " then");
        }
        w = (w + " goto " + get_goto_target_name(&goto.prod));
      }
    }
    _ => {}
  };

  SherpaResult::Ok(())
}
