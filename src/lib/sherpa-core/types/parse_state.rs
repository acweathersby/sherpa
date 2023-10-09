use sherpa_rust_runtime::types::bytecode::MatchInputType;

use super::*;
use crate::{
  parser::{self, ASTNode, State},
  utils::create_u64_hash,
  writer::code_writer::CodeWriter,
};

pub type ParseStatesVec = Array<(IString, Box<ParseState>)>;
pub type ParseStatesMap = OrderedMap<IString, Box<ParseState>>;

#[allow(unused)]
#[cfg(debug_assertions)]
use std::fmt::Debug;
use std::io::Write;

#[derive(Default, Clone)]

/// The intermediate representation of a sherpa parser
pub struct ParseState {
  pub hash_name: IString,
  pub name: IString,
  pub precedence: u32,
  pub comment: String,
  pub code: String,
  pub ast: Option<Box<State>>,
  pub compile_error: Option<SherpaError>,
  /// Collections of scanner based on TOKEN match statements
  pub(crate) scanner: Option<(IString, OrderedSet<PrecedentDBTerm>)>,
  pub root: bool,
}

impl<'db> ParseState {
  pub fn get_scanner(&self) -> Option<&(IString, OrderedSet<PrecedentDBTerm>)> {
    self.scanner.as_ref()
  }

  pub fn get_interned_scanner_name(scanner_syms: &OrderedSet<PrecedentDBTerm>, string_store: &IStringStore) -> IString {
    Self::get_scanner_name(scanner_syms).intern(string_store)
  }

  pub fn get_scanner_name(scanner_syms: &OrderedSet<PrecedentDBTerm>) -> String {
    "scan".to_string() + &create_u64_hash(scanner_syms).to_string()
  }

  /// Returns a reference to the AST.
  ///
  /// May be `None` if the ast
  /// has not yet been built through `build_ast`.
  ///
  /// May also be an `Err`
  /// if there was a problem building the ast.
  pub fn get_ast(&self) -> SherpaResult<&Box<State>> {
    match (self.compile_error.as_ref(), self.ast.as_ref()) {
      (Some(err), None) => SherpaResult::Err(err.clone()),
      (None, Some(ast)) => SherpaResult::Ok(ast),
      _ => unreachable!(),
    }
  }

  pub fn get_ast_mut(&mut self) -> SherpaResult<&mut Box<State>> {
    match (self.compile_error.as_ref(), self.ast.as_mut()) {
      (Some(err), None) => SherpaResult::Err(err.clone()),
      (None, Some(ast)) => SherpaResult::Ok(ast),
      _ => unreachable!(),
    }
  }

  /// Returns the hash of the body of the state, ignore the state declaration
  /// header.
  pub fn get_canonical_hash(&self, db: &ParserDatabase) -> SherpaResult<u64> {
    Ok(create_u64_hash(self.print(db, false)?))
  }

  /// Builds and returns a reference to the AST.
  ///
  /// May be an `Err` if there was a problem building the ast.
  pub fn build_ast(&mut self, db: &ParserDatabase) -> SherpaResult<&Box<State>> {
    if self.ast.is_none() {
      let code = String::from_utf8(self.source(db))?;

      // println!("{code}");

      match parser::ast::ir_from((&code).into()) {
        Ok(ast) => self.ast = Some(ast),
        Err(err) => self.compile_error = Some(err.into()),
      }
    }

    self.get_ast()
  }

  pub fn source(&self, db: &ParserDatabase) -> Vec<u8> {
    let mut w = CodeWriter::new(vec![]);
    let name = self.hash_name.to_string(db.string_store());

    let _ = &mut w + name.clone() + " =>\n" + self.code.as_str().replace("%%%%", name.as_str());

    let string = w.into_output();
    string
  }

  pub fn source_string(&self, db: &ParserDatabase) -> String {
    unsafe { String::from_utf8_unchecked(self.source(db)) }
  }

  pub fn print(&self, db: &ParserDatabase, print_header: bool) -> SherpaResult<String> {
    let ast = self.get_ast()?;

    let mut cw = CodeWriter::new(vec![]);

    render_IR(db, &mut cw, &ASTNode::State(ast.clone()), print_header, MatchInputType::Default)?;

    unsafe { SherpaResult::Ok(String::from_utf8_unchecked(cw.into_output())) }
  }

  /// Creates a new version of this State with a source that matches the
  /// original state's AST.
  pub fn remap_source(&self, db: &'db ParserDatabase) -> SherpaResult<Self> {
    SherpaResult::Ok(Self {
      scanner: self.scanner.clone(),
      code: self.print(db, false)?,
      hash_name: self.hash_name,
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
  scanner: {:?}
}}",
      self.hash_name.to_string(db.string_store()),
      &self.code.split("\n").collect::<Vec<_>>().join("\n    "),
      &self.get_ast(),
      self.scanner.as_ref().map(|(name, _)| { name.to_string(db.string_store()) })
    )
  }
}

/// Renders a string from an IR AST
fn render_IR<T: Write>(
  db: &ParserDatabase,
  mut w: &mut CodeWriter<T>,
  node: &ASTNode,
  add_header: bool,
  match_type: MatchInputType,
) -> SherpaResult<()> {
  match node {
    ASTNode::State(box parser::State { id, statement, .. }) => {
      if add_header {
        w = w + &id.name + " =>";
      }
      render_IR(db, w, &ASTNode::Statement(statement.clone()), false, match_type)?;
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
        render_IR(db, w, node, false, match_type)?;
        if i < nodes.len() - 1 {
          w = w + " then";
        }
      }
    }
    ASTNode::Matches(box parser::Matches { matches, scanner, mode, .. }) => {
      w = w.indent().newline()? + "match: " + mode;

      if !scanner.is_empty() {
        w = w + " :" + scanner;
      }

      w = w + " {";

      w.indent();

      for m in matches {
        w = w.newline()?;
        render_IR(db, w, m, false, MatchInputType::from(mode.as_str()))?;
      }

      _ = (w.dedent().newline()? + "}").dedent().newline()?;
    }

    ASTNode::DefaultMatch(box parser::DefaultMatch { statement, .. }) => {
      w = w + "default {";

      render_IR(db, w, &ASTNode::Statement(statement.clone()), false, MatchInputType::Default)?;

      _ = w + " }";
    }

    ASTNode::IntMatch(box parser::IntMatch { vals, statement, .. }) => {
      match match_type {
        MatchInputType::Token => {
          w = w
            + "( "
            + vals
              .iter()
              .map(|v| {
                v.to_string()
                  + " /*"
                  + &db.token((*v as usize).into()).name.to_string(db.string_store()).replace("*", "%asterisk%")
                  + "*/"
              })
              .collect::<Vec<_>>()
              .join(" | ")
            + " )";
        }
        MatchInputType::Byte | MatchInputType::ByteScanless => {
          w = w
            + "( "
            + vals
              .iter()
              .map(|v| {
                v.to_string()
                  + " /*"
                  + &char::from_u32(*v as u32).map(|v| v.to_string()).unwrap_or(v.to_string()).replace("*", "%asterisk%")
                  + "*/"
              })
              .collect::<Vec<_>>()
              .join(" | ")
            + " )";
        }
        MatchInputType::NonTerminal => {
          w = w
            + "( "
            + vals
              .iter()
              .map(|v| {
                v.to_string() + " /*" + &db.nonterm_friendly_name_string((*v as usize).into()).replace("*", "%asterisk%") + "*/"
              })
              .collect::<Vec<_>>()
              .join(" | ")
            + " )";
        }
        _ => w = w + "( " + vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" | ") + " )",
      };

      w = w + " {";

      render_IR(db, w, &ASTNode::Statement(statement.clone()), false, MatchInputType::Default)?;

      _ = w + " }";
    }
    ASTNode::Shift(shift) => {
      if shift.skip {
        _ = w + " shift-skip " + &shift.ptr_type;
      } else {
        _ = w + " shift " + &shift.ptr_type;
      }
    }
    ASTNode::Peek(peek) => {
      if peek.skip {
        _ = w + " peek-skip " + &peek.ptr_type;
      } else {
        _ = w + " peek " + &peek.ptr_type;
      }
    }
    ASTNode::Reset(reset) => w.w(" reset")?.write(&reset.ptr_type)?,
    ASTNode::Pop(pop) => {
      w.w(" pop ")?.w(&(pop.popped_state.max(1)).to_string())?;
    }
    ASTNode::Fail(..) => w.write(" fail")?,
    ASTNode::Pass(..) => w.write(" pass")?,
    ASTNode::Accept(..) => w.write(" accept")?,
    ASTNode::SetLine(..) => w.write(" set-line")?,
    ASTNode::ReduceRaw(box parser::ReduceRaw { len, nonterminal_id, rule_id, .. }) => {
      _ = w + " reduce " + len.to_string() + " symbols to " + nonterminal_id.to_string() + " with rule " + rule_id.to_string();
    }
    ASTNode::SetTokenId(box parser::SetTokenId { id, .. }) => {
      _ = w + " set-tok " + id.to_string();
    }

    ASTNode::Gotos(box parser::Gotos { goto, pushes, fork }) => {
      if pushes.len() > 1 {
        w.indent().newline()?;
        for push in pushes {
          w = (w + " push " + &push.name + " then").newline()?;
        }

        if let Some(goto) = goto {
          w = w + " goto " + &goto.name;
        }

        if let Some(fork) = fork {
          w = w + " fork {";
          w.indent().newline()?;
          for init in &fork.paths {
            w = (w + &init.name).newline()?;
          }
          w.dedent().newline()?;
          w = w + "}";
        }
        w.dedent().newline()?;
      } else {
        for push in pushes {
          w = w + " push " + &push.name + " then";
        }

        if let Some(goto) = goto {
          w = w + " goto " + &goto.name;
        }

        if let Some(fork) = fork {
          w = w + " fork {";
          w.indent().newline()?;
          for init in &fork.paths {
            w = (w + &init.name).newline()?;
          }
          w.dedent().newline()?;
          _ = w + "}";
        }
      }
    }
    _ => {}
  };

  SherpaResult::Ok(())
}

pub fn print_IR(node: &ASTNode, db: &ParserDatabase) -> SherpaResult<String> {
  let mut cw = CodeWriter::new(vec![]);

  render_IR(db, &mut cw, node, false, MatchInputType::Default)?;

  unsafe { SherpaResult::Ok(String::from_utf8_unchecked(cw.into_output())) }
}
