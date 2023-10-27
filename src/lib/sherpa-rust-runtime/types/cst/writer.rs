use super::super::*;
use std::io::Write;

pub struct Printer<'node, 'db> {
  node: &'node CSTNode,
  db:   &'db dyn RuntimeDatabase,
}

impl<'node, 'db> Printer<'node, 'db> {
  pub fn new<'a, 'b>(node: &'a CSTNode, db: &'b dyn RuntimeDatabase) -> Printer<'a, 'b> {
    Printer { node, db }
  }

  pub fn new_node(&self, node: &'node CSTNode) -> Self {
    Self { node, db: self.db }
  }

  pub fn write<W: Write>(&self, w: &mut W) -> std::io::Result<()> {
    use CSTNode::*;
    match self.node {
      Skipped(skipped) => {
        w.write(skipped.val.as_bytes())?;
      }
      Token(_, token) => {
        w.write(token.val.as_bytes())?;
      }
      MissingToken(.., missing) => {
        w.write(missing.val.as_bytes())?;
      }
      NonTerm(non_term) => {
        for node in &non_term.symbols {
          self.new_node(node).write(w)?;
        }
      }
      Alts(multi) => {
        if let Some(first) = multi.alternatives.first() {
          for node in &first.symbols {
            self.new_node(node).write(w)?;
          }
        }
      }
      Errata { .. } => {}
    }
    Ok(())
  }

  pub fn print(&self) {
    use CSTNode::*;
    match self.node {
      Skipped(..) => {
        //print!("{}", skipped.val);
      }
      Token(_, token) => {
        print!("{}", token.val);
      }
      MissingToken(.., missing) => {
        print!("{}", missing.val);
      }
      NonTerm(non_term) => {
        for node in &non_term.symbols {
          self.new_node(node).print();
        }
      }
      Alts(multi) => {
        if let Some(first) = multi.alternatives.first() {
          for node in &first.symbols {
            self.new_node(node).print();
          }
        }
      }
      Errata { .. } => {}
    }
  }

  pub fn print_all(&self) {
    println!("{}", self._print_all().join("\n"));
  }

  fn _print_all(&self) -> Vec<String> {
    use CSTNode::*;
    match self.node {
      Skipped(..) => {
        vec![Default::default()]
      }
      Token(_, token) => {
        vec![token.val.clone()]
      }
      MissingToken(.., missing) => {
        vec![missing.val.clone()]
      }
      NonTerm(non_term) => {
        let mut vals: Vec<String> = vec![];
        for node in &non_term.symbols {
          vals = self
            .new_node(node)
            ._print_all()
            .into_iter()
            .flat_map(|v| {
              if vals.len() > 0 {
                vals.iter().map(|a| -> String { a.clone() + &v }).collect::<Vec<_>>()
              } else {
                vec![v]
              }
            })
            .collect();
        }
        vals
      }
      Alts(multi) => {
        let mut out_vals = vec![];
        for alt in &multi.alternatives {
          let mut vals: Vec<String> = vec![];
          for node in &alt.symbols {
            vals = self
              .new_node(node)
              ._print_all()
              .into_iter()
              .flat_map(|v| {
                if vals.len() > 0 {
                  vals.iter().map(|a| -> String { a.clone() + &v }).collect::<Vec<_>>()
                } else {
                  vec![v]
                }
              })
              .collect();
          }
          out_vals.extend(vals);
        }
        out_vals
      }
      Errata { .. } => {
        vec![Default::default()]
      }
    }
  }
}
