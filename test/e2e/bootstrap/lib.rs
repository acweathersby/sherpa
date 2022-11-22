pub mod hc_symbol {
  include!(concat!(env!("OUT_DIR"), "/hc_parser/hc_symbol.rs"));
}

pub mod hc_ast {
  include!(concat!(env!("OUT_DIR"), "/hc_parser/ascript.rs"));
}

pub mod hc_productions {
  include!(concat!(env!("OUT_DIR"), "/hc_parser/hc_production.rs"));
}

#[cfg(test)]
mod test_symbol {
  use crate::hc_symbol::*;

  #[test]
  fn test_symbol_types() {
    // Terminal
    let node = AST::default_from("*\\++ (+)^test?");

    println!("{:#?}", node);

    if !node.is_ok() {
      panic!("Failed to parse input");
    }
  }
}

#[cfg(test)]
mod test_ascript {
  use crate::hc_ast::*;

  #[test]
  fn test_symbol_types() {
    // Terminal
    let node = AST::default_from("{ t_T, tlstr, tok }");

    println!("{:#?}", node);

    if !node.is_ok() {
      panic!("Failed to parse input");
    }
  }
}

#[cfg(test)]
mod test_production {
  use crate::hc_productions::*;

  #[test]
  fn test_symbol_types() {
    // Terminal
    let node = AST::grammar_from("
    
  NAME test 
  IMPORT scarilet/-test/.org as mango 
  EXPORT sym::name_clause as tom 
  
  <> T > (A|B) \n ast: { { t_Test } }

  <> T > (A|B) \n ast: { { t_Test } }

  <> T > ( [ A , B ] | R ) \n ast: { { t_Test } }
  
  ");

    println!("{:#?}", node);

    if !node.is_ok() {
      panic!("Failed to parse input");
    }
  }
}
