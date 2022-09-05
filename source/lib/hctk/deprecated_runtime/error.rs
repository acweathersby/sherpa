use std::fmt::Display;
use std::sync::Arc;

use crate::types::Token;
#[deprecated]
#[derive(Debug)]
pub struct TokenError {
  pub token:      Token,
  pub input:      Option<Arc<Vec<u8>>>,
  pub production: u32,
}

impl TokenError {
  pub fn new(production: u32, token: Token, input: Option<Arc<Vec<u8>>>) -> Self {
    TokenError { token, input, production }
  }
}

impl Display for TokenError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut token = self.token.clone();

    if token.is_empty() {
      token = token.to_length(1);
    }

    f.write_str(&token.blame(0, 0, "Unexpected Token"))
  }
}
