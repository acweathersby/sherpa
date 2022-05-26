use std::{rc::Rc, sync::Arc};

use crate::token::kernel_token::KernelToken;


pub struct TokenError {
    pub token: KernelToken,
    pub input: Option<Arc<Vec<u8>>>,
    pub production: u32
}

impl TokenError {
    pub fn new(production: u32, token: KernelToken, input: Option<Arc<Vec<u8>>>) -> Self {
        TokenError { token, input, production }
    }

    pub fn report(&self) -> String {
        let mut string = String::from("Unexpected Token");

        if let Some(source) = &self.input {
            // find beginning of line starting at offset of token
            let root = self.token.cp_offset as usize;
            let mut beg = root;
            let mut end = root;
            let mut lines: usize = 0;
            let mut i = 0;

            while i < root {
                if source[i] == 10 {
                    lines += 1
                }
                i += 1
            }

            while beg > 0 && source[beg as usize] != 10 {
                beg -= 1;
            }

            while (end as usize) < source.len() && source[end as usize] != 10 {
                end += 1;
            }

            let slice = &source[(beg)..end];

            if let Ok(utf_string) = String::from_utf8(Vec::from(slice)) {
                let lines = format!("{}", lines);
                string += &format!(
                    "\n\n{}: {}\n{}\n",
                    &lines,
                    utf_string,
                    String::from(" ").repeat(lines.len() + 1 + root - beg) + "^",
                );
            }
        }

        string
    }
}
